#define JEMALLOC_C_
#include "jemalloc/internal/jemalloc_preamble.h"
#include "jemalloc/internal/jemalloc_internal_includes.h"

#include "jemalloc/internal/assert.h"
#include "jemalloc/internal/atomic.h"

#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/extent_dss.h"
#include "jemalloc/internal/extent_mmap.h"
#include "jemalloc/internal/hook.h"
#include "jemalloc/internal/jemalloc_internal_types.h"
#include "jemalloc/internal/log.h"
#include "jemalloc/internal/malloc_io.h"
#include "jemalloc/internal/mutex.h"

#include "jemalloc/internal/rtree.h"
#include "jemalloc/internal/safety_check.h"

#include "jemalloc/internal/sc.h"

#include "jemalloc/internal/spin.h"
#include "jemalloc/internal/sz.h"

#include "jemalloc/internal/ticker.h"
#include "jemalloc/internal/util.h"

/******************************************************************************/
/* Data. */

/* Runtime configuration options. */
const char* je_malloc_conf
#ifndef _WIN32
  JEMALLOC_ATTR(weak)
#endif
    ;
bool opt_abort =
#ifdef JEMALLOC_DEBUG
  true
#else
  false
#endif
  ;
bool opt_abort_conf =
#ifdef JEMALLOC_DEBUG
  true
#else
  false
#endif
  ;
/* Intentionally default off, even with debug builds. */
bool opt_confirm_conf = false;
const char* opt_junk =
#if (defined(JEMALLOC_DEBUG) && defined(JEMALLOC_FILL))
  "true"
#else
  "false"
#endif
  ;
bool opt_junk_alloc =
#if (defined(JEMALLOC_DEBUG) && defined(JEMALLOC_FILL))
  true
#else
  false
#endif
  ;
bool opt_junk_free =
#if (defined(JEMALLOC_DEBUG) && defined(JEMALLOC_FILL))
  true
#else
  false
#endif
  ;

bool opt_utrace = false;
bool opt_xmalloc = false;
bool opt_zero = false;
unsigned opt_narenas = 0;

unsigned ncpus;

/* Protects arenas initialization. */
malloc_mutex_t arenas_lock;
/*
 * Arenas that are used to service external requests.  Not all elements of
 * the arenas array are necessarily used; arenas are created lazily as
 * needed.
 *
 * arenas[0..narenas_auto) are used for automatic multiplexing of threads
 * and arenas.  arenas[narenas_auto..narenas_total) are only used if the
 * application takes some action to create them and allocate from them.
 *
 * Points to an arena_t.
 */
JEMALLOC_ALIGNED(CACHELINE)
atomic_p_t arenas[MALLOCX_ARENA_LIMIT];
static atomic_u_t narenas_total; /* Use narenas_total_*(). */
/* Below three are read-only after initialization. */
static arena_t* a0; /* arenas[0]. */
unsigned narenas_auto;
unsigned manual_arena_base;

typedef enum {
  malloc_init_uninitialized = 3,
  malloc_init_a0_initialized = 2,
  malloc_init_recursible = 1,
  malloc_init_initialized = 0 /* Common case --> jnz. */
} malloc_init_t;
static malloc_init_t malloc_init_state = malloc_init_uninitialized;

/* False should be the common case.  Set to true to trigger initialization.
 */
bool malloc_slow = true;

/* When malloc_slow is true, set the corresponding bits for sanity check. */
enum {
  flag_opt_junk_alloc = (1U),
  flag_opt_junk_free = (1U << 1),
  flag_opt_zero = (1U << 2),
  flag_opt_utrace = (1U << 3),
  flag_opt_xmalloc = (1U << 4)
};
static uint8_t malloc_slow_flags;

#ifdef JEMALLOC_THREADED_INIT
/* Used to let the initializing thread recursively allocate. */
#define NO_INITIALIZER ((unsigned long)0)
#define INITIALIZER pthread_self()
#define IS_INITIALIZER (malloc_initializer == pthread_self())
static pthread_t malloc_initializer = NO_INITIALIZER;
#else
#define NO_INITIALIZER false
#define INITIALIZER true
#define IS_INITIALIZER malloc_initializer
static bool malloc_initializer = NO_INITIALIZER;
#endif

/* Used to avoid initialization races. */
#ifdef _WIN32
#if _WIN32_WINNT >= 0x0600
static malloc_mutex_t init_lock = SRWLOCK_INIT;
#else
static malloc_mutex_t init_lock;
static bool init_lock_initialized = false;

JEMALLOC_ATTR(constructor)
static void WINAPI _init_init_lock(void) {
  /*
   * If another constructor in the same binary is using mallctl to e.g.
   * set up extent hooks, it may end up running before this one, and
   * malloc_init_hard will crash trying to lock the uninitialized lock. So
   * we force an initialization of the lock in malloc_init_hard as well.
   * We don't try to care about atomicity of the accessed to the
   * init_lock_initialized boolean, since it really only matters early in
   * the process creation, before any separate thread normally starts
   * doing anything.
   */
  if (!init_lock_initialized) {
    malloc_mutex_init(
      &init_lock, "init", WITNESS_RANK_INIT, malloc_mutex_rank_exclusive);
  }
  init_lock_initialized = true;
}

#ifdef _MSC_VER
#pragma section(".CRT$XCU", read)
JEMALLOC_SECTION(".CRT$XCU")
JEMALLOC_ATTR(used)
static const void(WINAPI* init_init_lock)(void) = _init_init_lock;
#endif
#endif
#else
static malloc_mutex_t init_lock = MALLOC_MUTEX_INITIALIZER;
#endif

typedef struct {
  void* p;  /* Input pointer (as in realloc(p, s)). */
  size_t s; /* Request size. */
  void* r;  /* Result pointer. */
} malloc_utrace_t;

#ifdef JEMALLOC_UTRACE
#define UTRACE(a, b, c)                                                    \
  do {                                                                     \
    if (unlikely(opt_utrace)) {                                            \
      int utrace_serrno = errno;                                           \
      malloc_utrace_t ut;                                                  \
      ut.p = (a);                                                          \
      ut.s = (b);                                                          \
      ut.r = (c);                                                          \
      utrace(&ut, sizeof(ut));                                             \
      errno = utrace_serrno;                                               \
    }                                                                      \
  } while (0)
#else
#define UTRACE(a, b, c)
#endif

/* Whether encountered any invalid config options. */
static bool had_conf_error = false;

/******************************************************************************/
/*
 * Function prototypes for static functions that are referenced prior to
 * definition.
 */

static bool malloc_init_hard_a0(void);
static bool malloc_init_hard(void);

/******************************************************************************/
/*
 * Begin miscellaneous support functions.
 */

bool malloc_initialized(void) {
  return (malloc_init_state == malloc_init_initialized);
}

JEMALLOC_ALWAYS_INLINE bool malloc_init_a0(void) {
  if (unlikely(malloc_init_state == malloc_init_uninitialized)) {
    return malloc_init_hard_a0();
  }
  return false;
}

JEMALLOC_ALWAYS_INLINE bool malloc_init(void) {
  if (unlikely(!malloc_initialized()) && malloc_init_hard()) {
    return true;
  }
  return false;
}

/*
 * The a0*() functions are used instead of i{d,}alloc() in situations that
 * cannot tolerate TLS variable access.
 */

static void* a0ialloc(size_t size, bool zero, bool is_internal) {
  if (unlikely(malloc_init_a0())) { return NULL; }

  return iallocztm(TSDN_NULL, size, sz_size2index(size), zero, NULL,
    is_internal, arena_get(TSDN_NULL, 0, true), true);
}

static void a0idalloc(void* ptr, bool is_internal) {
  idalloctm(TSDN_NULL, ptr, NULL, NULL, is_internal, true);
}

static void* a0malloc(size_t size) { return a0ialloc(size, false, true); }

static void a0dalloc(void* ptr) { a0idalloc(ptr, true); }

/*
 * FreeBSD's libc uses the bootstrap_*() functions in bootstrap-senstive
 * situations that cannot tolerate TLS variable access (TLS allocation and
 * very early internal data structure initialization).
 */

static void* bootstrap_malloc(size_t size) {
  if (unlikely(size == 0)) { size = 1; }

  return a0ialloc(size, false, false);
}

static void* bootstrap_calloc(size_t num, size_t size) {
  size_t num_size;

  num_size = num * size;
  if (unlikely(num_size == 0)) {
    assert(num == 0 || size == 0);
    num_size = 1;
  }

  return a0ialloc(num_size, true, false);
}

static void bootstrap_free(void* ptr) {
  if (unlikely(ptr == NULL)) { return; }

  a0idalloc(ptr, false);
}

static void arena_set(unsigned ind, arena_t* arena) {
  atomic_store_p(&arenas[ind], arena, ATOMIC_RELEASE);
}

static void narenas_total_set(unsigned narenas) {
  atomic_store_u(&narenas_total, narenas, ATOMIC_RELEASE);
}

static void narenas_total_inc(void) {
  atomic_fetch_add_u(&narenas_total, 1, ATOMIC_RELEASE);
}

static unsigned narenas_total_get(void) {
  return atomic_load_u(&narenas_total, ATOMIC_ACQUIRE);
}

/* Create a new arena and insert it into the arenas array at index ind. */
static arena_t* arena_init_locked(
  tsdn_t* tsdn, unsigned ind, extent_hooks_t* extent_hooks) {
  arena_t* arena;

  assert(ind <= narenas_total_get());
  if (ind >= MALLOCX_ARENA_LIMIT) { return NULL; }
  if (ind == narenas_total_get()) { narenas_total_inc(); }

  /*
   * Another thread may have already initialized arenas[ind] if it's an
   * auto arena.
   */
  arena = arena_get(tsdn, ind, false);
  if (arena != NULL) {
    assert(arena_is_auto(arena));
    return arena;
  }

  /* Actually initialize the arena. */
  arena = arena_new(tsdn, ind, extent_hooks);

  return arena;
}

static void arena_new_create_background_thread(tsdn_t* tsdn, unsigned ind) {
  if (ind == 0) { return; }
  /*
   * Avoid creating a new background thread just for the huge arena, which
   * purges eagerly by default.
   */
  if (have_background_thread && !arena_is_huge(ind)) {
    if (background_thread_create(tsdn_tsd(tsdn), ind)) {
      malloc_printf("<jemalloc>: error in background thread "
                    "creation for arena %u. Abort.\n",
        ind);
      abort();
    }
  }
}

static arena_t* arena_init(
  tsdn_t* tsdn, unsigned ind, extent_hooks_t* extent_hooks) {
  arena_t* arena;

  malloc_mutex_lock(tsdn, &arenas_lock);
  arena = arena_init_locked(tsdn, ind, extent_hooks);
  malloc_mutex_unlock(tsdn, &arenas_lock);

  arena_new_create_background_thread(tsdn, ind);

  return arena;
}

static void arena_bind(tsd_t* tsd, unsigned ind, bool internal) {
  arena_t* arena = arena_get(tsd_tsdn(tsd), ind, false);
  arena_nthreads_inc(arena, internal);

  if (internal) {
    tsd_iarena_set(tsd, arena);
  } else {
    tsd_arena_set(tsd, arena);
    unsigned shard
      = atomic_fetch_add_u(&arena->binshard_next, 1, ATOMIC_RELAXED);
    tsd_binshards_t* bins = tsd_binshardsp_get(tsd);
    for (unsigned i = 0; i < SC_NBINS; i++) {
      assert(bin_infos[i].n_shards > 0
        && bin_infos[i].n_shards <= BIN_SHARDS_MAX);
      bins->binshard[i] = shard % bin_infos[i].n_shards;
    }
  }
}

static void arena_migrate(tsd_t* tsd, unsigned oldind, unsigned newind) {
  arena_t *oldarena, *newarena;

  oldarena = arena_get(tsd_tsdn(tsd), oldind, false);
  newarena = arena_get(tsd_tsdn(tsd), newind, false);
  arena_nthreads_dec(oldarena, false);
  arena_nthreads_inc(newarena, false);
  tsd_arena_set(tsd, newarena);
}

static void arena_unbind(tsd_t* tsd, unsigned ind, bool internal) {
  arena_t* arena;

  arena = arena_get(tsd_tsdn(tsd), ind, false);
  arena_nthreads_dec(arena, internal);

  if (internal) {
    tsd_iarena_set(tsd, NULL);
  } else {
    tsd_arena_set(tsd, NULL);
  }
}

static arena_tdata_t* arena_tdata_get_hard(tsd_t* tsd, unsigned ind) {
  arena_tdata_t *tdata, *arenas_tdata_old;
  arena_tdata_t* arenas_tdata = tsd_arenas_tdata_get(tsd);
  unsigned narenas_tdata_old, i;
  unsigned narenas_tdata = tsd_narenas_tdata_get(tsd);
  unsigned narenas_actual = narenas_total_get();

  /*
   * Dissociate old tdata array (and set up for deallocation upon return)
   * if it's too small.
   */
  if (arenas_tdata != NULL && narenas_tdata < narenas_actual) {
    arenas_tdata_old = arenas_tdata;
    narenas_tdata_old = narenas_tdata;
    arenas_tdata = NULL;
    narenas_tdata = 0;
    tsd_arenas_tdata_set(tsd, arenas_tdata);
    tsd_narenas_tdata_set(tsd, narenas_tdata);
  } else {
    arenas_tdata_old = NULL;
    narenas_tdata_old = 0;
  }

  /* Allocate tdata array if it's missing. */
  if (arenas_tdata == NULL) {
    bool* arenas_tdata_bypassp = tsd_arenas_tdata_bypassp_get(tsd);
    narenas_tdata = (ind < narenas_actual) ? narenas_actual : ind + 1;

    if (tsd_nominal(tsd) && !*arenas_tdata_bypassp) {
      *arenas_tdata_bypassp = true;
      arenas_tdata
        = (arena_tdata_t*)a0malloc(sizeof(arena_tdata_t) * narenas_tdata);
      *arenas_tdata_bypassp = false;
    }
    if (arenas_tdata == NULL) {
      tdata = NULL;
      goto label_return;
    }
    assert(tsd_nominal(tsd) && !*arenas_tdata_bypassp);
    tsd_arenas_tdata_set(tsd, arenas_tdata);
    tsd_narenas_tdata_set(tsd, narenas_tdata);
  }

  /*
   * Copy to tdata array.  It's possible that the actual number of arenas
   * has increased since narenas_total_get() was called above, but that
   * causes no correctness issues unless two threads concurrently execute
   * the arenas.create mallctl, which we trust mallctl synchronization to
   * prevent.
   */

  /* Copy/initialize tickers. */
  for (i = 0; i < narenas_actual; i++) {
    if (i < narenas_tdata_old) {
      ticker_copy(
        &arenas_tdata[i].decay_ticker, &arenas_tdata_old[i].decay_ticker);
    } else {
      ticker_init(&arenas_tdata[i].decay_ticker, DECAY_NTICKS_PER_UPDATE);
    }
  }
  if (narenas_tdata > narenas_actual) {
    memset(&arenas_tdata[narenas_actual], 0,
      sizeof(arena_tdata_t) * (narenas_tdata - narenas_actual));
  }

  /* Read the refreshed tdata array. */
  tdata = &arenas_tdata[ind];
label_return:
  if (arenas_tdata_old != NULL) { a0dalloc(arenas_tdata_old); }
  return tdata;
}

/* Slow path, called only by arena_choose(). */
arena_t* arena_choose_hard(tsd_t* tsd, bool internal) {
  arena_t* ret JEMALLOC_CC_SILENCE_INIT(NULL);

  if (have_percpu_arena && PERCPU_ARENA_ENABLED(opt_percpu_arena)) {
    unsigned choose = percpu_arena_choose();
    ret = arena_get(tsd_tsdn(tsd), choose, true);
    assert(ret != NULL);
    arena_bind(tsd, arena_ind_get(ret), false);
    arena_bind(tsd, arena_ind_get(ret), true);

    return ret;
  }

  if (narenas_auto > 1) {
    unsigned i, j, choose[2], first_null;
    bool is_new_arena[2];

    /*
     * Determine binding for both non-internal and internal
     * allocation.
     *
     *   choose[0]: For application allocation.
     *   choose[1]: For internal metadata allocation.
     */

    for (j = 0; j < 2; j++) {
      choose[j] = 0;
      is_new_arena[j] = false;
    }

    first_null = narenas_auto;
    malloc_mutex_lock(tsd_tsdn(tsd), &arenas_lock);
    assert(arena_get(tsd_tsdn(tsd), 0, false) != NULL);
    for (i = 1; i < narenas_auto; i++) {
      if (arena_get(tsd_tsdn(tsd), i, false) != NULL) {
        /*
         * Choose the first arena that has the lowest
         * number of threads assigned to it.
         */
        for (j = 0; j < 2; j++) {
          if (arena_nthreads_get(arena_get(tsd_tsdn(tsd), i, false), !!j)
            < arena_nthreads_get(
              arena_get(tsd_tsdn(tsd), choose[j], false), !!j)) {
            choose[j] = i;
          }
        }
      } else if (first_null == narenas_auto) {
        /*
         * Record the index of the first uninitialized
         * arena, in case all extant arenas are in use.
         *
         * NB: It is possible for there to be
         * discontinuities in terms of initialized
         * versus uninitialized arenas, due to the
         * "thread.arena" mallctl.
         */
        first_null = i;
      }
    }

    for (j = 0; j < 2; j++) {
      if (arena_nthreads_get(
            arena_get(tsd_tsdn(tsd), choose[j], false), !!j)
          == 0
        || first_null == narenas_auto) {
        /*
         * Use an unloaded arena, or the least loaded
         * arena if all arenas are already initialized.
         */
        if (!!j == internal) {
          ret = arena_get(tsd_tsdn(tsd), choose[j], false);
        }
      } else {
        arena_t* arena;

        /* Initialize a new arena. */
        choose[j] = first_null;
        arena = arena_init_locked(
          tsd_tsdn(tsd), choose[j], (extent_hooks_t*)&extent_hooks_default);
        if (arena == NULL) {
          malloc_mutex_unlock(tsd_tsdn(tsd), &arenas_lock);
          return NULL;
        }
        is_new_arena[j] = true;
        if (!!j == internal) { ret = arena; }
      }
      arena_bind(tsd, choose[j], !!j);
    }
    malloc_mutex_unlock(tsd_tsdn(tsd), &arenas_lock);

    for (j = 0; j < 2; j++) {
      if (is_new_arena[j]) {
        assert(choose[j] > 0);
        arena_new_create_background_thread(tsd_tsdn(tsd), choose[j]);
      }
    }

  } else {
    ret = arena_get(tsd_tsdn(tsd), 0, false);
    arena_bind(tsd, 0, false);
    arena_bind(tsd, 0, true);
  }

  return ret;
}

void iarena_cleanup(tsd_t* tsd) {
  arena_t* iarena;

  iarena = tsd_iarena_get(tsd);
  if (iarena != NULL) { arena_unbind(tsd, arena_ind_get(iarena), true); }
}

void arena_cleanup(tsd_t* tsd) {
  arena_t* arena;

  arena = tsd_arena_get(tsd);
  if (arena != NULL) { arena_unbind(tsd, arena_ind_get(arena), false); }
}

void arenas_tdata_cleanup(tsd_t* tsd) {
  arena_tdata_t* arenas_tdata;

  /* Prevent tsd->arenas_tdata from being (re)created. */
  *tsd_arenas_tdata_bypassp_get(tsd) = true;

  arenas_tdata = tsd_arenas_tdata_get(tsd);
  if (arenas_tdata != NULL) {
    tsd_arenas_tdata_set(tsd, NULL);
    a0dalloc(arenas_tdata);
  }
}

static void stats_print_atexit(void) {
  if (config_stats) {
    tsdn_t* tsdn;
    unsigned narenas, i;

    tsdn = tsdn_fetch();

    /*
     * Merge stats from extant threads.  This is racy, since
     * individual threads do not lock when recording tcache stats
     * events.  As a consequence, the final stats may be slightly
     * out of date by the time they are reported, if other threads
     * continue to allocate.
     */
    for (i = 0, narenas = narenas_total_get(); i < narenas; i++) {
      arena_t* arena = arena_get(tsdn, i, false);
      if (arena != NULL) {
        tcache_t* tcache;

        malloc_mutex_lock(tsdn, &arena->tcache_ql_mtx);
        ql_foreach(tcache, &arena->tcache_ql, link) {
          tcache_stats_merge(tsdn, tcache, arena);
        }
        malloc_mutex_unlock(tsdn, &arena->tcache_ql_mtx);
      }
    }
  }
  je_malloc_stats_print(NULL, NULL, opt_stats_print_opts);
}

/*
 * Ensure that we don't hold any locks upon entry to or exit from allocator
 * code (in a "broad" sense that doesn't count a reentrant allocation as an
 * entrance or exit).
 */
JEMALLOC_ALWAYS_INLINE void check_entry_exit_locking(tsdn_t* tsdn) {
  if (!config_debug) { return; }
  if (tsdn_null(tsdn)) { return; }
  tsd_t* tsd = tsdn_tsd(tsdn);
  /*
   * It's possible we hold locks at entry/exit if we're in a nested
   * allocation.
   */
  int8_t reentrancy_level = tsd_reentrancy_level_get(tsd);
  if (reentrancy_level != 0) { return; }
  witness_assert_lockless(tsdn_witness_tsdp_get(tsdn));
}

/*
 * End miscellaneous support functions.
 */
/******************************************************************************/
/*
 * Begin initialization functions.
 */

static char* jemalloc_secure_getenv(const char* name) {
#ifdef JEMALLOC_HAVE_SECURE_GETENV
  return secure_getenv(name);
#else
#ifdef JEMALLOC_HAVE_ISSETUGID
  if (issetugid() != 0) { return NULL; }
#endif
  return getenv(name);
#endif
}

static unsigned malloc_ncpus(void) {
  long result;

#ifdef _WIN32
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  result = si.dwNumberOfProcessors;
#elif defined(JEMALLOC_GLIBC_MALLOC_HOOK) && defined(CPU_COUNT)
  /*
   * glibc >= 2.6 has the CPU_COUNT macro.
   *
   * glibc's sysconf() uses isspace().  glibc allocates for the first time
   * *before* setting up the isspace tables.  Therefore we need a
   * different method to get the number of CPUs.
   */
  {
    cpu_set_t set;

    pthread_getaffinity_np(pthread_self(), sizeof(set), &set);
    result = CPU_COUNT(&set);
  }
#else
  result = sysconf(_SC_NPROCESSORS_ONLN);
#endif
  return ((result == -1) ? 1 : (unsigned)result);
}

static void init_opt_stats_print_opts(const char* v, size_t vlen) {
  size_t opts_len = cstr_len(opt_stats_print_opts);
  assert(opts_len <= stats_print_tot_num_options);

  for (size_t i = 0; i < vlen; i++) {
    switch (v[i]) {
#define OPTION(o, v, d, s)                                                 \
  case o: break;
      STATS_PRINT_OPTIONS
#undef OPTION
    default: continue;
    }

    if (strchr(opt_stats_print_opts, v[i]) != NULL) {
      /* Ignore repeated. */
      continue;
    }

    opt_stats_print_opts[opts_len++] = v[i];
    opt_stats_print_opts[opts_len] = '\0';
    assert(opts_len <= stats_print_tot_num_options);
  }
  assert(opts_len == cstr_len(opt_stats_print_opts));
}

/* Reads the next size pair in a multi-sized option. */
static bool malloc_conf_multi_sizes_next(const char** slab_size_segment_cur,
  size_t* vlen_left, size_t* slab_start, size_t* slab_end,
  size_t* new_size) {
  const char* cur = *slab_size_segment_cur;
  char* end;
  uintmax_t um;

  set_errno(0);

  /* First number, then '-' */
  um = malloc_strtoumax(cur, &end, 0);
  if (get_errno() != 0 || *end != '-') { return true; }
  *slab_start = (size_t)um;
  cur = end + 1;

  /* Second number, then ':' */
  um = malloc_strtoumax(cur, &end, 0);
  if (get_errno() != 0 || *end != ':') { return true; }
  *slab_end = (size_t)um;
  cur = end + 1;

  /* Last number */
  um = malloc_strtoumax(cur, &end, 0);
  if (get_errno() != 0) { return true; }
  *new_size = (size_t)um;

  /* Consume the separator if there is one. */
  if (*end == '|') { end++; }

  *vlen_left -= end - *slab_size_segment_cur;
  *slab_size_segment_cur = end;

  return false;
}

static bool malloc_conf_next(char const** opts_p, char const** k_p,
  size_t* klen_p, char const** v_p, size_t* vlen_p) {
  bool accept;
  const char* opts = *opts_p;

  *k_p = opts;

  for (accept = false; !accept;) {
    switch (*opts) {
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '_': opts++; break;
    case ':':
      opts++;
      *klen_p = (uintptr_t)opts - 1 - (uintptr_t)*k_p;
      *v_p = opts;
      accept = true;
      break;
    case '\0':
      if (opts != *opts_p) {
        malloc_write("<jemalloc>: Conf string ends "
                     "with key\n");
      }
      return true;
    default:
      malloc_write("<jemalloc>: Malformed conf string\n");
      return true;
    }
  }

  for (accept = false; !accept;) {
    switch (*opts) {
    case ',':
      opts++;
      /*
       * Look ahead one character here, because the next time
       * this function is called, it will assume that end of
       * input has been cleanly reached if no input remains,
       * but we have optimistically already consumed the
       * comma if one exists.
       */
      if (*opts == '\0') {
        malloc_write("<jemalloc>: Conf string ends "
                     "with comma\n");
      }
      *vlen_p = (uintptr_t)opts - 1 - (uintptr_t)*v_p;
      accept = true;
      break;
    case '\0':
      *vlen_p = (uintptr_t)opts - (uintptr_t)*v_p;
      accept = true;
      break;
    default: opts++; break;
    }
  }

  *opts_p = opts;
  return false;
}

static void malloc_abort_invalid_conf(void) {
  assert(opt_abort_conf);
  malloc_printf("<jemalloc>: Abort (abort_conf:true) on invalid conf "
                "value (see above).\n");
  abort();
}

static void malloc_conf_error(
  const char* msg, const char* k, size_t klen, const char* v, size_t vlen) {
  malloc_printf(
    "<jemalloc>: %s: %.*s:%.*s\n", msg, (int)klen, k, (int)vlen, v);
  /* If abort_conf is set, error out after processing all options. */
  const char* experimental = "experimental_";
  if (strncmp(k, experimental, cstr_len(experimental)) == 0) {
    /* However, tolerate experimental features. */
    return;
  }
  had_conf_error = true;
}

static void malloc_slow_flag_init(void) {
  /*
   * Combine the runtime options into malloc_slow for fast path.  Called
   * after processing all the options.
   */
  malloc_slow_flags |= (opt_junk_alloc ? flag_opt_junk_alloc : 0)
    | (opt_junk_free ? flag_opt_junk_free : 0)
    | (opt_zero ? flag_opt_zero : 0) | (opt_utrace ? flag_opt_utrace : 0)
    | (opt_xmalloc ? flag_opt_xmalloc : 0);

  malloc_slow = (malloc_slow_flags != 0);
}

/* Number of sources for initializing malloc_conf */
#define MALLOC_CONF_NSOURCES 4

static const char* obtain_malloc_conf(
  unsigned which_source, char buf[PATH_MAX + 1]) {
  if (config_debug) {
    static unsigned read_source = 0;
    /*
     * Each source should only be read once, to minimize # of
     * syscalls on init.
     */
    assert(read_source++ == which_source);
  }
  assert(which_source < MALLOC_CONF_NSOURCES);

  const char* ret;
  switch (which_source) {
  case 0: ret = config_malloc_conf; break;
  case 1:
    if (je_malloc_conf != NULL) {
      /* Use options that were compiled into the program. */
      ret = je_malloc_conf;
    } else {
      /* No configuration specified. */
      ret = NULL;
    }
    break;
  case 2: {
    ssize_t linklen = 0;
#ifndef _WIN32
    int saved_errno = errno;
    const char* linkname =
#ifdef JEMALLOC_PREFIX
      "/etc/" JEMALLOC_PREFIX "malloc.conf"
#else
      "/etc/malloc.conf"
#endif
      ;

    /*
     * Try to use the contents of the "/etc/malloc.conf" symbolic
     * link's name.
     */
#ifndef JEMALLOC_READLINKAT
    linklen = readlink(linkname, buf, PATH_MAX);
#else
    linklen = readlinkat(AT_FDCWD, linkname, buf, PATH_MAX);
#endif
    if (linklen == -1) {
      /* No configuration specified. */
      linklen = 0;
      /* Restore errno. */
      set_errno(saved_errno);
    }
#endif
    buf[linklen] = '\0';
    ret = buf;
    break;
  }
  case 3: {
    const char* envname =
#ifdef JEMALLOC_PREFIX
      JEMALLOC_CPREFIX "MALLOC_CONF"
#else
      "MALLOC_CONF"
#endif
      ;

    if ((ret = jemalloc_secure_getenv(envname)) != NULL) {
      /*
       * Do nothing; opts is already initialized to the value
       * of the MALLOC_CONF environment variable.
       */
    } else {
      /* No configuration specified. */
      ret = NULL;
    }
    break;
  }
  default: not_reached(); ret = NULL;
  }
  return ret;
}

static void malloc_conf_init_helper(sc_data_t* sc_data,
  unsigned bin_shard_sizes[SC_NBINS], bool initial_call,
  const char* opts_cache[MALLOC_CONF_NSOURCES], char buf[PATH_MAX + 1]) {
  static const char* opts_explain[MALLOC_CONF_NSOURCES]
    = { "string specified via --with-malloc-conf",
        "string pointed to by the global variable malloc_conf",
        "\"name\" of the file referenced by the symbolic link named "
        "/etc/malloc.conf",
        "value of the environment variable MALLOC_CONF" };
  unsigned i;
  const char *opts, *k, *v;
  size_t klen, vlen;

  for (i = 0; i < MALLOC_CONF_NSOURCES; i++) {
    /* Get runtime configuration. */
    if (initial_call) { opts_cache[i] = obtain_malloc_conf(i, buf); }
    opts = opts_cache[i];
    if (!initial_call && opt_confirm_conf) {
      malloc_printf("<jemalloc>: malloc_conf #%u (%s): \"%s\"\n", i + 1,
        opts_explain[i], opts != NULL ? opts : "");
    }
    if (opts == NULL) { continue; }

    while (
      *opts != '\0' && !malloc_conf_next(&opts, &k, &klen, &v, &vlen)) {

#define CONF_ERROR(msg, k, klen, v, vlen)                                  \
  if (!initial_call) {                                                     \
    malloc_conf_error(msg, k, klen, v, vlen);                              \
    cur_opt_valid = false;                                                 \
  }
#define CONF_CONTINUE                                                      \
  {                                                                        \
    if (!initial_call && opt_confirm_conf && cur_opt_valid) {              \
      malloc_printf("<jemalloc>: -- "                                      \
                    "Set conf value: %.*s:%.*s"                            \
                    "\n",                                                  \
        (int)klen, k, (int)vlen, v);                                       \
    }                                                                      \
    continue;                                                              \
  }
#define CONF_MATCH(n) (sizeof(n) - 1 == klen && strncmp(n, k, klen) == 0)
#define CONF_MATCH_VALUE(n)                                                \
  (sizeof(n) - 1 == vlen && strncmp(n, v, vlen) == 0)
#define CONF_HANDLE_BOOL(o, n)                                             \
  if (CONF_MATCH(n)) {                                                     \
    if (CONF_MATCH_VALUE("true")) {                                        \
      o = true;                                                            \
    } else if (CONF_MATCH_VALUE("false")) {                                \
      o = false;                                                           \
    } else {                                                               \
      CONF_ERROR("Invalid conf value", k, klen, v, vlen);                  \
    }                                                                      \
    CONF_CONTINUE;                                                         \
  }
      /*
       * One of the CONF_MIN macros below expands, in one of the use
       * points, to "unsigned integer < 0", which is always false,
       * triggering the GCC -Wtype-limits warning, which we disable here
       * and re-enable below.
       */
      JEMALLOC_DIAGNOSTIC_PUSH
      JEMALLOC_DIAGNOSTIC_IGNORE_TYPE_LIMITS

#define CONF_DONT_CHECK_MIN(um, min) false
#define CONF_CHECK_MIN(um, min) ((um) < (min))
#define CONF_DONT_CHECK_MAX(um, max) false
#define CONF_CHECK_MAX(um, max) ((um) > (max))
#define CONF_HANDLE_T_U(t, o, n, min, max, check_min, check_max, clip)     \
  if (CONF_MATCH(n)) {                                                     \
    uintmax_t um;                                                          \
    char* end;                                                             \
                                                                           \
    set_errno(0);                                                          \
    um = malloc_strtoumax(v, &end, 0);                                     \
    if (get_errno() != 0 || (uintptr_t)end - (uintptr_t)v != vlen) {       \
      CONF_ERROR("Invalid conf value", k, klen, v, vlen);                  \
    } else if (clip) {                                                     \
      if (check_min(um, (t)(min))) {                                       \
        o = (t)(min);                                                      \
      } else if (check_max(um, (t)(max))) {                                \
        o = (t)(max);                                                      \
      } else {                                                             \
        o = (t)um;                                                         \
      }                                                                    \
    } else {                                                               \
      if (check_min(um, (t)(min)) || check_max(um, (t)(max))) {            \
        CONF_ERROR("Out-of-range "                                         \
                   "conf value",                                           \
          k, klen, v, vlen);                                               \
      } else {                                                             \
        o = (t)um;                                                         \
      }                                                                    \
    }                                                                      \
    CONF_CONTINUE;                                                         \
  }
#define CONF_HANDLE_UNSIGNED(o, n, min, max, check_min, check_max, clip)   \
  CONF_HANDLE_T_U(unsigned, o, n, min, max, check_min, check_max, clip)
#define CONF_HANDLE_SIZE_T(o, n, min, max, check_min, check_max, clip)     \
  CONF_HANDLE_T_U(size_t, o, n, min, max, check_min, check_max, clip)
#define CONF_HANDLE_SSIZE_T(o, n, min, max)                                \
  if (CONF_MATCH(n)) {                                                     \
    long l;                                                                \
    char* end;                                                             \
                                                                           \
    set_errno(0);                                                          \
    l = strtol(v, &end, 0);                                                \
    if (get_errno() != 0 || (uintptr_t)end - (uintptr_t)v != vlen) {       \
      CONF_ERROR("Invalid conf value", k, klen, v, vlen);                  \
    } else if (l < (ssize_t)(min) || l > (ssize_t)(max)) {                 \
      CONF_ERROR("Out-of-range conf value", k, klen, v, vlen);             \
    } else {                                                               \
      o = l;                                                               \
    }                                                                      \
    CONF_CONTINUE;                                                         \
  }
#define CONF_HANDLE_CHAR_P(o, n, d)                                        \
  if (CONF_MATCH(n)) {                                                     \
    size_t cpylen = (vlen <= sizeof(o) - 1) ? vlen : sizeof(o) - 1;        \
    strncpy(o, v, cpylen);                                                 \
    o[cpylen] = '\0';                                                      \
    CONF_CONTINUE;                                                         \
  }

      bool cur_opt_valid = true;

      CONF_HANDLE_BOOL(opt_confirm_conf, "confirm_conf")
      if (initial_call) { continue; }

      CONF_HANDLE_BOOL(opt_abort, "abort")
      CONF_HANDLE_BOOL(opt_abort_conf, "abort_conf")
      if (strncmp("metadata_thp", k, klen) == 0) {
        int i;
        bool match = false;
        for (i = 0; i < metadata_thp_mode_limit; i++) {
          if (strncmp(metadata_thp_mode_names[i], v, vlen) == 0) {
            opt_metadata_thp = i;
            match = true;
            break;
          }
        }
        if (!match) { CONF_ERROR("Invalid conf value", k, klen, v, vlen); }
        CONF_CONTINUE;
      }
      CONF_HANDLE_BOOL(opt_retain, "retain")
      if (strncmp("dss", k, klen) == 0) {
        int i;
        bool match = false;
        for (i = 0; i < dss_prec_limit; i++) {
          if (strncmp(dss_prec_names[i], v, vlen) == 0) {
            if (extent_dss_prec_set(i)) {
              CONF_ERROR("Error setting dss", k, klen, v, vlen);
            } else {
              opt_dss = dss_prec_names[i];
              match = true;
              break;
            }
          }
        }
        if (!match) { CONF_ERROR("Invalid conf value", k, klen, v, vlen); }
        CONF_CONTINUE;
      }
      CONF_HANDLE_UNSIGNED(opt_narenas, "narenas", 1, UINT_MAX,
        CONF_CHECK_MIN, CONF_DONT_CHECK_MAX, false)
      if (CONF_MATCH("bin_shards")) {
        const char* bin_shards_segment_cur = v;
        size_t vlen_left = vlen;
        do {
          size_t size_start;
          size_t size_end;
          size_t nshards;
          bool err = malloc_conf_multi_sizes_next(&bin_shards_segment_cur,
            &vlen_left, &size_start, &size_end, &nshards);
          if (err
            || bin_update_shard_size(
              bin_shard_sizes, size_start, size_end, nshards)) {
            CONF_ERROR("Invalid settings for "
                       "bin_shards",
              k, klen, v, vlen);
            break;
          }
        } while (vlen_left > 0);
        CONF_CONTINUE;
      }
      CONF_HANDLE_SSIZE_T(opt_dirty_decay_ms, "dirty_decay_ms", -1,
        NSTIME_SEC_MAX * KQU(1000) < QU(SSIZE_MAX)
          ? NSTIME_SEC_MAX * KQU(1000)
          : SSIZE_MAX);
      CONF_HANDLE_SSIZE_T(opt_muzzy_decay_ms, "muzzy_decay_ms", -1,
        NSTIME_SEC_MAX * KQU(1000) < QU(SSIZE_MAX)
          ? NSTIME_SEC_MAX * KQU(1000)
          : SSIZE_MAX);
      CONF_HANDLE_BOOL(opt_stats_print, "stats_print")
      if (CONF_MATCH("stats_print_opts")) {
        init_opt_stats_print_opts(v, vlen);
        CONF_CONTINUE;
      }
      if (config_fill) {
        if (CONF_MATCH("junk")) {
          if (CONF_MATCH_VALUE("true")) {
            opt_junk = "true";
            opt_junk_alloc = opt_junk_free = true;
          } else if (CONF_MATCH_VALUE("false")) {
            opt_junk = "false";
            opt_junk_alloc = opt_junk_free = false;
          } else if (CONF_MATCH_VALUE("alloc")) {
            opt_junk = "alloc";
            opt_junk_alloc = true;
            opt_junk_free = false;
          } else if (CONF_MATCH_VALUE("free")) {
            opt_junk = "free";
            opt_junk_alloc = false;
            opt_junk_free = true;
          } else {
            CONF_ERROR("Invalid conf value", k, klen, v, vlen);
          }
          CONF_CONTINUE;
        }
        CONF_HANDLE_BOOL(opt_zero, "zero")
      }
      if (config_utrace) { CONF_HANDLE_BOOL(opt_utrace, "utrace") }
      if (config_xmalloc) { CONF_HANDLE_BOOL(opt_xmalloc, "xmalloc") }
      CONF_HANDLE_BOOL(opt_tcache, "tcache")
      CONF_HANDLE_SSIZE_T(
        opt_lg_tcache_max, "lg_tcache_max", -1, (sizeof(size_t) << 3) - 1)

      /*
       * The runtime option of oversize_threshold remains
       * undocumented.  It may be tweaked in the next major
       * release (6.0).  The default value 8M is rather
       * conservative / safe.  Tuning it further down may
       * improve fragmentation a bit more, but may also cause
       * contention on the huge arena.
       */
      CONF_HANDLE_SIZE_T(opt_oversize_threshold, "oversize_threshold", 0,
        SC_LARGE_MAXCLASS, CONF_DONT_CHECK_MIN, CONF_CHECK_MAX, false)
      CONF_HANDLE_SIZE_T(opt_lg_extent_max_active_fit,
        "lg_extent_max_active_fit", 0, (sizeof(size_t) << 3),
        CONF_DONT_CHECK_MIN, CONF_CHECK_MAX, false)

      if (strncmp("percpu_arena", k, klen) == 0) {
        bool match = false;
        for (int i = percpu_arena_mode_names_base;
             i < percpu_arena_mode_names_limit; i++) {
          if (strncmp(percpu_arena_mode_names[i], v, vlen) == 0) {
            if (!have_percpu_arena) {
              CONF_ERROR("No getcpu support", k, klen, v, vlen);
            }
            opt_percpu_arena = i;
            match = true;
            break;
          }
        }
        if (!match) { CONF_ERROR("Invalid conf value", k, klen, v, vlen); }
        CONF_CONTINUE;
      }
      CONF_HANDLE_BOOL(opt_background_thread, "background_thread");
      CONF_HANDLE_SIZE_T(opt_max_background_threads,
        "max_background_threads", 1, opt_max_background_threads,
        CONF_CHECK_MIN, CONF_CHECK_MAX, true);
      if (CONF_MATCH("slab_sizes")) {
        bool err;
        const char* slab_size_segment_cur = v;
        size_t vlen_left = vlen;
        do {
          size_t slab_start;
          size_t slab_end;
          size_t pgs;
          err = malloc_conf_multi_sizes_next(&slab_size_segment_cur,
            &vlen_left, &slab_start, &slab_end, &pgs);
          if (!err) {
            sc_data_update_slab_size(
              sc_data, slab_start, slab_end, (int)pgs);
          } else {
            CONF_ERROR("Invalid settings "
                       "for slab_sizes",
              k, klen, v, vlen);
          }
        } while (!err && vlen_left > 0);
        CONF_CONTINUE;
      }
      if (config_prof) {
        CONF_HANDLE_BOOL(opt_prof, "prof")
        CONF_HANDLE_CHAR_P(opt_prof_prefix, "prof_prefix", "jeprof")
        CONF_HANDLE_BOOL(opt_prof_active, "prof_active")
        CONF_HANDLE_BOOL(
          opt_prof_thread_active_init, "prof_thread_active_init")
        CONF_HANDLE_SIZE_T(opt_lg_prof_sample, "lg_prof_sample", 0,
          (sizeof(uint64_t) << 3) - 1, CONF_DONT_CHECK_MIN, CONF_CHECK_MAX,
          true)
        CONF_HANDLE_BOOL(opt_prof_accum, "prof_accum")
        CONF_HANDLE_SSIZE_T(opt_lg_prof_interval, "lg_prof_interval", -1,
          (sizeof(uint64_t) << 3) - 1)
        CONF_HANDLE_BOOL(opt_prof_gdump, "prof_gdump")
        CONF_HANDLE_BOOL(opt_prof_final, "prof_final")
        CONF_HANDLE_BOOL(opt_prof_leak, "prof_leak")
        CONF_HANDLE_BOOL(opt_prof_log, "prof_log")
      }
      if (config_log) {
        if (CONF_MATCH("log")) {
          size_t cpylen
            = (vlen <= sizeof(log_var_names) ? vlen
                                             : sizeof(log_var_names) - 1);
          strncpy(log_var_names, v, cpylen);
          log_var_names[cpylen] = '\0';
          CONF_CONTINUE;
        }
      }
      if (CONF_MATCH("thp")) {
        bool match = false;
        for (int i = 0; i < thp_mode_names_limit; i++) {
          if (strncmp(thp_mode_names[i], v, vlen) == 0) {
            if (!have_madvise_huge) {
              CONF_ERROR("No THP support", k, klen, v, vlen);
            }
            opt_thp = i;
            match = true;
            break;
          }
        }
        if (!match) { CONF_ERROR("Invalid conf value", k, klen, v, vlen); }
        CONF_CONTINUE;
      }
      CONF_ERROR("Invalid conf pair", k, klen, v, vlen);
#undef CONF_ERROR
#undef CONF_CONTINUE
#undef CONF_MATCH
#undef CONF_MATCH_VALUE
#undef CONF_HANDLE_BOOL
#undef CONF_DONT_CHECK_MIN
#undef CONF_CHECK_MIN
#undef CONF_DONT_CHECK_MAX
#undef CONF_CHECK_MAX
#undef CONF_HANDLE_T_U
#undef CONF_HANDLE_UNSIGNED
#undef CONF_HANDLE_SIZE_T
#undef CONF_HANDLE_SSIZE_T
#undef CONF_HANDLE_CHAR_P
      /* Re-enable diagnostic "-Wtype-limits" */
      JEMALLOC_DIAGNOSTIC_POP
    }
    if (opt_abort_conf && had_conf_error) { malloc_abort_invalid_conf(); }
  }
  atomic_store_b(&log_init_done, true, ATOMIC_RELEASE);
}

static void malloc_conf_init(
  sc_data_t* sc_data, unsigned bin_shard_sizes[SC_NBINS]) {
  const char* opts_cache[MALLOC_CONF_NSOURCES] = { NULL, NULL, NULL, NULL };
  char buf[PATH_MAX + 1];

  /* The first call only set the confirm_conf option and opts_cache */
  malloc_conf_init_helper(NULL, NULL, true, opts_cache, buf);
  malloc_conf_init_helper(
    sc_data, bin_shard_sizes, false, opts_cache, NULL);
}

#undef MALLOC_CONF_NSOURCES

static bool malloc_init_hard_needed(void) {
  if (malloc_initialized()
    || (IS_INITIALIZER && malloc_init_state == malloc_init_recursible)) {
    /*
     * Another thread initialized the allocator before this one
     * acquired init_lock, or this thread is the initializing
     * thread, and it is recursively allocating.
     */
    return false;
  }
#ifdef JEMALLOC_THREADED_INIT
  if (malloc_initializer != NO_INITIALIZER && !IS_INITIALIZER) {
    /* Busy-wait until the initializing thread completes. */
    spin_t spinner = SPIN_INITIALIZER;
    do {
      malloc_mutex_unlock(TSDN_NULL, &init_lock);
      spin_adaptive(&spinner);
      malloc_mutex_lock(TSDN_NULL, &init_lock);
    } while (!malloc_initialized());
    return false;
  }
#endif
  return true;
}

static bool malloc_init_hard_a0_locked() {
  malloc_initializer = INITIALIZER;

  JEMALLOC_DIAGNOSTIC_PUSH
  JEMALLOC_DIAGNOSTIC_IGNORE_MISSING_STRUCT_FIELD_INITIALIZERS
  sc_data_t sc_data = { 0 };
  JEMALLOC_DIAGNOSTIC_POP

  /*
   * Ordering here is somewhat tricky; we need sc_boot() first, since that
   * determines what the size classes will be, and then
   * malloc_conf_init(), since any slab size tweaking will need to be done
   * before sz_boot and bin_boot, which assume that the values they read
   * out of sc_data_global are final.
   */
  sc_boot(&sc_data);
  unsigned bin_shard_sizes[SC_NBINS];
  bin_shard_sizes_boot(bin_shard_sizes);
  /*
   * prof_boot0 only initializes opt_prof_prefix.  We need to do it before
   * we parse malloc_conf options, in case malloc_conf parsing overwrites
   * it.
   */
  if (config_prof) { prof_boot0(); }
  malloc_conf_init(&sc_data, bin_shard_sizes);
  sz_boot(&sc_data);
  bin_boot(&sc_data, bin_shard_sizes);

  if (opt_stats_print) {
    /* Print statistics at exit. */
    if (atexit(stats_print_atexit) != 0) {
      malloc_write("<jemalloc>: Error in atexit()\n");
      if (opt_abort) { abort(); }
    }
  }
  if (pages_boot()) { return true; }
  if (base_boot(TSDN_NULL)) { return true; }
  if (extent_boot()) { return true; }
  if (ctl_boot()) { return true; }
  if (config_prof) { prof_boot1(); }
  arena_boot(&sc_data);
  if (tcache_boot(TSDN_NULL)) { return true; }
  if (malloc_mutex_init(&arenas_lock, "arenas", WITNESS_RANK_ARENAS,
        malloc_mutex_rank_exclusive)) {
    return true;
  }
  hook_boot();
  /*
   * Create enough scaffolding to allow recursive allocation in
   * malloc_ncpus().
   */
  narenas_auto = 1;
  manual_arena_base = narenas_auto + 1;
  memset(arenas, 0, sizeof(arena_t*) * narenas_auto);
  /*
   * Initialize one arena here.  The rest are lazily created in
   * arena_choose_hard().
   */
  if (arena_init(TSDN_NULL, 0, (extent_hooks_t*)&extent_hooks_default)
    == NULL) {
    return true;
  }
  a0 = arena_get(TSDN_NULL, 0, false);
  malloc_init_state = malloc_init_a0_initialized;

  return false;
}

static bool malloc_init_hard_a0(void) {
  bool ret;

  malloc_mutex_lock(TSDN_NULL, &init_lock);
  ret = malloc_init_hard_a0_locked();
  malloc_mutex_unlock(TSDN_NULL, &init_lock);
  return ret;
}

/* Initialize data structures which may trigger recursive allocation. */
static bool malloc_init_hard_recursible(void) {
  malloc_init_state = malloc_init_recursible;

  ncpus = malloc_ncpus();

#if (defined(JEMALLOC_HAVE_PTHREAD_ATFORK)                                 \
  && !defined(JEMALLOC_MUTEX_INIT_CB) && !defined(JEMALLOC_ZONE)           \
  && !defined(_WIN32) && !defined(__native_client__))
  /* LinuxThreads' pthread_atfork() allocates. */
  if (pthread_atfork(
        jemalloc_prefork, jemalloc_postfork_parent, jemalloc_postfork_child)
    != 0) {
    malloc_write("<jemalloc>: Error in pthread_atfork()\n");
    if (opt_abort) { abort(); }
    return true;
  }
#endif

  if (background_thread_boot0()) { return true; }

  return false;
}

static unsigned malloc_narenas_default(void) {
  assert(ncpus > 0);
  /*
   * For SMP systems, create more than one arena per CPU by
   * default.
   */
  if (ncpus > 1) {
    return ncpus << 2;
  } else {
    return 1;
  }
}

static percpu_arena_mode_t percpu_arena_as_initialized(
  percpu_arena_mode_t mode) {
  assert(!malloc_initialized());
  assert(mode <= percpu_arena_disabled);

  if (mode != percpu_arena_disabled) {
    mode += percpu_arena_mode_enabled_base;
  }

  return mode;
}

static bool malloc_init_narenas(void) {
  assert(ncpus > 0);

  if (opt_percpu_arena != percpu_arena_disabled) {
    if (!have_percpu_arena || malloc_getcpu() < 0) {
      opt_percpu_arena = percpu_arena_disabled;
      malloc_printf("<jemalloc>: perCPU arena getcpu() not "
                    "available. Setting narenas to %u.\n",
        opt_narenas ? opt_narenas : malloc_narenas_default());
      if (opt_abort) { abort(); }
    } else {
      if (ncpus >= MALLOCX_ARENA_LIMIT) {
        malloc_printf("<jemalloc>: narenas w/ percpu"
                      "arena beyond limit (%d)\n",
          ncpus);
        if (opt_abort) { abort(); }
        return true;
      }
      /* NB: opt_percpu_arena isn't fully initialized yet. */
      if (percpu_arena_as_initialized(opt_percpu_arena) == per_phycpu_arena
        && ncpus % 2 != 0) {
        malloc_printf("<jemalloc>: invalid "
                      "configuration -- per physical CPU arena "
                      "with odd number (%u) of CPUs (no hyper "
                      "threading?).\n",
          ncpus);
        if (opt_abort) abort();
      }
      unsigned n = percpu_arena_ind_limit(
        percpu_arena_as_initialized(opt_percpu_arena));
      if (opt_narenas < n) {
        /*
         * If narenas is specified with percpu_arena
         * enabled, actual narenas is set as the greater
         * of the two. percpu_arena_choose will be free
         * to use any of the arenas based on CPU
         * id. This is conservative (at a small cost)
         * but ensures correctness.
         *
         * If for some reason the ncpus determined at
         * boot is not the actual number (e.g. because
         * of affinity setting from numactl), reserving
         * narenas this way provides a workaround for
         * percpu_arena.
         */
        opt_narenas = n;
      }
    }
  }
  if (opt_narenas == 0) { opt_narenas = malloc_narenas_default(); }
  assert(opt_narenas > 0);

  narenas_auto = opt_narenas;
  /*
   * Limit the number of arenas to the indexing range of MALLOCX_ARENA().
   */
  if (narenas_auto >= MALLOCX_ARENA_LIMIT) {
    narenas_auto = MALLOCX_ARENA_LIMIT - 1;
    malloc_printf(
      "<jemalloc>: Reducing narenas to limit (%d)\n", narenas_auto);
  }
  narenas_total_set(narenas_auto);
  if (arena_init_huge()) { narenas_total_inc(); }
  manual_arena_base = narenas_total_get();

  return false;
}

static void malloc_init_percpu(void) {
  opt_percpu_arena = percpu_arena_as_initialized(opt_percpu_arena);
}

static bool malloc_init_hard_finish(void) {
  if (malloc_mutex_boot()) { return true; }

  malloc_init_state = malloc_init_initialized;
  malloc_slow_flag_init();

  return false;
}

static void malloc_init_hard_cleanup(tsdn_t* tsdn, bool reentrancy_set) {
  malloc_mutex_assert_owner(tsdn, &init_lock);
  malloc_mutex_unlock(tsdn, &init_lock);
  if (reentrancy_set) {
    assert(!tsdn_null(tsdn));
    tsd_t* tsd = tsdn_tsd(tsdn);
    assert(tsd_reentrancy_level_get(tsd) > 0);
    post_reentrancy(tsd);
  }
}

static bool malloc_init_hard(void) {
  tsd_t* tsd;

#if defined(_WIN32) && _WIN32_WINNT < 0x0600
  _init_init_lock();
#endif
  malloc_mutex_lock(TSDN_NULL, &init_lock);

#define UNLOCK_RETURN(tsdn, ret, reentrancy)                               \
  malloc_init_hard_cleanup(tsdn, reentrancy);                              \
  return ret;

  if (!malloc_init_hard_needed()) { UNLOCK_RETURN(TSDN_NULL, false, false) }

  if (malloc_init_state != malloc_init_a0_initialized
    && malloc_init_hard_a0_locked()) {
    UNLOCK_RETURN(TSDN_NULL, true, false)
  }

  malloc_mutex_unlock(TSDN_NULL, &init_lock);
  /* Recursive allocation relies on functional tsd. */
  tsd = malloc_tsd_boot0();
  if (tsd == NULL) { return true; }
  if (malloc_init_hard_recursible()) { return true; }

  malloc_mutex_lock(tsd_tsdn(tsd), &init_lock);
  /* Set reentrancy level to 1 during init. */
  pre_reentrancy(tsd, NULL);
  /* Initialize narenas before prof_boot2 (for allocation). */
  if (malloc_init_narenas() || background_thread_boot1(tsd_tsdn(tsd))) {
    UNLOCK_RETURN(tsd_tsdn(tsd), true, true)
  }
  if (config_prof && prof_boot2(tsd)) {
    UNLOCK_RETURN(tsd_tsdn(tsd), true, true)
  }

  malloc_init_percpu();

  if (malloc_init_hard_finish()) {
    UNLOCK_RETURN(tsd_tsdn(tsd), true, true)
  }
  post_reentrancy(tsd);
  malloc_mutex_unlock(tsd_tsdn(tsd), &init_lock);

  witness_assert_lockless(
    witness_tsd_tsdn(tsd_witness_tsdp_get_unsafe(tsd)));
  malloc_tsd_boot1();
  /* Update TSD after tsd_boot1. */
  tsd = tsd_fetch();
  if (opt_background_thread) {
    assert(have_background_thread);
    /*
     * Need to finish init & unlock first before creating background
     * threads (pthread_create depends on malloc).  ctl_init (which
     * sets isthreaded) needs to be called without holding any lock.
     */
    background_thread_ctl_init(tsd_tsdn(tsd));
    if (background_thread_create(tsd, 0)) { return true; }
  }
#undef UNLOCK_RETURN
  return false;
}

/*
 * End initialization functions.
 */
/******************************************************************************/
/*
 * Begin allocation-path internal functions and data structures.
 */

/*
 * Settings determined by the documented behavior of the allocation
 * functions.
 */
typedef struct static_opts_s static_opts_t;
struct static_opts_s {
  /* Whether or not allocation size may overflow. */
  bool may_overflow;

  /*
   * Whether or not allocations (with alignment) of size 0 should be
   * treated as size 1.
   */
  bool bump_empty_aligned_alloc;
  /*
   * Whether to assert that allocations are not of size 0 (after any
   * bumping).
   */
  bool assert_nonempty_alloc;

  /*
   * Whether or not to modify the 'result' argument to malloc in case of
   * error.
   */
  bool null_out_result_on_error;
  /* Whether to set errno when we encounter an error condition. */
  bool set_errno_on_error;

  /*
   * The minimum valid alignment for functions requesting aligned storage.
   */
  size_t min_alignment;

  /* The error string to use if we oom. */
  const char* oom_string;
  /* The error string to use if the passed-in alignment is invalid. */
  const char* invalid_alignment_string;

  /*
   * False if we're configured to skip some time-consuming operations.
   *
   * This isn't really a malloc "behavior", but it acts as a useful
   * summary of several other static (or at least, static after program
   * initialization) options.
   */
  bool slow;
  /*
   * Return size.
   */
  bool usize;
};

JEMALLOC_ALWAYS_INLINE void static_opts_init(static_opts_t* static_opts) {
  static_opts->may_overflow = false;
  static_opts->bump_empty_aligned_alloc = false;
  static_opts->assert_nonempty_alloc = false;
  static_opts->null_out_result_on_error = false;
  static_opts->set_errno_on_error = false;
  static_opts->min_alignment = 0;
  static_opts->oom_string = "";
  static_opts->invalid_alignment_string = "";
  static_opts->slow = false;
  static_opts->usize = false;
}

/*
 * These correspond to the macros in jemalloc/jemalloc_macros.h.  Broadly,
 * we should have one constant here per magic value there.  Note however
 * that the representations need not be related.
 */
#define TCACHE_IND_NONE ((unsigned)-1)
#define TCACHE_IND_AUTOMATIC ((unsigned)-2)
#define ARENA_IND_AUTOMATIC ((unsigned)-1)

typedef struct dynamic_opts_s dynamic_opts_t;
struct dynamic_opts_s {
  void** result;
  size_t usize;
  size_t num_items;
  size_t item_size;
  size_t alignment;
  bool zero;
  unsigned tcache_ind;
  unsigned arena_ind;
};

JEMALLOC_ALWAYS_INLINE void dynamic_opts_init(
  dynamic_opts_t* dynamic_opts) {
  dynamic_opts->result = NULL;
  dynamic_opts->usize = 0;
  dynamic_opts->num_items = 0;
  dynamic_opts->item_size = 0;
  dynamic_opts->alignment = 0;
  dynamic_opts->zero = false;
  dynamic_opts->tcache_ind = TCACHE_IND_AUTOMATIC;
  dynamic_opts->arena_ind = ARENA_IND_AUTOMATIC;
}

/* ind is ignored if dopts->alignment > 0. */
JEMALLOC_ALWAYS_INLINE void* imalloc_no_sample(static_opts_t* sopts,
  dynamic_opts_t* dopts, tsd_t* tsd, size_t size, size_t usize,
  szind_t ind) {
  tcache_t* tcache;
  arena_t* arena;

  /* Fill in the tcache. */
  if (dopts->tcache_ind == TCACHE_IND_AUTOMATIC) {
    if (likely(!sopts->slow)) {
      /* Getting tcache ptr unconditionally. */
      tcache = tsd_tcachep_get(tsd);
      assert(tcache == tcache_get(tsd));
    } else {
      tcache = tcache_get(tsd);
    }
  } else if (dopts->tcache_ind == TCACHE_IND_NONE) {
    tcache = NULL;
  } else {
    tcache = tcaches_get(tsd, dopts->tcache_ind);
  }

  /* Fill in the arena. */
  if (dopts->arena_ind == ARENA_IND_AUTOMATIC) {
    /*
     * In case of automatic arena management, we defer arena
     * computation until as late as we can, hoping to fill the
     * allocation out of the tcache.
     */
    arena = NULL;
  } else {
    arena = arena_get(tsd_tsdn(tsd), dopts->arena_ind, true);
  }

  if (unlikely(dopts->alignment != 0)) {
    return ipalloct(
      tsd_tsdn(tsd), usize, dopts->alignment, dopts->zero, tcache, arena);
  }

  return iallocztm(tsd_tsdn(tsd), size, ind, dopts->zero, tcache, false,
    arena, sopts->slow);
}

JEMALLOC_ALWAYS_INLINE void* imalloc_sample(static_opts_t* sopts,
  dynamic_opts_t* dopts, tsd_t* tsd, size_t usize, szind_t ind) {
  void* ret;

  /*
   * For small allocations, sampling bumps the usize.  If so, we allocate
   * from the ind_large bucket.
   */
  szind_t ind_large;
  size_t bumped_usize = usize;

  if (usize <= SC_SMALL_MAXCLASS) {
    assert(((dopts->alignment == 0)
               ? sz_s2u(SC_LARGE_MINCLASS)
               : sz_sa2u(SC_LARGE_MINCLASS, dopts->alignment))
      == SC_LARGE_MINCLASS);
    ind_large = sz_size2index(SC_LARGE_MINCLASS);
    bumped_usize = sz_s2u(SC_LARGE_MINCLASS);
    ret = imalloc_no_sample(
      sopts, dopts, tsd, bumped_usize, bumped_usize, ind_large);
    if (unlikely(ret == NULL)) { return NULL; }
    arena_prof_promote(tsd_tsdn(tsd), ret, usize);
  } else {
    ret = imalloc_no_sample(sopts, dopts, tsd, usize, usize, ind);
  }

  return ret;
}

/*
 * Returns true if the allocation will overflow, and false otherwise.  Sets
 * *size to the product either way.
 */
JEMALLOC_ALWAYS_INLINE bool compute_size_with_overflow(
  bool may_overflow, dynamic_opts_t* dopts, size_t* size) {
  /*
   * This function is just num_items * item_size, except that we may have
   * to check for overflow.
   */

  if (!may_overflow) {
    assert(dopts->num_items == 1);
    *size = dopts->item_size;
    return false;
  }

  /* A size_t with its high-half bits all set to 1. */
  static const size_t high_bits = SIZE_T_MAX << (sizeof(size_t) * 8 / 2);

  *size = dopts->item_size * dopts->num_items;

  if (unlikely(*size == 0)) {
    return (dopts->num_items != 0 && dopts->item_size != 0);
  }

  /*
   * We got a non-zero size, but we don't know if we overflowed to get
   * there.  To avoid having to do a divide, we'll be clever and note that
   * if both A and B can be represented in N/2 bits, then their product
   * can be represented in N bits (without the possibility of overflow).
   */
  if (likely((high_bits & (dopts->num_items | dopts->item_size)) == 0)) {
    return false;
  }
  if (likely(*size / dopts->item_size == dopts->num_items)) {
    return false;
  }
  return true;
}

JEMALLOC_ALWAYS_INLINE int imalloc_body(
  static_opts_t* sopts, dynamic_opts_t* dopts, tsd_t* tsd) {
  /* Where the actual allocated memory will live. */
  void* allocation = NULL;
  /* Filled in by compute_size_with_overflow below. */
  size_t size = 0;
  /*
   * For unaligned allocations, we need only ind.  For aligned
   * allocations, or in case of stats or profiling we need usize.
   *
   * These are actually dead stores, in that their values are reset before
   * any branch on their value is taken.  Sometimes though, it's
   * convenient to pass them as arguments before this point.  To avoid
   * undefined behavior then, we initialize them with dummy stores.
   */
  szind_t ind = 0;
  size_t usize = 0;

  /* Reentrancy is only checked on slow path. */
  int8_t reentrancy_level;

  /* Compute the amount of memory the user wants. */
  if (unlikely(
        compute_size_with_overflow(sopts->may_overflow, dopts, &size))) {
    goto label_oom;
  }

  if (unlikely(dopts->alignment < sopts->min_alignment
        || (dopts->alignment & (dopts->alignment - 1)) != 0)) {
    goto label_invalid_alignment;
  }

  /* This is the beginning of the "core" algorithm. */

  if (dopts->alignment == 0) {
    ind = sz_size2index(size);
    if (unlikely(ind >= SC_NSIZES)) { goto label_oom; }
    if (config_stats || (config_prof && opt_prof) || sopts->usize) {
      usize = sz_index2size(ind);
      dopts->usize = usize;
      assert(usize > 0 && usize <= SC_LARGE_MAXCLASS);
    }
  } else {
    if (sopts->bump_empty_aligned_alloc) {
      if (unlikely(size == 0)) { size = 1; }
    }
    usize = sz_sa2u(size, dopts->alignment);
    dopts->usize = usize;
    if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) {
      goto label_oom;
    }
  }
  /* Validate the user input. */
  if (sopts->assert_nonempty_alloc) { assert(size != 0); }

  check_entry_exit_locking(tsd_tsdn(tsd));

  /*
   * If we need to handle reentrancy, we can do it out of a
   * known-initialized arena (i.e. arena 0).
   */
  reentrancy_level = tsd_reentrancy_level_get(tsd);
  if (sopts->slow && unlikely(reentrancy_level > 0)) {
    /*
     * We should never specify particular arenas or tcaches from
     * within our internal allocations.
     */
    assert(dopts->tcache_ind == TCACHE_IND_AUTOMATIC
      || dopts->tcache_ind == TCACHE_IND_NONE);
    assert(dopts->arena_ind == ARENA_IND_AUTOMATIC);
    dopts->tcache_ind = TCACHE_IND_NONE;
    /* We know that arena 0 has already been initialized. */
    dopts->arena_ind = 0;
  }

  /* If profiling is on, get our profiling context. */
  if (config_prof && opt_prof) {
    /*
     * Note that if we're going down this path, usize must have been
     * initialized in the previous if statement.
     */
    prof_tctx_t* tctx
      = prof_alloc_prep(tsd, usize, prof_active_get_unlocked(), true);

    alloc_ctx_t alloc_ctx;
    if (likely((uintptr_t)tctx == (uintptr_t)1U)) {
      alloc_ctx.slab = (usize <= SC_SMALL_MAXCLASS);
      allocation = imalloc_no_sample(sopts, dopts, tsd, usize, usize, ind);
    } else if ((uintptr_t)tctx > (uintptr_t)1U) {
      /*
       * Note that ind might still be 0 here.  This is fine;
       * imalloc_sample ignores ind if dopts->alignment > 0.
       */
      allocation = imalloc_sample(sopts, dopts, tsd, usize, ind);
      alloc_ctx.slab = false;
    } else {
      allocation = NULL;
    }

    if (unlikely(allocation == NULL)) {
      prof_alloc_rollback(tsd, tctx, true);
      goto label_oom;
    }
    prof_malloc(tsd_tsdn(tsd), allocation, usize, &alloc_ctx, tctx);
  } else {
    /*
     * If dopts->alignment > 0, then ind is still 0, but usize was
     * computed in the previous if statement.  Down the positive
     * alignment path, imalloc_no_sample ignores ind and size
     * (relying only on usize).
     */
    allocation = imalloc_no_sample(sopts, dopts, tsd, size, usize, ind);
    if (unlikely(allocation == NULL)) { goto label_oom; }
  }

  /*
   * Allocation has been done at this point.  We still have some
   * post-allocation work to do though.
   */
  assert(dopts->alignment == 0
    || ((uintptr_t)allocation & (dopts->alignment - 1)) == ZU(0));

  if (config_stats) {
    assert(usize == isalloc(tsd_tsdn(tsd), allocation));
    *tsd_thread_allocatedp_get(tsd) += usize;
  }

  if (sopts->slow) { UTRACE(0, size, allocation); }

  /* Success! */
  check_entry_exit_locking(tsd_tsdn(tsd));
  *dopts->result = allocation;
  return 0;

label_oom:
  if (unlikely(sopts->slow) && config_xmalloc && unlikely(opt_xmalloc)) {
    malloc_write(sopts->oom_string);
    abort();
  }

  if (sopts->slow) { UTRACE(NULL, size, NULL); }

  check_entry_exit_locking(tsd_tsdn(tsd));

  if (sopts->set_errno_on_error) { set_errno(ENOMEM); }

  if (sopts->null_out_result_on_error) { *dopts->result = NULL; }

  return ENOMEM;

  /*
   * This label is only jumped to by one goto; we move it out of line
   * anyways to avoid obscuring the non-error paths, and for symmetry with
   * the oom case.
   */
label_invalid_alignment:
  if (config_xmalloc && unlikely(opt_xmalloc)) {
    malloc_write(sopts->invalid_alignment_string);
    abort();
  }

  if (sopts->set_errno_on_error) { set_errno(EINVAL); }

  if (sopts->slow) { UTRACE(NULL, size, NULL); }

  check_entry_exit_locking(tsd_tsdn(tsd));

  if (sopts->null_out_result_on_error) { *dopts->result = NULL; }

  return EINVAL;
}

JEMALLOC_ALWAYS_INLINE bool imalloc_init_check(
  static_opts_t* sopts, dynamic_opts_t* dopts) {
  if (unlikely(!malloc_initialized()) && unlikely(malloc_init())) {
    if (config_xmalloc && unlikely(opt_xmalloc)) {
      malloc_write(sopts->oom_string);
      abort();
    }
    UTRACE(NULL, dopts->num_items * dopts->item_size, NULL);
    set_errno(ENOMEM);
    *dopts->result = NULL;

    return false;
  }

  return true;
}

/* Returns the errno-style error code of the allocation. */
JEMALLOC_ALWAYS_INLINE int imalloc(
  static_opts_t* sopts, dynamic_opts_t* dopts) {
  if (tsd_get_allocates() && !imalloc_init_check(sopts, dopts)) {
    return ENOMEM;
  }

  /* We always need the tsd.  Let's grab it right away. */
  tsd_t* tsd = tsd_fetch();
  assert(tsd);
  if (likely(tsd_fast(tsd))) {
    /* Fast and common path. */
    tsd_assert_fast(tsd);
    sopts->slow = false;
    return imalloc_body(sopts, dopts, tsd);
  } else {
    if (!tsd_get_allocates() && !imalloc_init_check(sopts, dopts)) {
      return ENOMEM;
    }

    sopts->slow = true;
    return imalloc_body(sopts, dopts, tsd);
  }
}

JEMALLOC_NOINLINE
void* malloc_default(size_t size) {
  void* ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.malloc.entry", "size: %zu", size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.null_out_result_on_error = true;
  sopts.set_errno_on_error = true;
  sopts.oom_string = "<jemalloc>: Error in malloc(): out of memory\n";

  dopts.result = &ret;
  dopts.num_items = 1;
  dopts.item_size = size;

  imalloc(&sopts, &dopts);
  /*
   * Note that this branch gets optimized away -- it immediately follows
   * the check on tsd_fast that sets sopts.slow.
   */
  if (sopts.slow) {
    uintptr_t args[3] = { size };
    hook_invoke_alloc(hook_alloc_malloc, ret, (uintptr_t)ret, args);
  }

  LOG("core.malloc.exit", "result: %p", ret);

  return ret;
}

/******************************************************************************/
/*
 * Begin malloc(3)-compatible functions.
 */

/*
 * malloc() fastpath.
 *
 * Fastpath assumes size <= SC_LOOKUP_MAXCLASS, and that we hit
 * tcache.  If either of these is false, we tail-call to the slowpath,
 * malloc_default().  Tail-calling is used to avoid any caller-saved
 * registers.
 *
 * fastpath supports ticker and profiling, both of which will also
 * tail-call to the slowpath if they fire.
 */
JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) JEMALLOC_ALLOC_SIZE(1) je_malloc(size_t size) {
  LOG("core.malloc.entry", "size: %zu", size);

  if (tsd_get_allocates() && unlikely(!malloc_initialized())) {
    return malloc_default(size);
  }

  tsd_t* tsd = tsd_get(false);
  if (unlikely(!tsd || !tsd_fast(tsd) || (size > SC_LOOKUP_MAXCLASS))) {
    return malloc_default(size);
  }

  tcache_t* tcache = tsd_tcachep_get(tsd);

  if (unlikely(ticker_trytick(&tcache->gc_ticker))) {
    return malloc_default(size);
  }

  szind_t ind = sz_size2index_lookup(size);
  size_t usize;
  if (config_stats || config_prof) { usize = sz_index2size(ind); }
  /* Fast path relies on size being a bin. I.e. SC_LOOKUP_MAXCLASS <
   * SC_SMALL_MAXCLASS */
  assert(ind < SC_NBINS);
  assert(size <= SC_SMALL_MAXCLASS);

  if (config_prof) {
    int64_t bytes_until_sample = tsd_bytes_until_sample_get(tsd);
    bytes_until_sample -= usize;
    tsd_bytes_until_sample_set(tsd, bytes_until_sample);

    if (unlikely(bytes_until_sample < 0)) {
      /*
       * Avoid a prof_active check on the fastpath.
       * If prof_active is false, set bytes_until_sample to
       * a large value.  If prof_active is set to true,
       * bytes_until_sample will be reset.
       */
      if (!prof_active) { tsd_bytes_until_sample_set(tsd, SSIZE_MAX); }
      return malloc_default(size);
    }
  }

  cache_bin_t* bin = tcache_small_bin_get(tcache, ind);
  bool tcache_success;
  void* ret = cache_bin_alloc_easy(bin, &tcache_success);

  if (tcache_success) {
    if (config_stats) {
      *tsd_thread_allocatedp_get(tsd) += usize;
      bin->tstats.nrequests++;
    }
    if (config_prof) { tcache->prof_accumbytes += usize; }

    LOG("core.malloc.exit", "result: %p", ret);

    /* Fastpath success */
    return ret;
  }

  return malloc_default(size);
}

JEMALLOC_EXPORT int JEMALLOC_NOTHROW JEMALLOC_ATTR(nonnull(1))
  je_posix_memalign(void** memptr, size_t alignment, size_t size) {
  int ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.posix_memalign.entry",
    "mem ptr: %p, alignment: %zu, "
    "size: %zu",
    memptr, alignment, size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.bump_empty_aligned_alloc = true;
  sopts.min_alignment = sizeof(void*);
  sopts.oom_string
    = "<jemalloc>: Error allocating aligned memory: out of memory\n";
  sopts.invalid_alignment_string
    = "<jemalloc>: Error allocating aligned memory: invalid alignment\n";

  dopts.result = memptr;
  dopts.num_items = 1;
  dopts.item_size = size;
  dopts.alignment = alignment;

  ret = imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3]
      = { (uintptr_t)memptr, (uintptr_t)alignment, (uintptr_t)size };
    hook_invoke_alloc(
      hook_alloc_posix_memalign, *memptr, (uintptr_t)ret, args);
  }

  LOG(
    "core.posix_memalign.exit", "result: %d, alloc ptr: %p", ret, *memptr);

  return ret;
}

JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) JEMALLOC_ALLOC_SIZE(2)
    je_aligned_alloc(size_t alignment, size_t size) {
  void* ret;

  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.aligned_alloc.entry", "alignment: %zu, size: %zu\n", alignment,
    size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.bump_empty_aligned_alloc = true;
  sopts.null_out_result_on_error = true;
  sopts.set_errno_on_error = true;
  sopts.min_alignment = 1;
  sopts.oom_string
    = "<jemalloc>: Error allocating aligned memory: out of memory\n";
  sopts.invalid_alignment_string
    = "<jemalloc>: Error allocating aligned memory: invalid alignment\n";

  dopts.result = &ret;
  dopts.num_items = 1;
  dopts.item_size = size;
  dopts.alignment = alignment;

  imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3] = { (uintptr_t)alignment, (uintptr_t)size };
    hook_invoke_alloc(hook_alloc_aligned_alloc, ret, (uintptr_t)ret, args);
  }

  LOG("core.aligned_alloc.exit", "result: %p", ret);

  return ret;
}

JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) JEMALLOC_ALLOC_SIZE2(1, 2)
    je_calloc(size_t num, size_t size) {
  void* ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.calloc.entry", "num: %zu, size: %zu\n", num, size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.may_overflow = true;
  sopts.null_out_result_on_error = true;
  sopts.set_errno_on_error = true;
  sopts.oom_string = "<jemalloc>: Error in calloc(): out of memory\n";

  dopts.result = &ret;
  dopts.num_items = num;
  dopts.item_size = size;
  dopts.zero = true;

  imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3] = { (uintptr_t)num, (uintptr_t)size };
    hook_invoke_alloc(hook_alloc_calloc, ret, (uintptr_t)ret, args);
  }

  LOG("core.calloc.exit", "result: %p", ret);

  return ret;
}

static void* irealloc_prof_sample(tsd_t* tsd, void* old_ptr,
  size_t old_usize, size_t usize, prof_tctx_t* tctx,
  hook_ralloc_args_t* hook_args) {
  void* p;

  if (tctx == NULL) { return NULL; }
  if (usize <= SC_SMALL_MAXCLASS) {
    p = iralloc(
      tsd, old_ptr, old_usize, SC_LARGE_MINCLASS, 0, false, hook_args);
    if (p == NULL) { return NULL; }
    arena_prof_promote(tsd_tsdn(tsd), p, usize);
  } else {
    p = iralloc(tsd, old_ptr, old_usize, usize, 0, false, hook_args);
  }

  return p;
}

JEMALLOC_ALWAYS_INLINE void* irealloc_prof(tsd_t* tsd, void* old_ptr,
  size_t old_usize, size_t usize, alloc_ctx_t* alloc_ctx,
  hook_ralloc_args_t* hook_args) {
  void* p;
  bool prof_active;
  prof_tctx_t *old_tctx, *tctx;

  prof_active = prof_active_get_unlocked();
  old_tctx = prof_tctx_get(tsd_tsdn(tsd), old_ptr, alloc_ctx);
  tctx = prof_alloc_prep(tsd, usize, prof_active, true);
  if (unlikely((uintptr_t)tctx != (uintptr_t)1U)) {
    p = irealloc_prof_sample(
      tsd, old_ptr, old_usize, usize, tctx, hook_args);
  } else {
    p = iralloc(tsd, old_ptr, old_usize, usize, 0, false, hook_args);
  }
  if (unlikely(p == NULL)) {
    prof_alloc_rollback(tsd, tctx, true);
    return NULL;
  }
  prof_realloc(
    tsd, p, usize, tctx, prof_active, true, old_ptr, old_usize, old_tctx);

  return p;
}

JEMALLOC_ALWAYS_INLINE void ifree(
  tsd_t* tsd, void* ptr, tcache_t* tcache, bool slow_path) {
  if (!slow_path) { tsd_assert_fast(tsd); }
  check_entry_exit_locking(tsd_tsdn(tsd));
  if (tsd_reentrancy_level_get(tsd) != 0) { assert(slow_path); }

  assert(ptr != NULL);
  assert(malloc_initialized() || IS_INITIALIZER);

  alloc_ctx_t alloc_ctx;
  rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
  rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
    (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
  assert(alloc_ctx.szind != SC_NSIZES);

  size_t usize;
  if (config_prof && opt_prof) {
    usize = sz_index2size(alloc_ctx.szind);
    prof_free(tsd, ptr, usize, &alloc_ctx);
  } else if (config_stats) {
    usize = sz_index2size(alloc_ctx.szind);
  }
  if (config_stats) { *tsd_thread_deallocatedp_get(tsd) += usize; }

  if (likely(!slow_path)) {
    idalloctm(tsd_tsdn(tsd), ptr, tcache, &alloc_ctx, false, false);
  } else {
    idalloctm(tsd_tsdn(tsd), ptr, tcache, &alloc_ctx, false, true);
  }
}

JEMALLOC_ALWAYS_INLINE void isfree(
  tsd_t* tsd, void* ptr, size_t usize, tcache_t* tcache, bool slow_path) {
  if (!slow_path) { tsd_assert_fast(tsd); }
  check_entry_exit_locking(tsd_tsdn(tsd));
  if (tsd_reentrancy_level_get(tsd) != 0) { assert(slow_path); }

  assert(ptr != NULL);
  assert(malloc_initialized() || IS_INITIALIZER);

  alloc_ctx_t alloc_ctx, *ctx;
  if (!config_cache_oblivious && ((uintptr_t)ptr & PAGE_MASK) != 0) {
    /*
     * When cache_oblivious is disabled and ptr is not page aligned,
     * the allocation was not sampled -- usize can be used to
     * determine szind directly.
     */
    alloc_ctx.szind = sz_size2index(usize);
    alloc_ctx.slab = true;
    ctx = &alloc_ctx;
    if (config_debug) {
      alloc_ctx_t dbg_ctx;
      rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
      rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
        (uintptr_t)ptr, true, &dbg_ctx.szind, &dbg_ctx.slab);
      assert(dbg_ctx.szind == alloc_ctx.szind);
      assert(dbg_ctx.slab == alloc_ctx.slab);
    }
  } else if (config_prof && opt_prof) {
    rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
    rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
      (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
    assert(alloc_ctx.szind == sz_size2index(usize));
    ctx = &alloc_ctx;
  } else {
    ctx = NULL;
  }

  if (config_prof && opt_prof) { prof_free(tsd, ptr, usize, ctx); }
  if (config_stats) { *tsd_thread_deallocatedp_get(tsd) += usize; }

  if (likely(!slow_path)) {
    isdalloct(tsd_tsdn(tsd), ptr, usize, tcache, ctx, false);
  } else {
    isdalloct(tsd_tsdn(tsd), ptr, usize, tcache, ctx, true);
  }
}

JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ALLOC_SIZE(2) je_realloc(void* ptr, size_t arg_size) {
  void* ret;
  tsdn_t* tsdn JEMALLOC_CC_SILENCE_INIT(NULL);
  size_t usize JEMALLOC_CC_SILENCE_INIT(0);
  size_t old_usize = 0;
  size_t size = arg_size;

  LOG("core.realloc.entry", "ptr: %p, size: %zu\n", ptr, size);

  if (unlikely(size == 0)) {
    if (ptr != NULL) {
      /* realloc(ptr, 0) is equivalent to free(ptr). */
      UTRACE(ptr, 0, 0);
      tcache_t* tcache;
      tsd_t* tsd = tsd_fetch();
      if (tsd_reentrancy_level_get(tsd) == 0) {
        tcache = tcache_get(tsd);
      } else {
        tcache = NULL;
      }

      uintptr_t args[3] = { (uintptr_t)ptr, size };
      hook_invoke_dalloc(hook_dalloc_realloc, ptr, args);

      ifree(tsd, ptr, tcache, true);

      LOG("core.realloc.exit", "result: %p", NULL);
      return NULL;
    }
    size = 1;
  }

  if (likely(ptr != NULL)) {
    assert(malloc_initialized() || IS_INITIALIZER);
    tsd_t* tsd = tsd_fetch();

    check_entry_exit_locking(tsd_tsdn(tsd));

    hook_ralloc_args_t hook_args
      = { true, { (uintptr_t)ptr, (uintptr_t)arg_size, 0, 0 } };

    alloc_ctx_t alloc_ctx;
    rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
    rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
      (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
    assert(alloc_ctx.szind != SC_NSIZES);
    old_usize = sz_index2size(alloc_ctx.szind);
    assert(old_usize == isalloc(tsd_tsdn(tsd), ptr));
    if (config_prof && opt_prof) {
      usize = sz_s2u(size);
      if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) {
        ret = NULL;
      } else {
        ret = irealloc_prof(
          tsd, ptr, old_usize, usize, &alloc_ctx, &hook_args);
      }
    } else {
      if (config_stats) { usize = sz_s2u(size); }
      ret = iralloc(tsd, ptr, old_usize, size, 0, false, &hook_args);
    }
    tsdn = tsd_tsdn(tsd);
  } else {
    /* realloc(NULL, size) is equivalent to malloc(size). */
    static_opts_t sopts;
    dynamic_opts_t dopts;

    static_opts_init(&sopts);
    dynamic_opts_init(&dopts);

    sopts.null_out_result_on_error = true;
    sopts.set_errno_on_error = true;
    sopts.oom_string = "<jemalloc>: Error in realloc(): out of memory\n";

    dopts.result = &ret;
    dopts.num_items = 1;
    dopts.item_size = size;

    imalloc(&sopts, &dopts);
    if (sopts.slow) {
      uintptr_t args[3] = { (uintptr_t)ptr, arg_size };
      hook_invoke_alloc(hook_alloc_realloc, ret, (uintptr_t)ret, args);
    }

    return ret;
  }

  if (unlikely(ret == NULL)) {
    if (config_xmalloc && unlikely(opt_xmalloc)) {
      malloc_write("<jemalloc>: Error in realloc(): "
                   "out of memory\n");
      abort();
    }
    set_errno(ENOMEM);
  }
  if (config_stats && likely(ret != NULL)) {
    tsd_t* tsd;

    assert(usize == isalloc(tsdn, ret));
    tsd = tsdn_tsd(tsdn);
    *tsd_thread_allocatedp_get(tsd) += usize;
    *tsd_thread_deallocatedp_get(tsd) += old_usize;
  }
  UTRACE(ptr, size, ret);
  check_entry_exit_locking(tsdn);

  LOG("core.realloc.exit", "result: %p", ret);
  return ret;
}

JEMALLOC_NOINLINE
void free_default(void* ptr) {
  UTRACE(ptr, 0, 0);
  if (likely(ptr != NULL)) {
    /*
     * We avoid setting up tsd fully (e.g. tcache, arena binding)
     * based on only free() calls -- other activities trigger the
     * minimal to full transition.  This is because free() may
     * happen during thread shutdown after tls deallocation: if a
     * thread never had any malloc activities until then, a
     * fully-setup tsd won't be destructed properly.
     */
    tsd_t* tsd = tsd_fetch_min();
    check_entry_exit_locking(tsd_tsdn(tsd));

    tcache_t* tcache;
    if (likely(tsd_fast(tsd))) {
      tsd_assert_fast(tsd);
      /* Unconditionally get tcache ptr on fast path. */
      tcache = tsd_tcachep_get(tsd);
      ifree(tsd, ptr, tcache, false);
    } else {
      if (likely(tsd_reentrancy_level_get(tsd) == 0)) {
        tcache = tcache_get(tsd);
      } else {
        tcache = NULL;
      }
      uintptr_t args_raw[3] = { (uintptr_t)ptr };
      hook_invoke_dalloc(hook_dalloc_free, ptr, args_raw);
      ifree(tsd, ptr, tcache, true);
    }
    check_entry_exit_locking(tsd_tsdn(tsd));
  }
}

JEMALLOC_ALWAYS_INLINE
bool free_fastpath(void* ptr, size_t size, bool size_hint) {
  tsd_t* tsd = tsd_get(false);
  if (unlikely(!tsd || !tsd_fast(tsd))) { return false; }

  tcache_t* tcache = tsd_tcachep_get(tsd);

  alloc_ctx_t alloc_ctx;
  /*
   * If !config_cache_oblivious, we can check PAGE alignment to
   * detect sampled objects.  Otherwise addresses are
   * randomized, and we have to look it up in the rtree anyway.
   * See also isfree().
   */
  if (!size_hint || config_cache_oblivious) {
    rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
    bool res = rtree_szind_slab_read_fast(tsd_tsdn(tsd), &extents_rtree,
      rtree_ctx, (uintptr_t)ptr, &alloc_ctx.szind, &alloc_ctx.slab);

    /* Note: profiled objects will have alloc_ctx.slab set */
    if (!res || !alloc_ctx.slab) { return false; }
    assert(alloc_ctx.szind != SC_NSIZES);
  } else {
    /*
     * Check for both sizes that are too large, and for sampled objects.
     * Sampled objects are always page-aligned.  The sampled object check
     * will also check for null ptr.
     */
    if (size > SC_LOOKUP_MAXCLASS || (((uintptr_t)ptr & PAGE_MASK) == 0)) {
      return false;
    }
    alloc_ctx.szind = sz_size2index_lookup(size);
  }

  if (unlikely(ticker_trytick(&tcache->gc_ticker))) { return false; }

  cache_bin_t* bin = tcache_small_bin_get(tcache, alloc_ctx.szind);
  cache_bin_info_t* bin_info = &tcache_bin_info[alloc_ctx.szind];
  if (!cache_bin_dalloc_easy(bin, bin_info, ptr)) { return false; }

  if (config_stats) {
    size_t usize = sz_index2size(alloc_ctx.szind);
    *tsd_thread_deallocatedp_get(tsd) += usize;
  }

  return true;
}

JEMALLOC_EXPORT void JEMALLOC_NOTHROW je_free(void* ptr) {
  LOG("core.free.entry", "ptr: %p", ptr);

  if (!free_fastpath(ptr, 0, false)) { free_default(ptr); }

  LOG("core.free.exit", "");
}

/*
 * End malloc(3)-compatible functions.
 */
/******************************************************************************/
/*
 * Begin non-standard override functions.
 */

#ifdef JEMALLOC_OVERRIDE_MEMALIGN
JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) je_memalign(size_t alignment, size_t size) {
  void* ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG(
    "core.memalign.entry", "alignment: %zu, size: %zu\n", alignment, size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.min_alignment = 1;
  sopts.oom_string
    = "<jemalloc>: Error allocating aligned memory: out of memory\n";
  sopts.invalid_alignment_string
    = "<jemalloc>: Error allocating aligned memory: invalid alignment\n";
  sopts.null_out_result_on_error = true;

  dopts.result = &ret;
  dopts.num_items = 1;
  dopts.item_size = size;
  dopts.alignment = alignment;

  imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3] = { alignment, size };
    hook_invoke_alloc(hook_alloc_memalign, ret, (uintptr_t)ret, args);
  }

  LOG("core.memalign.exit", "result: %p", ret);
  return ret;
}
#endif

#ifdef JEMALLOC_OVERRIDE_VALLOC
JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) je_valloc(size_t size) {
  void* ret;

  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.valloc.entry", "size: %zu\n", size);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.null_out_result_on_error = true;
  sopts.min_alignment = PAGE;
  sopts.oom_string
    = "<jemalloc>: Error allocating aligned memory: out of memory\n";
  sopts.invalid_alignment_string
    = "<jemalloc>: Error allocating aligned memory: invalid alignment\n";

  dopts.result = &ret;
  dopts.num_items = 1;
  dopts.item_size = size;
  dopts.alignment = PAGE;

  imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3] = { size };
    hook_invoke_alloc(hook_alloc_valloc, ret, (uintptr_t)ret, args);
  }

  LOG("core.valloc.exit", "result: %p\n", ret);
  return ret;
}
#endif

#if defined(JEMALLOC_IS_MALLOC) && defined(JEMALLOC_GLIBC_MALLOC_HOOK)
/*
 * glibc provides the RTLD_DEEPBIND flag for dlopen which can make it
 * possible to inconsistently reference libc's malloc(3)-compatible
 * functions (https://bugzilla.mozilla.org/show_bug.cgi?id=493541).
 *
 * These definitions interpose hooks in glibc.  The functions are actually
 * passed an extra argument for the caller return address, which will be
 * ignored.
 */
JEMALLOC_EXPORT void (*__free_hook)(void* ptr) = je_free;
JEMALLOC_EXPORT void* (*__malloc_hook)(size_t size) = je_malloc;
JEMALLOC_EXPORT void* (*__realloc_hook)(void* ptr, size_t size)
  = je_realloc;
#ifdef JEMALLOC_GLIBC_MEMALIGN_HOOK
JEMALLOC_EXPORT void* (*__memalign_hook)(size_t alignment, size_t size)
  = je_memalign;
#endif

#ifdef CPU_COUNT
/*
 * To enable static linking with glibc, the libc specific malloc interface
 * must be implemented also, so none of glibc's malloc.o functions are added
 * to the link.
 */
#define ALIAS(je_fn) __attribute__((alias(#je_fn), used))
/* To force macro expansion of je_ prefix before stringification. */
#define PREALIAS(je_fn) ALIAS(je_fn)
#ifdef JEMALLOC_OVERRIDE___LIBC_CALLOC
void* __libc_calloc(size_t n, size_t size) PREALIAS(je_calloc);
#endif
#ifdef JEMALLOC_OVERRIDE___LIBC_FREE
void __libc_free(void* ptr) PREALIAS(je_free);
#endif
#ifdef JEMALLOC_OVERRIDE___LIBC_MALLOC
void* __libc_malloc(size_t size) PREALIAS(je_malloc);
#endif
#ifdef JEMALLOC_OVERRIDE___LIBC_MEMALIGN
void* __libc_memalign(size_t align, size_t s) PREALIAS(je_memalign);
#endif
#ifdef JEMALLOC_OVERRIDE___LIBC_REALLOC
void* __libc_realloc(void* ptr, size_t size) PREALIAS(je_realloc);
#endif
#ifdef JEMALLOC_OVERRIDE___LIBC_VALLOC
void* __libc_valloc(size_t size) PREALIAS(je_valloc);
#endif
#ifdef JEMALLOC_OVERRIDE___POSIX_MEMALIGN
int __posix_memalign(void** r, size_t a, size_t s)
  PREALIAS(je_posix_memalign);
#endif
#undef PREALIAS
#undef ALIAS
#endif
#endif

/*
 * End non-standard override functions.
 */
/******************************************************************************/
/*
 * Begin non-standard functions.
 */

#ifdef JEMALLOC_EXPERIMENTAL_SMALLOCX_API

#define JEMALLOC_SMALLOCX_CONCAT_HELPER(x, y) x##y
#define JEMALLOC_SMALLOCX_CONCAT_HELPER2(x, y)                             \
  JEMALLOC_SMALLOCX_CONCAT_HELPER(x, y)

typedef struct {
  void* ptr;
  size_t size;
} smallocx_return_t;

JEMALLOC_EXPORT
JEMALLOC_ALLOCATOR
JEMALLOC_RESTRICT_RETURN
smallocx_return_t
        JEMALLOC_NOTHROW
        /*
         * The attribute JEMALLOC_ATTR(malloc) cannot be used due to:
         *  - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=86488
         */
        JEMALLOC_SMALLOCX_CONCAT_HELPER2(
            je_smallocx_, JEMALLOC_VERSION_GID_IDENT)(size_t size, int flags) {
  /*
   * Note: the attribute JEMALLOC_ALLOC_SIZE(1) cannot be
   * used here because it makes writing beyond the `size`
   * of the `ptr` undefined behavior, but the objective
   * of this function is to allow writing beyond `size`
   * up to `smallocx_return_t::size`.
   */
  smallocx_return_t ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.smallocx.entry", "size: %zu, flags: %d", size, flags);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.assert_nonempty_alloc = true;
  sopts.null_out_result_on_error = true;
  sopts.oom_string = "<jemalloc>: Error in mallocx(): out of memory\n";
  sopts.usize = true;

  dopts.result = &ret.ptr;
  dopts.num_items = 1;
  dopts.item_size = size;
  if (unlikely(flags != 0)) {
    if ((flags & MALLOCX_LG_ALIGN_MASK) != 0) {
      dopts.alignment = MALLOCX_ALIGN_GET_SPECIFIED(flags);
    }

    dopts.zero = MALLOCX_ZERO_GET(flags);

    if ((flags & MALLOCX_TCACHE_MASK) != 0) {
      if ((flags & MALLOCX_TCACHE_MASK) == MALLOCX_TCACHE_NONE) {
        dopts.tcache_ind = TCACHE_IND_NONE;
      } else {
        dopts.tcache_ind = MALLOCX_TCACHE_GET(flags);
      }
    } else {
      dopts.tcache_ind = TCACHE_IND_AUTOMATIC;
    }

    if ((flags & MALLOCX_ARENA_MASK) != 0)
      dopts.arena_ind = MALLOCX_ARENA_GET(flags);
  }

  imalloc(&sopts, &dopts);
  assert(dopts.usize == je_nallocx(size, flags));
  ret.size = dopts.usize;

  LOG("core.smallocx.exit", "result: %p, size: %zu", ret.ptr, ret.size);
  return ret;
}
#undef JEMALLOC_SMALLOCX_CONCAT_HELPER
#undef JEMALLOC_SMALLOCX_CONCAT_HELPER2
#endif

JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ATTR(malloc) JEMALLOC_ALLOC_SIZE(1)
    je_mallocx(size_t size, int flags) {
  void* ret;
  static_opts_t sopts;
  dynamic_opts_t dopts;

  LOG("core.mallocx.entry", "size: %zu, flags: %d", size, flags);

  static_opts_init(&sopts);
  dynamic_opts_init(&dopts);

  sopts.assert_nonempty_alloc = true;
  sopts.null_out_result_on_error = true;
  sopts.oom_string = "<jemalloc>: Error in mallocx(): out of memory\n";

  dopts.result = &ret;
  dopts.num_items = 1;
  dopts.item_size = size;
  if (unlikely(flags != 0)) {
    if ((flags & MALLOCX_LG_ALIGN_MASK) != 0) {
      dopts.alignment = MALLOCX_ALIGN_GET_SPECIFIED(flags);
    }

    dopts.zero = MALLOCX_ZERO_GET(flags);

    if ((flags & MALLOCX_TCACHE_MASK) != 0) {
      if ((flags & MALLOCX_TCACHE_MASK) == MALLOCX_TCACHE_NONE) {
        dopts.tcache_ind = TCACHE_IND_NONE;
      } else {
        dopts.tcache_ind = MALLOCX_TCACHE_GET(flags);
      }
    } else {
      dopts.tcache_ind = TCACHE_IND_AUTOMATIC;
    }

    if ((flags & MALLOCX_ARENA_MASK) != 0)
      dopts.arena_ind = MALLOCX_ARENA_GET(flags);
  }

  imalloc(&sopts, &dopts);
  if (sopts.slow) {
    uintptr_t args[3] = { size, flags };
    hook_invoke_alloc(hook_alloc_mallocx, ret, (uintptr_t)ret, args);
  }

  LOG("core.mallocx.exit", "result: %p", ret);
  return ret;
}

static void* irallocx_prof_sample(tsdn_t* tsdn, void* old_ptr,
  size_t old_usize, size_t usize, size_t alignment, bool zero,
  tcache_t* tcache, arena_t* arena, prof_tctx_t* tctx,
  hook_ralloc_args_t* hook_args) {
  void* p;

  if (tctx == NULL) { return NULL; }
  if (usize <= SC_SMALL_MAXCLASS) {
    p = iralloct(tsdn, old_ptr, old_usize, SC_LARGE_MINCLASS, alignment,
      zero, tcache, arena, hook_args);
    if (p == NULL) { return NULL; }
    arena_prof_promote(tsdn, p, usize);
  } else {
    p = iralloct(tsdn, old_ptr, old_usize, usize, alignment, zero, tcache,
      arena, hook_args);
  }

  return p;
}

JEMALLOC_ALWAYS_INLINE void* irallocx_prof(tsd_t* tsd, void* old_ptr,
  size_t old_usize, size_t size, size_t alignment, size_t* usize, bool zero,
  tcache_t* tcache, arena_t* arena, alloc_ctx_t* alloc_ctx,
  hook_ralloc_args_t* hook_args) {
  void* p;
  bool prof_active;
  prof_tctx_t *old_tctx, *tctx;

  prof_active = prof_active_get_unlocked();
  old_tctx = prof_tctx_get(tsd_tsdn(tsd), old_ptr, alloc_ctx);
  tctx = prof_alloc_prep(tsd, *usize, prof_active, false);
  if (unlikely((uintptr_t)tctx != (uintptr_t)1U)) {
    p = irallocx_prof_sample(tsd_tsdn(tsd), old_ptr, old_usize, *usize,
      alignment, zero, tcache, arena, tctx, hook_args);
  } else {
    p = iralloct(tsd_tsdn(tsd), old_ptr, old_usize, size, alignment, zero,
      tcache, arena, hook_args);
  }
  if (unlikely(p == NULL)) {
    prof_alloc_rollback(tsd, tctx, false);
    return NULL;
  }

  if (p == old_ptr && alignment != 0) {
    /*
     * The allocation did not move, so it is possible that the size
     * class is smaller than would guarantee the requested
     * alignment, and that the alignment constraint was
     * serendipitously satisfied.  Additionally, old_usize may not
     * be the same as the current usize because of in-place large
     * reallocation.  Therefore, query the actual value of usize.
     */
    *usize = isalloc(tsd_tsdn(tsd), p);
  }
  prof_realloc(
    tsd, p, *usize, tctx, prof_active, false, old_ptr, old_usize, old_tctx);

  return p;
}

JEMALLOC_EXPORT JEMALLOC_ALLOCATOR JEMALLOC_RESTRICT_RETURN void
  JEMALLOC_NOTHROW*
  JEMALLOC_ALLOC_SIZE(2) je_rallocx(void* ptr, size_t size, int flags) {
  void* p;
  tsd_t* tsd;
  size_t usize;
  size_t old_usize;
  size_t alignment = MALLOCX_ALIGN_GET(flags);
  bool zero = flags & MALLOCX_ZERO;
  arena_t* arena;
  tcache_t* tcache;

  LOG("core.rallocx.entry", "ptr: %p, size: %zu, flags: %d", ptr, size,
    flags);

  assert(ptr != NULL);
  assert(size != 0);
  assert(malloc_initialized() || IS_INITIALIZER);
  tsd = tsd_fetch();
  check_entry_exit_locking(tsd_tsdn(tsd));

  if (unlikely((flags & MALLOCX_ARENA_MASK) != 0)) {
    unsigned arena_ind = MALLOCX_ARENA_GET(flags);
    arena = arena_get(tsd_tsdn(tsd), arena_ind, true);
    if (unlikely(arena == NULL)) { goto label_oom; }
  } else {
    arena = NULL;
  }

  if (unlikely((flags & MALLOCX_TCACHE_MASK) != 0)) {
    if ((flags & MALLOCX_TCACHE_MASK) == MALLOCX_TCACHE_NONE) {
      tcache = NULL;
    } else {
      tcache = tcaches_get(tsd, MALLOCX_TCACHE_GET(flags));
    }
  } else {
    tcache = tcache_get(tsd);
  }

  alloc_ctx_t alloc_ctx;
  rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
  rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
    (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
  assert(alloc_ctx.szind != SC_NSIZES);
  old_usize = sz_index2size(alloc_ctx.szind);
  assert(old_usize == isalloc(tsd_tsdn(tsd), ptr));

  hook_ralloc_args_t hook_args
    = { false, { (uintptr_t)ptr, size, flags, 0 } };
  if (config_prof && opt_prof) {
    usize = (alignment == 0) ? sz_s2u(size) : sz_sa2u(size, alignment);
    if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) {
      goto label_oom;
    }
    p = irallocx_prof(tsd, ptr, old_usize, size, alignment, &usize, zero,
      tcache, arena, &alloc_ctx, &hook_args);
    if (unlikely(p == NULL)) { goto label_oom; }
  } else {
    p = iralloct(tsd_tsdn(tsd), ptr, old_usize, size, alignment, zero,
      tcache, arena, &hook_args);
    if (unlikely(p == NULL)) { goto label_oom; }
    if (config_stats) { usize = isalloc(tsd_tsdn(tsd), p); }
  }
  assert(alignment == 0 || ((uintptr_t)p & (alignment - 1)) == ZU(0));

  if (config_stats) {
    *tsd_thread_allocatedp_get(tsd) += usize;
    *tsd_thread_deallocatedp_get(tsd) += old_usize;
  }
  UTRACE(ptr, size, p);
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.rallocx.exit", "result: %p", p);
  return p;
label_oom:
  if (config_xmalloc && unlikely(opt_xmalloc)) {
    malloc_write("<jemalloc>: Error in rallocx(): out of memory\n");
    abort();
  }
  UTRACE(ptr, size, 0);
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.rallocx.exit", "result: %p", NULL);
  return NULL;
}

JEMALLOC_ALWAYS_INLINE size_t ixallocx_helper(tsdn_t* tsdn, void* ptr,
  size_t old_usize, size_t size, size_t extra, size_t alignment,
  bool zero) {
  size_t newsize;

  if (ixalloc(
        tsdn, ptr, old_usize, size, extra, alignment, zero, &newsize)) {
    return old_usize;
  }

  return newsize;
}

static size_t ixallocx_prof_sample(tsdn_t* tsdn, void* ptr,
  size_t old_usize, size_t size, size_t extra, size_t alignment, bool zero,
  prof_tctx_t* tctx) {
  size_t usize;

  if (tctx == NULL) { return old_usize; }
  usize
    = ixallocx_helper(tsdn, ptr, old_usize, size, extra, alignment, zero);

  return usize;
}

JEMALLOC_ALWAYS_INLINE size_t ixallocx_prof(tsd_t* tsd, void* ptr,
  size_t old_usize, size_t size, size_t extra, size_t alignment, bool zero,
  alloc_ctx_t* alloc_ctx) {
  size_t usize_max, usize;
  bool prof_active;
  prof_tctx_t *old_tctx, *tctx;

  prof_active = prof_active_get_unlocked();
  old_tctx = prof_tctx_get(tsd_tsdn(tsd), ptr, alloc_ctx);
  /*
   * usize isn't knowable before ixalloc() returns when extra is non-zero.
   * Therefore, compute its maximum possible value and use that in
   * prof_alloc_prep() to decide whether to capture a backtrace.
   * prof_realloc() will use the actual usize to decide whether to sample.
   */
  if (alignment == 0) {
    usize_max = sz_s2u(size + extra);
    assert(usize_max > 0 && usize_max <= SC_LARGE_MAXCLASS);
  } else {
    usize_max = sz_sa2u(size + extra, alignment);
    if (unlikely(usize_max == 0 || usize_max > SC_LARGE_MAXCLASS)) {
      /*
       * usize_max is out of range, and chances are that
       * allocation will fail, but use the maximum possible
       * value and carry on with prof_alloc_prep(), just in
       * case allocation succeeds.
       */
      usize_max = SC_LARGE_MAXCLASS;
    }
  }
  tctx = prof_alloc_prep(tsd, usize_max, prof_active, false);

  if (unlikely((uintptr_t)tctx != (uintptr_t)1U)) {
    usize = ixallocx_prof_sample(
      tsd_tsdn(tsd), ptr, old_usize, size, extra, alignment, zero, tctx);
  } else {
    usize = ixallocx_helper(
      tsd_tsdn(tsd), ptr, old_usize, size, extra, alignment, zero);
  }
  if (usize == old_usize) {
    prof_alloc_rollback(tsd, tctx, false);
    return usize;
  }
  prof_realloc(
    tsd, ptr, usize, tctx, prof_active, false, ptr, old_usize, old_tctx);

  return usize;
}

JEMALLOC_EXPORT size_t JEMALLOC_NOTHROW je_xallocx(
  void* ptr, size_t size, size_t extra, int flags) {
  tsd_t* tsd;
  size_t usize, old_usize;
  size_t alignment = MALLOCX_ALIGN_GET(flags);
  bool zero = flags & MALLOCX_ZERO;

  LOG("core.xallocx.entry",
    "ptr: %p, size: %zu, extra: %zu, "
    "flags: %d",
    ptr, size, extra, flags);

  assert(ptr != NULL);
  assert(size != 0);
  assert(SIZE_T_MAX - size >= extra);
  assert(malloc_initialized() || IS_INITIALIZER);
  tsd = tsd_fetch();
  check_entry_exit_locking(tsd_tsdn(tsd));

  alloc_ctx_t alloc_ctx;
  rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
  rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
    (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
  assert(alloc_ctx.szind != SC_NSIZES);
  old_usize = sz_index2size(alloc_ctx.szind);
  assert(old_usize == isalloc(tsd_tsdn(tsd), ptr));
  /*
   * The API explicitly absolves itself of protecting against (size +
   * extra) numerical overflow, but we may need to clamp extra to avoid
   * exceeding SC_LARGE_MAXCLASS.
   *
   * Ordinarily, size limit checking is handled deeper down, but here we
   * have to check as part of (size + extra) clamping, since we need the
   * clamped value in the above helper functions.
   */
  if (unlikely(size > SC_LARGE_MAXCLASS)) {
    usize = old_usize;
    goto label_not_resized;
  }
  if (unlikely(SC_LARGE_MAXCLASS - size < extra)) {
    extra = SC_LARGE_MAXCLASS - size;
  }

  if (config_prof && opt_prof) {
    usize = ixallocx_prof(
      tsd, ptr, old_usize, size, extra, alignment, zero, &alloc_ctx);
  } else {
    usize = ixallocx_helper(
      tsd_tsdn(tsd), ptr, old_usize, size, extra, alignment, zero);
  }
  if (unlikely(usize == old_usize)) { goto label_not_resized; }

  if (config_stats) {
    *tsd_thread_allocatedp_get(tsd) += usize;
    *tsd_thread_deallocatedp_get(tsd) += old_usize;
  }
label_not_resized:
  if (unlikely(!tsd_fast(tsd))) {
    uintptr_t args[4] = { (uintptr_t)ptr, size, extra, flags };
    hook_invoke_expand(
      hook_expand_xallocx, ptr, old_usize, usize, (uintptr_t)usize, args);
  }

  UTRACE(ptr, size, ptr);
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.xallocx.exit", "result: %zu", usize);
  return usize;
}

JEMALLOC_EXPORT size_t JEMALLOC_NOTHROW JEMALLOC_ATTR(pure)
  je_sallocx(const void* ptr, int flags) {
  size_t usize;
  tsdn_t* tsdn;

  LOG("core.sallocx.entry", "ptr: %p, flags: %d", ptr, flags);

  assert(malloc_initialized() || IS_INITIALIZER);
  assert(ptr != NULL);

  tsdn = tsdn_fetch();
  check_entry_exit_locking(tsdn);

  if (config_debug || force_ivsalloc) {
    usize = ivsalloc(tsdn, ptr);
    assert(force_ivsalloc || usize != 0);
  } else {
    usize = isalloc(tsdn, ptr);
  }

  check_entry_exit_locking(tsdn);

  LOG("core.sallocx.exit", "result: %zu", usize);
  return usize;
}

JEMALLOC_EXPORT void JEMALLOC_NOTHROW je_dallocx(void* ptr, int flags) {
  LOG("core.dallocx.entry", "ptr: %p, flags: %d", ptr, flags);

  assert(ptr != NULL);
  assert(malloc_initialized() || IS_INITIALIZER);

  tsd_t* tsd = tsd_fetch();
  bool fast = tsd_fast(tsd);
  check_entry_exit_locking(tsd_tsdn(tsd));

  tcache_t* tcache;
  if (unlikely((flags & MALLOCX_TCACHE_MASK) != 0)) {
    /* Not allowed to be reentrant and specify a custom tcache. */
    assert(tsd_reentrancy_level_get(tsd) == 0);
    if ((flags & MALLOCX_TCACHE_MASK) == MALLOCX_TCACHE_NONE) {
      tcache = NULL;
    } else {
      tcache = tcaches_get(tsd, MALLOCX_TCACHE_GET(flags));
    }
  } else {
    if (likely(fast)) {
      tcache = tsd_tcachep_get(tsd);
      assert(tcache == tcache_get(tsd));
    } else {
      if (likely(tsd_reentrancy_level_get(tsd) == 0)) {
        tcache = tcache_get(tsd);
      } else {
        tcache = NULL;
      }
    }
  }

  UTRACE(ptr, 0, 0);
  if (likely(fast)) {
    tsd_assert_fast(tsd);
    ifree(tsd, ptr, tcache, false);
  } else {
    uintptr_t args_raw[3] = { (uintptr_t)ptr, flags };
    hook_invoke_dalloc(hook_dalloc_dallocx, ptr, args_raw);
    ifree(tsd, ptr, tcache, true);
  }
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.dallocx.exit", "");
}

JEMALLOC_ALWAYS_INLINE size_t inallocx(
  tsdn_t* tsdn, size_t size, int flags) {
  check_entry_exit_locking(tsdn);

  size_t usize;
  if (likely((flags & MALLOCX_LG_ALIGN_MASK) == 0)) {
    usize = sz_s2u(size);
  } else {
    usize = sz_sa2u(size, MALLOCX_ALIGN_GET_SPECIFIED(flags));
  }
  check_entry_exit_locking(tsdn);
  return usize;
}

JEMALLOC_NOINLINE void sdallocx_default(void* ptr, size_t size, int flags) {
  assert(ptr != NULL);
  assert(malloc_initialized() || IS_INITIALIZER);

  tsd_t* tsd = tsd_fetch();
  bool fast = tsd_fast(tsd);
  size_t usize = inallocx(tsd_tsdn(tsd), size, flags);
  assert(usize == isalloc(tsd_tsdn(tsd), ptr));
  check_entry_exit_locking(tsd_tsdn(tsd));

  tcache_t* tcache;
  if (unlikely((flags & MALLOCX_TCACHE_MASK) != 0)) {
    /* Not allowed to be reentrant and specify a custom tcache. */
    assert(tsd_reentrancy_level_get(tsd) == 0);
    if ((flags & MALLOCX_TCACHE_MASK) == MALLOCX_TCACHE_NONE) {
      tcache = NULL;
    } else {
      tcache = tcaches_get(tsd, MALLOCX_TCACHE_GET(flags));
    }
  } else {
    if (likely(fast)) {
      tcache = tsd_tcachep_get(tsd);
      assert(tcache == tcache_get(tsd));
    } else {
      if (likely(tsd_reentrancy_level_get(tsd) == 0)) {
        tcache = tcache_get(tsd);
      } else {
        tcache = NULL;
      }
    }
  }

  UTRACE(ptr, 0, 0);
  if (likely(fast)) {
    tsd_assert_fast(tsd);
    isfree(tsd, ptr, usize, tcache, false);
  } else {
    uintptr_t args_raw[3] = { (uintptr_t)ptr, size, flags };
    hook_invoke_dalloc(hook_dalloc_sdallocx, ptr, args_raw);
    isfree(tsd, ptr, usize, tcache, true);
  }
  check_entry_exit_locking(tsd_tsdn(tsd));
}

JEMALLOC_EXPORT void JEMALLOC_NOTHROW je_sdallocx(
  void* ptr, size_t size, int flags) {
  LOG("core.sdallocx.entry", "ptr: %p, size: %zu, flags: %d", ptr, size,
    flags);

  if (flags != 0 || !free_fastpath(ptr, size, true)) {
    sdallocx_default(ptr, size, flags);
  }

  LOG("core.sdallocx.exit", "");
}

void JEMALLOC_NOTHROW je_sdallocx_noflags(void* ptr, size_t size) {
  LOG("core.sdallocx.entry", "ptr: %p, size: %zu, flags: 0", ptr, size);

  if (!free_fastpath(ptr, size, true)) { sdallocx_default(ptr, size, 0); }

  LOG("core.sdallocx.exit", "");
}

JEMALLOC_EXPORT size_t JEMALLOC_NOTHROW JEMALLOC_ATTR(pure)
  je_nallocx(size_t size, int flags) {
  size_t usize;
  tsdn_t* tsdn;

  assert(size != 0);

  if (unlikely(malloc_init())) {
    LOG("core.nallocx.exit", "result: %zu", ZU(0));
    return 0;
  }

  tsdn = tsdn_fetch();
  check_entry_exit_locking(tsdn);

  usize = inallocx(tsdn, size, flags);
  if (unlikely(usize > SC_LARGE_MAXCLASS)) {
    LOG("core.nallocx.exit", "result: %zu", ZU(0));
    return 0;
  }

  check_entry_exit_locking(tsdn);
  LOG("core.nallocx.exit", "result: %zu", usize);
  return usize;
}

JEMALLOC_EXPORT int JEMALLOC_NOTHROW je_mallctl(const char* name,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  tsd_t* tsd;

  LOG("core.mallctl.entry", "name: %s", name);

  if (unlikely(malloc_init())) {
    LOG("core.mallctl.exit", "result: %d", EAGAIN);
    return EAGAIN;
  }

  tsd = tsd_fetch();
  check_entry_exit_locking(tsd_tsdn(tsd));
  ret = ctl_byname(tsd, name, oldp, oldlenp, newp, newlen);
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.mallctl.exit", "result: %d", ret);
  return ret;
}

JEMALLOC_EXPORT int JEMALLOC_NOTHROW je_mallctlnametomib(
  const char* name, size_t* mibp, size_t* miblenp) {
  int ret;

  LOG("core.mallctlnametomib.entry", "name: %s", name);

  if (unlikely(malloc_init())) {
    LOG("core.mallctlnametomib.exit", "result: %d", EAGAIN);
    return EAGAIN;
  }

  tsd_t* tsd = tsd_fetch();
  check_entry_exit_locking(tsd_tsdn(tsd));
  ret = ctl_nametomib(tsd, name, mibp, miblenp);
  check_entry_exit_locking(tsd_tsdn(tsd));

  LOG("core.mallctlnametomib.exit", "result: %d", ret);
  return ret;
}

JEMALLOC_EXPORT int JEMALLOC_NOTHROW je_mallctlbymib(const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  tsd_t* tsd;

  LOG("core.mallctlbymib.entry", "");

  if (unlikely(malloc_init())) {
    LOG("core.mallctlbymib.exit", "result: %d", EAGAIN);
    return EAGAIN;
  }

  tsd = tsd_fetch();
  check_entry_exit_locking(tsd_tsdn(tsd));
  ret = ctl_bymib(tsd, mib, miblen, oldp, oldlenp, newp, newlen);
  check_entry_exit_locking(tsd_tsdn(tsd));
  LOG("core.mallctlbymib.exit", "result: %d", ret);
  return ret;
}

JEMALLOC_EXPORT void JEMALLOC_NOTHROW je_malloc_stats_print(
  void (*write_cb)(void*, const char*), void* cbopaque, const char* opts) {
  tsdn_t* tsdn;

  LOG("core.malloc_stats_print.entry", "");

  tsdn = tsdn_fetch();
  check_entry_exit_locking(tsdn);
  stats_print(write_cb, cbopaque, opts);
  check_entry_exit_locking(tsdn);
  LOG("core.malloc_stats_print.exit", "");
}

JEMALLOC_EXPORT size_t JEMALLOC_NOTHROW je_malloc_usable_size(
  JEMALLOC_USABLE_SIZE_CONST void* ptr) {
  size_t ret;
  tsdn_t* tsdn;

  LOG("core.malloc_usable_size.entry", "ptr: %p", ptr);

  assert(malloc_initialized() || IS_INITIALIZER);

  tsdn = tsdn_fetch();
  check_entry_exit_locking(tsdn);

  if (unlikely(ptr == NULL)) {
    ret = 0;
  } else {
    if (config_debug || force_ivsalloc) {
      ret = ivsalloc(tsdn, ptr);
      assert(force_ivsalloc || ret != 0);
    } else {
      ret = isalloc(tsdn, ptr);
    }
  }

  check_entry_exit_locking(tsdn);
  LOG("core.malloc_usable_size.exit", "result: %zu", ret);
  return ret;
}

/*
 * End non-standard functions.
 */
/******************************************************************************/
/*
 * The following functions are used by threading libraries for protection of
 * malloc during fork().
 */

/*
 * If an application creates a thread before doing any allocation in the
 * main thread, then calls fork(2) in the main thread followed by memory
 * allocation in the child process, a race can occur that results in
 * deadlock within the child: the main thread may have forked while the
 * created thread had partially initialized the allocator.  Ordinarily
 * jemalloc prevents fork/malloc races via the following functions it
 * registers during initialization using pthread_atfork(), but of course
 * that does no good if the allocator isn't fully initialized at fork time.
 * The following library constructor is a partial solution to this problem.
 * It may still be possible to trigger the deadlock described above, but
 * doing so would involve forking via a library constructor that runs before
 * jemalloc's runs.
 */
#ifndef JEMALLOC_JET
JEMALLOC_ATTR(constructor)
static void jemalloc_constructor(void) { malloc_init(); }
#endif

#ifndef JEMALLOC_MUTEX_INIT_CB
void jemalloc_prefork(void)
#else
JEMALLOC_EXPORT void _malloc_prefork(void)
#endif
{
  tsd_t* tsd;
  unsigned i, j, narenas;
  arena_t* arena;

#ifdef JEMALLOC_MUTEX_INIT_CB
  if (!malloc_initialized()) { return; }
#endif
  assert(malloc_initialized());

  tsd = tsd_fetch();

  narenas = narenas_total_get();

  witness_prefork(tsd_witness_tsdp_get(tsd));
  /* Acquire all mutexes in a safe order. */
  ctl_prefork(tsd_tsdn(tsd));
  tcache_prefork(tsd_tsdn(tsd));
  malloc_mutex_prefork(tsd_tsdn(tsd), &arenas_lock);
  if (have_background_thread) { background_thread_prefork0(tsd_tsdn(tsd)); }
  prof_prefork0(tsd_tsdn(tsd));
  if (have_background_thread) { background_thread_prefork1(tsd_tsdn(tsd)); }
  /* Break arena prefork into stages to preserve lock order. */
  for (i = 0; i < 8; i++) {
    for (j = 0; j < narenas; j++) {
      if ((arena = arena_get(tsd_tsdn(tsd), j, false)) != NULL) {
        switch (i) {
        case 0: arena_prefork0(tsd_tsdn(tsd), arena); break;
        case 1: arena_prefork1(tsd_tsdn(tsd), arena); break;
        case 2: arena_prefork2(tsd_tsdn(tsd), arena); break;
        case 3: arena_prefork3(tsd_tsdn(tsd), arena); break;
        case 4: arena_prefork4(tsd_tsdn(tsd), arena); break;
        case 5: arena_prefork5(tsd_tsdn(tsd), arena); break;
        case 6: arena_prefork6(tsd_tsdn(tsd), arena); break;
        case 7: arena_prefork7(tsd_tsdn(tsd), arena); break;
        default: not_reached();
        }
      }
    }
  }
  prof_prefork1(tsd_tsdn(tsd));
  tsd_prefork(tsd);
}

#ifndef JEMALLOC_MUTEX_INIT_CB
void jemalloc_postfork_parent(void)
#else
JEMALLOC_EXPORT void _malloc_postfork(void)
#endif
{
  tsd_t* tsd;
  unsigned i, narenas;

#ifdef JEMALLOC_MUTEX_INIT_CB
  if (!malloc_initialized()) { return; }
#endif
  assert(malloc_initialized());

  tsd = tsd_fetch();

  tsd_postfork_parent(tsd);

  witness_postfork_parent(tsd_witness_tsdp_get(tsd));
  /* Release all mutexes, now that fork() has completed. */
  for (i = 0, narenas = narenas_total_get(); i < narenas; i++) {
    arena_t* arena;

    if ((arena = arena_get(tsd_tsdn(tsd), i, false)) != NULL) {
      arena_postfork_parent(tsd_tsdn(tsd), arena);
    }
  }
  prof_postfork_parent(tsd_tsdn(tsd));
  if (have_background_thread) {
    background_thread_postfork_parent(tsd_tsdn(tsd));
  }
  malloc_mutex_postfork_parent(tsd_tsdn(tsd), &arenas_lock);
  tcache_postfork_parent(tsd_tsdn(tsd));
  ctl_postfork_parent(tsd_tsdn(tsd));
}

void jemalloc_postfork_child(void) {
  tsd_t* tsd;
  unsigned i, narenas;

  assert(malloc_initialized());

  tsd = tsd_fetch();

  tsd_postfork_child(tsd);

  witness_postfork_child(tsd_witness_tsdp_get(tsd));
  /* Release all mutexes, now that fork() has completed. */
  for (i = 0, narenas = narenas_total_get(); i < narenas; i++) {
    arena_t* arena;

    if ((arena = arena_get(tsd_tsdn(tsd), i, false)) != NULL) {
      arena_postfork_child(tsd_tsdn(tsd), arena);
    }
  }
  prof_postfork_child(tsd_tsdn(tsd));
  if (have_background_thread) {
    background_thread_postfork_child(tsd_tsdn(tsd));
  }
  malloc_mutex_postfork_child(tsd_tsdn(tsd), &arenas_lock);
  tcache_postfork_child(tsd_tsdn(tsd));
  ctl_postfork_child(tsd_tsdn(tsd));
}

/******************************************************************************/
#define JEMALLOC_ARENA_C_

#include "jemalloc/internal/div.h"
#include "jemalloc/internal/extent_dss.h"
#include "jemalloc/internal/extent_mmap.h"

JEMALLOC_DIAGNOSTIC_DISABLE_SPURIOUS

/******************************************************************************/
/* Data. */

/*
 * Define names for both unininitialized and initialized phases, so that
 * options and mallctl processing are straightforward.
 */
const char* percpu_arena_mode_names[]
  = { "percpu", "phycpu", "disabled", "percpu", "phycpu" };
percpu_arena_mode_t opt_percpu_arena = PERCPU_ARENA_DEFAULT;

ssize_t opt_dirty_decay_ms = DIRTY_DECAY_MS_DEFAULT;
ssize_t opt_muzzy_decay_ms = MUZZY_DECAY_MS_DEFAULT;

static atomic_zd_t dirty_decay_ms_default;
static atomic_zd_t muzzy_decay_ms_default;

const uint64_t h_steps[SMOOTHSTEP_NSTEPS] = {
#define STEP(step, h, x, y) h,
  SMOOTHSTEP
#undef STEP
};

static div_info_t arena_binind_div_info[SC_NBINS];

size_t opt_oversize_threshold = OVERSIZE_THRESHOLD_DEFAULT;
size_t oversize_threshold = OVERSIZE_THRESHOLD_DEFAULT;
static unsigned huge_arena_ind;

/******************************************************************************/
/*
 * Function prototypes for static functions that are referenced prior to
 * definition.
 */

static void arena_decay_to_limit(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, bool all, size_t npages_limit,
  size_t npages_decay_max, bool is_background_thread);
static bool arena_decay_dirty(
  tsdn_t* tsdn, arena_t* arena, bool is_background_thread, bool all);
static void arena_dalloc_bin_slab(
  tsdn_t* tsdn, arena_t* arena, extent_t* slab, bin_t* bin);
static void arena_bin_lower_slab(
  tsdn_t* tsdn, arena_t* arena, extent_t* slab, bin_t* bin);

/******************************************************************************/

void arena_basic_stats_merge(tsdn_t* tsdn, arena_t* arena,
  unsigned* nthreads, const char** dss, ssize_t* dirty_decay_ms,
  ssize_t* muzzy_decay_ms, size_t* nactive, size_t* ndirty,
  size_t* nmuzzy) {
  *nthreads += arena_nthreads_get(arena, false);
  *dss = dss_prec_names[arena_dss_prec_get(arena)];
  *dirty_decay_ms = arena_dirty_decay_ms_get(arena);
  *muzzy_decay_ms = arena_muzzy_decay_ms_get(arena);
  *nactive += atomic_load_zu(&arena->nactive, ATOMIC_RELAXED);
  *ndirty += extents_npages_get(&arena->extents_dirty);
  *nmuzzy += extents_npages_get(&arena->extents_muzzy);
}

void arena_stats_merge(tsdn_t* tsdn, arena_t* arena, unsigned* nthreads,
  const char** dss, ssize_t* dirty_decay_ms, ssize_t* muzzy_decay_ms,
  size_t* nactive, size_t* ndirty, size_t* nmuzzy, arena_stats_t* astats,
  bin_stats_t* bstats, arena_stats_large_t* lstats,
  arena_stats_extents_t* estats) {
  cassert(config_stats);

  arena_basic_stats_merge(tsdn, arena, nthreads, dss, dirty_decay_ms,
    muzzy_decay_ms, nactive, ndirty, nmuzzy);

  size_t base_allocated, base_resident, base_mapped, metadata_thp;
  base_stats_get(tsdn, arena->base, &base_allocated, &base_resident,
    &base_mapped, &metadata_thp);

  arena_stats_lock(tsdn, &arena->stats);

  arena_stats_accum_zu(&astats->mapped,
    base_mapped
      + arena_stats_read_zu(tsdn, &arena->stats, &arena->stats.mapped));
  arena_stats_accum_zu(&astats->retained,
    extents_npages_get(&arena->extents_retained) << LG_PAGE);

  atomic_store_zu(&astats->extent_avail,
    atomic_load_zu(&arena->extent_avail_cnt, ATOMIC_RELAXED),
    ATOMIC_RELAXED);

  arena_stats_accum_u64(&astats->decay_dirty.npurge,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_dirty.npurge));
  arena_stats_accum_u64(&astats->decay_dirty.nmadvise,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_dirty.nmadvise));
  arena_stats_accum_u64(&astats->decay_dirty.purged,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_dirty.purged));

  arena_stats_accum_u64(&astats->decay_muzzy.npurge,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_muzzy.npurge));
  arena_stats_accum_u64(&astats->decay_muzzy.nmadvise,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_muzzy.nmadvise));
  arena_stats_accum_u64(&astats->decay_muzzy.purged,
    arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.decay_muzzy.purged));

  arena_stats_accum_zu(&astats->base, base_allocated);
  arena_stats_accum_zu(&astats->internal, arena_internal_get(arena));
  arena_stats_accum_zu(&astats->metadata_thp, metadata_thp);
  arena_stats_accum_zu(&astats->resident,
    base_resident
      + (((atomic_load_zu(&arena->nactive, ATOMIC_RELAXED)
            + extents_npages_get(&arena->extents_dirty)
            + extents_npages_get(&arena->extents_muzzy))
        << LG_PAGE)));
  arena_stats_accum_zu(&astats->abandoned_vm,
    atomic_load_zu(&arena->stats.abandoned_vm, ATOMIC_RELAXED));

  for (szind_t i = 0; i < SC_NSIZES - SC_NBINS; i++) {
    uint64_t nmalloc = arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.lstats[i].nmalloc);
    arena_stats_accum_u64(&lstats[i].nmalloc, nmalloc);
    arena_stats_accum_u64(&astats->nmalloc_large, nmalloc);

    uint64_t ndalloc = arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.lstats[i].ndalloc);
    arena_stats_accum_u64(&lstats[i].ndalloc, ndalloc);
    arena_stats_accum_u64(&astats->ndalloc_large, ndalloc);

    uint64_t nrequests = arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.lstats[i].nrequests);
    arena_stats_accum_u64(&lstats[i].nrequests, nmalloc + nrequests);
    arena_stats_accum_u64(&astats->nrequests_large, nmalloc + nrequests);

    /* nfill == nmalloc for large currently. */
    arena_stats_accum_u64(&lstats[i].nfills, nmalloc);
    arena_stats_accum_u64(&astats->nfills_large, nmalloc);

    uint64_t nflush = arena_stats_read_u64(
      tsdn, &arena->stats, &arena->stats.lstats[i].nflushes);
    arena_stats_accum_u64(&lstats[i].nflushes, nflush);
    arena_stats_accum_u64(&astats->nflushes_large, nflush);

    assert(nmalloc >= ndalloc);
    assert(nmalloc - ndalloc <= SIZE_T_MAX);
    size_t curlextents = (size_t)(nmalloc - ndalloc);
    lstats[i].curlextents += curlextents;
    arena_stats_accum_zu(
      &astats->allocated_large, curlextents * sz_index2size(SC_NBINS + i));
  }

  for (pszind_t i = 0; i < SC_NPSIZES; i++) {
    size_t dirty, muzzy, retained, dirty_bytes, muzzy_bytes, retained_bytes;
    dirty = extents_nextents_get(&arena->extents_dirty, i);
    muzzy = extents_nextents_get(&arena->extents_muzzy, i);
    retained = extents_nextents_get(&arena->extents_retained, i);
    dirty_bytes = extents_nbytes_get(&arena->extents_dirty, i);
    muzzy_bytes = extents_nbytes_get(&arena->extents_muzzy, i);
    retained_bytes = extents_nbytes_get(&arena->extents_retained, i);

    atomic_store_zu(&estats[i].ndirty, dirty, ATOMIC_RELAXED);
    atomic_store_zu(&estats[i].nmuzzy, muzzy, ATOMIC_RELAXED);
    atomic_store_zu(&estats[i].nretained, retained, ATOMIC_RELAXED);
    atomic_store_zu(&estats[i].dirty_bytes, dirty_bytes, ATOMIC_RELAXED);
    atomic_store_zu(&estats[i].muzzy_bytes, muzzy_bytes, ATOMIC_RELAXED);
    atomic_store_zu(
      &estats[i].retained_bytes, retained_bytes, ATOMIC_RELAXED);
  }

  arena_stats_unlock(tsdn, &arena->stats);

  /* tcache_bytes counts currently cached bytes. */
  atomic_store_zu(&astats->tcache_bytes, 0, ATOMIC_RELAXED);
  malloc_mutex_lock(tsdn, &arena->tcache_ql_mtx);
  cache_bin_array_descriptor_t* descriptor;
  ql_foreach(descriptor, &arena->cache_bin_array_descriptor_ql, link) {
    szind_t i = 0;
    for (; i < SC_NBINS; i++) {
      cache_bin_t* tbin = &descriptor->bins_small[i];
      arena_stats_accum_zu(
        &astats->tcache_bytes, tbin->ncached * sz_index2size(i));
    }
    for (; i < nhbins; i++) {
      cache_bin_t* tbin = &descriptor->bins_large[i];
      arena_stats_accum_zu(
        &astats->tcache_bytes, tbin->ncached * sz_index2size(i));
    }
  }
  malloc_mutex_prof_read(tsdn,
    &astats->mutex_prof_data[arena_prof_mutex_tcache_list],
    &arena->tcache_ql_mtx);
  malloc_mutex_unlock(tsdn, &arena->tcache_ql_mtx);

#define READ_ARENA_MUTEX_PROF_DATA(mtx, ind)                               \
  malloc_mutex_lock(tsdn, &arena->mtx);                                    \
  malloc_mutex_prof_read(                                                  \
    tsdn, &astats->mutex_prof_data[ind], &arena->mtx);                     \
  malloc_mutex_unlock(tsdn, &arena->mtx);

  /* Gather per arena mutex profiling data. */
  READ_ARENA_MUTEX_PROF_DATA(large_mtx, arena_prof_mutex_large);
  READ_ARENA_MUTEX_PROF_DATA(
    extent_avail_mtx, arena_prof_mutex_extent_avail)
  READ_ARENA_MUTEX_PROF_DATA(
    extents_dirty.mtx, arena_prof_mutex_extents_dirty)
  READ_ARENA_MUTEX_PROF_DATA(
    extents_muzzy.mtx, arena_prof_mutex_extents_muzzy)
  READ_ARENA_MUTEX_PROF_DATA(
    extents_retained.mtx, arena_prof_mutex_extents_retained)
  READ_ARENA_MUTEX_PROF_DATA(decay_dirty.mtx, arena_prof_mutex_decay_dirty)
  READ_ARENA_MUTEX_PROF_DATA(decay_muzzy.mtx, arena_prof_mutex_decay_muzzy)
  READ_ARENA_MUTEX_PROF_DATA(base->mtx, arena_prof_mutex_base)
#undef READ_ARENA_MUTEX_PROF_DATA

  nstime_copy(&astats->uptime, &arena->create_time);
  nstime_update(&astats->uptime);
  nstime_subtract(&astats->uptime, &arena->create_time);

  for (szind_t i = 0; i < SC_NBINS; i++) {
    for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
      bin_stats_merge(tsdn, &bstats[i], &arena->bins[i].bin_shards[j]);
    }
  }
}

void arena_extents_dirty_dalloc(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extents_dalloc(
    tsdn, arena, r_extent_hooks, &arena->extents_dirty, extent);
  if (arena_dirty_decay_ms_get(arena) == 0) {
    arena_decay_dirty(tsdn, arena, false, true);
  } else {
    arena_background_thread_inactivity_check(tsdn, arena, false);
  }
}

static void* arena_slab_reg_alloc(
  extent_t* slab, const bin_info_t* bin_info) {
  void* ret;
  arena_slab_data_t* slab_data = extent_slab_data_get(slab);
  size_t regind;

  assert(extent_nfree_get(slab) > 0);
  assert(!bitmap_full(slab_data->bitmap, &bin_info->bitmap_info));

  regind = bitmap_sfu(slab_data->bitmap, &bin_info->bitmap_info);
  ret = (void*)((uintptr_t)extent_addr_get(slab)
    + (uintptr_t)(bin_info->reg_size * regind));
  extent_nfree_dec(slab);
  return ret;
}

static void arena_slab_reg_alloc_batch(
  extent_t* slab, const bin_info_t* bin_info, unsigned cnt, void** ptrs) {
  arena_slab_data_t* slab_data = extent_slab_data_get(slab);

  assert(extent_nfree_get(slab) >= cnt);
  assert(!bitmap_full(slab_data->bitmap, &bin_info->bitmap_info));

#if (!defined JEMALLOC_INTERNAL_POPCOUNTL) || (defined BITMAP_USE_TREE)
  for (unsigned i = 0; i < cnt; i++) {
    size_t regind = bitmap_sfu(slab_data->bitmap, &bin_info->bitmap_info);
    *(ptrs + i) = (void*)((uintptr_t)extent_addr_get(slab)
      + (uintptr_t)(bin_info->reg_size * regind));
  }
#else
  unsigned group = 0;
  bitmap_t g = slab_data->bitmap[group];
  unsigned i = 0;
  while (i < cnt) {
    while (g == 0) { g = slab_data->bitmap[++group]; }
    size_t shift = group << LG_BITMAP_GROUP_NBITS;
    size_t pop = popcount_lu(g);
    if (pop > (cnt - i)) { pop = cnt - i; }

    /*
     * Load from memory locations only once, outside the
     * hot loop below.
     */
    uintptr_t base = (uintptr_t)extent_addr_get(slab);
    uintptr_t regsize = (uintptr_t)bin_info->reg_size;
    while (pop--) {
      size_t bit = cfs_lu(&g);
      size_t regind = shift + bit;
      *(ptrs + i) = (void*)(base + regsize * regind);

      i++;
    }
    slab_data->bitmap[group] = g;
  }
#endif
  extent_nfree_sub(slab, cnt);
}

#ifndef JEMALLOC_JET
static
#endif
  size_t
  arena_slab_regind(extent_t* slab, szind_t binind, const void* ptr) {
  size_t diff, regind;

  /* Freeing a pointer outside the slab can cause assertion failure. */
  assert((uintptr_t)ptr >= (uintptr_t)extent_addr_get(slab));
  assert((uintptr_t)ptr < (uintptr_t)extent_past_get(slab));
  /* Freeing an interior pointer can cause assertion failure. */
  assert(((uintptr_t)ptr - (uintptr_t)extent_addr_get(slab))
      % (uintptr_t)bin_infos[binind].reg_size
    == 0);

  diff = (size_t)((uintptr_t)ptr - (uintptr_t)extent_addr_get(slab));

  /* Avoid doing division with a variable divisor. */
  regind = div_compute(&arena_binind_div_info[binind], diff);

  assert(regind < bin_infos[binind].nregs);

  return regind;
}

static void arena_slab_reg_dalloc(
  extent_t* slab, arena_slab_data_t* slab_data, void* ptr) {
  szind_t binind = extent_szind_get(slab);
  const bin_info_t* bin_info = &bin_infos[binind];
  size_t regind = arena_slab_regind(slab, binind, ptr);

  assert(extent_nfree_get(slab) < bin_info->nregs);
  /* Freeing an unallocated pointer can cause assertion failure. */
  assert(bitmap_get(slab_data->bitmap, &bin_info->bitmap_info, regind));

  bitmap_unset(slab_data->bitmap, &bin_info->bitmap_info, regind);
  extent_nfree_inc(slab);
}

static void arena_nactive_add(arena_t* arena, size_t add_pages) {
  atomic_fetch_add_zu(&arena->nactive, add_pages, ATOMIC_RELAXED);
}

static void arena_nactive_sub(arena_t* arena, size_t sub_pages) {
  assert(atomic_load_zu(&arena->nactive, ATOMIC_RELAXED) >= sub_pages);
  atomic_fetch_sub_zu(&arena->nactive, sub_pages, ATOMIC_RELAXED);
}

static void arena_large_malloc_stats_update(
  tsdn_t* tsdn, arena_t* arena, size_t usize) {
  szind_t index, hindex;

  cassert(config_stats);

  if (usize < SC_LARGE_MINCLASS) { usize = SC_LARGE_MINCLASS; }
  index = sz_size2index(usize);
  hindex = (index >= SC_NBINS) ? index - SC_NBINS : 0;

  arena_stats_add_u64(
    tsdn, &arena->stats, &arena->stats.lstats[hindex].nmalloc, 1);
}

static void arena_large_dalloc_stats_update(
  tsdn_t* tsdn, arena_t* arena, size_t usize) {
  szind_t index, hindex;

  cassert(config_stats);

  if (usize < SC_LARGE_MINCLASS) { usize = SC_LARGE_MINCLASS; }
  index = sz_size2index(usize);
  hindex = (index >= SC_NBINS) ? index - SC_NBINS : 0;

  arena_stats_add_u64(
    tsdn, &arena->stats, &arena->stats.lstats[hindex].ndalloc, 1);
}

static void arena_large_ralloc_stats_update(
  tsdn_t* tsdn, arena_t* arena, size_t oldusize, size_t usize) {
  arena_large_dalloc_stats_update(tsdn, arena, oldusize);
  arena_large_malloc_stats_update(tsdn, arena, usize);
}

static bool arena_may_have_muzzy(arena_t* arena) {
  return (pages_can_purge_lazy && (arena_muzzy_decay_ms_get(arena) != 0));
}

extent_t* arena_extent_alloc_large(tsdn_t* tsdn, arena_t* arena,
  size_t usize, size_t alignment, bool* zero) {
  extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;

  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  szind_t szind = sz_size2index(usize);
  size_t mapped_add;
  bool commit = true;
  extent_t* extent
    = extents_alloc(tsdn, arena, &extent_hooks, &arena->extents_dirty, NULL,
      usize, sz_large_pad, alignment, false, szind, zero, &commit);
  if (extent == NULL && arena_may_have_muzzy(arena)) {
    extent
      = extents_alloc(tsdn, arena, &extent_hooks, &arena->extents_muzzy,
        NULL, usize, sz_large_pad, alignment, false, szind, zero, &commit);
  }
  size_t size = usize + sz_large_pad;
  if (extent == NULL) {
    extent = extent_alloc_wrapper(tsdn, arena, &extent_hooks, NULL, usize,
      sz_large_pad, alignment, false, szind, zero, &commit);
    if (config_stats) {
      /*
       * extent may be NULL on OOM, but in that case
       * mapped_add isn't used below, so there's no need to
       * conditionlly set it to 0 here.
       */
      mapped_add = size;
    }
  } else if (config_stats) {
    mapped_add = 0;
  }

  if (extent != NULL) {
    if (config_stats) {
      arena_stats_lock(tsdn, &arena->stats);
      arena_large_malloc_stats_update(tsdn, arena, usize);
      if (mapped_add != 0) {
        arena_stats_add_zu(
          tsdn, &arena->stats, &arena->stats.mapped, mapped_add);
      }
      arena_stats_unlock(tsdn, &arena->stats);
    }
    arena_nactive_add(arena, size >> LG_PAGE);
  }

  return extent;
}

void arena_extent_dalloc_large_prep(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent) {
  if (config_stats) {
    arena_stats_lock(tsdn, &arena->stats);
    arena_large_dalloc_stats_update(tsdn, arena, extent_usize_get(extent));
    arena_stats_unlock(tsdn, &arena->stats);
  }
  arena_nactive_sub(arena, extent_size_get(extent) >> LG_PAGE);
}

void arena_extent_ralloc_large_shrink(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent, size_t oldusize) {
  size_t usize = extent_usize_get(extent);
  size_t udiff = oldusize - usize;

  if (config_stats) {
    arena_stats_lock(tsdn, &arena->stats);
    arena_large_ralloc_stats_update(tsdn, arena, oldusize, usize);
    arena_stats_unlock(tsdn, &arena->stats);
  }
  arena_nactive_sub(arena, udiff >> LG_PAGE);
}

void arena_extent_ralloc_large_expand(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent, size_t oldusize) {
  size_t usize = extent_usize_get(extent);
  size_t udiff = usize - oldusize;

  if (config_stats) {
    arena_stats_lock(tsdn, &arena->stats);
    arena_large_ralloc_stats_update(tsdn, arena, oldusize, usize);
    arena_stats_unlock(tsdn, &arena->stats);
  }
  arena_nactive_add(arena, udiff >> LG_PAGE);
}

static ssize_t arena_decay_ms_read(arena_decay_t* decay) {
  return atomic_load_zd(&decay->time_ms, ATOMIC_RELAXED);
}

static void arena_decay_ms_write(arena_decay_t* decay, ssize_t decay_ms) {
  atomic_store_zd(&decay->time_ms, decay_ms, ATOMIC_RELAXED);
}

static void arena_decay_deadline_init(arena_decay_t* decay) {
  /*
   * Generate a new deadline that is uniformly random within the next
   * epoch after the current one.
   */
  nstime_copy(&decay->deadline, &decay->epoch);
  nstime_add(&decay->deadline, &decay->interval);
  if (arena_decay_ms_read(decay) > 0) {
    nstime_t jitter;

    nstime_init(&jitter,
      prng_range_u64(&decay->jitter_state, nstime_ns(&decay->interval)));
    nstime_add(&decay->deadline, &jitter);
  }
}

static bool arena_decay_deadline_reached(
  const arena_decay_t* decay, const nstime_t* time) {
  return (nstime_compare(&decay->deadline, time) <= 0);
}

static size_t arena_decay_backlog_npages_limit(const arena_decay_t* decay) {
  uint64_t sum;
  size_t npages_limit_backlog;
  unsigned i;

  /*
   * For each element of decay_backlog, multiply by the corresponding
   * fixed-point smoothstep decay factor.  Sum the products, then divide
   * to round down to the nearest whole number of pages.
   */
  sum = 0;
  for (i = 0; i < SMOOTHSTEP_NSTEPS; i++) {
    sum += decay->backlog[i] * h_steps[i];
  }
  npages_limit_backlog = (size_t)(sum >> SMOOTHSTEP_BFP);

  return npages_limit_backlog;
}

static void arena_decay_backlog_update_last(
  arena_decay_t* decay, size_t current_npages) {
  size_t npages_delta = (current_npages > decay->nunpurged)
    ? current_npages - decay->nunpurged
    : 0;
  decay->backlog[SMOOTHSTEP_NSTEPS - 1] = npages_delta;

  if (config_debug) {
    if (current_npages > decay->ceil_npages) {
      decay->ceil_npages = current_npages;
    }
    size_t npages_limit = arena_decay_backlog_npages_limit(decay);
    assert(decay->ceil_npages >= npages_limit);
    if (decay->ceil_npages > npages_limit) {
      decay->ceil_npages = npages_limit;
    }
  }
}

static void arena_decay_backlog_update(
  arena_decay_t* decay, uint64_t nadvance_u64, size_t current_npages) {
  if (nadvance_u64 >= SMOOTHSTEP_NSTEPS) {
    memset(decay->backlog, 0, (SMOOTHSTEP_NSTEPS - 1) * sizeof(size_t));
  } else {
    size_t nadvance_z = (size_t)nadvance_u64;

    assert((uint64_t)nadvance_z == nadvance_u64);

    memmove(decay->backlog, &decay->backlog[nadvance_z],
      (SMOOTHSTEP_NSTEPS - nadvance_z) * sizeof(size_t));
    if (nadvance_z > 1) {
      memset(&decay->backlog[SMOOTHSTEP_NSTEPS - nadvance_z], 0,
        (nadvance_z - 1) * sizeof(size_t));
    }
  }

  arena_decay_backlog_update_last(decay, current_npages);
}

static void arena_decay_try_purge(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, size_t current_npages,
  size_t npages_limit, bool is_background_thread) {
  if (current_npages > npages_limit) {
    arena_decay_to_limit(tsdn, arena, decay, extents, false, npages_limit,
      current_npages - npages_limit, is_background_thread);
  }
}

static void arena_decay_epoch_advance_helper(
  arena_decay_t* decay, const nstime_t* time, size_t current_npages) {
  assert(arena_decay_deadline_reached(decay, time));

  nstime_t delta;
  nstime_copy(&delta, time);
  nstime_subtract(&delta, &decay->epoch);

  uint64_t nadvance_u64 = nstime_divide(&delta, &decay->interval);
  assert(nadvance_u64 > 0);

  /* Add nadvance_u64 decay intervals to epoch. */
  nstime_copy(&delta, &decay->interval);
  nstime_imultiply(&delta, nadvance_u64);
  nstime_add(&decay->epoch, &delta);

  /* Set a new deadline. */
  arena_decay_deadline_init(decay);

  /* Update the backlog. */
  arena_decay_backlog_update(decay, nadvance_u64, current_npages);
}

static void arena_decay_epoch_advance(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, const nstime_t* time,
  bool is_background_thread) {
  size_t current_npages = extents_npages_get(extents);
  arena_decay_epoch_advance_helper(decay, time, current_npages);

  size_t npages_limit = arena_decay_backlog_npages_limit(decay);
  /* We may unlock decay->mtx when try_purge(). Finish logging first. */
  decay->nunpurged
    = (npages_limit > current_npages) ? npages_limit : current_npages;

  if (!background_thread_enabled() || is_background_thread) {
    arena_decay_try_purge(tsdn, arena, decay, extents, current_npages,
      npages_limit, is_background_thread);
  }
}

static void arena_decay_reinit(arena_decay_t* decay, ssize_t decay_ms) {
  arena_decay_ms_write(decay, decay_ms);
  if (decay_ms > 0) {
    nstime_init(&decay->interval, (uint64_t)decay_ms * KQU(1000000));
    nstime_idivide(&decay->interval, SMOOTHSTEP_NSTEPS);
  }

  nstime_init(&decay->epoch, 0);
  nstime_update(&decay->epoch);
  decay->jitter_state = (uint64_t)(uintptr_t)decay;
  arena_decay_deadline_init(decay);
  decay->nunpurged = 0;
  memset(decay->backlog, 0, SMOOTHSTEP_NSTEPS * sizeof(size_t));
}

static bool arena_decay_init(
  arena_decay_t* decay, ssize_t decay_ms, arena_stats_decay_t* stats) {
  if (config_debug) {
    for (size_t i = 0; i < sizeof(arena_decay_t); i++) {
      assert(((char*)decay)[i] == 0);
    }
    decay->ceil_npages = 0;
  }
  if (malloc_mutex_init(&decay->mtx, "decay", WITNESS_RANK_DECAY,
        malloc_mutex_rank_exclusive)) {
    return true;
  }
  decay->purging = false;
  arena_decay_reinit(decay, decay_ms);
  /* Memory is zeroed, so there is no need to clear stats. */
  if (config_stats) { decay->stats = stats; }
  return false;
}

static bool arena_decay_ms_valid(ssize_t decay_ms) {
  if (decay_ms < -1) { return false; }
  if (decay_ms == -1 || (uint64_t)decay_ms <= NSTIME_SEC_MAX * KQU(1000)) {
    return true;
  }
  return false;
}

static bool arena_maybe_decay(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, bool is_background_thread) {
  malloc_mutex_assert_owner(tsdn, &decay->mtx);

  /* Purge all or nothing if the option is disabled. */
  ssize_t decay_ms = arena_decay_ms_read(decay);
  if (decay_ms <= 0) {
    if (decay_ms == 0) {
      arena_decay_to_limit(tsdn, arena, decay, extents, false, 0,
        extents_npages_get(extents), is_background_thread);
    }
    return false;
  }

  nstime_t time;
  nstime_init(&time, 0);
  nstime_update(&time);
  if (unlikely(
        !nstime_monotonic() && nstime_compare(&decay->epoch, &time) > 0)) {
    /*
     * Time went backwards.  Move the epoch back in time and
     * generate a new deadline, with the expectation that time
     * typically flows forward for long enough periods of time that
     * epochs complete.  Unfortunately, this strategy is susceptible
     * to clock jitter triggering premature epoch advances, but
     * clock jitter estimation and compensation isn't feasible here
     * because calls into this code are event-driven.
     */
    nstime_copy(&decay->epoch, &time);
    arena_decay_deadline_init(decay);
  } else {
    /* Verify that time does not go backwards. */
    assert(nstime_compare(&decay->epoch, &time) <= 0);
  }

  /*
   * If the deadline has been reached, advance to the current epoch and
   * purge to the new limit if necessary.  Note that dirty pages created
   * during the current epoch are not subject to purge until a future
   * epoch, so as a result purging only happens during epoch advances, or
   * being triggered by background threads (scheduled event).
   */
  bool advance_epoch = arena_decay_deadline_reached(decay, &time);
  if (advance_epoch) {
    arena_decay_epoch_advance(
      tsdn, arena, decay, extents, &time, is_background_thread);
  } else if (is_background_thread) {
    arena_decay_try_purge(tsdn, arena, decay, extents,
      extents_npages_get(extents), arena_decay_backlog_npages_limit(decay),
      is_background_thread);
  }

  return advance_epoch;
}

static ssize_t arena_decay_ms_get(arena_decay_t* decay) {
  return arena_decay_ms_read(decay);
}

ssize_t arena_dirty_decay_ms_get(arena_t* arena) {
  return arena_decay_ms_get(&arena->decay_dirty);
}

ssize_t arena_muzzy_decay_ms_get(arena_t* arena) {
  return arena_decay_ms_get(&arena->decay_muzzy);
}

static bool arena_decay_ms_set(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, ssize_t decay_ms) {
  if (!arena_decay_ms_valid(decay_ms)) { return true; }

  malloc_mutex_lock(tsdn, &decay->mtx);
  /*
   * Restart decay backlog from scratch, which may cause many dirty pages
   * to be immediately purged.  It would conceptually be possible to map
   * the old backlog onto the new backlog, but there is no justification
   * for such complexity since decay_ms changes are intended to be
   * infrequent, either between the {-1, 0, >0} states, or a one-time
   * arbitrary change during initial arena configuration.
   */
  arena_decay_reinit(decay, decay_ms);
  arena_maybe_decay(tsdn, arena, decay, extents, false);
  malloc_mutex_unlock(tsdn, &decay->mtx);

  return false;
}

bool arena_dirty_decay_ms_set(
  tsdn_t* tsdn, arena_t* arena, ssize_t decay_ms) {
  return arena_decay_ms_set(
    tsdn, arena, &arena->decay_dirty, &arena->extents_dirty, decay_ms);
}

bool arena_muzzy_decay_ms_set(
  tsdn_t* tsdn, arena_t* arena, ssize_t decay_ms) {
  return arena_decay_ms_set(
    tsdn, arena, &arena->decay_muzzy, &arena->extents_muzzy, decay_ms);
}

static size_t arena_stash_decayed(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, size_t npages_limit,
  size_t npages_decay_max, extent_list_t* decay_extents) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  /* Stash extents according to npages_limit. */
  size_t nstashed = 0;
  extent_t* extent;
  while (nstashed < npages_decay_max
    && (extent = extents_evict(
          tsdn, arena, r_extent_hooks, extents, npages_limit))
      != NULL) {
    extent_list_append(decay_extents, extent);
    nstashed += extent_size_get(extent) >> LG_PAGE;
  }
  return nstashed;
}

static size_t arena_decay_stashed(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, arena_decay_t* decay, extents_t* extents,
  bool all, extent_list_t* decay_extents, bool is_background_thread) {
  size_t nmadvise, nunmapped;
  size_t npurged;

  if (config_stats) {
    nmadvise = 0;
    nunmapped = 0;
  }
  npurged = 0;

  ssize_t muzzy_decay_ms = arena_muzzy_decay_ms_get(arena);
  for (extent_t* extent = extent_list_first(decay_extents); extent != NULL;
       extent = extent_list_first(decay_extents)) {
    if (config_stats) { nmadvise++; }
    size_t npages = extent_size_get(extent) >> LG_PAGE;
    npurged += npages;
    extent_list_remove(decay_extents, extent);
    switch (extents_state_get(extents)) {
    case extent_state_active: not_reached();
    case extent_state_dirty:
      if (!all && muzzy_decay_ms != 0
        && !extent_purge_lazy_wrapper(tsdn, arena, r_extent_hooks, extent,
          0, extent_size_get(extent))) {
        extents_dalloc(
          tsdn, arena, r_extent_hooks, &arena->extents_muzzy, extent);
        arena_background_thread_inactivity_check(
          tsdn, arena, is_background_thread);
        break;
      }
      /* Fall through. */
    case extent_state_muzzy:
      extent_dalloc_wrapper(tsdn, arena, r_extent_hooks, extent);
      if (config_stats) { nunmapped += npages; }
      break;
    case extent_state_retained:
    default: not_reached();
    }
  }

  if (config_stats) {
    arena_stats_lock(tsdn, &arena->stats);
    arena_stats_add_u64(tsdn, &arena->stats, &decay->stats->npurge, 1);
    arena_stats_add_u64(
      tsdn, &arena->stats, &decay->stats->nmadvise, nmadvise);
    arena_stats_add_u64(
      tsdn, &arena->stats, &decay->stats->purged, npurged);
    arena_stats_sub_zu(
      tsdn, &arena->stats, &arena->stats.mapped, nunmapped << LG_PAGE);
    arena_stats_unlock(tsdn, &arena->stats);
  }

  return npurged;
}

/*
 * npages_limit: Decay at most npages_decay_max pages without violating the
 * invariant: (extents_npages_get(extents) >= npages_limit).  We need an
 * upper bound on number of pages in order to prevent unbounded growth
 * (namely in stashed), otherwise unbounded new pages could be added to
 * extents during the current decay run, so that the purging thread never
 * finishes.
 */
static void arena_decay_to_limit(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, bool all, size_t npages_limit,
  size_t npages_decay_max, bool is_background_thread) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 1);
  malloc_mutex_assert_owner(tsdn, &decay->mtx);

  if (decay->purging) { return; }
  decay->purging = true;
  malloc_mutex_unlock(tsdn, &decay->mtx);

  extent_hooks_t* extent_hooks = extent_hooks_get(arena);

  extent_list_t decay_extents;
  extent_list_init(&decay_extents);

  size_t npurge = arena_stash_decayed(tsdn, arena, &extent_hooks, extents,
    npages_limit, npages_decay_max, &decay_extents);
  if (npurge != 0) {
    size_t npurged = arena_decay_stashed(tsdn, arena, &extent_hooks, decay,
      extents, all, &decay_extents, is_background_thread);
    assert(npurged == npurge);
  }

  malloc_mutex_lock(tsdn, &decay->mtx);
  decay->purging = false;
}

static bool arena_decay_impl(tsdn_t* tsdn, arena_t* arena,
  arena_decay_t* decay, extents_t* extents, bool is_background_thread,
  bool all) {
  if (all) {
    malloc_mutex_lock(tsdn, &decay->mtx);
    arena_decay_to_limit(tsdn, arena, decay, extents, all, 0,
      extents_npages_get(extents), is_background_thread);
    malloc_mutex_unlock(tsdn, &decay->mtx);

    return false;
  }

  if (malloc_mutex_trylock(tsdn, &decay->mtx)) {
    /* No need to wait if another thread is in progress. */
    return true;
  }

  bool epoch_advanced
    = arena_maybe_decay(tsdn, arena, decay, extents, is_background_thread);
  size_t npages_new;
  if (epoch_advanced) {
    /* Backlog is updated on epoch advance. */
    npages_new = decay->backlog[SMOOTHSTEP_NSTEPS - 1];
  }
  malloc_mutex_unlock(tsdn, &decay->mtx);

  if (have_background_thread && background_thread_enabled()
    && epoch_advanced && !is_background_thread) {
    background_thread_interval_check(tsdn, arena, decay, npages_new);
  }

  return false;
}

static bool arena_decay_dirty(
  tsdn_t* tsdn, arena_t* arena, bool is_background_thread, bool all) {
  return arena_decay_impl(tsdn, arena, &arena->decay_dirty,
    &arena->extents_dirty, is_background_thread, all);
}

static bool arena_decay_muzzy(
  tsdn_t* tsdn, arena_t* arena, bool is_background_thread, bool all) {
  return arena_decay_impl(tsdn, arena, &arena->decay_muzzy,
    &arena->extents_muzzy, is_background_thread, all);
}

void arena_decay(
  tsdn_t* tsdn, arena_t* arena, bool is_background_thread, bool all) {
  if (arena_decay_dirty(tsdn, arena, is_background_thread, all)) { return; }
  arena_decay_muzzy(tsdn, arena, is_background_thread, all);
}

static void arena_slab_dalloc(
  tsdn_t* tsdn, arena_t* arena, extent_t* slab) {
  arena_nactive_sub(arena, extent_size_get(slab) >> LG_PAGE);

  extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;
  arena_extents_dirty_dalloc(tsdn, arena, &extent_hooks, slab);
}

static void arena_bin_slabs_nonfull_insert(bin_t* bin, extent_t* slab) {
  assert(extent_nfree_get(slab) > 0);
  extent_heap_insert(&bin->slabs_nonfull, slab);
  if (config_stats) { bin->stats.nonfull_slabs++; }
}

static void arena_bin_slabs_nonfull_remove(bin_t* bin, extent_t* slab) {
  extent_heap_remove(&bin->slabs_nonfull, slab);
  if (config_stats) { bin->stats.nonfull_slabs--; }
}

static extent_t* arena_bin_slabs_nonfull_tryget(bin_t* bin) {
  extent_t* slab = extent_heap_remove_first(&bin->slabs_nonfull);
  if (slab == NULL) { return NULL; }
  if (config_stats) {
    bin->stats.reslabs++;
    bin->stats.nonfull_slabs--;
  }
  return slab;
}

static void arena_bin_slabs_full_insert(
  arena_t* arena, bin_t* bin, extent_t* slab) {
  assert(extent_nfree_get(slab) == 0);
  /*
   *  Tracking extents is required by arena_reset, which is not allowed
   *  for auto arenas.  Bypass this step to avoid touching the extent
   *  linkage (often results in cache misses) for auto arenas.
   */
  if (arena_is_auto(arena)) { return; }
  extent_list_append(&bin->slabs_full, slab);
}

static void arena_bin_slabs_full_remove(
  arena_t* arena, bin_t* bin, extent_t* slab) {
  if (arena_is_auto(arena)) { return; }
  extent_list_remove(&bin->slabs_full, slab);
}

static void arena_bin_reset(tsd_t* tsd, arena_t* arena, bin_t* bin) {
  extent_t* slab;

  malloc_mutex_lock(tsd_tsdn(tsd), &bin->lock);
  if (bin->slabcur != NULL) {
    slab = bin->slabcur;
    bin->slabcur = NULL;
    malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
    arena_slab_dalloc(tsd_tsdn(tsd), arena, slab);
    malloc_mutex_lock(tsd_tsdn(tsd), &bin->lock);
  }
  while ((slab = extent_heap_remove_first(&bin->slabs_nonfull)) != NULL) {
    malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
    arena_slab_dalloc(tsd_tsdn(tsd), arena, slab);
    malloc_mutex_lock(tsd_tsdn(tsd), &bin->lock);
  }
  for (slab = extent_list_first(&bin->slabs_full); slab != NULL;
       slab = extent_list_first(&bin->slabs_full)) {
    arena_bin_slabs_full_remove(arena, bin, slab);
    malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
    arena_slab_dalloc(tsd_tsdn(tsd), arena, slab);
    malloc_mutex_lock(tsd_tsdn(tsd), &bin->lock);
  }
  if (config_stats) {
    bin->stats.curregs = 0;
    bin->stats.curslabs = 0;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
}

void arena_reset(tsd_t* tsd, arena_t* arena) {
  /*
   * Locking in this function is unintuitive.  The caller guarantees that
   * no concurrent operations are happening in this arena, but there are
   * still reasons that some locking is necessary:
   *
   * - Some of the functions in the transitive closure of calls assume
   *   appropriate locks are held, and in some cases these locks are
   *   temporarily dropped to avoid lock order reversal or deadlock due to
   *   reentry.
   * - mallctl("epoch", ...) may concurrently refresh stats.  While
   *   strictly speaking this is a "concurrent operation", disallowing
   *   stats refreshes would impose an inconvenient burden.
   */

  /* Large allocations. */
  malloc_mutex_lock(tsd_tsdn(tsd), &arena->large_mtx);

  for (extent_t* extent = extent_list_first(&arena->large); extent != NULL;
       extent = extent_list_first(&arena->large)) {
    void* ptr = extent_base_get(extent);
    size_t usize;

    malloc_mutex_unlock(tsd_tsdn(tsd), &arena->large_mtx);
    alloc_ctx_t alloc_ctx;
    rtree_ctx_t* rtree_ctx = tsd_rtree_ctx(tsd);
    rtree_szind_slab_read(tsd_tsdn(tsd), &extents_rtree, rtree_ctx,
      (uintptr_t)ptr, true, &alloc_ctx.szind, &alloc_ctx.slab);
    assert(alloc_ctx.szind != SC_NSIZES);

    if (config_stats || (config_prof && opt_prof)) {
      usize = sz_index2size(alloc_ctx.szind);
      assert(usize == isalloc(tsd_tsdn(tsd), ptr));
    }
    /* Remove large allocation from prof sample set. */
    if (config_prof && opt_prof) { prof_free(tsd, ptr, usize, &alloc_ctx); }
    large_dalloc(tsd_tsdn(tsd), extent);
    malloc_mutex_lock(tsd_tsdn(tsd), &arena->large_mtx);
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), &arena->large_mtx);

  /* Bins. */
  for (unsigned i = 0; i < SC_NBINS; i++) {
    for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
      arena_bin_reset(tsd, arena, &arena->bins[i].bin_shards[j]);
    }
  }

  atomic_store_zu(&arena->nactive, 0, ATOMIC_RELAXED);
}

static void arena_destroy_retained(tsdn_t* tsdn, arena_t* arena) {
  /*
   * Iterate over the retained extents and destroy them.  This gives the
   * extent allocator underlying the extent hooks an opportunity to unmap
   * all retained memory without having to keep its own metadata
   * structures.  In practice, virtual memory for dss-allocated extents is
   * leaked here, so best practice is to avoid dss for arenas to be
   * destroyed, or provide custom extent hooks that track retained
   * dss-based extents for later reuse.
   */
  extent_hooks_t* extent_hooks = extent_hooks_get(arena);
  extent_t* extent;
  while ((extent = extents_evict(
            tsdn, arena, &extent_hooks, &arena->extents_retained, 0))
    != NULL) {
    extent_destroy_wrapper(tsdn, arena, &extent_hooks, extent);
  }
}

void arena_destroy(tsd_t* tsd, arena_t* arena) {
  assert(base_ind_get(arena->base) >= narenas_auto);
  assert(arena_nthreads_get(arena, false) == 0);
  assert(arena_nthreads_get(arena, true) == 0);

  /*
   * No allocations have occurred since arena_reset() was called.
   * Furthermore, the caller (arena_i_destroy_ctl()) purged all cached
   * extents, so only retained extents may remain.
   */
  assert(extents_npages_get(&arena->extents_dirty) == 0);
  assert(extents_npages_get(&arena->extents_muzzy) == 0);

  /* Deallocate retained memory. */
  arena_destroy_retained(tsd_tsdn(tsd), arena);

  /*
   * Remove the arena pointer from the arenas array.  We rely on the fact
   * that there is no way for the application to get a dirty read from the
   * arenas array unless there is an inherent race in the application
   * involving access of an arena being concurrently destroyed.  The
   * application must synchronize knowledge of the arena's validity, so as
   * long as we use an atomic write to update the arenas array, the
   * application will get a clean read any time after it synchronizes
   * knowledge that the arena is no longer valid.
   */
  arena_set(base_ind_get(arena->base), NULL);

  /*
   * Destroy the base allocator, which manages all metadata ever mapped by
   * this arena.
   */
  base_delete(tsd_tsdn(tsd), arena->base);
}

static extent_t* arena_slab_alloc_hard(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, const bin_info_t* bin_info,
  szind_t szind) {
  extent_t* slab;
  bool zero, commit;

  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  zero = false;
  commit = true;
  slab = extent_alloc_wrapper(tsdn, arena, r_extent_hooks, NULL,
    bin_info->slab_size, 0, PAGE, true, szind, &zero, &commit);

  if (config_stats && slab != NULL) {
    arena_stats_mapped_add(tsdn, &arena->stats, bin_info->slab_size);
  }

  return slab;
}

static extent_t* arena_slab_alloc(tsdn_t* tsdn, arena_t* arena,
  szind_t binind, unsigned binshard, const bin_info_t* bin_info) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;
  szind_t szind = sz_size2index(bin_info->reg_size);
  bool zero = false;
  bool commit = true;
  extent_t* slab
    = extents_alloc(tsdn, arena, &extent_hooks, &arena->extents_dirty, NULL,
      bin_info->slab_size, 0, PAGE, true, binind, &zero, &commit);
  if (slab == NULL && arena_may_have_muzzy(arena)) {
    slab = extents_alloc(tsdn, arena, &extent_hooks, &arena->extents_muzzy,
      NULL, bin_info->slab_size, 0, PAGE, true, binind, &zero, &commit);
  }
  if (slab == NULL) {
    slab
      = arena_slab_alloc_hard(tsdn, arena, &extent_hooks, bin_info, szind);
    if (slab == NULL) { return NULL; }
  }
  assert(extent_slab_get(slab));

  /* Initialize slab internals. */
  arena_slab_data_t* slab_data = extent_slab_data_get(slab);
  extent_nfree_binshard_set(slab, bin_info->nregs, binshard);
  bitmap_init(slab_data->bitmap, &bin_info->bitmap_info, false);

  arena_nactive_add(arena, extent_size_get(slab) >> LG_PAGE);

  return slab;
}

static extent_t* arena_bin_nonfull_slab_get(tsdn_t* tsdn, arena_t* arena,
  bin_t* bin, szind_t binind, unsigned binshard) {
  extent_t* slab;
  const bin_info_t* bin_info;

  /* Look for a usable slab. */
  slab = arena_bin_slabs_nonfull_tryget(bin);
  if (slab != NULL) { return slab; }
  /* No existing slabs have any space available. */

  bin_info = &bin_infos[binind];

  /* Allocate a new slab. */
  malloc_mutex_unlock(tsdn, &bin->lock);
  /******************************/
  slab = arena_slab_alloc(tsdn, arena, binind, binshard, bin_info);
  /********************************/
  malloc_mutex_lock(tsdn, &bin->lock);
  if (slab != NULL) {
    if (config_stats) {
      bin->stats.nslabs++;
      bin->stats.curslabs++;
    }
    return slab;
  }

  /*
   * arena_slab_alloc() failed, but another thread may have made
   * sufficient memory available while this one dropped bin->lock above,
   * so search one more time.
   */
  slab = arena_bin_slabs_nonfull_tryget(bin);
  if (slab != NULL) { return slab; }

  return NULL;
}

/* Re-fill bin->slabcur, then call arena_slab_reg_alloc(). */
static void* arena_bin_malloc_hard(tsdn_t* tsdn, arena_t* arena, bin_t* bin,
  szind_t binind, unsigned binshard) {
  const bin_info_t* bin_info;
  extent_t* slab;

  bin_info = &bin_infos[binind];
  if (!arena_is_auto(arena) && bin->slabcur != NULL) {
    arena_bin_slabs_full_insert(arena, bin, bin->slabcur);
    bin->slabcur = NULL;
  }
  slab = arena_bin_nonfull_slab_get(tsdn, arena, bin, binind, binshard);
  if (bin->slabcur != NULL) {
    /*
     * Another thread updated slabcur while this one ran without the
     * bin lock in arena_bin_nonfull_slab_get().
     */
    if (extent_nfree_get(bin->slabcur) > 0) {
      void* ret = arena_slab_reg_alloc(bin->slabcur, bin_info);
      if (slab != NULL) {
        /*
         * arena_slab_alloc() may have allocated slab,
         * or it may have been pulled from
         * slabs_nonfull.  Therefore it is unsafe to
         * make any assumptions about how slab has
         * previously been used, and
         * arena_bin_lower_slab() must be called, as if
         * a region were just deallocated from the slab.
         */
        if (extent_nfree_get(slab) == bin_info->nregs) {
          arena_dalloc_bin_slab(tsdn, arena, slab, bin);
        } else {
          arena_bin_lower_slab(tsdn, arena, slab, bin);
        }
      }
      return ret;
    }

    arena_bin_slabs_full_insert(arena, bin, bin->slabcur);
    bin->slabcur = NULL;
  }

  if (slab == NULL) { return NULL; }
  bin->slabcur = slab;

  assert(extent_nfree_get(bin->slabcur) > 0);

  return arena_slab_reg_alloc(slab, bin_info);
}

/* Choose a bin shard and return the locked bin. */
bin_t* arena_bin_choose_lock(
  tsdn_t* tsdn, arena_t* arena, szind_t binind, unsigned* binshard) {
  bin_t* bin;
  if (tsdn_null(tsdn) || tsd_arena_get(tsdn_tsd(tsdn)) == NULL) {
    *binshard = 0;
  } else {
    *binshard = tsd_binshardsp_get(tsdn_tsd(tsdn))->binshard[binind];
  }
  assert(*binshard < bin_infos[binind].n_shards);
  bin = &arena->bins[binind].bin_shards[*binshard];
  malloc_mutex_lock(tsdn, &bin->lock);

  return bin;
}

void arena_tcache_fill_small(tsdn_t* tsdn, arena_t* arena, tcache_t* tcache,
  cache_bin_t* tbin, szind_t binind, uint64_t prof_accumbytes) {
  unsigned i, nfill, cnt;

  assert(tbin->ncached == 0);

  if (config_prof && arena_prof_accum(tsdn, arena, prof_accumbytes)) {
    prof_idump(tsdn);
  }

  unsigned binshard;
  bin_t* bin = arena_bin_choose_lock(tsdn, arena, binind, &binshard);

  for (i = 0,
      nfill = (tcache_bin_info[binind].ncached_max
        >> tcache->lg_fill_div[binind]);
       i < nfill; i += cnt) {
    extent_t* slab;
    if ((slab = bin->slabcur) != NULL && extent_nfree_get(slab) > 0) {
      unsigned tofill = nfill - i;
      cnt
        = tofill < extent_nfree_get(slab) ? tofill : extent_nfree_get(slab);
      arena_slab_reg_alloc_batch(
        slab, &bin_infos[binind], cnt, tbin->avail - nfill + i);
    } else {
      cnt = 1;
      void* ptr = arena_bin_malloc_hard(tsdn, arena, bin, binind, binshard);
      /*
       * OOM.  tbin->avail isn't yet filled down to its first
       * element, so the successful allocations (if any) must
       * be moved just before tbin->avail before bailing out.
       */
      if (ptr == NULL) {
        if (i > 0) {
          memmove(tbin->avail - i, tbin->avail - nfill, i * sizeof(void*));
        }
        break;
      }
      /* Insert such that low regions get used first. */
      *(tbin->avail - nfill + i) = ptr;
    }
    if (config_fill && unlikely(opt_junk_alloc)) {
      for (unsigned j = 0; j < cnt; j++) {
        void* ptr = *(tbin->avail - nfill + i + j);
        arena_alloc_junk_small(ptr, &bin_infos[binind], true);
      }
    }
  }
  if (config_stats) {
    bin->stats.nmalloc += i;
    bin->stats.nrequests += tbin->tstats.nrequests;
    bin->stats.curregs += i;
    bin->stats.nfills++;
    tbin->tstats.nrequests = 0;
  }
  malloc_mutex_unlock(tsdn, &bin->lock);
  tbin->ncached = i;
  arena_decay_tick(tsdn, arena);
}

void arena_alloc_junk_small(
  void* ptr, const bin_info_t* bin_info, bool zero) {
  if (!zero) { memset(ptr, JEMALLOC_ALLOC_JUNK, bin_info->reg_size); }
}

static void arena_dalloc_junk_small_impl(
  void* ptr, const bin_info_t* bin_info) {
  memset(ptr, JEMALLOC_FREE_JUNK, bin_info->reg_size);
}
arena_dalloc_junk_small_t* JET_MUTABLE arena_dalloc_junk_small
  = arena_dalloc_junk_small_impl;

static void* arena_malloc_small(
  tsdn_t* tsdn, arena_t* arena, szind_t binind, bool zero) {
  void* ret;
  bin_t* bin;
  size_t usize;
  extent_t* slab;

  assert(binind < SC_NBINS);
  usize = sz_index2size(binind);
  unsigned binshard;
  bin = arena_bin_choose_lock(tsdn, arena, binind, &binshard);

  if ((slab = bin->slabcur) != NULL && extent_nfree_get(slab) > 0) {
    ret = arena_slab_reg_alloc(slab, &bin_infos[binind]);
  } else {
    ret = arena_bin_malloc_hard(tsdn, arena, bin, binind, binshard);
  }

  if (ret == NULL) {
    malloc_mutex_unlock(tsdn, &bin->lock);
    return NULL;
  }

  if (config_stats) {
    bin->stats.nmalloc++;
    bin->stats.nrequests++;
    bin->stats.curregs++;
  }
  malloc_mutex_unlock(tsdn, &bin->lock);
  if (config_prof && arena_prof_accum(tsdn, arena, usize)) {
    prof_idump(tsdn);
  }

  if (!zero) {
    if (config_fill) {
      if (unlikely(opt_junk_alloc)) {
        arena_alloc_junk_small(ret, &bin_infos[binind], false);
      } else if (unlikely(opt_zero)) {
        memset(ret, 0, usize);
      }
    }
  } else {
    if (config_fill && unlikely(opt_junk_alloc)) {
      arena_alloc_junk_small(ret, &bin_infos[binind], true);
    }
    memset(ret, 0, usize);
  }

  arena_decay_tick(tsdn, arena);
  return ret;
}

void* arena_malloc_hard(
  tsdn_t* tsdn, arena_t* arena, size_t size, szind_t ind, bool zero) {
  assert(!tsdn_null(tsdn) || arena != NULL);

  if (likely(!tsdn_null(tsdn))) {
    arena = arena_choose_maybe_huge(tsdn_tsd(tsdn), arena, size);
  }
  if (unlikely(arena == NULL)) { return NULL; }

  if (likely(size <= SC_SMALL_MAXCLASS)) {
    return arena_malloc_small(tsdn, arena, ind, zero);
  }
  return large_malloc(tsdn, arena, sz_index2size(ind), zero);
}

void* arena_palloc(tsdn_t* tsdn, arena_t* arena, size_t usize,
  size_t alignment, bool zero, tcache_t* tcache) {
  void* ret;

  if (usize <= SC_SMALL_MAXCLASS
    && (alignment < PAGE
      || (alignment == PAGE && (usize & PAGE_MASK) == 0))) {
    /* Small; alignment doesn't require special slab placement. */
    ret = arena_malloc(
      tsdn, arena, usize, sz_size2index(usize), zero, tcache, true);
  } else {
    if (likely(alignment <= CACHELINE)) {
      ret = large_malloc(tsdn, arena, usize, zero);
    } else {
      ret = large_palloc(tsdn, arena, usize, alignment, zero);
    }
  }
  return ret;
}

void arena_prof_promote(tsdn_t* tsdn, void* ptr, size_t usize) {
  cassert(config_prof);
  assert(ptr != NULL);
  assert(isalloc(tsdn, ptr) == SC_LARGE_MINCLASS);
  assert(usize <= SC_SMALL_MAXCLASS);

  if (config_opt_safety_checks) {
    safety_check_set_redzone(ptr, usize, SC_LARGE_MINCLASS);
  }

  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  extent_t* extent = rtree_extent_read(
    tsdn, &extents_rtree, rtree_ctx, (uintptr_t)ptr, true);
  arena_t* arena = extent_arena_get(extent);

  szind_t szind = sz_size2index(usize);
  extent_szind_set(extent, szind);
  rtree_szind_slab_update(
    tsdn, &extents_rtree, rtree_ctx, (uintptr_t)ptr, szind, false);

  prof_accum_cancel(tsdn, &arena->prof_accum, usize);

  assert(isalloc(tsdn, ptr) == usize);
}

static size_t arena_prof_demote(
  tsdn_t* tsdn, extent_t* extent, const void* ptr) {
  cassert(config_prof);
  assert(ptr != NULL);

  extent_szind_set(extent, SC_NBINS);
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  rtree_szind_slab_update(
    tsdn, &extents_rtree, rtree_ctx, (uintptr_t)ptr, SC_NBINS, false);

  assert(isalloc(tsdn, ptr) == SC_LARGE_MINCLASS);

  return SC_LARGE_MINCLASS;
}

void arena_dalloc_promoted(
  tsdn_t* tsdn, void* ptr, tcache_t* tcache, bool slow_path) {
  cassert(config_prof);
  assert(opt_prof);

  extent_t* extent = iealloc(tsdn, ptr);
  size_t usize = extent_usize_get(extent);
  size_t bumped_usize = arena_prof_demote(tsdn, extent, ptr);
  if (config_opt_safety_checks && usize < SC_LARGE_MINCLASS) {
    /*
     * Currently, we only do redzoning for small sampled
     * allocations.
     */
    assert(bumped_usize == SC_LARGE_MINCLASS);
    safety_check_verify_redzone(ptr, usize, bumped_usize);
  }
  if (bumped_usize <= tcache_maxclass && tcache != NULL) {
    tcache_dalloc_large(
      tsdn_tsd(tsdn), tcache, ptr, sz_size2index(bumped_usize), slow_path);
  } else {
    large_dalloc(tsdn, extent);
  }
}

static void arena_dissociate_bin_slab(
  arena_t* arena, extent_t* slab, bin_t* bin) {
  /* Dissociate slab from bin. */
  if (slab == bin->slabcur) {
    bin->slabcur = NULL;
  } else {
    szind_t binind = extent_szind_get(slab);
    const bin_info_t* bin_info = &bin_infos[binind];

    /*
     * The following block's conditional is necessary because if the
     * slab only contains one region, then it never gets inserted
     * into the non-full slabs heap.
     */
    if (bin_info->nregs == 1) {
      arena_bin_slabs_full_remove(arena, bin, slab);
    } else {
      arena_bin_slabs_nonfull_remove(bin, slab);
    }
  }
}

static void arena_dalloc_bin_slab(
  tsdn_t* tsdn, arena_t* arena, extent_t* slab, bin_t* bin) {
  assert(slab != bin->slabcur);

  malloc_mutex_unlock(tsdn, &bin->lock);
  /******************************/
  arena_slab_dalloc(tsdn, arena, slab);
  /****************************/
  malloc_mutex_lock(tsdn, &bin->lock);
  if (config_stats) { bin->stats.curslabs--; }
}

static void arena_bin_lower_slab(
  tsdn_t* tsdn, arena_t* arena, extent_t* slab, bin_t* bin) {
  assert(extent_nfree_get(slab) > 0);

  /*
   * Make sure that if bin->slabcur is non-NULL, it refers to the
   * oldest/lowest non-full slab.  It is okay to NULL slabcur out rather
   * than proactively keeping it pointing at the oldest/lowest non-full
   * slab.
   */
  if (bin->slabcur != NULL && extent_snad_comp(bin->slabcur, slab) > 0) {
    /* Switch slabcur. */
    if (extent_nfree_get(bin->slabcur) > 0) {
      arena_bin_slabs_nonfull_insert(bin, bin->slabcur);
    } else {
      arena_bin_slabs_full_insert(arena, bin, bin->slabcur);
    }
    bin->slabcur = slab;
    if (config_stats) { bin->stats.reslabs++; }
  } else {
    arena_bin_slabs_nonfull_insert(bin, slab);
  }
}

static void arena_dalloc_bin_locked_impl(tsdn_t* tsdn, arena_t* arena,
  bin_t* bin, szind_t binind, extent_t* slab, void* ptr, bool junked) {
  arena_slab_data_t* slab_data = extent_slab_data_get(slab);
  const bin_info_t* bin_info = &bin_infos[binind];

  if (!junked && config_fill && unlikely(opt_junk_free)) {
    arena_dalloc_junk_small(ptr, bin_info);
  }

  arena_slab_reg_dalloc(slab, slab_data, ptr);
  unsigned nfree = extent_nfree_get(slab);
  if (nfree == bin_info->nregs) {
    arena_dissociate_bin_slab(arena, slab, bin);
    arena_dalloc_bin_slab(tsdn, arena, slab, bin);
  } else if (nfree == 1 && slab != bin->slabcur) {
    arena_bin_slabs_full_remove(arena, bin, slab);
    arena_bin_lower_slab(tsdn, arena, slab, bin);
  }

  if (config_stats) {
    bin->stats.ndalloc++;
    bin->stats.curregs--;
  }
}

void arena_dalloc_bin_junked_locked(tsdn_t* tsdn, arena_t* arena,
  bin_t* bin, szind_t binind, extent_t* extent, void* ptr) {
  arena_dalloc_bin_locked_impl(tsdn, arena, bin, binind, extent, ptr, true);
}

static void arena_dalloc_bin(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent, void* ptr) {
  szind_t binind = extent_szind_get(extent);
  unsigned binshard = extent_binshard_get(extent);
  bin_t* bin = &arena->bins[binind].bin_shards[binshard];

  malloc_mutex_lock(tsdn, &bin->lock);
  arena_dalloc_bin_locked_impl(
    tsdn, arena, bin, binind, extent, ptr, false);
  malloc_mutex_unlock(tsdn, &bin->lock);
}

void arena_dalloc_small(tsdn_t* tsdn, void* ptr) {
  extent_t* extent = iealloc(tsdn, ptr);
  arena_t* arena = extent_arena_get(extent);

  arena_dalloc_bin(tsdn, arena, extent, ptr);
  arena_decay_tick(tsdn, arena);
}

bool arena_ralloc_no_move(tsdn_t* tsdn, void* ptr, size_t oldsize,
  size_t size, size_t extra, bool zero, size_t* newsize) {
  bool ret;
  /* Calls with non-zero extra had to clamp extra. */
  assert(extra == 0 || size + extra <= SC_LARGE_MAXCLASS);

  extent_t* extent = iealloc(tsdn, ptr);
  if (unlikely(size > SC_LARGE_MAXCLASS)) {
    ret = true;
    goto done;
  }

  size_t usize_min = sz_s2u(size);
  size_t usize_max = sz_s2u(size + extra);
  if (likely(
        oldsize <= SC_SMALL_MAXCLASS && usize_min <= SC_SMALL_MAXCLASS)) {
    /*
     * Avoid moving the allocation if the size class can be left the
     * same.
     */
    assert(bin_infos[sz_size2index(oldsize)].reg_size == oldsize);
    if ((usize_max > SC_SMALL_MAXCLASS
          || sz_size2index(usize_max) != sz_size2index(oldsize))
      && (size > oldsize || usize_max < oldsize)) {
      ret = true;
      goto done;
    }

    arena_decay_tick(tsdn, extent_arena_get(extent));
    ret = false;
  } else if (oldsize >= SC_LARGE_MINCLASS
    && usize_max >= SC_LARGE_MINCLASS) {
    ret = large_ralloc_no_move(tsdn, extent, usize_min, usize_max, zero);
  } else {
    ret = true;
  }
done:
  assert(extent == iealloc(tsdn, ptr));
  *newsize = extent_usize_get(extent);

  return ret;
}

static void* arena_ralloc_move_helper(tsdn_t* tsdn, arena_t* arena,
  size_t usize, size_t alignment, bool zero, tcache_t* tcache) {
  if (alignment == 0) {
    return arena_malloc(
      tsdn, arena, usize, sz_size2index(usize), zero, tcache, true);
  }
  usize = sz_sa2u(usize, alignment);
  if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) { return NULL; }
  return ipalloct(tsdn, usize, alignment, zero, tcache, arena);
}

void* arena_ralloc(tsdn_t* tsdn, arena_t* arena, void* ptr, size_t oldsize,
  size_t size, size_t alignment, bool zero, tcache_t* tcache,
  hook_ralloc_args_t* hook_args) {
  size_t usize = sz_s2u(size);
  if (unlikely(usize == 0 || size > SC_LARGE_MAXCLASS)) { return NULL; }

  if (likely(usize <= SC_SMALL_MAXCLASS)) {
    /* Try to avoid moving the allocation. */
    UNUSED size_t newsize;
    if (!arena_ralloc_no_move(
          tsdn, ptr, oldsize, usize, 0, zero, &newsize)) {
      hook_invoke_expand(
        hook_args->is_realloc ? hook_expand_realloc : hook_expand_rallocx,
        ptr, oldsize, usize, (uintptr_t)ptr, hook_args->args);
      return ptr;
    }
  }

  if (oldsize >= SC_LARGE_MINCLASS && usize >= SC_LARGE_MINCLASS) {
    return large_ralloc(
      tsdn, arena, ptr, usize, alignment, zero, tcache, hook_args);
  }

  /*
   * size and oldsize are different enough that we need to move the
   * object.  In that case, fall back to allocating new space and copying.
   */
  void* ret
    = arena_ralloc_move_helper(tsdn, arena, usize, alignment, zero, tcache);
  if (ret == NULL) { return NULL; }

  hook_invoke_alloc(
    hook_args->is_realloc ? hook_alloc_realloc : hook_alloc_rallocx, ret,
    (uintptr_t)ret, hook_args->args);
  hook_invoke_dalloc(
    hook_args->is_realloc ? hook_dalloc_realloc : hook_dalloc_rallocx, ptr,
    hook_args->args);

  /*
   * Junk/zero-filling were already done by
   * ipalloc()/arena_malloc().
   */
  size_t copysize = (usize < oldsize) ? usize : oldsize;
  jet_memcpy(ret, ptr, copysize);
  isdalloct(tsdn, ptr, oldsize, tcache, NULL, true);
  return ret;
}

dss_prec_t arena_dss_prec_get(arena_t* arena) {
  return (dss_prec_t)atomic_load_u(&arena->dss_prec, ATOMIC_ACQUIRE);
}

bool arena_dss_prec_set(arena_t* arena, dss_prec_t dss_prec) {
  if (!have_dss) { return (dss_prec != dss_prec_disabled); }
  atomic_store_u(&arena->dss_prec, (unsigned)dss_prec, ATOMIC_RELEASE);
  return false;
}

ssize_t arena_dirty_decay_ms_default_get(void) {
  return atomic_load_zd(&dirty_decay_ms_default, ATOMIC_RELAXED);
}

bool arena_dirty_decay_ms_default_set(ssize_t decay_ms) {
  if (!arena_decay_ms_valid(decay_ms)) { return true; }
  atomic_store_zd(&dirty_decay_ms_default, decay_ms, ATOMIC_RELAXED);
  return false;
}

ssize_t arena_muzzy_decay_ms_default_get(void) {
  return atomic_load_zd(&muzzy_decay_ms_default, ATOMIC_RELAXED);
}

bool arena_muzzy_decay_ms_default_set(ssize_t decay_ms) {
  if (!arena_decay_ms_valid(decay_ms)) { return true; }
  atomic_store_zd(&muzzy_decay_ms_default, decay_ms, ATOMIC_RELAXED);
  return false;
}

bool arena_retain_grow_limit_get_set(
  tsd_t* tsd, arena_t* arena, size_t* old_limit, size_t* new_limit) {
  assert(opt_retain);

  pszind_t new_ind JEMALLOC_CC_SILENCE_INIT(0);
  if (new_limit != NULL) {
    size_t limit = *new_limit;
    /* Grow no more than the new limit. */
    if ((new_ind = sz_psz2ind(limit + 1) - 1) >= SC_NPSIZES) {
      return true;
    }
  }

  malloc_mutex_lock(tsd_tsdn(tsd), &arena->extent_grow_mtx);
  if (old_limit != NULL) {
    *old_limit = sz_pind2sz(arena->retain_grow_limit);
  }
  if (new_limit != NULL) { arena->retain_grow_limit = new_ind; }
  malloc_mutex_unlock(tsd_tsdn(tsd), &arena->extent_grow_mtx);

  return false;
}

unsigned arena_nthreads_get(arena_t* arena, bool internal) {
  return atomic_load_u(&arena->nthreads[internal], ATOMIC_RELAXED);
}

void arena_nthreads_inc(arena_t* arena, bool internal) {
  atomic_fetch_add_u(&arena->nthreads[internal], 1, ATOMIC_RELAXED);
}

void arena_nthreads_dec(arena_t* arena, bool internal) {
  atomic_fetch_sub_u(&arena->nthreads[internal], 1, ATOMIC_RELAXED);
}

size_t arena_extent_sn_next(arena_t* arena) {
  return atomic_fetch_add_zu(&arena->extent_sn_next, 1, ATOMIC_RELAXED);
}

arena_t* arena_new(
  tsdn_t* tsdn, unsigned ind, extent_hooks_t* extent_hooks) {
  arena_t* arena;
  base_t* base;
  unsigned i;

  if (ind == 0) {
    base = b0get();
  } else {
    base = base_new(tsdn, ind, extent_hooks);
    if (base == NULL) { return NULL; }
  }

  unsigned nbins_total = 0;
  for (i = 0; i < SC_NBINS; i++) { nbins_total += bin_infos[i].n_shards; }
  size_t arena_size = sizeof(arena_t) + sizeof(bin_t) * nbins_total;
  arena = (arena_t*)base_alloc(tsdn, base, arena_size, CACHELINE);
  if (arena == NULL) { goto label_error; }

  atomic_store_u(&arena->nthreads[0], 0, ATOMIC_RELAXED);
  atomic_store_u(&arena->nthreads[1], 0, ATOMIC_RELAXED);
  arena->last_thd = NULL;

  if (config_stats) {
    if (arena_stats_init(tsdn, &arena->stats)) { goto label_error; }

    ql_new(&arena->tcache_ql);
    ql_new(&arena->cache_bin_array_descriptor_ql);
    if (malloc_mutex_init(&arena->tcache_ql_mtx, "tcache_ql",
          WITNESS_RANK_TCACHE_QL, malloc_mutex_rank_exclusive)) {
      goto label_error;
    }
  }

  if (config_prof) {
    if (prof_accum_init(tsdn, &arena->prof_accum)) { goto label_error; }
  }

  if (config_cache_oblivious) {
    /*
     * A nondeterministic seed based on the address of arena reduces
     * the likelihood of lockstep non-uniform cache index
     * utilization among identical concurrent processes, but at the
     * cost of test repeatability.  For debug builds, instead use a
     * deterministic seed.
     */
    atomic_store_zu(&arena->offset_state,
      config_debug ? ind : (size_t)(uintptr_t)arena, ATOMIC_RELAXED);
  }

  atomic_store_zu(&arena->extent_sn_next, 0, ATOMIC_RELAXED);

  atomic_store_u(
    &arena->dss_prec, (unsigned)extent_dss_prec_get(), ATOMIC_RELAXED);

  atomic_store_zu(&arena->nactive, 0, ATOMIC_RELAXED);

  extent_list_init(&arena->large);
  if (malloc_mutex_init(&arena->large_mtx, "arena_large",
        WITNESS_RANK_ARENA_LARGE, malloc_mutex_rank_exclusive)) {
    goto label_error;
  }

  /*
   * Delay coalescing for dirty extents despite the disruptive effect on
   * memory layout for best-fit extent allocation, since cached extents
   * are likely to be reused soon after deallocation, and the cost of
   * merging/splitting extents is non-trivial.
   */
  if (extents_init(tsdn, &arena->extents_dirty, extent_state_dirty, true)) {
    goto label_error;
  }
  /*
   * Coalesce muzzy extents immediately, because operations on them are in
   * the critical path much less often than for dirty extents.
   */
  if (extents_init(
        tsdn, &arena->extents_muzzy, extent_state_muzzy, false)) {
    goto label_error;
  }
  /*
   * Coalesce retained extents immediately, in part because they will
   * never be evicted (and therefore there's no opportunity for delayed
   * coalescing), but also because operations on retained extents are not
   * in the critical path.
   */
  if (extents_init(
        tsdn, &arena->extents_retained, extent_state_retained, false)) {
    goto label_error;
  }

  if (arena_decay_init(&arena->decay_dirty,
        arena_dirty_decay_ms_default_get(), &arena->stats.decay_dirty)) {
    goto label_error;
  }
  if (arena_decay_init(&arena->decay_muzzy,
        arena_muzzy_decay_ms_default_get(), &arena->stats.decay_muzzy)) {
    goto label_error;
  }

  arena->extent_grow_next = sz_psz2ind(HUGEPAGE);
  arena->retain_grow_limit = sz_psz2ind(SC_LARGE_MAXCLASS);
  if (malloc_mutex_init(&arena->extent_grow_mtx, "extent_grow",
        WITNESS_RANK_EXTENT_GROW, malloc_mutex_rank_exclusive)) {
    goto label_error;
  }

  extent_avail_new(&arena->extent_avail);
  if (malloc_mutex_init(&arena->extent_avail_mtx, "extent_avail",
        WITNESS_RANK_EXTENT_AVAIL, malloc_mutex_rank_exclusive)) {
    goto label_error;
  }

  /* Initialize bins. */
  uintptr_t bin_addr = (uintptr_t)arena + sizeof(arena_t);
  atomic_store_u(&arena->binshard_next, 0, ATOMIC_RELEASE);
  for (i = 0; i < SC_NBINS; i++) {
    unsigned nshards = bin_infos[i].n_shards;
    arena->bins[i].bin_shards = (bin_t*)bin_addr;
    bin_addr += nshards * sizeof(bin_t);
    for (unsigned j = 0; j < nshards; j++) {
      bool err = bin_init(&arena->bins[i].bin_shards[j]);
      if (err) { goto label_error; }
    }
  }
  assert(bin_addr == (uintptr_t)arena + arena_size);

  arena->base = base;
  /* Set arena before creating background threads. */
  arena_set(ind, arena);

  nstime_init(&arena->create_time, 0);
  nstime_update(&arena->create_time);

  /* We don't support reentrancy for arena 0 bootstrapping. */
  if (ind != 0) {
    /*
     * If we're here, then arena 0 already exists, so bootstrapping
     * is done enough that we should have tsd.
     */
    assert(!tsdn_null(tsdn));
    pre_reentrancy(tsdn_tsd(tsdn), arena);
    if (test_hooks_arena_new_hook) { test_hooks_arena_new_hook(); }
    post_reentrancy(tsdn_tsd(tsdn));
  }

  return arena;
label_error:
  if (ind != 0) { base_delete(tsdn, base); }
  return NULL;
}

arena_t* arena_choose_huge(tsd_t* tsd) {
  /* huge_arena_ind can be 0 during init (will use a0). */
  if (huge_arena_ind == 0) { assert(!malloc_initialized()); }

  arena_t* huge_arena = arena_get(tsd_tsdn(tsd), huge_arena_ind, false);
  if (huge_arena == NULL) {
    /* Create the huge arena on demand. */
    assert(huge_arena_ind != 0);
    huge_arena = arena_get(tsd_tsdn(tsd), huge_arena_ind, true);
    if (huge_arena == NULL) { return NULL; }
    /*
     * Purge eagerly for huge allocations, because: 1) number of
     * huge allocations is usually small, which means ticker based
     * decay is not reliable; and 2) less immediate reuse is
     * expected for huge allocations.
     */
    if (arena_dirty_decay_ms_default_get() > 0) {
      arena_dirty_decay_ms_set(tsd_tsdn(tsd), huge_arena, 0);
    }
    if (arena_muzzy_decay_ms_default_get() > 0) {
      arena_muzzy_decay_ms_set(tsd_tsdn(tsd), huge_arena, 0);
    }
  }

  return huge_arena;
}

bool arena_init_huge(void) {
  bool huge_enabled;

  /* The threshold should be large size class. */
  if (opt_oversize_threshold > SC_LARGE_MAXCLASS
    || opt_oversize_threshold < SC_LARGE_MINCLASS) {
    opt_oversize_threshold = 0;
    oversize_threshold = SC_LARGE_MAXCLASS + PAGE;
    huge_enabled = false;
  } else {
    /* Reserve the index for the huge arena. */
    huge_arena_ind = narenas_total_get();
    oversize_threshold = opt_oversize_threshold;
    huge_enabled = true;
  }

  return huge_enabled;
}

bool arena_is_huge(unsigned arena_ind) {
  if (huge_arena_ind == 0) { return false; }
  return (arena_ind == huge_arena_ind);
}

void arena_boot(sc_data_t* sc_data) {
  arena_dirty_decay_ms_default_set(opt_dirty_decay_ms);
  arena_muzzy_decay_ms_default_set(opt_muzzy_decay_ms);
  for (unsigned i = 0; i < SC_NBINS; i++) {
    sc_t* sc = &sc_data->sc[i];
    div_init(&arena_binind_div_info[i],
      (1U << sc->lg_base) + (sc->ndelta << sc->lg_delta));
  }
}

void arena_prefork0(tsdn_t* tsdn, arena_t* arena) {
  malloc_mutex_prefork(tsdn, &arena->decay_dirty.mtx);
  malloc_mutex_prefork(tsdn, &arena->decay_muzzy.mtx);
}

void arena_prefork1(tsdn_t* tsdn, arena_t* arena) {
  if (config_stats) { malloc_mutex_prefork(tsdn, &arena->tcache_ql_mtx); }
}

void arena_prefork2(tsdn_t* tsdn, arena_t* arena) {
  malloc_mutex_prefork(tsdn, &arena->extent_grow_mtx);
}

void arena_prefork3(tsdn_t* tsdn, arena_t* arena) {
  extents_prefork(tsdn, &arena->extents_dirty);
  extents_prefork(tsdn, &arena->extents_muzzy);
  extents_prefork(tsdn, &arena->extents_retained);
}

void arena_prefork4(tsdn_t* tsdn, arena_t* arena) {
  malloc_mutex_prefork(tsdn, &arena->extent_avail_mtx);
}

void arena_prefork5(tsdn_t* tsdn, arena_t* arena) {
  base_prefork(tsdn, arena->base);
}

void arena_prefork6(tsdn_t* tsdn, arena_t* arena) {
  malloc_mutex_prefork(tsdn, &arena->large_mtx);
}

void arena_prefork7(tsdn_t* tsdn, arena_t* arena) {
  for (unsigned i = 0; i < SC_NBINS; i++) {
    for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
      bin_prefork(tsdn, &arena->bins[i].bin_shards[j]);
    }
  }
}

void arena_postfork_parent(tsdn_t* tsdn, arena_t* arena) {
  unsigned i;

  for (i = 0; i < SC_NBINS; i++) {
    for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
      bin_postfork_parent(tsdn, &arena->bins[i].bin_shards[j]);
    }
  }
  malloc_mutex_postfork_parent(tsdn, &arena->large_mtx);
  base_postfork_parent(tsdn, arena->base);
  malloc_mutex_postfork_parent(tsdn, &arena->extent_avail_mtx);
  extents_postfork_parent(tsdn, &arena->extents_dirty);
  extents_postfork_parent(tsdn, &arena->extents_muzzy);
  extents_postfork_parent(tsdn, &arena->extents_retained);
  malloc_mutex_postfork_parent(tsdn, &arena->extent_grow_mtx);
  malloc_mutex_postfork_parent(tsdn, &arena->decay_dirty.mtx);
  malloc_mutex_postfork_parent(tsdn, &arena->decay_muzzy.mtx);
  if (config_stats) {
    malloc_mutex_postfork_parent(tsdn, &arena->tcache_ql_mtx);
  }
}

void arena_postfork_child(tsdn_t* tsdn, arena_t* arena) {
  unsigned i;

  atomic_store_u(&arena->nthreads[0], 0, ATOMIC_RELAXED);
  atomic_store_u(&arena->nthreads[1], 0, ATOMIC_RELAXED);
  if (tsd_arena_get(tsdn_tsd(tsdn)) == arena) {
    arena_nthreads_inc(arena, false);
  }
  if (tsd_iarena_get(tsdn_tsd(tsdn)) == arena) {
    arena_nthreads_inc(arena, true);
  }
  if (config_stats) {
    ql_new(&arena->tcache_ql);
    ql_new(&arena->cache_bin_array_descriptor_ql);
    tcache_t* tcache = tcache_get(tsdn_tsd(tsdn));
    if (tcache != NULL && tcache->arena == arena) {
      ql_elm_new(tcache, link);
      ql_tail_insert(&arena->tcache_ql, tcache, link);
      cache_bin_array_descriptor_init(&tcache->cache_bin_array_descriptor,
        tcache->bins_small, tcache->bins_large);
      ql_tail_insert(&arena->cache_bin_array_descriptor_ql,
        &tcache->cache_bin_array_descriptor, link);
    }
  }

  for (i = 0; i < SC_NBINS; i++) {
    for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
      bin_postfork_child(tsdn, &arena->bins[i].bin_shards[j]);
    }
  }
  malloc_mutex_postfork_child(tsdn, &arena->large_mtx);
  base_postfork_child(tsdn, arena->base);
  malloc_mutex_postfork_child(tsdn, &arena->extent_avail_mtx);
  extents_postfork_child(tsdn, &arena->extents_dirty);
  extents_postfork_child(tsdn, &arena->extents_muzzy);
  extents_postfork_child(tsdn, &arena->extents_retained);
  malloc_mutex_postfork_child(tsdn, &arena->extent_grow_mtx);
  malloc_mutex_postfork_child(tsdn, &arena->decay_dirty.mtx);
  malloc_mutex_postfork_child(tsdn, &arena->decay_muzzy.mtx);
  if (config_stats) {
    malloc_mutex_postfork_child(tsdn, &arena->tcache_ql_mtx);
  }
}
#define JEMALLOC_BACKGROUND_THREAD_C_

JEMALLOC_DIAGNOSTIC_DISABLE_SPURIOUS

/******************************************************************************/
/* Data. */

/* This option should be opt-in only. */
#define BACKGROUND_THREAD_DEFAULT false
/* Read-only after initialization. */
bool opt_background_thread = BACKGROUND_THREAD_DEFAULT;
size_t opt_max_background_threads = MAX_BACKGROUND_THREAD_LIMIT + 1;

/* Used for thread creation, termination and stats. */
malloc_mutex_t background_thread_lock;
/* Indicates global state.  Atomic because decay reads this w/o locking. */
atomic_b_t background_thread_enabled_state;
size_t n_background_threads;
size_t max_background_threads;
/* Thread info per-index. */
background_thread_info_t* background_thread_info;

/******************************************************************************/

#ifdef JEMALLOC_PTHREAD_CREATE_WRAPPER

static int (*pthread_create_fptr)(pthread_t* __restrict,
  const pthread_attr_t*, void* (*)(void*), void* __restrict);

static void pthread_create_wrapper_init(void) {
#ifdef JEMALLOC_LAZY_LOCK
  if (!isthreaded) { isthreaded = true; }
#endif
}

int pthread_create_wrapper(pthread_t* __restrict thread,
  const pthread_attr_t* attr, void* (*start_routine)(void*),
  void* __restrict arg) {
  pthread_create_wrapper_init();

  return pthread_create_fptr(thread, attr, start_routine, arg);
}
#endif /* JEMALLOC_PTHREAD_CREATE_WRAPPER */

#ifndef JEMALLOC_BACKGROUND_THREAD
#define NOT_REACHED                                                        \
  { not_reached(); }
bool background_thread_create(tsd_t* tsd, unsigned arena_ind) NOT_REACHED
  bool background_threads_enable(tsd_t* tsd) NOT_REACHED
  bool background_threads_disable(tsd_t* tsd) NOT_REACHED
  void background_thread_interval_check(tsdn_t* tsdn, arena_t* arena,
    arena_decay_t* decay, size_t npages_new) NOT_REACHED
  void background_thread_prefork0(tsdn_t* tsdn) NOT_REACHED
  void background_thread_prefork1(tsdn_t* tsdn) NOT_REACHED
  void background_thread_postfork_parent(tsdn_t* tsdn) NOT_REACHED
  void background_thread_postfork_child(tsdn_t* tsdn) NOT_REACHED
  bool background_thread_stats_read(
    tsdn_t* tsdn, background_thread_stats_t* stats) NOT_REACHED
  void background_thread_ctl_init(tsdn_t* tsdn) NOT_REACHED
#undef NOT_REACHED
#else

static bool background_thread_enabled_at_fork;

static void background_thread_info_init(
  tsdn_t* tsdn, background_thread_info_t* info) {
  background_thread_wakeup_time_set(tsdn, info, 0);
  info->npages_to_purge_new = 0;
  if (config_stats) {
    info->tot_n_runs = 0;
    nstime_init(&info->tot_sleep_time, 0);
  }
}

static inline bool set_current_thread_affinity(int cpu) {
#if defined(JEMALLOC_HAVE_SCHED_SETAFFINITY)
  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  CPU_SET(cpu, &cpuset);
  int ret = sched_setaffinity(0, sizeof(cpu_set_t), &cpuset);

  return (ret != 0);
#else
  return false;
#endif
}

/* Threshold for determining when to wake up the background thread. */
#define BACKGROUND_THREAD_NPAGES_THRESHOLD UINT64_C(1024)
#define BILLION UINT64_C(1000000000)
/* Minimal sleep interval 100 ms. */
#define BACKGROUND_THREAD_MIN_INTERVAL_NS (BILLION / 10)

static inline size_t decay_npurge_after_interval(
  arena_decay_t* decay, size_t interval) {
  size_t i;
  uint64_t sum = 0;
  for (i = 0; i < interval; i++) { sum += decay->backlog[i] * h_steps[i]; }
  for (; i < SMOOTHSTEP_NSTEPS; i++) {
    sum += decay->backlog[i] * (h_steps[i] - h_steps[i - interval]);
  }

  return (size_t)(sum >> SMOOTHSTEP_BFP);
}

static uint64_t arena_decay_compute_purge_interval_impl(
  tsdn_t* tsdn, arena_decay_t* decay, extents_t* extents) {
  if (malloc_mutex_trylock(tsdn, &decay->mtx)) {
    /* Use minimal interval if decay is contended. */
    return BACKGROUND_THREAD_MIN_INTERVAL_NS;
  }

  uint64_t interval;
  ssize_t decay_time = atomic_load_zd(&decay->time_ms, ATOMIC_RELAXED);
  if (decay_time <= 0) {
    /* Purging is eagerly done or disabled currently. */
    interval = BACKGROUND_THREAD_INDEFINITE_SLEEP;
    goto label_done;
  }

  uint64_t decay_interval_ns = nstime_ns(&decay->interval);
  assert(decay_interval_ns > 0);
  size_t npages = extents_npages_get(extents);
  if (npages == 0) {
    unsigned i;
    for (i = 0; i < SMOOTHSTEP_NSTEPS; i++) {
      if (decay->backlog[i] > 0) { break; }
    }
    if (i == SMOOTHSTEP_NSTEPS) {
      /* No dirty pages recorded.  Sleep indefinitely. */
      interval = BACKGROUND_THREAD_INDEFINITE_SLEEP;
      goto label_done;
    }
  }
  if (npages <= BACKGROUND_THREAD_NPAGES_THRESHOLD) {
    /* Use max interval. */
    interval = decay_interval_ns * SMOOTHSTEP_NSTEPS;
    goto label_done;
  }

  size_t lb = BACKGROUND_THREAD_MIN_INTERVAL_NS / decay_interval_ns;
  size_t ub = SMOOTHSTEP_NSTEPS;
  /* Minimal 2 intervals to ensure reaching next epoch deadline. */
  lb = (lb < 2) ? 2 : lb;
  if ((decay_interval_ns * ub <= BACKGROUND_THREAD_MIN_INTERVAL_NS)
    || (lb + 2 > ub)) {
    interval = BACKGROUND_THREAD_MIN_INTERVAL_NS;
    goto label_done;
  }

  assert(lb + 2 <= ub);
  size_t npurge_lb, npurge_ub;
  npurge_lb = decay_npurge_after_interval(decay, lb);
  if (npurge_lb > BACKGROUND_THREAD_NPAGES_THRESHOLD) {
    interval = decay_interval_ns * lb;
    goto label_done;
  }
  npurge_ub = decay_npurge_after_interval(decay, ub);
  if (npurge_ub < BACKGROUND_THREAD_NPAGES_THRESHOLD) {
    interval = decay_interval_ns * ub;
    goto label_done;
  }

  unsigned n_search = 0;
  size_t target, npurge;
  while ((npurge_lb + BACKGROUND_THREAD_NPAGES_THRESHOLD < npurge_ub)
    && (lb + 2 < ub)) {
    target = (lb + ub) / 2;
    npurge = decay_npurge_after_interval(decay, target);
    if (npurge > BACKGROUND_THREAD_NPAGES_THRESHOLD) {
      ub = target;
      npurge_ub = npurge;
    } else {
      lb = target;
      npurge_lb = npurge;
    }
    assert(n_search++ < lg_floor(SMOOTHSTEP_NSTEPS) + 1);
  }
  interval = decay_interval_ns * (ub + lb) / 2;
label_done:
  interval = (interval < BACKGROUND_THREAD_MIN_INTERVAL_NS)
    ? BACKGROUND_THREAD_MIN_INTERVAL_NS
    : interval;
  malloc_mutex_unlock(tsdn, &decay->mtx);

  return interval;
}

/* Compute purge interval for background threads. */
static uint64_t arena_decay_compute_purge_interval(
  tsdn_t* tsdn, arena_t* arena) {
  uint64_t i1, i2;
  i1 = arena_decay_compute_purge_interval_impl(
    tsdn, &arena->decay_dirty, &arena->extents_dirty);
  if (i1 == BACKGROUND_THREAD_MIN_INTERVAL_NS) { return i1; }
  i2 = arena_decay_compute_purge_interval_impl(
    tsdn, &arena->decay_muzzy, &arena->extents_muzzy);

  return i1 < i2 ? i1 : i2;
}

static void background_thread_sleep(
  tsdn_t* tsdn, background_thread_info_t* info, uint64_t interval) {
  if (config_stats) { info->tot_n_runs++; }
  info->npages_to_purge_new = 0;

  struct timeval tv;
  /* Specific clock required by timedwait. */
  gettimeofday(&tv, NULL);
  nstime_t before_sleep;
  nstime_init2(&before_sleep, tv.tv_sec, tv.tv_usec * 1000);

  int ret;
  if (interval == BACKGROUND_THREAD_INDEFINITE_SLEEP) {
    assert(background_thread_indefinite_sleep(info));
    ret = pthread_cond_wait(&info->cond, &info->mtx.lock);
    assert(ret == 0);
  } else {
    assert(interval >= BACKGROUND_THREAD_MIN_INTERVAL_NS
      && interval <= BACKGROUND_THREAD_INDEFINITE_SLEEP);
    /* We need malloc clock (can be different from tv). */
    nstime_t next_wakeup;
    nstime_init(&next_wakeup, 0);
    nstime_update(&next_wakeup);
    nstime_iadd(&next_wakeup, interval);
    assert(nstime_ns(&next_wakeup) < BACKGROUND_THREAD_INDEFINITE_SLEEP);
    background_thread_wakeup_time_set(tsdn, info, nstime_ns(&next_wakeup));

    nstime_t ts_wakeup;
    nstime_copy(&ts_wakeup, &before_sleep);
    nstime_iadd(&ts_wakeup, interval);
    struct timespec ts;
    ts.tv_sec = (size_t)nstime_sec(&ts_wakeup);
    ts.tv_nsec = (size_t)nstime_nsec(&ts_wakeup);

    assert(!background_thread_indefinite_sleep(info));
    ret = pthread_cond_timedwait(&info->cond, &info->mtx.lock, &ts);
    assert(ret == ETIMEDOUT || ret == 0);
    background_thread_wakeup_time_set(
      tsdn, info, BACKGROUND_THREAD_INDEFINITE_SLEEP);
  }
  if (config_stats) {
    gettimeofday(&tv, NULL);
    nstime_t after_sleep;
    nstime_init2(&after_sleep, tv.tv_sec, tv.tv_usec * 1000);
    if (nstime_compare(&after_sleep, &before_sleep) > 0) {
      nstime_subtract(&after_sleep, &before_sleep);
      nstime_add(&info->tot_sleep_time, &after_sleep);
    }
  }
}

static bool background_thread_pause_check(
  tsdn_t* tsdn, background_thread_info_t* info) {
  if (unlikely(info->state == background_thread_paused)) {
    malloc_mutex_unlock(tsdn, &info->mtx);
    /* Wait on global lock to update status. */
    malloc_mutex_lock(tsdn, &background_thread_lock);
    malloc_mutex_unlock(tsdn, &background_thread_lock);
    malloc_mutex_lock(tsdn, &info->mtx);
    return true;
  }

  return false;
}

static inline void background_work_sleep_once(
  tsdn_t* tsdn, background_thread_info_t* info, unsigned ind) {
  uint64_t min_interval = BACKGROUND_THREAD_INDEFINITE_SLEEP;
  unsigned narenas = narenas_total_get();

  for (unsigned i = ind; i < narenas; i += max_background_threads) {
    arena_t* arena = arena_get(tsdn, i, false);
    if (!arena) { continue; }
    arena_decay(tsdn, arena, true, false);
    if (min_interval == BACKGROUND_THREAD_MIN_INTERVAL_NS) {
      /* Min interval will be used. */
      continue;
    }
    uint64_t interval = arena_decay_compute_purge_interval(tsdn, arena);
    assert(interval >= BACKGROUND_THREAD_MIN_INTERVAL_NS);
    if (min_interval > interval) { min_interval = interval; }
  }
  background_thread_sleep(tsdn, info, min_interval);
}

static bool background_threads_disable_single(
  tsd_t* tsd, background_thread_info_t* info) {
  if (info == &background_thread_info[0]) {
    malloc_mutex_assert_owner(tsd_tsdn(tsd), &background_thread_lock);
  } else {
    malloc_mutex_assert_not_owner(tsd_tsdn(tsd), &background_thread_lock);
  }

  pre_reentrancy(tsd, NULL);
  malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
  bool has_thread;
  assert(info->state != background_thread_paused);
  if (info->state == background_thread_started) {
    has_thread = true;
    info->state = background_thread_stopped;
    pthread_cond_signal(&info->cond);
  } else {
    has_thread = false;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);

  if (!has_thread) {
    post_reentrancy(tsd);
    return false;
  }
  void* ret;
  if (pthread_join(info->thread, &ret)) {
    post_reentrancy(tsd);
    return true;
  }
  assert(ret == NULL);
  n_background_threads--;
  post_reentrancy(tsd);

  return false;
}

static void* background_thread_entry(void* ind_arg);

static int background_thread_create_signals_masked(pthread_t* thread,
  const pthread_attr_t* attr, void* (*start_routine)(void*), void* arg) {
  /*
   * Mask signals during thread creation so that the thread inherits
   * an empty signal set.
   */
  sigset_t set;
  sigfillset(&set);
  sigset_t oldset;
  int mask_err = pthread_sigmask(SIG_SETMASK, &set, &oldset);
  if (mask_err != 0) { return mask_err; }
  int create_err = pthread_create_wrapper(thread, attr, start_routine, arg);
  /*
   * Restore the signal mask.  Failure to restore the signal mask here
   * changes program behavior.
   */
  int restore_err = pthread_sigmask(SIG_SETMASK, &oldset, NULL);
  if (restore_err != 0) {
    malloc_printf("<jemalloc>: background thread creation "
                  "failed (%d), and signal mask restoration failed "
                  "(%d)\n",
      create_err, restore_err);
    if (opt_abort) { abort(); }
  }
  return create_err;
}

static bool check_background_thread_creation(
  tsd_t* tsd, unsigned* n_created, bool* created_threads) {
  bool ret = false;
  if (likely(*n_created == n_background_threads)) { return ret; }

  tsdn_t* tsdn = tsd_tsdn(tsd);
  malloc_mutex_unlock(tsdn, &background_thread_info[0].mtx);
  for (unsigned i = 1; i < max_background_threads; i++) {
    if (created_threads[i]) { continue; }
    background_thread_info_t* info = &background_thread_info[i];
    malloc_mutex_lock(tsdn, &info->mtx);
    /*
     * In case of the background_thread_paused state because of
     * arena reset, delay the creation.
     */
    bool create = (info->state == background_thread_started);
    malloc_mutex_unlock(tsdn, &info->mtx);
    if (!create) { continue; }

    pre_reentrancy(tsd, NULL);
    int err = background_thread_create_signals_masked(
      &info->thread, NULL, background_thread_entry, (void*)(uintptr_t)i);
    post_reentrancy(tsd);

    if (err == 0) {
      (*n_created)++;
      created_threads[i] = true;
    } else {
      malloc_printf("<jemalloc>: background thread "
                    "creation failed (%d)\n",
        err);
      if (opt_abort) { abort(); }
    }
    /* Return to restart the loop since we unlocked. */
    ret = true;
    break;
  }
  malloc_mutex_lock(tsdn, &background_thread_info[0].mtx);

  return ret;
}

static void background_thread0_work(tsd_t* tsd) {
  /* Thread0 is also responsible for launching / terminating threads. */
  VARIABLE_ARRAY(bool, created_threads, max_background_threads);
  unsigned i;
  for (i = 1; i < max_background_threads; i++) {
    created_threads[i] = false;
  }
  /* Start working, and create more threads when asked. */
  unsigned n_created = 1;
  while (background_thread_info[0].state != background_thread_stopped) {
    if (background_thread_pause_check(
          tsd_tsdn(tsd), &background_thread_info[0])) {
      continue;
    }
    if (check_background_thread_creation(
          tsd, &n_created, (bool*)&created_threads)) {
      continue;
    }
    background_work_sleep_once(
      tsd_tsdn(tsd), &background_thread_info[0], 0);
  }

  /*
   * Shut down other threads at exit.  Note that the ctl thread is holding
   * the global background_thread mutex (and is waiting) for us.
   */
  assert(!background_thread_enabled());
  for (i = 1; i < max_background_threads; i++) {
    background_thread_info_t* info = &background_thread_info[i];
    assert(info->state != background_thread_paused);
    if (created_threads[i]) {
      background_threads_disable_single(tsd, info);
    } else {
      malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
      if (info->state != background_thread_stopped) {
        /* The thread was not created. */
        assert(info->state == background_thread_started);
        n_background_threads--;
        info->state = background_thread_stopped;
      }
      malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
    }
  }
  background_thread_info[0].state = background_thread_stopped;
  assert(n_background_threads == 1);
}

static void background_work(tsd_t* tsd, unsigned ind) {
  background_thread_info_t* info = &background_thread_info[ind];

  malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
  background_thread_wakeup_time_set(
    tsd_tsdn(tsd), info, BACKGROUND_THREAD_INDEFINITE_SLEEP);
  if (ind == 0) {
    background_thread0_work(tsd);
  } else {
    while (info->state != background_thread_stopped) {
      if (background_thread_pause_check(tsd_tsdn(tsd), info)) { continue; }
      background_work_sleep_once(tsd_tsdn(tsd), info, ind);
    }
  }
  assert(info->state == background_thread_stopped);
  background_thread_wakeup_time_set(tsd_tsdn(tsd), info, 0);
  malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
}

static void* background_thread_entry(void* ind_arg) {
  unsigned thread_ind = (unsigned)(uintptr_t)ind_arg;
  assert(thread_ind < max_background_threads);
#ifdef JEMALLOC_HAVE_PTHREAD_SETNAME_NP
  pthread_setname_np(pthread_self(), "jemalloc_bg_thd");
#elif defined(__FreeBSD__)
  pthread_set_name_np(pthread_self(), "jemalloc_bg_thd");
#endif
  if (opt_percpu_arena != percpu_arena_disabled) {
    set_current_thread_affinity((int)thread_ind);
  }
  /*
   * Start periodic background work.  We use internal tsd which avoids
   * side effects, for example triggering new arena creation (which in
   * turn triggers another background thread creation).
   */
  background_work(tsd_internal_fetch(), thread_ind);
  assert(pthread_equal(
    pthread_self(), background_thread_info[thread_ind].thread));

  return NULL;
}

static void background_thread_init(
  tsd_t* tsd, background_thread_info_t* info) {
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &background_thread_lock);
  info->state = background_thread_started;
  background_thread_info_init(tsd_tsdn(tsd), info);
  n_background_threads++;
}

static bool background_thread_create_locked(
  tsd_t* tsd, unsigned arena_ind) {
  assert(have_background_thread);
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &background_thread_lock);

  /* We create at most NCPUs threads. */
  size_t thread_ind = arena_ind % max_background_threads;
  background_thread_info_t* info = &background_thread_info[thread_ind];

  bool need_new_thread;
  malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
  need_new_thread = background_thread_enabled()
    && (info->state == background_thread_stopped);
  if (need_new_thread) { background_thread_init(tsd, info); }
  malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
  if (!need_new_thread) { return false; }
  if (arena_ind != 0) {
    /* Threads are created asynchronously by Thread 0. */
    background_thread_info_t* t0 = &background_thread_info[0];
    malloc_mutex_lock(tsd_tsdn(tsd), &t0->mtx);
    assert(t0->state == background_thread_started);
    pthread_cond_signal(&t0->cond);
    malloc_mutex_unlock(tsd_tsdn(tsd), &t0->mtx);

    return false;
  }

  pre_reentrancy(tsd, NULL);
  /*
   * To avoid complications (besides reentrancy), create internal
   * background threads with the underlying pthread_create.
   */
  int err = background_thread_create_signals_masked(
    &info->thread, NULL, background_thread_entry, (void*)thread_ind);
  post_reentrancy(tsd);

  if (err != 0) {
    malloc_printf("<jemalloc>: arena 0 background thread creation "
                  "failed (%d)\n",
      err);
    malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
    info->state = background_thread_stopped;
    n_background_threads--;
    malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);

    return true;
  }

  return false;
}

/* Create a new background thread if needed. */
bool background_thread_create(tsd_t* tsd, unsigned arena_ind) {
  assert(have_background_thread);

  bool ret;
  malloc_mutex_lock(tsd_tsdn(tsd), &background_thread_lock);
  ret = background_thread_create_locked(tsd, arena_ind);
  malloc_mutex_unlock(tsd_tsdn(tsd), &background_thread_lock);

  return ret;
}

bool background_threads_enable(tsd_t* tsd) {
  assert(n_background_threads == 0);
  assert(background_thread_enabled());
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &background_thread_lock);

  VARIABLE_ARRAY(bool, marked, max_background_threads);
  unsigned i, nmarked;
  for (i = 0; i < max_background_threads; i++) { marked[i] = false; }
  nmarked = 0;
  /* Thread 0 is required and created at the end. */
  marked[0] = true;
  /* Mark the threads we need to create for thread 0. */
  unsigned n = narenas_total_get();
  for (i = 1; i < n; i++) {
    if (marked[i % max_background_threads]
      || arena_get(tsd_tsdn(tsd), i, false) == NULL) {
      continue;
    }
    background_thread_info_t* info
      = &background_thread_info[i % max_background_threads];
    malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
    assert(info->state == background_thread_stopped);
    background_thread_init(tsd, info);
    malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
    marked[i % max_background_threads] = true;
    if (++nmarked == max_background_threads) { break; }
  }

  return background_thread_create_locked(tsd, 0);
}

bool background_threads_disable(tsd_t* tsd) {
  assert(!background_thread_enabled());
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &background_thread_lock);

  /* Thread 0 will be responsible for terminating other threads. */
  if (background_threads_disable_single(tsd, &background_thread_info[0])) {
    return true;
  }
  assert(n_background_threads == 0);

  return false;
}

/* Check if we need to signal the background thread early. */
void background_thread_interval_check(
  tsdn_t* tsdn, arena_t* arena, arena_decay_t* decay, size_t npages_new) {
  background_thread_info_t* info = arena_background_thread_info_get(arena);
  if (malloc_mutex_trylock(tsdn, &info->mtx)) {
    /*
     * Background thread may hold the mutex for a long period of
     * time.  We'd like to avoid the variance on application
     * threads.  So keep this non-blocking, and leave the work to a
     * future epoch.
     */
    return;
  }

  if (info->state != background_thread_started) { goto label_done; }
  if (malloc_mutex_trylock(tsdn, &decay->mtx)) { goto label_done; }

  ssize_t decay_time = atomic_load_zd(&decay->time_ms, ATOMIC_RELAXED);
  if (decay_time <= 0) {
    /* Purging is eagerly done or disabled currently. */
    goto label_done_unlock2;
  }
  uint64_t decay_interval_ns = nstime_ns(&decay->interval);
  assert(decay_interval_ns > 0);

  nstime_t diff;
  nstime_init(&diff, background_thread_wakeup_time_get(info));
  if (nstime_compare(&diff, &decay->epoch) <= 0) {
    goto label_done_unlock2;
  }
  nstime_subtract(&diff, &decay->epoch);
  if (nstime_ns(&diff) < BACKGROUND_THREAD_MIN_INTERVAL_NS) {
    goto label_done_unlock2;
  }

  if (npages_new > 0) {
    size_t n_epoch = (size_t)(nstime_ns(&diff) / decay_interval_ns);
    /*
     * Compute how many new pages we would need to purge by the next
     * wakeup, which is used to determine if we should signal the
     * background thread.
     */
    uint64_t npurge_new;
    if (n_epoch >= SMOOTHSTEP_NSTEPS) {
      npurge_new = npages_new;
    } else {
      uint64_t h_steps_max = h_steps[SMOOTHSTEP_NSTEPS - 1];
      assert(h_steps_max >= h_steps[SMOOTHSTEP_NSTEPS - 1 - n_epoch]);
      npurge_new = npages_new
        * (h_steps_max - h_steps[SMOOTHSTEP_NSTEPS - 1 - n_epoch]);
      npurge_new >>= SMOOTHSTEP_BFP;
    }
    info->npages_to_purge_new += npurge_new;
  }

  bool should_signal;
  if (info->npages_to_purge_new > BACKGROUND_THREAD_NPAGES_THRESHOLD) {
    should_signal = true;
  } else if (unlikely(background_thread_indefinite_sleep(info))
    && (extents_npages_get(&arena->extents_dirty) > 0
      || extents_npages_get(&arena->extents_muzzy) > 0
      || info->npages_to_purge_new > 0)) {
    should_signal = true;
  } else {
    should_signal = false;
  }

  if (should_signal) {
    info->npages_to_purge_new = 0;
    pthread_cond_signal(&info->cond);
  }
label_done_unlock2:
  malloc_mutex_unlock(tsdn, &decay->mtx);
label_done:
  malloc_mutex_unlock(tsdn, &info->mtx);
}

void background_thread_prefork0(tsdn_t* tsdn) {
  malloc_mutex_prefork(tsdn, &background_thread_lock);
  background_thread_enabled_at_fork = background_thread_enabled();
}

void background_thread_prefork1(tsdn_t* tsdn) {
  for (unsigned i = 0; i < max_background_threads; i++) {
    malloc_mutex_prefork(tsdn, &background_thread_info[i].mtx);
  }
}

void background_thread_postfork_parent(tsdn_t* tsdn) {
  for (unsigned i = 0; i < max_background_threads; i++) {
    malloc_mutex_postfork_parent(tsdn, &background_thread_info[i].mtx);
  }
  malloc_mutex_postfork_parent(tsdn, &background_thread_lock);
}

void background_thread_postfork_child(tsdn_t* tsdn) {
  for (unsigned i = 0; i < max_background_threads; i++) {
    malloc_mutex_postfork_child(tsdn, &background_thread_info[i].mtx);
  }
  malloc_mutex_postfork_child(tsdn, &background_thread_lock);
  if (!background_thread_enabled_at_fork) { return; }

  /* Clear background_thread state (reset to disabled for child). */
  malloc_mutex_lock(tsdn, &background_thread_lock);
  n_background_threads = 0;
  background_thread_enabled_set(tsdn, false);
  for (unsigned i = 0; i < max_background_threads; i++) {
    background_thread_info_t* info = &background_thread_info[i];
    malloc_mutex_lock(tsdn, &info->mtx);
    info->state = background_thread_stopped;
    int ret = pthread_cond_init(&info->cond, NULL);
    assert(ret == 0);
    background_thread_info_init(tsdn, info);
    malloc_mutex_unlock(tsdn, &info->mtx);
  }
  malloc_mutex_unlock(tsdn, &background_thread_lock);
}

bool background_thread_stats_read(
  tsdn_t* tsdn, background_thread_stats_t* stats) {
  assert(config_stats);
  malloc_mutex_lock(tsdn, &background_thread_lock);
  if (!background_thread_enabled()) {
    malloc_mutex_unlock(tsdn, &background_thread_lock);
    return true;
  }

  stats->num_threads = n_background_threads;
  uint64_t num_runs = 0;
  nstime_init(&stats->run_interval, 0);
  for (unsigned i = 0; i < max_background_threads; i++) {
    background_thread_info_t* info = &background_thread_info[i];
    if (malloc_mutex_trylock(tsdn, &info->mtx)) {
      /*
       * Each background thread run may take a long time;
       * avoid waiting on the stats if the thread is active.
       */
      continue;
    }
    if (info->state != background_thread_stopped) {
      num_runs += info->tot_n_runs;
      nstime_add(&stats->run_interval, &info->tot_sleep_time);
    }
    malloc_mutex_unlock(tsdn, &info->mtx);
  }
  stats->num_runs = num_runs;
  if (num_runs > 0) { nstime_idivide(&stats->run_interval, num_runs); }
  malloc_mutex_unlock(tsdn, &background_thread_lock);

  return false;
}

#undef BACKGROUND_THREAD_NPAGES_THRESHOLD
#undef BILLION
#undef BACKGROUND_THREAD_MIN_INTERVAL_NS

#ifdef JEMALLOC_HAVE_DLSYM
#include <dlfcn.h>
#endif

static bool pthread_create_fptr_init(void) {
  if (pthread_create_fptr != NULL) { return false; }
  /*
   * Try the next symbol first, because 1) when use lazy_lock we have a
   * wrapper for pthread_create; and 2) application may define its own
   * wrapper as well (and can call malloc within the wrapper).
   */
#ifdef JEMALLOC_HAVE_DLSYM
  pthread_create_fptr = dlsym(RTLD_NEXT, "pthread_create");
#else
  pthread_create_fptr = NULL;
#endif
  if (pthread_create_fptr == NULL) {
    if (config_lazy_lock) {
      malloc_write("<jemalloc>: Error in dlsym(RTLD_NEXT, "
                   "\"pthread_create\")\n");
      abort();
    } else {
      /* Fall back to the default symbol. */
      pthread_create_fptr = pthread_create;
    }
  }

  return false;
}

/*
 * When lazy lock is enabled, we need to make sure setting isthreaded before
 * taking any background_thread locks.  This is called early in ctl (instead
 * of wait for the pthread_create calls to trigger) because the mutex is
 * required before creating background threads.
 */
void background_thread_ctl_init(tsdn_t* tsdn) {
  malloc_mutex_assert_not_owner(tsdn, &background_thread_lock);
#ifdef JEMALLOC_PTHREAD_CREATE_WRAPPER
  pthread_create_fptr_init();
  pthread_create_wrapper_init();
#endif
}

#endif /* defined(JEMALLOC_BACKGROUND_THREAD) */

  bool background_thread_boot0(void) {
  if (!have_background_thread && opt_background_thread) {
    malloc_printf("<jemalloc>: option background_thread currently "
                  "supports pthread only\n");
    return true;
  }
#ifdef JEMALLOC_PTHREAD_CREATE_WRAPPER
  if ((config_lazy_lock || opt_background_thread)
    && pthread_create_fptr_init()) {
    return true;
  }
#endif
  return false;
}

bool background_thread_boot1(tsdn_t* tsdn) {
#ifdef JEMALLOC_BACKGROUND_THREAD
  assert(have_background_thread);
  assert(narenas_total_get() > 0);

  if (opt_max_background_threads > MAX_BACKGROUND_THREAD_LIMIT) {
    opt_max_background_threads = DEFAULT_NUM_BACKGROUND_THREAD;
  }
  max_background_threads = opt_max_background_threads;

  background_thread_enabled_set(tsdn, opt_background_thread);
  if (malloc_mutex_init(&background_thread_lock, "background_thread_global",
        WITNESS_RANK_BACKGROUND_THREAD_GLOBAL,
        malloc_mutex_rank_exclusive)) {
    return true;
  }

  background_thread_info = (background_thread_info_t*)base_alloc(tsdn,
    b0get(), opt_max_background_threads * sizeof(background_thread_info_t),
    CACHELINE);
  if (background_thread_info == NULL) { return true; }

  for (unsigned i = 0; i < max_background_threads; i++) {
    background_thread_info_t* info = &background_thread_info[i];
    /* Thread mutex is rank_inclusive because of thread0. */
    if (malloc_mutex_init(&info->mtx, "background_thread",
          WITNESS_RANK_BACKGROUND_THREAD, malloc_mutex_address_ordered)) {
      return true;
    }
    if (pthread_cond_init(&info->cond, NULL)) { return true; }
    malloc_mutex_lock(tsdn, &info->mtx);
    info->state = background_thread_stopped;
    background_thread_info_init(tsdn, info);
    malloc_mutex_unlock(tsdn, &info->mtx);
  }
#endif

  return false;
}
#define JEMALLOC_BASE_C_

#include "jemalloc/internal/extent_mmap.h"

/******************************************************************************/
/* Data. */

static base_t* b0;

metadata_thp_mode_t opt_metadata_thp = METADATA_THP_DEFAULT;

const char* metadata_thp_mode_names[] = { "disabled", "auto", "always" };

/******************************************************************************/

static inline bool metadata_thp_madvise(void) {
  return (
    metadata_thp_enabled() && (init_system_thp_mode == thp_mode_default));
}

static void* base_map(
  tsdn_t* tsdn, extent_hooks_t* extent_hooks, unsigned ind, size_t size) {
  void* addr;
  bool zero = true;
  bool commit = true;

  /* Use huge page sizes and alignment regardless of opt_metadata_thp. */
  assert(size == HUGEPAGE_CEILING(size));
  size_t alignment = HUGEPAGE;
  if (extent_hooks == &extent_hooks_default) {
    addr = extent_alloc_mmap(NULL, size, alignment, &zero, &commit);
  } else {
    /* No arena context as we are creating new arenas. */
    tsd_t* tsd = tsdn_null(tsdn) ? tsd_fetch() : tsdn_tsd(tsdn);
    pre_reentrancy(tsd, NULL);
    addr = extent_hooks->alloc(
      extent_hooks, NULL, size, alignment, &zero, &commit, ind);
    post_reentrancy(tsd);
  }

  return addr;
}

static void base_unmap(tsdn_t* tsdn, extent_hooks_t* extent_hooks,
  unsigned ind, void* addr, size_t size) {
  /*
   * Cascade through dalloc, decommit, purge_forced, and purge_lazy,
   * stopping at first success.  This cascade is performed for consistency
   * with the cascade in extent_dalloc_wrapper() because an application's
   * custom hooks may not support e.g. dalloc.  This function is only ever
   * called as a side effect of arena destruction, so although it might
   * seem pointless to do anything besides dalloc here, the application
   * may in fact want the end state of all associated virtual memory to be
   * in some consistent-but-allocated state.
   */
  if (extent_hooks == &extent_hooks_default) {
    if (!extent_dalloc_mmap(addr, size)) { goto label_done; }
    if (!pages_decommit(addr, size)) { goto label_done; }
    if (!pages_purge_forced(addr, size)) { goto label_done; }
    if (!pages_purge_lazy(addr, size)) { goto label_done; }
    /* Nothing worked.  This should never happen. */
    not_reached();
  } else {
    tsd_t* tsd = tsdn_null(tsdn) ? tsd_fetch() : tsdn_tsd(tsdn);
    pre_reentrancy(tsd, NULL);
    if (extent_hooks->dalloc != NULL
      && !extent_hooks->dalloc(extent_hooks, addr, size, true, ind)) {
      goto label_post_reentrancy;
    }
    if (extent_hooks->decommit != NULL
      && !extent_hooks->decommit(extent_hooks, addr, size, 0, size, ind)) {
      goto label_post_reentrancy;
    }
    if (extent_hooks->purge_forced != NULL
      && !extent_hooks->purge_forced(
        extent_hooks, addr, size, 0, size, ind)) {
      goto label_post_reentrancy;
    }
    if (extent_hooks->purge_lazy != NULL
      && !extent_hooks->purge_lazy(
        extent_hooks, addr, size, 0, size, ind)) {
      goto label_post_reentrancy;
    }
    /* Nothing worked.  That's the application's problem. */
  label_post_reentrancy:
    post_reentrancy(tsd);
  }
label_done:
  if (metadata_thp_madvise()) {
    /* Set NOHUGEPAGE after unmap to avoid kernel defrag. */
    assert(((uintptr_t)addr & HUGEPAGE_MASK) == 0
      && (size & HUGEPAGE_MASK) == 0);
    pages_nohuge(addr, size);
  }
}

static void base_extent_init(
  size_t* extent_sn_next, extent_t* extent, void* addr, size_t size) {
  size_t sn;

  sn = *extent_sn_next;
  (*extent_sn_next)++;

  extent_binit(extent, addr, size, sn);
}

static size_t base_get_num_blocks(base_t* base, bool with_new_block) {
  base_block_t* b = base->blocks;
  assert(b != NULL);

  size_t n_blocks = with_new_block ? 2 : 1;
  while (b->next != NULL) {
    n_blocks++;
    b = b->next;
  }

  return n_blocks;
}

static void base_auto_thp_switch(tsdn_t* tsdn, base_t* base) {
  assert(opt_metadata_thp == metadata_thp_auto);
  malloc_mutex_assert_owner(tsdn, &base->mtx);
  if (base->auto_thp_switched) { return; }
  /* Called when adding a new block. */
  bool should_switch;
  if (base_ind_get(base) != 0) {
    should_switch
      = (base_get_num_blocks(base, true) == BASE_AUTO_THP_THRESHOLD);
  } else {
    should_switch
      = (base_get_num_blocks(base, true) == BASE_AUTO_THP_THRESHOLD_A0);
  }
  if (!should_switch) { return; }

  base->auto_thp_switched = true;
  assert(!config_stats || base->n_thp == 0);
  /* Make the initial blocks THP lazily. */
  base_block_t* block = base->blocks;
  while (block != NULL) {
    assert((block->size & HUGEPAGE_MASK) == 0);
    pages_huge(block, block->size);
    if (config_stats) {
      base->n_thp
        += HUGEPAGE_CEILING(block->size - extent_bsize_get(&block->extent))
        >> LG_HUGEPAGE;
    }
    block = block->next;
    assert(block == NULL || (base_ind_get(base) == 0));
  }
}

static void* base_extent_bump_alloc_helper(
  extent_t* extent, size_t* gap_size, size_t size, size_t alignment) {
  void* ret;

  assert(alignment == ALIGNMENT_CEILING(alignment, QUANTUM));
  assert(size == ALIGNMENT_CEILING(size, alignment));

  *gap_size
    = ALIGNMENT_CEILING((uintptr_t)extent_addr_get(extent), alignment)
    - (uintptr_t)extent_addr_get(extent);
  ret = (void*)((uintptr_t)extent_addr_get(extent) + *gap_size);
  assert(extent_bsize_get(extent) >= *gap_size + size);
  extent_binit(extent,
    (void*)((uintptr_t)extent_addr_get(extent) + *gap_size + size),
    extent_bsize_get(extent) - *gap_size - size, extent_sn_get(extent));
  return ret;
}

static void base_extent_bump_alloc_post(base_t* base, extent_t* extent,
  size_t gap_size, void* addr, size_t size) {
  if (extent_bsize_get(extent) > 0) {
    /*
     * Compute the index for the largest size class that does not
     * exceed extent's size.
     */
    szind_t index_floor = sz_size2index(extent_bsize_get(extent) + 1) - 1;
    extent_heap_insert(&base->avail[index_floor], extent);
  }

  if (config_stats) {
    base->allocated += size;
    /*
     * Add one PAGE to base_resident for every page boundary that is
     * crossed by the new allocation. Adjust n_thp similarly when
     * metadata_thp is enabled.
     */
    base->resident += PAGE_CEILING((uintptr_t)addr + size)
      - PAGE_CEILING((uintptr_t)addr - gap_size);
    assert(base->allocated <= base->resident);
    assert(base->resident <= base->mapped);
    if (metadata_thp_madvise()
      && (opt_metadata_thp == metadata_thp_always
        || base->auto_thp_switched)) {
      base->n_thp += (HUGEPAGE_CEILING((uintptr_t)addr + size)
                       - HUGEPAGE_CEILING((uintptr_t)addr - gap_size))
        >> LG_HUGEPAGE;
      assert(base->mapped >= base->n_thp << LG_HUGEPAGE);
    }
  }
}

static void* base_extent_bump_alloc(
  base_t* base, extent_t* extent, size_t size, size_t alignment) {
  void* ret;
  size_t gap_size;

  ret = base_extent_bump_alloc_helper(extent, &gap_size, size, alignment);
  base_extent_bump_alloc_post(base, extent, gap_size, ret, size);
  return ret;
}

/*
 * Allocate a block of virtual memory that is large enough to start with a
 * base_block_t header, followed by an object of specified size and
 * alignment. On success a pointer to the initialized base_block_t header is
 * returned.
 */
static base_block_t* base_block_alloc(tsdn_t* tsdn, base_t* base,
  extent_hooks_t* extent_hooks, unsigned ind, pszind_t* pind_last,
  size_t* extent_sn_next, size_t size, size_t alignment) {
  alignment = ALIGNMENT_CEILING(alignment, QUANTUM);
  size_t usize = ALIGNMENT_CEILING(size, alignment);
  size_t header_size = sizeof(base_block_t);
  size_t gap_size = ALIGNMENT_CEILING(header_size, alignment) - header_size;
  /*
   * Create increasingly larger blocks in order to limit the total number
   * of disjoint virtual memory ranges.  Choose the next size in the page
   * size class series (skipping size classes that are not a multiple of
   * HUGEPAGE), or a size large enough to satisfy the requested size and
   * alignment, whichever is larger.
   */
  size_t min_block_size
    = HUGEPAGE_CEILING(sz_psz2u(header_size + gap_size + usize));
  pszind_t pind_next = (*pind_last + 1 < sz_psz2ind(SC_LARGE_MAXCLASS))
    ? *pind_last + 1
    : *pind_last;
  size_t next_block_size = HUGEPAGE_CEILING(sz_pind2sz(pind_next));
  size_t block_size
    = (min_block_size > next_block_size) ? min_block_size : next_block_size;
  base_block_t* block
    = (base_block_t*)base_map(tsdn, extent_hooks, ind, block_size);
  if (block == NULL) { return NULL; }

  if (metadata_thp_madvise()) {
    void* addr = (void*)block;
    assert(((uintptr_t)addr & HUGEPAGE_MASK) == 0
      && (block_size & HUGEPAGE_MASK) == 0);
    if (opt_metadata_thp == metadata_thp_always) {
      pages_huge(addr, block_size);
    } else if (opt_metadata_thp == metadata_thp_auto && base != NULL) {
      /* base != NULL indicates this is not a new base. */
      malloc_mutex_lock(tsdn, &base->mtx);
      base_auto_thp_switch(tsdn, base);
      if (base->auto_thp_switched) { pages_huge(addr, block_size); }
      malloc_mutex_unlock(tsdn, &base->mtx);
    }
  }

  *pind_last = sz_psz2ind(block_size);
  block->size = block_size;
  block->next = NULL;
  assert(block_size >= header_size);
  base_extent_init(extent_sn_next, &block->extent,
    (void*)((uintptr_t)block + header_size), block_size - header_size);
  return block;
}

/*
 * Allocate an extent that is at least as large as specified size, with
 * specified alignment.
 */
static extent_t* base_extent_alloc(
  tsdn_t* tsdn, base_t* base, size_t size, size_t alignment) {
  malloc_mutex_assert_owner(tsdn, &base->mtx);

  extent_hooks_t* extent_hooks = base_extent_hooks_get(base);
  /*
   * Drop mutex during base_block_alloc(), because an extent hook will be
   * called.
   */
  malloc_mutex_unlock(tsdn, &base->mtx);
  base_block_t* block
    = base_block_alloc(tsdn, base, extent_hooks, base_ind_get(base),
      &base->pind_last, &base->extent_sn_next, size, alignment);
  malloc_mutex_lock(tsdn, &base->mtx);
  if (block == NULL) { return NULL; }
  block->next = base->blocks;
  base->blocks = block;
  if (config_stats) {
    base->allocated += sizeof(base_block_t);
    base->resident += PAGE_CEILING(sizeof(base_block_t));
    base->mapped += block->size;
    if (metadata_thp_madvise()
      && !(opt_metadata_thp == metadata_thp_auto
        && !base->auto_thp_switched)) {
      assert(base->n_thp > 0);
      base->n_thp += HUGEPAGE_CEILING(sizeof(base_block_t)) >> LG_HUGEPAGE;
    }
    assert(base->allocated <= base->resident);
    assert(base->resident <= base->mapped);
    assert(base->n_thp << LG_HUGEPAGE <= base->mapped);
  }
  return &block->extent;
}

base_t* b0get(void) { return b0; }

base_t* base_new(tsdn_t* tsdn, unsigned ind, extent_hooks_t* extent_hooks) {
  pszind_t pind_last = 0;
  size_t extent_sn_next = 0;
  base_block_t* block = base_block_alloc(tsdn, NULL, extent_hooks, ind,
    &pind_last, &extent_sn_next, sizeof(base_t), QUANTUM);
  if (block == NULL) { return NULL; }

  size_t gap_size;
  size_t base_alignment = CACHELINE;
  size_t base_size = ALIGNMENT_CEILING(sizeof(base_t), base_alignment);
  base_t* base = (base_t*)base_extent_bump_alloc_helper(
    &block->extent, &gap_size, base_size, base_alignment);
  base->ind = ind;
  atomic_store_p(&base->extent_hooks, extent_hooks, ATOMIC_RELAXED);
  if (malloc_mutex_init(&base->mtx, "base", WITNESS_RANK_BASE,
        malloc_mutex_rank_exclusive)) {
    base_unmap(tsdn, extent_hooks, ind, block, block->size);
    return NULL;
  }
  base->pind_last = pind_last;
  base->extent_sn_next = extent_sn_next;
  base->blocks = block;
  base->auto_thp_switched = false;
  for (szind_t i = 0; i < SC_NSIZES; i++) {
    extent_heap_new(&base->avail[i]);
  }
  if (config_stats) {
    base->allocated = sizeof(base_block_t);
    base->resident = PAGE_CEILING(sizeof(base_block_t));
    base->mapped = block->size;
    base->n_thp
      = (opt_metadata_thp == metadata_thp_always) && metadata_thp_madvise()
      ? HUGEPAGE_CEILING(sizeof(base_block_t)) >> LG_HUGEPAGE
      : 0;
    assert(base->allocated <= base->resident);
    assert(base->resident <= base->mapped);
    assert(base->n_thp << LG_HUGEPAGE <= base->mapped);
  }
  base_extent_bump_alloc_post(
    base, &block->extent, gap_size, base, base_size);

  return base;
}

void base_delete(tsdn_t* tsdn, base_t* base) {
  extent_hooks_t* extent_hooks = base_extent_hooks_get(base);
  base_block_t* next = base->blocks;
  do {
    base_block_t* block = next;
    next = block->next;
    base_unmap(tsdn, extent_hooks, base_ind_get(base), block, block->size);
  } while (next != NULL);
}

extent_hooks_t* base_extent_hooks_get(base_t* base) {
  return (extent_hooks_t*)atomic_load_p(
    &base->extent_hooks, ATOMIC_ACQUIRE);
}

extent_hooks_t* base_extent_hooks_set(
  base_t* base, extent_hooks_t* extent_hooks) {
  extent_hooks_t* old_extent_hooks = base_extent_hooks_get(base);
  atomic_store_p(&base->extent_hooks, extent_hooks, ATOMIC_RELEASE);
  return old_extent_hooks;
}

static void* base_alloc_impl(
  tsdn_t* tsdn, base_t* base, size_t size, size_t alignment, size_t* esn) {
  alignment = QUANTUM_CEILING(alignment);
  size_t usize = ALIGNMENT_CEILING(size, alignment);
  size_t asize = usize + alignment - QUANTUM;

  extent_t* extent = NULL;
  malloc_mutex_lock(tsdn, &base->mtx);
  for (szind_t i = sz_size2index(asize); i < SC_NSIZES; i++) {
    extent = extent_heap_remove_first(&base->avail[i]);
    if (extent != NULL) {
      /* Use existing space. */
      break;
    }
  }
  if (extent == NULL) {
    /* Try to allocate more space. */
    extent = base_extent_alloc(tsdn, base, usize, alignment);
  }
  void* ret;
  if (extent == NULL) {
    ret = NULL;
    goto label_return;
  }

  ret = base_extent_bump_alloc(base, extent, usize, alignment);
  if (esn != NULL) { *esn = extent_sn_get(extent); }
label_return:
  malloc_mutex_unlock(tsdn, &base->mtx);
  return ret;
}

/*
 * base_alloc() returns zeroed memory, which is always demand-zeroed for the
 * auto arenas, in order to make multi-page sparse data structures such as
 * radix tree nodes efficient with respect to physical memory usage.  Upon
 * success a pointer to at least size bytes with specified alignment is
 * returned.  Note that size is rounded up to the nearest multiple of
 * alignment to avoid false sharing.
 */
void* base_alloc(
  tsdn_t* tsdn, base_t* base, size_t size, size_t alignment) {
  return base_alloc_impl(tsdn, base, size, alignment, NULL);
}

extent_t* base_alloc_extent(tsdn_t* tsdn, base_t* base) {
  size_t esn;
  extent_t* extent
    = base_alloc_impl(tsdn, base, sizeof(extent_t), CACHELINE, &esn);
  if (extent == NULL) { return NULL; }
  extent_esn_set(extent, esn);
  return extent;
}

void base_stats_get(tsdn_t* tsdn, base_t* base, size_t* allocated,
  size_t* resident, size_t* mapped, size_t* n_thp) {
  cassert(config_stats);

  malloc_mutex_lock(tsdn, &base->mtx);
  assert(base->allocated <= base->resident);
  assert(base->resident <= base->mapped);
  *allocated = base->allocated;
  *resident = base->resident;
  *mapped = base->mapped;
  *n_thp = base->n_thp;
  malloc_mutex_unlock(tsdn, &base->mtx);
}

void base_prefork(tsdn_t* tsdn, base_t* base) {
  malloc_mutex_prefork(tsdn, &base->mtx);
}

void base_postfork_parent(tsdn_t* tsdn, base_t* base) {
  malloc_mutex_postfork_parent(tsdn, &base->mtx);
}

void base_postfork_child(tsdn_t* tsdn, base_t* base) {
  malloc_mutex_postfork_child(tsdn, &base->mtx);
}

bool base_boot(tsdn_t* tsdn) {
  b0 = base_new(tsdn, 0, (extent_hooks_t*)&extent_hooks_default);
  return (b0 == NULL);
}

#include "jemalloc/internal/bin.h"

#include "jemalloc/internal/witness.h"

bin_info_t bin_infos[SC_NBINS];

static void bin_infos_init(sc_data_t* sc_data,
  unsigned bin_shard_sizes[SC_NBINS], bin_info_t bin_infos[SC_NBINS]) {
  for (unsigned i = 0; i < SC_NBINS; i++) {
    bin_info_t* bin_info = &bin_infos[i];
    sc_t* sc = &sc_data->sc[i];
    bin_info->reg_size
      = ((size_t)1U << sc->lg_base) + ((size_t)sc->ndelta << sc->lg_delta);
    bin_info->slab_size = (sc->pgs << LG_PAGE);
    bin_info->nregs = (uint32_t)(bin_info->slab_size / bin_info->reg_size);
    bin_info->n_shards = bin_shard_sizes[i];
    bitmap_info_t bitmap_info = BITMAP_INFO_INITIALIZER(bin_info->nregs);
    bin_info->bitmap_info = bitmap_info;
  }
}

bool bin_update_shard_size(unsigned bin_shard_sizes[SC_NBINS],
  size_t start_size, size_t end_size, size_t nshards) {
  if (nshards > BIN_SHARDS_MAX || nshards == 0) { return true; }

  if (start_size > SC_SMALL_MAXCLASS) { return false; }
  if (end_size > SC_SMALL_MAXCLASS) { end_size = SC_SMALL_MAXCLASS; }

  /* Compute the index since this may happen before sz init. */
  szind_t ind1 = sz_size2index_compute(start_size);
  szind_t ind2 = sz_size2index_compute(end_size);
  for (unsigned i = ind1; i <= ind2; i++) {
    bin_shard_sizes[i] = (unsigned)nshards;
  }

  return false;
}

void bin_shard_sizes_boot(unsigned bin_shard_sizes[SC_NBINS]) {
  /* Load the default number of shards. */
  for (unsigned i = 0; i < SC_NBINS; i++) {
    bin_shard_sizes[i] = N_BIN_SHARDS_DEFAULT;
  }
}

void bin_boot(sc_data_t* sc_data, unsigned bin_shard_sizes[SC_NBINS]) {
  assert(sc_data->initialized);
  bin_infos_init(sc_data, bin_shard_sizes, bin_infos);
}

bool bin_init(bin_t* bin) {
  if (malloc_mutex_init(
        &bin->lock, "bin", WITNESS_RANK_BIN, malloc_mutex_rank_exclusive)) {
    return true;
  }
  bin->slabcur = NULL;
  extent_heap_new(&bin->slabs_nonfull);
  extent_list_init(&bin->slabs_full);
  if (config_stats) { memset(&bin->stats, 0, sizeof(bin_stats_t)); }
  return false;
}

void bin_prefork(tsdn_t* tsdn, bin_t* bin) {
  malloc_mutex_prefork(tsdn, &bin->lock);
}

void bin_postfork_parent(tsdn_t* tsdn, bin_t* bin) {
  malloc_mutex_postfork_parent(tsdn, &bin->lock);
}

void bin_postfork_child(tsdn_t* tsdn, bin_t* bin) {
  malloc_mutex_postfork_child(tsdn, &bin->lock);
}
#define JEMALLOC_BITMAP_C_

/******************************************************************************/

#ifdef BITMAP_USE_TREE

void bitmap_info_init(bitmap_info_t* binfo, size_t nbits) {
  unsigned i;
  size_t group_count;

  assert(nbits > 0);
  assert(nbits <= (ZU(1) << LG_BITMAP_MAXBITS));

  /*
   * Compute the number of groups necessary to store nbits bits, and
   * progressively work upward through the levels until reaching a level
   * that requires only one group.
   */
  binfo->levels[0].group_offset = 0;
  group_count = BITMAP_BITS2GROUPS(nbits);
  for (i = 1; group_count > 1; i++) {
    assert(i < BITMAP_MAX_LEVELS);
    binfo->levels[i].group_offset
      = binfo->levels[i - 1].group_offset + group_count;
    group_count = BITMAP_BITS2GROUPS(group_count);
  }
  binfo->levels[i].group_offset
    = binfo->levels[i - 1].group_offset + group_count;
  assert(binfo->levels[i].group_offset <= BITMAP_GROUPS_MAX);
  binfo->nlevels = i;
  binfo->nbits = nbits;
}

static size_t bitmap_info_ngroups(const bitmap_info_t* binfo) {
  return binfo->levels[binfo->nlevels].group_offset;
}

void bitmap_init(bitmap_t* bitmap, const bitmap_info_t* binfo, bool fill) {
  size_t extra;
  unsigned i;

  /*
   * Bits are actually inverted with regard to the external bitmap
   * interface.
   */

  if (fill) {
    /* The "filled" bitmap starts out with all 0 bits. */
    memset(bitmap, 0, bitmap_size(binfo));
    return;
  }

  /*
   * The "empty" bitmap starts out with all 1 bits, except for trailing
   * unused bits (if any).  Note that each group uses bit 0 to correspond
   * to the first logical bit in the group, so extra bits are the most
   * significant bits of the last group.
   */
  memset(bitmap, 0xffU, bitmap_size(binfo));
  extra = (BITMAP_GROUP_NBITS - (binfo->nbits & BITMAP_GROUP_NBITS_MASK))
    & BITMAP_GROUP_NBITS_MASK;
  if (extra != 0) { bitmap[binfo->levels[1].group_offset - 1] >>= extra; }
  for (i = 1; i < binfo->nlevels; i++) {
    size_t group_count
      = binfo->levels[i].group_offset - binfo->levels[i - 1].group_offset;
    extra = (BITMAP_GROUP_NBITS - (group_count & BITMAP_GROUP_NBITS_MASK))
      & BITMAP_GROUP_NBITS_MASK;
    if (extra != 0) {
      bitmap[binfo->levels[i + 1].group_offset - 1] >>= extra;
    }
  }
}

#else /* BITMAP_USE_TREE */

void bitmap_info_init(bitmap_info_t* binfo, size_t nbits) {
  assert(nbits > 0);
  assert(nbits <= (ZU(1) << LG_BITMAP_MAXBITS));

  binfo->ngroups = BITMAP_BITS2GROUPS(nbits);
  binfo->nbits = nbits;
}

static size_t bitmap_info_ngroups(const bitmap_info_t* binfo) {
  return binfo->ngroups;
}

void bitmap_init(bitmap_t* bitmap, const bitmap_info_t* binfo, bool fill) {
  size_t extra;

  if (fill) {
    memset(bitmap, 0, bitmap_size(binfo));
    return;
  }

  memset(bitmap, 0xffU, bitmap_size(binfo));
  extra = (BITMAP_GROUP_NBITS - (binfo->nbits & BITMAP_GROUP_NBITS_MASK))
    & BITMAP_GROUP_NBITS_MASK;
  if (extra != 0) { bitmap[binfo->ngroups - 1] >>= extra; }
}

#endif /* BITMAP_USE_TREE */

size_t bitmap_size(const bitmap_info_t* binfo) {
  return (bitmap_info_ngroups(binfo) << LG_SIZEOF_BITMAP);
}
/*
 *******************************************************************************
 * Implementation of (2^1+,2) cuckoo hashing, where 2^1+ indicates that each
 * hash bucket contains 2^n cells, for n >= 1, and 2 indicates that two hash
 * functions are employed.  The original cuckoo hashing algorithm was
 *described in:
 *
 *   Pagh, R., F.F. Rodler (2004) Cuckoo Hashing.  Journal of Algorithms
 *     51(2):122-144.
 *
 * Generalization of cuckoo hashing was discussed in:
 *
 *   Erlingsson, U., M. Manasse, F. McSherry (2006) A cool and practical
 *     alternative to traditional hash tables.  In Proceedings of the 7th
 *     Workshop on Distributed Data and Structures (WDAS'06), Santa Clara,
 *CA, January 2006.
 *
 * This implementation uses precisely two hash functions because that is the
 * fewest that can work, and supporting multiple hashes is an implementation
 * burden.  Here is a reproduction of Figure 1 from Erlingsson et al. (2006)
 * that shows approximate expected maximum load factors for various
 * configurations:
 *
 *           |         #cells/bucket         |
 *   #hashes |   1   |   2   |   4   |   8   |
 *   --------+-------+-------+-------+-------+
 *         1 | 0.006 | 0.006 | 0.03  | 0.12  |
 *         2 | 0.49  | 0.86  |>0.93< |>0.96< |
 *         3 | 0.91  | 0.97  | 0.98  | 0.999 |
 *         4 | 0.97  | 0.99  | 0.999 |       |
 *
 * The number of cells per bucket is chosen such that a bucket fits in one
 *cache line.  So, on 32- and 64-bit systems, we use (8,2) and (4,2) cuckoo
 *hashing, respectively.
 *
 ******************************************************************************/
#define JEMALLOC_CKH_C_

#include "jemalloc/internal/ckh.h"

#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/malloc_io.h"
#include "jemalloc/internal/prng.h"

/******************************************************************************/
/* Function prototypes for non-inline static functions. */

static bool ckh_grow(tsd_t* tsd, ckh_t* ckh);
static void ckh_shrink(tsd_t* tsd, ckh_t* ckh);

/******************************************************************************/

/*
 * Search bucket for key and return the cell number if found; SIZE_T_MAX
 * otherwise.
 */
static size_t ckh_bucket_search(
  ckh_t* ckh, size_t bucket, const void* key) {
  ckhc_t* cell;
  unsigned i;

  for (i = 0; i < (ZU(1) << LG_CKH_BUCKET_CELLS); i++) {
    cell = &ckh->tab[(bucket << LG_CKH_BUCKET_CELLS) + i];
    if (cell->key != NULL && ckh->keycomp(key, cell->key)) {
      return (bucket << LG_CKH_BUCKET_CELLS) + i;
    }
  }

  return SIZE_T_MAX;
}

/*
 * Search table for key and return cell number if found; SIZE_T_MAX
 * otherwise.
 */
static size_t ckh_isearch(ckh_t* ckh, const void* key) {
  size_t hashes[2], bucket, cell;

  assert(ckh != NULL);

  ckh->hash(key, hashes);

  /* Search primary bucket. */
  bucket = hashes[0] & ((ZU(1) << ckh->lg_curbuckets) - 1);
  cell = ckh_bucket_search(ckh, bucket, key);
  if (cell != SIZE_T_MAX) { return cell; }

  /* Search secondary bucket. */
  bucket = hashes[1] & ((ZU(1) << ckh->lg_curbuckets) - 1);
  cell = ckh_bucket_search(ckh, bucket, key);
  return cell;
}

static bool ckh_try_bucket_insert(
  ckh_t* ckh, size_t bucket, const void* key, const void* data) {
  ckhc_t* cell;
  unsigned offset, i;

  /*
   * Cycle through the cells in the bucket, starting at a random position.
   * The randomness avoids worst-case search overhead as buckets fill up.
   */
  offset
    = (unsigned)prng_lg_range_u64(&ckh->prng_state, LG_CKH_BUCKET_CELLS);
  for (i = 0; i < (ZU(1) << LG_CKH_BUCKET_CELLS); i++) {
    cell = &ckh->tab[(bucket << LG_CKH_BUCKET_CELLS)
      + ((i + offset) & ((ZU(1) << LG_CKH_BUCKET_CELLS) - 1))];
    if (cell->key == NULL) {
      cell->key = key;
      cell->data = data;
      ckh->count++;
      return false;
    }
  }

  return true;
}

/*
 * No space is available in bucket.  Randomly evict an item, then try to
 * find an alternate location for that item.  Iteratively repeat this
 * eviction/relocation procedure until either success or detection of an
 * eviction/relocation bucket cycle.
 */
static bool ckh_evict_reloc_insert(
  ckh_t* ckh, size_t argbucket, void const** argkey, void const** argdata) {
  const void *key, *data, *tkey, *tdata;
  ckhc_t* cell;
  size_t hashes[2], bucket, tbucket;
  unsigned i;

  bucket = argbucket;
  key = *argkey;
  data = *argdata;
  while (true) {
    /*
     * Choose a random item within the bucket to evict.  This is
     * critical to correct function, because without (eventually)
     * evicting all items within a bucket during iteration, it
     * would be possible to get stuck in an infinite loop if there
     * were an item for which both hashes indicated the same
     * bucket.
     */
    i = (unsigned)prng_lg_range_u64(&ckh->prng_state, LG_CKH_BUCKET_CELLS);
    cell = &ckh->tab[(bucket << LG_CKH_BUCKET_CELLS) + i];
    assert(cell->key != NULL);

    /* Swap cell->{key,data} and {key,data} (evict). */
    tkey = cell->key;
    tdata = cell->data;
    cell->key = key;
    cell->data = data;
    key = tkey;
    data = tdata;

#ifdef CKH_COUNT
    ckh->nrelocs++;
#endif

    /* Find the alternate bucket for the evicted item. */
    ckh->hash(key, hashes);
    tbucket = hashes[1] & ((ZU(1) << ckh->lg_curbuckets) - 1);
    if (tbucket == bucket) {
      tbucket = hashes[0] & ((ZU(1) << ckh->lg_curbuckets) - 1);
      /*
       * It may be that (tbucket == bucket) still, if the
       * item's hashes both indicate this bucket.  However,
       * we are guaranteed to eventually escape this bucket
       * during iteration, assuming pseudo-random item
       * selection (true randomness would make infinite
       * looping a remote possibility).  The reason we can
       * never get trapped forever is that there are two
       * cases:
       *
       * 1) This bucket == argbucket, so we will quickly
       *    detect an eviction cycle and terminate.
       * 2) An item was evicted to this bucket from another,
       *    which means that at least one item in this bucket
       *    has hashes that indicate distinct buckets.
       */
    }
    /* Check for a cycle. */
    if (tbucket == argbucket) {
      *argkey = key;
      *argdata = data;
      return true;
    }

    bucket = tbucket;
    if (!ckh_try_bucket_insert(ckh, bucket, key, data)) { return false; }
  }
}

static bool ckh_try_insert(
  ckh_t* ckh, void const** argkey, void const** argdata) {
  size_t hashes[2], bucket;
  const void* key = *argkey;
  const void* data = *argdata;

  ckh->hash(key, hashes);

  /* Try to insert in primary bucket. */
  bucket = hashes[0] & ((ZU(1) << ckh->lg_curbuckets) - 1);
  if (!ckh_try_bucket_insert(ckh, bucket, key, data)) { return false; }

  /* Try to insert in secondary bucket. */
  bucket = hashes[1] & ((ZU(1) << ckh->lg_curbuckets) - 1);
  if (!ckh_try_bucket_insert(ckh, bucket, key, data)) { return false; }

  /*
   * Try to find a place for this item via iterative eviction/relocation.
   */
  return ckh_evict_reloc_insert(ckh, bucket, argkey, argdata);
}

/*
 * Try to rebuild the hash table from scratch by inserting all items from
 * the old table into the new.
 */
static bool ckh_rebuild(ckh_t* ckh, ckhc_t* aTab) {
  size_t count, i, nins;
  const void *key, *data;

  count = ckh->count;
  ckh->count = 0;
  for (i = nins = 0; nins < count; i++) {
    if (aTab[i].key != NULL) {
      key = aTab[i].key;
      data = aTab[i].data;
      if (ckh_try_insert(ckh, &key, &data)) {
        ckh->count = count;
        return true;
      }
      nins++;
    }
  }

  return false;
}

static bool ckh_grow(tsd_t* tsd, ckh_t* ckh) {
  bool ret;
  ckhc_t *tab, *ttab;
  unsigned lg_prevbuckets, lg_curcells;

#ifdef CKH_COUNT
  ckh->ngrows++;
#endif

  /*
   * It is possible (though unlikely, given well behaved hashes) that the
   * table will have to be doubled more than once in order to create a
   * usable table.
   */
  lg_prevbuckets = ckh->lg_curbuckets;
  lg_curcells = ckh->lg_curbuckets + LG_CKH_BUCKET_CELLS;
  while (true) {
    size_t usize;

    lg_curcells++;
    usize = sz_sa2u(sizeof(ckhc_t) << lg_curcells, CACHELINE);
    if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) {
      ret = true;
      goto label_return;
    }
    tab = (ckhc_t*)ipallocztm(tsd_tsdn(tsd), usize, CACHELINE, true, NULL,
      true, arena_ichoose(tsd, NULL));
    if (tab == NULL) {
      ret = true;
      goto label_return;
    }
    /* Swap in new table. */
    ttab = ckh->tab;
    ckh->tab = tab;
    tab = ttab;
    ckh->lg_curbuckets = lg_curcells - LG_CKH_BUCKET_CELLS;

    if (!ckh_rebuild(ckh, tab)) {
      idalloctm(tsd_tsdn(tsd), tab, NULL, NULL, true, true);
      break;
    }

    /* Rebuilding failed, so back out partially rebuilt table. */
    idalloctm(tsd_tsdn(tsd), ckh->tab, NULL, NULL, true, true);
    ckh->tab = tab;
    ckh->lg_curbuckets = lg_prevbuckets;
  }

  ret = false;
label_return:
  return ret;
}

static void ckh_shrink(tsd_t* tsd, ckh_t* ckh) {
  ckhc_t *tab, *ttab;
  size_t usize;
  unsigned lg_prevbuckets, lg_curcells;

  /*
   * It is possible (though unlikely, given well behaved hashes) that the
   * table rebuild will fail.
   */
  lg_prevbuckets = ckh->lg_curbuckets;
  lg_curcells = ckh->lg_curbuckets + LG_CKH_BUCKET_CELLS - 1;
  usize = sz_sa2u(sizeof(ckhc_t) << lg_curcells, CACHELINE);
  if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) { return; }
  tab = (ckhc_t*)ipallocztm(tsd_tsdn(tsd), usize, CACHELINE, true, NULL,
    true, arena_ichoose(tsd, NULL));
  if (tab == NULL) {
    /*
     * An OOM error isn't worth propagating, since it doesn't
     * prevent this or future operations from proceeding.
     */
    return;
  }
  /* Swap in new table. */
  ttab = ckh->tab;
  ckh->tab = tab;
  tab = ttab;
  ckh->lg_curbuckets = lg_curcells - LG_CKH_BUCKET_CELLS;

  if (!ckh_rebuild(ckh, tab)) {
    idalloctm(tsd_tsdn(tsd), tab, NULL, NULL, true, true);
#ifdef CKH_COUNT
    ckh->nshrinks++;
#endif
    return;
  }

  /* Rebuilding failed, so back out partially rebuilt table. */
  idalloctm(tsd_tsdn(tsd), ckh->tab, NULL, NULL, true, true);
  ckh->tab = tab;
  ckh->lg_curbuckets = lg_prevbuckets;
#ifdef CKH_COUNT
  ckh->nshrinkfails++;
#endif
}

bool ckh_new(tsd_t* tsd, ckh_t* ckh, size_t minitems, ckh_hash_t* hash,
  ckh_keycomp_t* keycomp) {
  bool ret;
  size_t mincells, usize;
  unsigned lg_mincells;

  assert(minitems > 0);
  assert(hash != NULL);
  assert(keycomp != NULL);

#ifdef CKH_COUNT
  ckh->ngrows = 0;
  ckh->nshrinks = 0;
  ckh->nshrinkfails = 0;
  ckh->ninserts = 0;
  ckh->nrelocs = 0;
#endif
  ckh->prng_state = 42; /* Value doesn't really matter. */
  ckh->count = 0;

  /*
   * Find the minimum power of 2 that is large enough to fit minitems
   * entries.  We are using (2+,2) cuckoo hashing, which has an expected
   * maximum load factor of at least ~0.86, so 0.75 is a conservative load
   * factor that will typically allow mincells items to fit without ever
   * growing the table.
   */
  assert(LG_CKH_BUCKET_CELLS > 0);
  mincells = ((minitems + (3 - (minitems % 3))) / 3) << 2;
  for (lg_mincells = LG_CKH_BUCKET_CELLS; (ZU(1) << lg_mincells) < mincells;
       lg_mincells++) {
    /* Do nothing. */
  }
  ckh->lg_minbuckets = lg_mincells - LG_CKH_BUCKET_CELLS;
  ckh->lg_curbuckets = lg_mincells - LG_CKH_BUCKET_CELLS;
  ckh->hash = hash;
  ckh->keycomp = keycomp;

  usize = sz_sa2u(sizeof(ckhc_t) << lg_mincells, CACHELINE);
  if (unlikely(usize == 0 || usize > SC_LARGE_MAXCLASS)) {
    ret = true;
    goto label_return;
  }
  ckh->tab = (ckhc_t*)ipallocztm(tsd_tsdn(tsd), usize, CACHELINE, true,
    NULL, true, arena_ichoose(tsd, NULL));
  if (ckh->tab == NULL) {
    ret = true;
    goto label_return;
  }

  ret = false;
label_return:
  return ret;
}

void ckh_delete(tsd_t* tsd, ckh_t* ckh) {
  assert(ckh != NULL);

#ifdef CKH_VERBOSE
  malloc_printf("%s(%p): ngrows: %" FMTu64 ", nshrinks: %" FMTu64 ","
                " nshrinkfails: %" FMTu64 ", ninserts: %" FMTu64 ","
                " nrelocs: %" FMTu64 "\n",
    __func__, ckh, (unsigned long long)ckh->ngrows,
    (unsigned long long)ckh->nshrinks,
    (unsigned long long)ckh->nshrinkfails,
    (unsigned long long)ckh->ninserts, (unsigned long long)ckh->nrelocs);
#endif

  idalloctm(tsd_tsdn(tsd), ckh->tab, NULL, NULL, true, true);
  if (config_debug) { memset(ckh, JEMALLOC_FREE_JUNK, sizeof(ckh_t)); }
}

size_t ckh_count(ckh_t* ckh) {
  assert(ckh != NULL);

  return ckh->count;
}

bool ckh_iter(ckh_t* ckh, size_t* tabind, void** key, void** data) {
  size_t i, ncells;

  for (i = *tabind,
      ncells = (ZU(1) << (ckh->lg_curbuckets + LG_CKH_BUCKET_CELLS));
       i < ncells; i++) {
    if (ckh->tab[i].key != NULL) {
      if (key != NULL) { *key = (void*)ckh->tab[i].key; }
      if (data != NULL) { *data = (void*)ckh->tab[i].data; }
      *tabind = i + 1;
      return false;
    }
  }

  return true;
}

bool ckh_insert(tsd_t* tsd, ckh_t* ckh, const void* key, const void* data) {
  bool ret;

  assert(ckh != NULL);
  assert(ckh_search(ckh, key, NULL, NULL));

#ifdef CKH_COUNT
  ckh->ninserts++;
#endif

  while (ckh_try_insert(ckh, &key, &data)) {
    if (ckh_grow(tsd, ckh)) {
      ret = true;
      goto label_return;
    }
  }

  ret = false;
label_return:
  return ret;
}

bool ckh_remove(
  tsd_t* tsd, ckh_t* ckh, const void* searchkey, void** key, void** data) {
  size_t cell;

  assert(ckh != NULL);

  cell = ckh_isearch(ckh, searchkey);
  if (cell != SIZE_T_MAX) {
    if (key != NULL) { *key = (void*)ckh->tab[cell].key; }
    if (data != NULL) { *data = (void*)ckh->tab[cell].data; }
    ckh->tab[cell].key = NULL;
    ckh->tab[cell].data = NULL; /* Not necessary. */

    ckh->count--;
    /* Try to halve the table if it is less than 1/4 full. */
    if (ckh->count
        < (ZU(1) << (ckh->lg_curbuckets + LG_CKH_BUCKET_CELLS - 2))
      && ckh->lg_curbuckets > ckh->lg_minbuckets) {
      /* Ignore error due to OOM. */
      ckh_shrink(tsd, ckh);
    }

    return false;
  }

  return true;
}

bool ckh_search(
  ckh_t* ckh, const void* searchkey, void** key, void** data) {
  size_t cell;

  assert(ckh != NULL);

  cell = ckh_isearch(ckh, searchkey);
  if (cell != SIZE_T_MAX) {
    if (key != NULL) { *key = (void*)ckh->tab[cell].key; }
    if (data != NULL) { *data = (void*)ckh->tab[cell].data; }
    return false;
  }

  return true;
}

void ckh_string_hash(const void* key, size_t r_hash[2]) {
  hash(key, cstr_len((const char*)key), 0x94122f33U, r_hash);
}

bool ckh_string_keycomp(const void* k1, const void* k2) {
  assert(k1 != NULL);
  assert(k2 != NULL);

  return !strcmp((char*)k1, (char*)k2);
}

void ckh_pointer_hash(const void* key, size_t r_hash[2]) {
  union {
    const void* v;
    size_t i;
  } u;

  assert(sizeof(u.v) == sizeof(u.i));
  u.v = key;
  hash(&u.i, sizeof(u.i), 0xd983396eU, r_hash);
}

bool ckh_pointer_keycomp(const void* k1, const void* k2) {
  return (k1 == k2);
}
#define JEMALLOC_CTL_C_

#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/extent_dss.h"
#include "jemalloc/internal/extent_mmap.h"

#include "jemalloc/internal/nstime.h"

/******************************************************************************/
/* Data. */

/*
 * ctl_mtx protects the following:
 * - ctl_stats->*
 */
static malloc_mutex_t ctl_mtx;
static bool ctl_initialized;
static ctl_stats_t* ctl_stats;
static ctl_arenas_t* ctl_arenas;

/******************************************************************************/
/* Helpers for named and indexed nodes. */

static const ctl_named_node_t* ctl_named_node(const ctl_node_t* node) {
  return ((node->named) ? (const ctl_named_node_t*)node : NULL);
}

static const ctl_named_node_t* ctl_named_children(
  const ctl_named_node_t* node, size_t index) {
  const ctl_named_node_t* children = ctl_named_node(node->children);

  return (children ? &children[index] : NULL);
}

static const ctl_indexed_node_t* ctl_indexed_node(const ctl_node_t* node) {
  return (!node->named ? (const ctl_indexed_node_t*)node : NULL);
}

/******************************************************************************/
/* Function prototypes for non-inline static functions. */

#define CTL_PROTO(n)                                                       \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen);

#define INDEX_PROTO(n)                                                     \
  static const ctl_named_node_t* n##_index(                                \
    tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i);

CTL_PROTO(version)
CTL_PROTO(epoch)
CTL_PROTO(background_thread)
CTL_PROTO(max_background_threads)
CTL_PROTO(thread_tcache_enabled)
CTL_PROTO(thread_tcache_flush)
CTL_PROTO(thread_prof_name)
CTL_PROTO(thread_prof_active)
CTL_PROTO(thread_arena)
CTL_PROTO(thread_allocated)
CTL_PROTO(thread_allocatedp)
CTL_PROTO(thread_deallocated)
CTL_PROTO(thread_deallocatedp)
CTL_PROTO(config_cache_oblivious)
CTL_PROTO(config_debug)
CTL_PROTO(config_fill)
CTL_PROTO(config_lazy_lock)
CTL_PROTO(config_malloc_conf)
CTL_PROTO(config_opt_safety_checks)
CTL_PROTO(config_prof)
CTL_PROTO(config_prof_libgcc)
CTL_PROTO(config_prof_libunwind)
CTL_PROTO(config_stats)
CTL_PROTO(config_utrace)
CTL_PROTO(config_xmalloc)
CTL_PROTO(opt_abort)
CTL_PROTO(opt_abort_conf)
CTL_PROTO(opt_confirm_conf)
CTL_PROTO(opt_metadata_thp)
CTL_PROTO(opt_retain)
CTL_PROTO(opt_dss)
CTL_PROTO(opt_narenas)
CTL_PROTO(opt_percpu_arena)
CTL_PROTO(opt_oversize_threshold)
CTL_PROTO(opt_background_thread)
CTL_PROTO(opt_max_background_threads)
CTL_PROTO(opt_dirty_decay_ms)
CTL_PROTO(opt_muzzy_decay_ms)
CTL_PROTO(opt_stats_print)
CTL_PROTO(opt_stats_print_opts)
CTL_PROTO(opt_junk)
CTL_PROTO(opt_zero)
CTL_PROTO(opt_utrace)
CTL_PROTO(opt_xmalloc)
CTL_PROTO(opt_tcache)
CTL_PROTO(opt_thp)
CTL_PROTO(opt_lg_extent_max_active_fit)
CTL_PROTO(opt_lg_tcache_max)
CTL_PROTO(opt_prof)
CTL_PROTO(opt_prof_prefix)
CTL_PROTO(opt_prof_active)
CTL_PROTO(opt_prof_thread_active_init)
CTL_PROTO(opt_lg_prof_sample)
CTL_PROTO(opt_lg_prof_interval)
CTL_PROTO(opt_prof_gdump)
CTL_PROTO(opt_prof_final)
CTL_PROTO(opt_prof_leak)
CTL_PROTO(opt_prof_accum)
CTL_PROTO(tcache_create)
CTL_PROTO(tcache_flush)
CTL_PROTO(tcache_destroy)
CTL_PROTO(arena_i_initialized)
CTL_PROTO(arena_i_decay)
CTL_PROTO(arena_i_purge)
CTL_PROTO(arena_i_reset)
CTL_PROTO(arena_i_destroy)
CTL_PROTO(arena_i_dss)
CTL_PROTO(arena_i_dirty_decay_ms)
CTL_PROTO(arena_i_muzzy_decay_ms)
CTL_PROTO(arena_i_extent_hooks)
CTL_PROTO(arena_i_retain_grow_limit)
INDEX_PROTO(arena_i)
CTL_PROTO(arenas_bin_i_size)
CTL_PROTO(arenas_bin_i_nregs)
CTL_PROTO(arenas_bin_i_slab_size)
CTL_PROTO(arenas_bin_i_nshards)
INDEX_PROTO(arenas_bin_i)
CTL_PROTO(arenas_lextent_i_size)
INDEX_PROTO(arenas_lextent_i)
CTL_PROTO(arenas_narenas)
CTL_PROTO(arenas_dirty_decay_ms)
CTL_PROTO(arenas_muzzy_decay_ms)
CTL_PROTO(arenas_quantum)
CTL_PROTO(arenas_page)
CTL_PROTO(arenas_tcache_max)
CTL_PROTO(arenas_nbins)
CTL_PROTO(arenas_nhbins)
CTL_PROTO(arenas_nlextents)
CTL_PROTO(arenas_create)
CTL_PROTO(arenas_lookup)
CTL_PROTO(prof_thread_active_init)
CTL_PROTO(prof_active)
CTL_PROTO(prof_dump)
CTL_PROTO(prof_gdump)
CTL_PROTO(prof_reset)
CTL_PROTO(prof_interval)
CTL_PROTO(lg_prof_sample)
CTL_PROTO(prof_log_start)
CTL_PROTO(prof_log_stop)
CTL_PROTO(stats_arenas_i_small_allocated)
CTL_PROTO(stats_arenas_i_small_nmalloc)
CTL_PROTO(stats_arenas_i_small_ndalloc)
CTL_PROTO(stats_arenas_i_small_nrequests)
CTL_PROTO(stats_arenas_i_small_nfills)
CTL_PROTO(stats_arenas_i_small_nflushes)
CTL_PROTO(stats_arenas_i_large_allocated)
CTL_PROTO(stats_arenas_i_large_nmalloc)
CTL_PROTO(stats_arenas_i_large_ndalloc)
CTL_PROTO(stats_arenas_i_large_nrequests)
CTL_PROTO(stats_arenas_i_large_nfills)
CTL_PROTO(stats_arenas_i_large_nflushes)
CTL_PROTO(stats_arenas_i_bins_j_nmalloc)
CTL_PROTO(stats_arenas_i_bins_j_ndalloc)
CTL_PROTO(stats_arenas_i_bins_j_nrequests)
CTL_PROTO(stats_arenas_i_bins_j_curregs)
CTL_PROTO(stats_arenas_i_bins_j_nfills)
CTL_PROTO(stats_arenas_i_bins_j_nflushes)
CTL_PROTO(stats_arenas_i_bins_j_nslabs)
CTL_PROTO(stats_arenas_i_bins_j_nreslabs)
CTL_PROTO(stats_arenas_i_bins_j_curslabs)
CTL_PROTO(stats_arenas_i_bins_j_nonfull_slabs)
INDEX_PROTO(stats_arenas_i_bins_j)
CTL_PROTO(stats_arenas_i_lextents_j_nmalloc)
CTL_PROTO(stats_arenas_i_lextents_j_ndalloc)
CTL_PROTO(stats_arenas_i_lextents_j_nrequests)
CTL_PROTO(stats_arenas_i_lextents_j_curlextents)
INDEX_PROTO(stats_arenas_i_lextents_j)
CTL_PROTO(stats_arenas_i_extents_j_ndirty)
CTL_PROTO(stats_arenas_i_extents_j_nmuzzy)
CTL_PROTO(stats_arenas_i_extents_j_nretained)
CTL_PROTO(stats_arenas_i_extents_j_dirty_bytes)
CTL_PROTO(stats_arenas_i_extents_j_muzzy_bytes)
CTL_PROTO(stats_arenas_i_extents_j_retained_bytes)
INDEX_PROTO(stats_arenas_i_extents_j)
CTL_PROTO(stats_arenas_i_nthreads)
CTL_PROTO(stats_arenas_i_uptime)
CTL_PROTO(stats_arenas_i_dss)
CTL_PROTO(stats_arenas_i_dirty_decay_ms)
CTL_PROTO(stats_arenas_i_muzzy_decay_ms)
CTL_PROTO(stats_arenas_i_pactive)
CTL_PROTO(stats_arenas_i_pdirty)
CTL_PROTO(stats_arenas_i_pmuzzy)
CTL_PROTO(stats_arenas_i_mapped)
CTL_PROTO(stats_arenas_i_retained)
CTL_PROTO(stats_arenas_i_extent_avail)
CTL_PROTO(stats_arenas_i_dirty_npurge)
CTL_PROTO(stats_arenas_i_dirty_nmadvise)
CTL_PROTO(stats_arenas_i_dirty_purged)
CTL_PROTO(stats_arenas_i_muzzy_npurge)
CTL_PROTO(stats_arenas_i_muzzy_nmadvise)
CTL_PROTO(stats_arenas_i_muzzy_purged)
CTL_PROTO(stats_arenas_i_base)
CTL_PROTO(stats_arenas_i_internal)
CTL_PROTO(stats_arenas_i_metadata_thp)
CTL_PROTO(stats_arenas_i_tcache_bytes)
CTL_PROTO(stats_arenas_i_resident)
CTL_PROTO(stats_arenas_i_abandoned_vm)
INDEX_PROTO(stats_arenas_i)
CTL_PROTO(stats_allocated)
CTL_PROTO(stats_active)
CTL_PROTO(stats_background_thread_num_threads)
CTL_PROTO(stats_background_thread_num_runs)
CTL_PROTO(stats_background_thread_run_interval)
CTL_PROTO(stats_metadata)
CTL_PROTO(stats_metadata_thp)
CTL_PROTO(stats_resident)
CTL_PROTO(stats_mapped)
CTL_PROTO(stats_retained)
CTL_PROTO(experimental_hooks_install)
CTL_PROTO(experimental_hooks_remove)
CTL_PROTO(experimental_utilization_query)
CTL_PROTO(experimental_utilization_batch_query)
CTL_PROTO(experimental_arenas_i_pactivep)
INDEX_PROTO(experimental_arenas_i)

#define MUTEX_STATS_CTL_PROTO_GEN(n)                                       \
  CTL_PROTO(stats_##n##_num_ops)                                           \
  CTL_PROTO(stats_##n##_num_wait)                                          \
  CTL_PROTO(stats_##n##_num_spin_acq)                                      \
  CTL_PROTO(stats_##n##_num_owner_switch)                                  \
  CTL_PROTO(stats_##n##_total_wait_time)                                   \
  CTL_PROTO(stats_##n##_max_wait_time)                                     \
  CTL_PROTO(stats_##n##_max_num_thds)

/* Global mutexes. */
#define OP(mtx) MUTEX_STATS_CTL_PROTO_GEN(mutexes_##mtx)
MUTEX_PROF_GLOBAL_MUTEXES
#undef OP

/* Per arena mutexes. */
#define OP(mtx) MUTEX_STATS_CTL_PROTO_GEN(arenas_i_mutexes_##mtx)
MUTEX_PROF_ARENA_MUTEXES
#undef OP

/* Arena bin mutexes. */
MUTEX_STATS_CTL_PROTO_GEN(arenas_i_bins_j_mutex)
#undef MUTEX_STATS_CTL_PROTO_GEN

CTL_PROTO(stats_mutexes_reset)

/******************************************************************************/
/* mallctl tree. */

#define NAME(n) { true }, n
#define CHILD(t, c)                                                        \
  sizeof(c##_node) / sizeof(ctl_##t##_node_t), (ctl_node_t*)c##_node, NULL
#define CTL(c) 0, NULL, c##_ctl

/*
 * Only handles internal indexed nodes, since there are currently no
 * external ones.
 */
#define INDEX(i) { false }, i##_index

static const ctl_named_node_t thread_tcache_node[]
  = { { NAME("enabled"), CTL(thread_tcache_enabled) },
      { NAME("flush"), CTL(thread_tcache_flush) } };

static const ctl_named_node_t thread_prof_node[]
  = { { NAME("name"), CTL(thread_prof_name) },
      { NAME("active"), CTL(thread_prof_active) } };

static const ctl_named_node_t thread_node[]
  = { { NAME("arena"), CTL(thread_arena) },
      { NAME("allocated"), CTL(thread_allocated) },
      { NAME("allocatedp"), CTL(thread_allocatedp) },
      { NAME("deallocated"), CTL(thread_deallocated) },
      { NAME("deallocatedp"), CTL(thread_deallocatedp) },
      { NAME("tcache"), CHILD(named, thread_tcache) },
      { NAME("prof"), CHILD(named, thread_prof) } };

static const ctl_named_node_t config_node[]
  = { { NAME("cache_oblivious"), CTL(config_cache_oblivious) },
      { NAME("debug"), CTL(config_debug) },
      { NAME("fill"), CTL(config_fill) },
      { NAME("lazy_lock"), CTL(config_lazy_lock) },
      { NAME("malloc_conf"), CTL(config_malloc_conf) },
      { NAME("opt_safety_checks"), CTL(config_opt_safety_checks) },
      { NAME("prof"), CTL(config_prof) },
      { NAME("prof_libgcc"), CTL(config_prof_libgcc) },
      { NAME("prof_libunwind"), CTL(config_prof_libunwind) },
      { NAME("stats"), CTL(config_stats) },
      { NAME("utrace"), CTL(config_utrace) },
      { NAME("xmalloc"), CTL(config_xmalloc) } };

static const ctl_named_node_t opt_node[] = { { NAME("abort"),
                                               CTL(opt_abort) },
  { NAME("abort_conf"), CTL(opt_abort_conf) },
  { NAME("confirm_conf"), CTL(opt_confirm_conf) },
  { NAME("metadata_thp"), CTL(opt_metadata_thp) },
  { NAME("retain"), CTL(opt_retain) }, { NAME("dss"), CTL(opt_dss) },
  { NAME("narenas"), CTL(opt_narenas) },
  { NAME("percpu_arena"), CTL(opt_percpu_arena) },
  { NAME("oversize_threshold"), CTL(opt_oversize_threshold) },
  { NAME("background_thread"), CTL(opt_background_thread) },
  { NAME("max_background_threads"), CTL(opt_max_background_threads) },
  { NAME("dirty_decay_ms"), CTL(opt_dirty_decay_ms) },
  { NAME("muzzy_decay_ms"), CTL(opt_muzzy_decay_ms) },
  { NAME("stats_print"), CTL(opt_stats_print) },
  { NAME("stats_print_opts"), CTL(opt_stats_print_opts) },
  { NAME("junk"), CTL(opt_junk) }, { NAME("zero"), CTL(opt_zero) },
  { NAME("utrace"), CTL(opt_utrace) },
  { NAME("xmalloc"), CTL(opt_xmalloc) },
  { NAME("tcache"), CTL(opt_tcache) }, { NAME("thp"), CTL(opt_thp) },
  { NAME("lg_extent_max_active_fit"), CTL(opt_lg_extent_max_active_fit) },
  { NAME("lg_tcache_max"), CTL(opt_lg_tcache_max) },
  { NAME("prof"), CTL(opt_prof) },
  { NAME("prof_prefix"), CTL(opt_prof_prefix) },
  { NAME("prof_active"), CTL(opt_prof_active) },
  { NAME("prof_thread_active_init"), CTL(opt_prof_thread_active_init) },
  { NAME("lg_prof_sample"), CTL(opt_lg_prof_sample) },
  { NAME("lg_prof_interval"), CTL(opt_lg_prof_interval) },
  { NAME("prof_gdump"), CTL(opt_prof_gdump) },
  { NAME("prof_final"), CTL(opt_prof_final) },
  { NAME("prof_leak"), CTL(opt_prof_leak) },
  { NAME("prof_accum"), CTL(opt_prof_accum) } };

static const ctl_named_node_t tcache_node[]
  = { { NAME("create"), CTL(tcache_create) },
      { NAME("flush"), CTL(tcache_flush) },
      { NAME("destroy"), CTL(tcache_destroy) } };

static const ctl_named_node_t arena_i_node[]
  = { { NAME("initialized"), CTL(arena_i_initialized) },
      { NAME("decay"), CTL(arena_i_decay) },
      { NAME("purge"), CTL(arena_i_purge) },
      { NAME("reset"), CTL(arena_i_reset) },
      { NAME("destroy"), CTL(arena_i_destroy) },
      { NAME("dss"), CTL(arena_i_dss) },
      { NAME("dirty_decay_ms"), CTL(arena_i_dirty_decay_ms) },
      { NAME("muzzy_decay_ms"), CTL(arena_i_muzzy_decay_ms) },
      { NAME("extent_hooks"), CTL(arena_i_extent_hooks) },
      { NAME("retain_grow_limit"), CTL(arena_i_retain_grow_limit) } };
static const ctl_named_node_t super_arena_i_node[]
  = { { NAME(""), CHILD(named, arena_i) } };

static const ctl_indexed_node_t arena_node[] = { { INDEX(arena_i) } };

static const ctl_named_node_t arenas_bin_i_node[]
  = { { NAME("size"), CTL(arenas_bin_i_size) },
      { NAME("nregs"), CTL(arenas_bin_i_nregs) },
      { NAME("slab_size"), CTL(arenas_bin_i_slab_size) },
      { NAME("nshards"), CTL(arenas_bin_i_nshards) } };
static const ctl_named_node_t super_arenas_bin_i_node[]
  = { { NAME(""), CHILD(named, arenas_bin_i) } };

static const ctl_indexed_node_t arenas_bin_node[]
  = { { INDEX(arenas_bin_i) } };

static const ctl_named_node_t arenas_lextent_i_node[]
  = { { NAME("size"), CTL(arenas_lextent_i_size) } };
static const ctl_named_node_t super_arenas_lextent_i_node[]
  = { { NAME(""), CHILD(named, arenas_lextent_i) } };

static const ctl_indexed_node_t arenas_lextent_node[]
  = { { INDEX(arenas_lextent_i) } };

static const ctl_named_node_t arenas_node[]
  = { { NAME("narenas"), CTL(arenas_narenas) },
      { NAME("dirty_decay_ms"), CTL(arenas_dirty_decay_ms) },
      { NAME("muzzy_decay_ms"), CTL(arenas_muzzy_decay_ms) },
      { NAME("quantum"), CTL(arenas_quantum) },
      { NAME("page"), CTL(arenas_page) },
      { NAME("tcache_max"), CTL(arenas_tcache_max) },
      { NAME("nbins"), CTL(arenas_nbins) },
      { NAME("nhbins"), CTL(arenas_nhbins) },
      { NAME("bin"), CHILD(indexed, arenas_bin) },
      { NAME("nlextents"), CTL(arenas_nlextents) },
      { NAME("lextent"), CHILD(indexed, arenas_lextent) },
      { NAME("create"), CTL(arenas_create) },
      { NAME("lookup"), CTL(arenas_lookup) } };

static const ctl_named_node_t prof_node[]
  = { { NAME("thread_active_init"), CTL(prof_thread_active_init) },
      { NAME("active"), CTL(prof_active) },
      { NAME("dump"), CTL(prof_dump) }, { NAME("gdump"), CTL(prof_gdump) },
      { NAME("reset"), CTL(prof_reset) },
      { NAME("interval"), CTL(prof_interval) },
      { NAME("lg_sample"), CTL(lg_prof_sample) },
      { NAME("log_start"), CTL(prof_log_start) },
      { NAME("log_stop"), CTL(prof_log_stop) } };
static const ctl_named_node_t stats_arenas_i_small_node[]
  = { { NAME("allocated"), CTL(stats_arenas_i_small_allocated) },
      { NAME("nmalloc"), CTL(stats_arenas_i_small_nmalloc) },
      { NAME("ndalloc"), CTL(stats_arenas_i_small_ndalloc) },
      { NAME("nrequests"), CTL(stats_arenas_i_small_nrequests) },
      { NAME("nfills"), CTL(stats_arenas_i_small_nfills) },
      { NAME("nflushes"), CTL(stats_arenas_i_small_nflushes) } };

static const ctl_named_node_t stats_arenas_i_large_node[]
  = { { NAME("allocated"), CTL(stats_arenas_i_large_allocated) },
      { NAME("nmalloc"), CTL(stats_arenas_i_large_nmalloc) },
      { NAME("ndalloc"), CTL(stats_arenas_i_large_ndalloc) },
      { NAME("nrequests"), CTL(stats_arenas_i_large_nrequests) },
      { NAME("nfills"), CTL(stats_arenas_i_large_nfills) },
      { NAME("nflushes"), CTL(stats_arenas_i_large_nflushes) } };

#define MUTEX_PROF_DATA_NODE(prefix)                                       \
  static const ctl_named_node_t stats_##prefix##_node[] = {                \
    { NAME("num_ops"), CTL(stats_##prefix##_num_ops) },                    \
    { NAME("num_wait"), CTL(stats_##prefix##_num_wait) },                  \
    { NAME("num_spin_acq"), CTL(stats_##prefix##_num_spin_acq) },          \
    { NAME("num_owner_switch"), CTL(stats_##prefix##_num_owner_switch) },  \
    { NAME("total_wait_time"), CTL(stats_##prefix##_total_wait_time) },    \
    { NAME("max_wait_time"), CTL(stats_##prefix##_max_wait_time) },        \
    { NAME("max_num_thds"),                                                \
      CTL(stats_##prefix##_max_num_thds) } /* Note that # of current       \
                                              waiting thread not           \
                                              provided. */                 \
  };

MUTEX_PROF_DATA_NODE(arenas_i_bins_j_mutex)

static const ctl_named_node_t stats_arenas_i_bins_j_node[]
  = { { NAME("nmalloc"), CTL(stats_arenas_i_bins_j_nmalloc) },
      { NAME("ndalloc"), CTL(stats_arenas_i_bins_j_ndalloc) },
      { NAME("nrequests"), CTL(stats_arenas_i_bins_j_nrequests) },
      { NAME("curregs"), CTL(stats_arenas_i_bins_j_curregs) },
      { NAME("nfills"), CTL(stats_arenas_i_bins_j_nfills) },
      { NAME("nflushes"), CTL(stats_arenas_i_bins_j_nflushes) },
      { NAME("nslabs"), CTL(stats_arenas_i_bins_j_nslabs) },
      { NAME("nreslabs"), CTL(stats_arenas_i_bins_j_nreslabs) },
      { NAME("curslabs"), CTL(stats_arenas_i_bins_j_curslabs) },
      { NAME("nonfull_slabs"), CTL(stats_arenas_i_bins_j_nonfull_slabs) },
      { NAME("mutex"), CHILD(named, stats_arenas_i_bins_j_mutex) } };

static const ctl_named_node_t super_stats_arenas_i_bins_j_node[]
  = { { NAME(""), CHILD(named, stats_arenas_i_bins_j) } };

static const ctl_indexed_node_t stats_arenas_i_bins_node[]
  = { { INDEX(stats_arenas_i_bins_j) } };

static const ctl_named_node_t stats_arenas_i_lextents_j_node[]
  = { { NAME("nmalloc"), CTL(stats_arenas_i_lextents_j_nmalloc) },
      { NAME("ndalloc"), CTL(stats_arenas_i_lextents_j_ndalloc) },
      { NAME("nrequests"), CTL(stats_arenas_i_lextents_j_nrequests) },
      { NAME("curlextents"), CTL(stats_arenas_i_lextents_j_curlextents) } };
static const ctl_named_node_t super_stats_arenas_i_lextents_j_node[]
  = { { NAME(""), CHILD(named, stats_arenas_i_lextents_j) } };

static const ctl_indexed_node_t stats_arenas_i_lextents_node[]
  = { { INDEX(stats_arenas_i_lextents_j) } };

static const ctl_named_node_t stats_arenas_i_extents_j_node[] = {
  { NAME("ndirty"), CTL(stats_arenas_i_extents_j_ndirty) },
  { NAME("nmuzzy"), CTL(stats_arenas_i_extents_j_nmuzzy) },
  { NAME("nretained"), CTL(stats_arenas_i_extents_j_nretained) },
  { NAME("dirty_bytes"), CTL(stats_arenas_i_extents_j_dirty_bytes) },
  { NAME("muzzy_bytes"), CTL(stats_arenas_i_extents_j_muzzy_bytes) },
  { NAME("retained_bytes"), CTL(stats_arenas_i_extents_j_retained_bytes) }
};

static const ctl_named_node_t super_stats_arenas_i_extents_j_node[]
  = { { NAME(""), CHILD(named, stats_arenas_i_extents_j) } };

static const ctl_indexed_node_t stats_arenas_i_extents_node[]
  = { { INDEX(stats_arenas_i_extents_j) } };

#define OP(mtx) MUTEX_PROF_DATA_NODE(arenas_i_mutexes_##mtx)
MUTEX_PROF_ARENA_MUTEXES
#undef OP

static const ctl_named_node_t stats_arenas_i_mutexes_node[] = {
#define OP(mtx) { NAME(#mtx), CHILD(named, stats_arenas_i_mutexes_##mtx) },
  MUTEX_PROF_ARENA_MUTEXES
#undef OP
};

static const ctl_named_node_t stats_arenas_i_node[]
  = { { NAME("nthreads"), CTL(stats_arenas_i_nthreads) },
      { NAME("uptime"), CTL(stats_arenas_i_uptime) },
      { NAME("dss"), CTL(stats_arenas_i_dss) },
      { NAME("dirty_decay_ms"), CTL(stats_arenas_i_dirty_decay_ms) },
      { NAME("muzzy_decay_ms"), CTL(stats_arenas_i_muzzy_decay_ms) },
      { NAME("pactive"), CTL(stats_arenas_i_pactive) },
      { NAME("pdirty"), CTL(stats_arenas_i_pdirty) },
      { NAME("pmuzzy"), CTL(stats_arenas_i_pmuzzy) },
      { NAME("mapped"), CTL(stats_arenas_i_mapped) },
      { NAME("retained"), CTL(stats_arenas_i_retained) },
      { NAME("extent_avail"), CTL(stats_arenas_i_extent_avail) },
      { NAME("dirty_npurge"), CTL(stats_arenas_i_dirty_npurge) },
      { NAME("dirty_nmadvise"), CTL(stats_arenas_i_dirty_nmadvise) },
      { NAME("dirty_purged"), CTL(stats_arenas_i_dirty_purged) },
      { NAME("muzzy_npurge"), CTL(stats_arenas_i_muzzy_npurge) },
      { NAME("muzzy_nmadvise"), CTL(stats_arenas_i_muzzy_nmadvise) },
      { NAME("muzzy_purged"), CTL(stats_arenas_i_muzzy_purged) },
      { NAME("base"), CTL(stats_arenas_i_base) },
      { NAME("internal"), CTL(stats_arenas_i_internal) },
      { NAME("metadata_thp"), CTL(stats_arenas_i_metadata_thp) },
      { NAME("tcache_bytes"), CTL(stats_arenas_i_tcache_bytes) },
      { NAME("resident"), CTL(stats_arenas_i_resident) },
      { NAME("abandoned_vm"), CTL(stats_arenas_i_abandoned_vm) },
      { NAME("small"), CHILD(named, stats_arenas_i_small) },
      { NAME("large"), CHILD(named, stats_arenas_i_large) },
      { NAME("bins"), CHILD(indexed, stats_arenas_i_bins) },
      { NAME("lextents"), CHILD(indexed, stats_arenas_i_lextents) },
      { NAME("extents"), CHILD(indexed, stats_arenas_i_extents) },
      { NAME("mutexes"), CHILD(named, stats_arenas_i_mutexes) } };
static const ctl_named_node_t super_stats_arenas_i_node[]
  = { { NAME(""), CHILD(named, stats_arenas_i) } };

static const ctl_indexed_node_t stats_arenas_node[]
  = { { INDEX(stats_arenas_i) } };

static const ctl_named_node_t stats_background_thread_node[]
  = { { NAME("num_threads"), CTL(stats_background_thread_num_threads) },
      { NAME("num_runs"), CTL(stats_background_thread_num_runs) },
      { NAME("run_interval"), CTL(stats_background_thread_run_interval) } };

#define OP(mtx) MUTEX_PROF_DATA_NODE(mutexes_##mtx)
MUTEX_PROF_GLOBAL_MUTEXES
#undef OP

static const ctl_named_node_t stats_mutexes_node[] = {
#define OP(mtx) { NAME(#mtx), CHILD(named, stats_mutexes_##mtx) },
  MUTEX_PROF_GLOBAL_MUTEXES
#undef OP
  { NAME("reset"), CTL(stats_mutexes_reset) }
};
#undef MUTEX_PROF_DATA_NODE

static const ctl_named_node_t stats_node[]
  = { { NAME("allocated"), CTL(stats_allocated) },
      { NAME("active"), CTL(stats_active) },
      { NAME("metadata"), CTL(stats_metadata) },
      { NAME("metadata_thp"), CTL(stats_metadata_thp) },
      { NAME("resident"), CTL(stats_resident) },
      { NAME("mapped"), CTL(stats_mapped) },
      { NAME("retained"), CTL(stats_retained) },
      { NAME("background_thread"), CHILD(named, stats_background_thread) },
      { NAME("mutexes"), CHILD(named, stats_mutexes) },
      { NAME("arenas"), CHILD(indexed, stats_arenas) } };

static const ctl_named_node_t experimental_hooks_node[]
  = { { NAME("install"), CTL(experimental_hooks_install) },
      { NAME("remove"), CTL(experimental_hooks_remove) } };

static const ctl_named_node_t experimental_utilization_node[]
  = { { NAME("query"), CTL(experimental_utilization_query) },
      { NAME("batch_query"), CTL(experimental_utilization_batch_query) } };

static const ctl_named_node_t experimental_arenas_i_node[]
  = { { NAME("pactivep"), CTL(experimental_arenas_i_pactivep) } };
static const ctl_named_node_t super_experimental_arenas_i_node[]
  = { { NAME(""), CHILD(named, experimental_arenas_i) } };

static const ctl_indexed_node_t experimental_arenas_node[]
  = { { INDEX(experimental_arenas_i) } };

static const ctl_named_node_t experimental_node[]
  = { { NAME("hooks"), CHILD(named, experimental_hooks) },
      { NAME("utilization"), CHILD(named, experimental_utilization) },
      { NAME("arenas"), CHILD(indexed, experimental_arenas) } };

static const ctl_named_node_t root_node[]
  = { { NAME("version"), CTL(version) }, { NAME("epoch"), CTL(epoch) },
      { NAME("background_thread"), CTL(background_thread) },
      { NAME("max_background_threads"), CTL(max_background_threads) },
      { NAME("thread"), CHILD(named, thread) },
      { NAME("config"), CHILD(named, config) },
      { NAME("opt"), CHILD(named, opt) },
      { NAME("tcache"), CHILD(named, tcache) },
      { NAME("arena"), CHILD(indexed, arena) },
      { NAME("arenas"), CHILD(named, arenas) },
      { NAME("prof"), CHILD(named, prof) },
      { NAME("stats"), CHILD(named, stats) },
      { NAME("experimental"), CHILD(named, experimental) } };
static const ctl_named_node_t super_root_node[]
  = { { NAME(""), CHILD(named, root) } };

#undef NAME
#undef CHILD
#undef CTL
#undef INDEX

/******************************************************************************/

/*
 * Sets *dst + *src non-atomically.  This is safe, since everything is
 * synchronized by the ctl mutex.
 */
static void ctl_accum_arena_stats_u64(
  arena_stats_u64_t* dst, arena_stats_u64_t* src) {
#ifdef JEMALLOC_ATOMIC_U64
  uint64_t cur_dst = atomic_load_u64(dst, ATOMIC_RELAXED);
  uint64_t cur_src = atomic_load_u64(src, ATOMIC_RELAXED);
  atomic_store_u64(dst, cur_dst + cur_src, ATOMIC_RELAXED);
#else
  *dst += *src;
#endif
}

/* Likewise: with ctl mutex synchronization, reading is simple. */
static uint64_t ctl_arena_stats_read_u64(arena_stats_u64_t* p) {
#ifdef JEMALLOC_ATOMIC_U64
  return atomic_load_u64(p, ATOMIC_RELAXED);
#else
  return *p;
#endif
}

static void accum_atomic_zu(atomic_zu_t* dst, atomic_zu_t* src) {
  size_t cur_dst = atomic_load_zu(dst, ATOMIC_RELAXED);
  size_t cur_src = atomic_load_zu(src, ATOMIC_RELAXED);
  atomic_store_zu(dst, cur_dst + cur_src, ATOMIC_RELAXED);
}

/******************************************************************************/

static unsigned arenas_i2a_impl(size_t i, bool compat, bool validate) {
  unsigned a;

  switch (i) {
  case MALLCTL_ARENAS_ALL: a = 0; break;
  case MALLCTL_ARENAS_DESTROYED: a = 1; break;
  default:
    if (compat && i == ctl_arenas->narenas) {
      /*
       * Provide deprecated backward compatibility for
       * accessing the merged stats at index narenas rather
       * than via MALLCTL_ARENAS_ALL.  This is scheduled for
       * removal in 6.0.0.
       */
      a = 0;
    } else if (validate && i >= ctl_arenas->narenas) {
      a = UINT_MAX;
    } else {
      /*
       * This function should never be called for an index
       * more than one past the range of indices that have
       * initialized ctl data.
       */
      assert(
        i < ctl_arenas->narenas || (!validate && i == ctl_arenas->narenas));
      a = (unsigned)i + 2;
    }
    break;
  }

  return a;
}

static unsigned arenas_i2a(size_t i) {
  return arenas_i2a_impl(i, true, false);
}

static ctl_arena_t* arenas_i_impl(
  tsd_t* tsd, size_t i, bool compat, bool init) {
  ctl_arena_t* ret;

  assert(!compat || !init);

  ret = ctl_arenas->arenas[arenas_i2a_impl(i, compat, false)];
  if (init && ret == NULL) {
    if (config_stats) {
      struct container_s {
        ctl_arena_t ctl_arena;
        ctl_arena_stats_t astats;
      };
      struct container_s* cont = (struct container_s*)base_alloc(
        tsd_tsdn(tsd), b0get(), sizeof(struct container_s), QUANTUM);
      if (cont == NULL) { return NULL; }
      ret = &cont->ctl_arena;
      ret->astats = &cont->astats;
    } else {
      ret = (ctl_arena_t*)base_alloc(
        tsd_tsdn(tsd), b0get(), sizeof(ctl_arena_t), QUANTUM);
      if (ret == NULL) { return NULL; }
    }
    ret->arena_ind = (unsigned)i;
    ctl_arenas->arenas[arenas_i2a_impl(i, compat, false)] = ret;
  }

  assert(ret == NULL || arenas_i2a(ret->arena_ind) == arenas_i2a(i));
  return ret;
}

static ctl_arena_t* arenas_i(size_t i) {
  ctl_arena_t* ret = arenas_i_impl(tsd_fetch(), i, true, false);
  assert(ret != NULL);
  return ret;
}

static void ctl_arena_clear(ctl_arena_t* ctl_arena) {
  ctl_arena->nthreads = 0;
  ctl_arena->dss = dss_prec_names[dss_prec_limit];
  ctl_arena->dirty_decay_ms = -1;
  ctl_arena->muzzy_decay_ms = -1;
  ctl_arena->pactive = 0;
  ctl_arena->pdirty = 0;
  ctl_arena->pmuzzy = 0;
  if (config_stats) {
    memset(&ctl_arena->astats->astats, 0, sizeof(arena_stats_t));
    ctl_arena->astats->allocated_small = 0;
    ctl_arena->astats->nmalloc_small = 0;
    ctl_arena->astats->ndalloc_small = 0;
    ctl_arena->astats->nrequests_small = 0;
    ctl_arena->astats->nfills_small = 0;
    ctl_arena->astats->nflushes_small = 0;
    memset(ctl_arena->astats->bstats, 0, SC_NBINS * sizeof(bin_stats_t));
    memset(ctl_arena->astats->lstats, 0,
      (SC_NSIZES - SC_NBINS) * sizeof(arena_stats_large_t));
    memset(ctl_arena->astats->estats, 0,
      SC_NPSIZES * sizeof(arena_stats_extents_t));
  }
}

static void ctl_arena_stats_amerge(
  tsdn_t* tsdn, ctl_arena_t* ctl_arena, arena_t* arena) {
  unsigned i;

  if (config_stats) {
    arena_stats_merge(tsdn, arena, &ctl_arena->nthreads, &ctl_arena->dss,
      &ctl_arena->dirty_decay_ms, &ctl_arena->muzzy_decay_ms,
      &ctl_arena->pactive, &ctl_arena->pdirty, &ctl_arena->pmuzzy,
      &ctl_arena->astats->astats, ctl_arena->astats->bstats,
      ctl_arena->astats->lstats, ctl_arena->astats->estats);

    for (i = 0; i < SC_NBINS; i++) {
      ctl_arena->astats->allocated_small
        += ctl_arena->astats->bstats[i].curregs * sz_index2size(i);
      ctl_arena->astats->nmalloc_small
        += ctl_arena->astats->bstats[i].nmalloc;
      ctl_arena->astats->ndalloc_small
        += ctl_arena->astats->bstats[i].ndalloc;
      ctl_arena->astats->nrequests_small
        += ctl_arena->astats->bstats[i].nrequests;
      ctl_arena->astats->nfills_small
        += ctl_arena->astats->bstats[i].nfills;
      ctl_arena->astats->nflushes_small
        += ctl_arena->astats->bstats[i].nflushes;
    }
  } else {
    arena_basic_stats_merge(tsdn, arena, &ctl_arena->nthreads,
      &ctl_arena->dss, &ctl_arena->dirty_decay_ms,
      &ctl_arena->muzzy_decay_ms, &ctl_arena->pactive, &ctl_arena->pdirty,
      &ctl_arena->pmuzzy);
  }
}

static void ctl_arena_stats_sdmerge(
  ctl_arena_t* ctl_sdarena, ctl_arena_t* ctl_arena, bool destroyed) {
  unsigned i;

  if (!destroyed) {
    ctl_sdarena->nthreads += ctl_arena->nthreads;
    ctl_sdarena->pactive += ctl_arena->pactive;
    ctl_sdarena->pdirty += ctl_arena->pdirty;
    ctl_sdarena->pmuzzy += ctl_arena->pmuzzy;
  } else {
    assert(ctl_arena->nthreads == 0);
    assert(ctl_arena->pactive == 0);
    assert(ctl_arena->pdirty == 0);
    assert(ctl_arena->pmuzzy == 0);
  }

  if (config_stats) {
    ctl_arena_stats_t* sdstats = ctl_sdarena->astats;
    ctl_arena_stats_t* astats = ctl_arena->astats;

    if (!destroyed) {
      accum_atomic_zu(&sdstats->astats.mapped, &astats->astats.mapped);
      accum_atomic_zu(&sdstats->astats.retained, &astats->astats.retained);
      accum_atomic_zu(
        &sdstats->astats.extent_avail, &astats->astats.extent_avail);
    }

    ctl_accum_arena_stats_u64(&sdstats->astats.decay_dirty.npurge,
      &astats->astats.decay_dirty.npurge);
    ctl_accum_arena_stats_u64(&sdstats->astats.decay_dirty.nmadvise,
      &astats->astats.decay_dirty.nmadvise);
    ctl_accum_arena_stats_u64(&sdstats->astats.decay_dirty.purged,
      &astats->astats.decay_dirty.purged);

    ctl_accum_arena_stats_u64(&sdstats->astats.decay_muzzy.npurge,
      &astats->astats.decay_muzzy.npurge);
    ctl_accum_arena_stats_u64(&sdstats->astats.decay_muzzy.nmadvise,
      &astats->astats.decay_muzzy.nmadvise);
    ctl_accum_arena_stats_u64(&sdstats->astats.decay_muzzy.purged,
      &astats->astats.decay_muzzy.purged);

#define OP(mtx)                                                            \
  malloc_mutex_prof_merge(                                                 \
    &(sdstats->astats.mutex_prof_data[arena_prof_mutex_##mtx]),            \
    &(astats->astats.mutex_prof_data[arena_prof_mutex_##mtx]));
    MUTEX_PROF_ARENA_MUTEXES
#undef OP
    if (!destroyed) {
      accum_atomic_zu(&sdstats->astats.base, &astats->astats.base);
      accum_atomic_zu(&sdstats->astats.internal, &astats->astats.internal);
      accum_atomic_zu(&sdstats->astats.resident, &astats->astats.resident);
      accum_atomic_zu(
        &sdstats->astats.metadata_thp, &astats->astats.metadata_thp);
    } else {
      assert(atomic_load_zu(&astats->astats.internal, ATOMIC_RELAXED) == 0);
    }

    if (!destroyed) {
      sdstats->allocated_small += astats->allocated_small;
    } else {
      assert(astats->allocated_small == 0);
    }
    sdstats->nmalloc_small += astats->nmalloc_small;
    sdstats->ndalloc_small += astats->ndalloc_small;
    sdstats->nrequests_small += astats->nrequests_small;
    sdstats->nfills_small += astats->nfills_small;
    sdstats->nflushes_small += astats->nflushes_small;

    if (!destroyed) {
      accum_atomic_zu(
        &sdstats->astats.allocated_large, &astats->astats.allocated_large);
    } else {
      assert(atomic_load_zu(&astats->astats.allocated_large, ATOMIC_RELAXED)
        == 0);
    }
    ctl_accum_arena_stats_u64(
      &sdstats->astats.nmalloc_large, &astats->astats.nmalloc_large);
    ctl_accum_arena_stats_u64(
      &sdstats->astats.ndalloc_large, &astats->astats.ndalloc_large);
    ctl_accum_arena_stats_u64(
      &sdstats->astats.nrequests_large, &astats->astats.nrequests_large);
    accum_atomic_zu(
      &sdstats->astats.abandoned_vm, &astats->astats.abandoned_vm);

    accum_atomic_zu(
      &sdstats->astats.tcache_bytes, &astats->astats.tcache_bytes);

    if (ctl_arena->arena_ind == 0) {
      sdstats->astats.uptime = astats->astats.uptime;
    }

    /* Merge bin stats. */
    for (i = 0; i < SC_NBINS; i++) {
      sdstats->bstats[i].nmalloc += astats->bstats[i].nmalloc;
      sdstats->bstats[i].ndalloc += astats->bstats[i].ndalloc;
      sdstats->bstats[i].nrequests += astats->bstats[i].nrequests;
      if (!destroyed) {
        sdstats->bstats[i].curregs += astats->bstats[i].curregs;
      } else {
        assert(astats->bstats[i].curregs == 0);
      }
      sdstats->bstats[i].nfills += astats->bstats[i].nfills;
      sdstats->bstats[i].nflushes += astats->bstats[i].nflushes;
      sdstats->bstats[i].nslabs += astats->bstats[i].nslabs;
      sdstats->bstats[i].reslabs += astats->bstats[i].reslabs;
      if (!destroyed) {
        sdstats->bstats[i].curslabs += astats->bstats[i].curslabs;
        sdstats->bstats[i].nonfull_slabs += astats->bstats[i].nonfull_slabs;
      } else {
        assert(astats->bstats[i].curslabs == 0);
        assert(astats->bstats[i].nonfull_slabs == 0);
      }
      malloc_mutex_prof_merge(
        &sdstats->bstats[i].mutex_data, &astats->bstats[i].mutex_data);
    }

    /* Merge stats for large allocations. */
    for (i = 0; i < SC_NSIZES - SC_NBINS; i++) {
      ctl_accum_arena_stats_u64(
        &sdstats->lstats[i].nmalloc, &astats->lstats[i].nmalloc);
      ctl_accum_arena_stats_u64(
        &sdstats->lstats[i].ndalloc, &astats->lstats[i].ndalloc);
      ctl_accum_arena_stats_u64(
        &sdstats->lstats[i].nrequests, &astats->lstats[i].nrequests);
      if (!destroyed) {
        sdstats->lstats[i].curlextents += astats->lstats[i].curlextents;
      } else {
        assert(astats->lstats[i].curlextents == 0);
      }
    }

    /* Merge extents stats. */
    for (i = 0; i < SC_NPSIZES; i++) {
      accum_atomic_zu(
        &sdstats->estats[i].ndirty, &astats->estats[i].ndirty);
      accum_atomic_zu(
        &sdstats->estats[i].nmuzzy, &astats->estats[i].nmuzzy);
      accum_atomic_zu(
        &sdstats->estats[i].nretained, &astats->estats[i].nretained);
      accum_atomic_zu(
        &sdstats->estats[i].dirty_bytes, &astats->estats[i].dirty_bytes);
      accum_atomic_zu(
        &sdstats->estats[i].muzzy_bytes, &astats->estats[i].muzzy_bytes);
      accum_atomic_zu(&sdstats->estats[i].retained_bytes,
        &astats->estats[i].retained_bytes);
    }
  }
}

static void ctl_arena_refresh(tsdn_t* tsdn, arena_t* arena,
  ctl_arena_t* ctl_sdarena, unsigned i, bool destroyed) {
  ctl_arena_t* ctl_arena = arenas_i(i);

  ctl_arena_clear(ctl_arena);
  ctl_arena_stats_amerge(tsdn, ctl_arena, arena);
  /* Merge into sum stats as well. */
  ctl_arena_stats_sdmerge(ctl_sdarena, ctl_arena, destroyed);
}

static unsigned ctl_arena_init(tsd_t* tsd, extent_hooks_t* extent_hooks) {
  unsigned arena_ind;
  ctl_arena_t* ctl_arena;

  if ((ctl_arena = ql_last(&ctl_arenas->destroyed, destroyed_link))
    != NULL) {
    ql_remove(&ctl_arenas->destroyed, ctl_arena, destroyed_link);
    arena_ind = ctl_arena->arena_ind;
  } else {
    arena_ind = ctl_arenas->narenas;
  }

  /* Trigger stats allocation. */
  if (arenas_i_impl(tsd, arena_ind, false, true) == NULL) {
    return UINT_MAX;
  }

  /* Initialize new arena. */
  if (arena_init(tsd_tsdn(tsd), arena_ind, extent_hooks) == NULL) {
    return UINT_MAX;
  }

  if (arena_ind == ctl_arenas->narenas) { ctl_arenas->narenas++; }

  return arena_ind;
}

static void ctl_background_thread_stats_read(tsdn_t* tsdn) {
  background_thread_stats_t* stats = &ctl_stats->background_thread;
  if (!have_background_thread
    || background_thread_stats_read(tsdn, stats)) {
    memset(stats, 0, sizeof(background_thread_stats_t));
    nstime_init(&stats->run_interval, 0);
  }
}

static void ctl_refresh(tsdn_t* tsdn) {
  unsigned i;
  ctl_arena_t* ctl_sarena = arenas_i(MALLCTL_ARENAS_ALL);
  VARIABLE_ARRAY(arena_t*, tarenas, ctl_arenas->narenas);

  /*
   * Clear sum stats, since they will be merged into by
   * ctl_arena_refresh().
   */
  ctl_arena_clear(ctl_sarena);

  for (i = 0; i < ctl_arenas->narenas; i++) {
    tarenas[i] = arena_get(tsdn, i, false);
  }

  for (i = 0; i < ctl_arenas->narenas; i++) {
    ctl_arena_t* ctl_arena = arenas_i(i);
    bool initialized = (tarenas[i] != NULL);

    ctl_arena->initialized = initialized;
    if (initialized) {
      ctl_arena_refresh(tsdn, tarenas[i], ctl_sarena, i, false);
    }
  }

  if (config_stats) {
    ctl_stats->allocated = ctl_sarena->astats->allocated_small
      + atomic_load_zu(
        &ctl_sarena->astats->astats.allocated_large, ATOMIC_RELAXED);
    ctl_stats->active = (ctl_sarena->pactive << LG_PAGE);
    ctl_stats->metadata
      = atomic_load_zu(&ctl_sarena->astats->astats.base, ATOMIC_RELAXED)
      + atomic_load_zu(
        &ctl_sarena->astats->astats.internal, ATOMIC_RELAXED);
    ctl_stats->metadata_thp = atomic_load_zu(
      &ctl_sarena->astats->astats.metadata_thp, ATOMIC_RELAXED);
    ctl_stats->resident = atomic_load_zu(
      &ctl_sarena->astats->astats.resident, ATOMIC_RELAXED);
    ctl_stats->mapped
      = atomic_load_zu(&ctl_sarena->astats->astats.mapped, ATOMIC_RELAXED);
    ctl_stats->retained = atomic_load_zu(
      &ctl_sarena->astats->astats.retained, ATOMIC_RELAXED);

    ctl_background_thread_stats_read(tsdn);

#define READ_GLOBAL_MUTEX_PROF_DATA(i, mtx)                                \
  malloc_mutex_lock(tsdn, &mtx);                                           \
  malloc_mutex_prof_read(tsdn, &ctl_stats->mutex_prof_data[i], &mtx);      \
  malloc_mutex_unlock(tsdn, &mtx);

    if (config_prof && opt_prof) {
      READ_GLOBAL_MUTEX_PROF_DATA(global_prof_mutex_prof, bt2gctx_mtx);
    }
    if (have_background_thread) {
      READ_GLOBAL_MUTEX_PROF_DATA(
        global_prof_mutex_background_thread, background_thread_lock);
    } else {
      memset(
        &ctl_stats->mutex_prof_data[global_prof_mutex_background_thread], 0,
        sizeof(mutex_prof_data_t));
    }
    /* We own ctl mutex already. */
    malloc_mutex_prof_read(
      tsdn, &ctl_stats->mutex_prof_data[global_prof_mutex_ctl], &ctl_mtx);
#undef READ_GLOBAL_MUTEX_PROF_DATA
  }
  ctl_arenas->epoch++;
}

static bool ctl_init(tsd_t* tsd) {
  bool ret;
  tsdn_t* tsdn = tsd_tsdn(tsd);

  malloc_mutex_lock(tsdn, &ctl_mtx);
  if (!ctl_initialized) {
    ctl_arena_t *ctl_sarena, *ctl_darena;
    unsigned i;

    /*
     * Allocate demand-zeroed space for pointers to the full
     * range of supported arena indices.
     */
    if (ctl_arenas == NULL) {
      ctl_arenas = (ctl_arenas_t*)base_alloc(
        tsdn, b0get(), sizeof(ctl_arenas_t), QUANTUM);
      if (ctl_arenas == NULL) {
        ret = true;
        goto label_return;
      }
    }

    if (config_stats && ctl_stats == NULL) {
      ctl_stats = (ctl_stats_t*)base_alloc(
        tsdn, b0get(), sizeof(ctl_stats_t), QUANTUM);
      if (ctl_stats == NULL) {
        ret = true;
        goto label_return;
      }
    }

    /*
     * Allocate space for the current full range of arenas
     * here rather than doing it lazily elsewhere, in order
     * to limit when OOM-caused errors can occur.
     */
    if ((ctl_sarena = arenas_i_impl(tsd, MALLCTL_ARENAS_ALL, false, true))
      == NULL) {
      ret = true;
      goto label_return;
    }
    ctl_sarena->initialized = true;

    if ((ctl_darena
          = arenas_i_impl(tsd, MALLCTL_ARENAS_DESTROYED, false, true))
      == NULL) {
      ret = true;
      goto label_return;
    }
    ctl_arena_clear(ctl_darena);
    /*
     * Don't toggle ctl_darena to initialized until an arena is
     * actually destroyed, so that arena.<i>.initialized can be used
     * to query whether the stats are relevant.
     */

    ctl_arenas->narenas = narenas_total_get();
    for (i = 0; i < ctl_arenas->narenas; i++) {
      if (arenas_i_impl(tsd, i, false, true) == NULL) {
        ret = true;
        goto label_return;
      }
    }

    ql_new(&ctl_arenas->destroyed);
    ctl_refresh(tsdn);

    ctl_initialized = true;
  }

  ret = false;
label_return:
  malloc_mutex_unlock(tsdn, &ctl_mtx);
  return ret;
}

static int ctl_lookup(tsdn_t* tsdn, const char* name,
  ctl_node_t const** nodesp, size_t* mibp, size_t* depthp) {
  int ret;
  const char *elm, *tdot, *dot;
  size_t elen, i, j;
  const ctl_named_node_t* node;

  elm = name;
  /* Equivalent to strchrnul(). */
  dot = ((tdot = strchr(elm, '.')) != NULL) ? tdot : strchr(elm, '\0');
  elen = (size_t)((uintptr_t)dot - (uintptr_t)elm);
  if (elen == 0) {
    ret = ENOENT;
    goto label_return;
  }
  node = super_root_node;
  for (i = 0; i < *depthp; i++) {
    assert(node);
    assert(node->nchildren > 0);
    if (ctl_named_node(node->children) != NULL) {
      const ctl_named_node_t* pnode = node;

      /* Children are named. */
      for (j = 0; j < node->nchildren; j++) {
        const ctl_named_node_t* child = ctl_named_children(node, j);
        if (cstr_len(child->name) == elen
          && strncmp(elm, child->name, elen) == 0) {
          node = child;
          if (nodesp != NULL) { nodesp[i] = (const ctl_node_t*)node; }
          mibp[i] = j;
          break;
        }
      }
      if (node == pnode) {
        ret = ENOENT;
        goto label_return;
      }
    } else {
      uintmax_t index;
      const ctl_indexed_node_t* inode;

      /* Children are indexed. */
      index = malloc_strtoumax(elm, NULL, 10);
      if (index == UINTMAX_MAX || index > SIZE_T_MAX) {
        ret = ENOENT;
        goto label_return;
      }

      inode = ctl_indexed_node(node->children);
      node = inode->index(tsdn, mibp, *depthp, (size_t)index);
      if (node == NULL) {
        ret = ENOENT;
        goto label_return;
      }

      if (nodesp != NULL) { nodesp[i] = (const ctl_node_t*)node; }
      mibp[i] = (size_t)index;
    }

    if (node->ctl != NULL) {
      /* Terminal node. */
      if (*dot != '\0') {
        /*
         * The name contains more elements than are
         * in this path through the tree.
         */
        ret = ENOENT;
        goto label_return;
      }
      /* Complete lookup successful. */
      *depthp = i + 1;
      break;
    }

    /* Update elm. */
    if (*dot == '\0') {
      /* No more elements. */
      ret = ENOENT;
      goto label_return;
    }
    elm = &dot[1];
    dot = ((tdot = strchr(elm, '.')) != NULL) ? tdot : strchr(elm, '\0');
    elen = (size_t)((uintptr_t)dot - (uintptr_t)elm);
  }

  ret = 0;
label_return:
  return ret;
}

int ctl_byname(tsd_t* tsd, const char* name, void* oldp, size_t* oldlenp,
  void* newp, size_t newlen) {
  int ret;
  size_t depth;
  ctl_node_t const* nodes[CTL_MAX_DEPTH];
  size_t mib[CTL_MAX_DEPTH];
  const ctl_named_node_t* node;

  if (!ctl_initialized && ctl_init(tsd)) {
    ret = EAGAIN;
    goto label_return;
  }

  depth = CTL_MAX_DEPTH;
  ret = ctl_lookup(tsd_tsdn(tsd), name, nodes, mib, &depth);
  if (ret != 0) { goto label_return; }

  node = ctl_named_node(nodes[depth - 1]);
  if (node != NULL && node->ctl) {
    ret = node->ctl(tsd, mib, depth, oldp, oldlenp, newp, newlen);
  } else {
    /* The name refers to a partial path through the ctl tree. */
    ret = ENOENT;
  }

label_return:
  return (ret);
}

int ctl_nametomib(
  tsd_t* tsd, const char* name, size_t* mibp, size_t* miblenp) {
  int ret;

  if (!ctl_initialized && ctl_init(tsd)) {
    ret = EAGAIN;
    goto label_return;
  }

  ret = ctl_lookup(tsd_tsdn(tsd), name, NULL, mibp, miblenp);
label_return:
  return (ret);
}

int ctl_bymib(tsd_t* tsd, const size_t* mib, size_t miblen, void* oldp,
  size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  const ctl_named_node_t* node;
  size_t i;

  if (!ctl_initialized && ctl_init(tsd)) {
    ret = EAGAIN;
    goto label_return;
  }

  /* Iterate down the tree. */
  node = super_root_node;
  for (i = 0; i < miblen; i++) {
    assert(node);
    assert(node->nchildren > 0);
    if (ctl_named_node(node->children) != NULL) {
      /* Children are named. */
      if (node->nchildren <= mib[i]) {
        ret = ENOENT;
        goto label_return;
      }
      node = ctl_named_children(node, mib[i]);
    } else {
      const ctl_indexed_node_t* inode;

      /* Indexed element. */
      inode = ctl_indexed_node(node->children);
      node = inode->index(tsd_tsdn(tsd), mib, miblen, mib[i]);
      if (node == NULL) {
        ret = ENOENT;
        goto label_return;
      }
    }
  }

  /* Call the ctl function. */
  if (node && node->ctl) {
    ret = node->ctl(tsd, mib, miblen, oldp, oldlenp, newp, newlen);
  } else {
    /* Partial MIB. */
    ret = ENOENT;
  }

label_return:
  return (ret);
}

bool ctl_boot(void) {
  if (malloc_mutex_init(
        &ctl_mtx, "ctl", WITNESS_RANK_CTL, malloc_mutex_rank_exclusive)) {
    return true;
  }

  ctl_initialized = false;

  return false;
}

void ctl_prefork(tsdn_t* tsdn) { malloc_mutex_prefork(tsdn, &ctl_mtx); }

void ctl_postfork_parent(tsdn_t* tsdn) {
  malloc_mutex_postfork_parent(tsdn, &ctl_mtx);
}

void ctl_postfork_child(tsdn_t* tsdn) {
  malloc_mutex_postfork_child(tsdn, &ctl_mtx);
}

/******************************************************************************/
/* *_ctl() functions. */

#define READONLY()                                                         \
  do {                                                                     \
    if (newp != NULL || newlen != 0) {                                     \
      ret = EPERM;                                                         \
      goto label_return;                                                   \
    }                                                                      \
  } while (0)

#define WRITEONLY()                                                        \
  do {                                                                     \
    if (oldp != NULL || oldlenp != NULL) {                                 \
      ret = EPERM;                                                         \
      goto label_return;                                                   \
    }                                                                      \
  } while (0)

#define READ_XOR_WRITE()                                                   \
  do {                                                                     \
    if ((oldp != NULL && oldlenp != NULL)                                  \
      && (newp != NULL || newlen != 0)) {                                  \
      ret = EPERM;                                                         \
      goto label_return;                                                   \
    }                                                                      \
  } while (0)

#define READ(v, t)                                                         \
  do {                                                                     \
    if (oldp != NULL && oldlenp != NULL) {                                 \
      if (*oldlenp != sizeof(t)) {                                         \
        size_t copylen = (sizeof(t) <= *oldlenp) ? sizeof(t) : *oldlenp;   \
        jet_memcpy(oldp, (void*)&(v), copylen);                            \
        ret = EINVAL;                                                      \
        goto label_return;                                                 \
      }                                                                    \
      *(t*)oldp = (v);                                                     \
    }                                                                      \
  } while (0)

#define WRITE(v, t)                                                        \
  do {                                                                     \
    if (newp != NULL) {                                                    \
      if (newlen != sizeof(t)) {                                           \
        ret = EINVAL;                                                      \
        goto label_return;                                                 \
      }                                                                    \
      (v) = *(t*)newp;                                                     \
    }                                                                      \
  } while (0)

#define MIB_UNSIGNED(v, i)                                                 \
  do {                                                                     \
    if (mib[i] > UINT_MAX) {                                               \
      ret = EFAULT;                                                        \
      goto label_return;                                                   \
    }                                                                      \
    v = (unsigned)mib[i];                                                  \
  } while (0)

/*
 * There's a lot of code duplication in the following macros due to
 * limitations in how nested cpp macros are expanded.
 */
#define CTL_RO_CLGEN(c, l, n, v, t)                                        \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    if (!(c)) { return ENOENT; }                                           \
    if (l) { malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx); }                 \
    READONLY();                                                            \
    oldval = (v);                                                          \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    if (l) { malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx); }               \
    return ret;                                                            \
  }

#define CTL_RO_CGEN(c, n, v, t)                                            \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    if (!(c)) { return ENOENT; }                                           \
    malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);                            \
    READONLY();                                                            \
    oldval = (v);                                                          \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);                          \
    return ret;                                                            \
  }

#define CTL_RO_GEN(n, v, t)                                                \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);                            \
    READONLY();                                                            \
    oldval = (v);                                                          \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);                          \
    return ret;                                                            \
  }

/*
 * ctl_mtx is not acquired, under the assumption that no pertinent data will
 * mutate during the call.
 */
#define CTL_RO_NL_CGEN(c, n, v, t)                                         \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    if (!(c)) { return ENOENT; }                                           \
    READONLY();                                                            \
    oldval = (v);                                                          \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    return ret;                                                            \
  }

#define CTL_RO_NL_GEN(n, v, t)                                             \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    READONLY();                                                            \
    oldval = (v);                                                          \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    return ret;                                                            \
  }

#define CTL_TSD_RO_NL_CGEN(c, n, m, t)                                     \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    if (!(c)) { return ENOENT; }                                           \
    READONLY();                                                            \
    oldval = (m(tsd));                                                     \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    return ret;                                                            \
  }

#define CTL_RO_CONFIG_GEN(n, t)                                            \
  static int n##_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,         \
    void* oldp, size_t* oldlenp, void* newp, size_t newlen) {              \
    int ret;                                                               \
    t oldval;                                                              \
                                                                           \
    READONLY();                                                            \
    oldval = n;                                                            \
    READ(oldval, t);                                                       \
                                                                           \
    ret = 0;                                                               \
  label_return:                                                            \
    return ret;                                                            \
  }

/******************************************************************************/

CTL_RO_NL_GEN(version, JEMALLOC_VERSION, const char*)

static int epoch_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  UNUSED uint64_t newval;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  WRITE(newval, uint64_t);
  if (newp != NULL) { ctl_refresh(tsd_tsdn(tsd)); }
  READ(ctl_arenas->epoch, uint64_t);

  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static int background_thread_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  if (!have_background_thread) { return ENOENT; }
  background_thread_ctl_init(tsd_tsdn(tsd));

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  malloc_mutex_lock(tsd_tsdn(tsd), &background_thread_lock);
  if (newp == NULL) {
    oldval = background_thread_enabled();
    READ(oldval, bool);
  } else {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    oldval = background_thread_enabled();
    READ(oldval, bool);

    bool newval = *(bool*)newp;
    if (newval == oldval) {
      ret = 0;
      goto label_return;
    }

    background_thread_enabled_set(tsd_tsdn(tsd), newval);
    if (newval) {
      if (background_threads_enable(tsd)) {
        ret = EFAULT;
        goto label_return;
      }
    } else {
      if (background_threads_disable(tsd)) {
        ret = EFAULT;
        goto label_return;
      }
    }
  }
  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &background_thread_lock);
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);

  return ret;
}

static int max_background_threads_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  size_t oldval;

  if (!have_background_thread) { return ENOENT; }
  background_thread_ctl_init(tsd_tsdn(tsd));

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  malloc_mutex_lock(tsd_tsdn(tsd), &background_thread_lock);
  if (newp == NULL) {
    oldval = max_background_threads;
    READ(oldval, size_t);
  } else {
    if (newlen != sizeof(size_t)) {
      ret = EINVAL;
      goto label_return;
    }
    oldval = max_background_threads;
    READ(oldval, size_t);

    size_t newval = *(size_t*)newp;
    if (newval == oldval) {
      ret = 0;
      goto label_return;
    }
    if (newval > opt_max_background_threads) {
      ret = EINVAL;
      goto label_return;
    }

    if (background_thread_enabled()) {
      background_thread_enabled_set(tsd_tsdn(tsd), false);
      if (background_threads_disable(tsd)) {
        ret = EFAULT;
        goto label_return;
      }
      max_background_threads = newval;
      background_thread_enabled_set(tsd_tsdn(tsd), true);
      if (background_threads_enable(tsd)) {
        ret = EFAULT;
        goto label_return;
      }
    } else {
      max_background_threads = newval;
    }
  }
  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &background_thread_lock);
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);

  return ret;
}

/******************************************************************************/

CTL_RO_CONFIG_GEN(config_cache_oblivious, bool)
CTL_RO_CONFIG_GEN(config_debug, bool)
CTL_RO_CONFIG_GEN(config_fill, bool)
CTL_RO_CONFIG_GEN(config_lazy_lock, bool)
CTL_RO_CONFIG_GEN(config_malloc_conf, const char*)
CTL_RO_CONFIG_GEN(config_opt_safety_checks, bool)
CTL_RO_CONFIG_GEN(config_prof, bool)
CTL_RO_CONFIG_GEN(config_prof_libgcc, bool)
CTL_RO_CONFIG_GEN(config_prof_libunwind, bool)
CTL_RO_CONFIG_GEN(config_stats, bool)
CTL_RO_CONFIG_GEN(config_utrace, bool)
CTL_RO_CONFIG_GEN(config_xmalloc, bool)

/******************************************************************************/

CTL_RO_NL_GEN(opt_abort, opt_abort, bool)
CTL_RO_NL_GEN(opt_abort_conf, opt_abort_conf, bool)
CTL_RO_NL_GEN(opt_confirm_conf, opt_confirm_conf, bool)
CTL_RO_NL_GEN(
  opt_metadata_thp, metadata_thp_mode_names[opt_metadata_thp], const char*)
CTL_RO_NL_GEN(opt_retain, opt_retain, bool)
CTL_RO_NL_GEN(opt_dss, opt_dss, const char*)
CTL_RO_NL_GEN(opt_narenas, opt_narenas, unsigned)
CTL_RO_NL_GEN(
  opt_percpu_arena, percpu_arena_mode_names[opt_percpu_arena], const char*)
CTL_RO_NL_GEN(opt_oversize_threshold, opt_oversize_threshold, size_t)
CTL_RO_NL_GEN(opt_background_thread, opt_background_thread, bool)
CTL_RO_NL_GEN(
  opt_max_background_threads, opt_max_background_threads, size_t)
CTL_RO_NL_GEN(opt_dirty_decay_ms, opt_dirty_decay_ms, ssize_t)
CTL_RO_NL_GEN(opt_muzzy_decay_ms, opt_muzzy_decay_ms, ssize_t)
CTL_RO_NL_GEN(opt_stats_print, opt_stats_print, bool)
CTL_RO_NL_GEN(opt_stats_print_opts, opt_stats_print_opts, const char*)
CTL_RO_NL_CGEN(config_fill, opt_junk, opt_junk, const char*)
CTL_RO_NL_CGEN(config_fill, opt_zero, opt_zero, bool)
CTL_RO_NL_CGEN(config_utrace, opt_utrace, opt_utrace, bool)
CTL_RO_NL_CGEN(config_xmalloc, opt_xmalloc, opt_xmalloc, bool)
CTL_RO_NL_GEN(opt_tcache, opt_tcache, bool)
CTL_RO_NL_GEN(opt_thp, thp_mode_names[opt_thp], const char*)
CTL_RO_NL_GEN(
  opt_lg_extent_max_active_fit, opt_lg_extent_max_active_fit, size_t)
CTL_RO_NL_GEN(opt_lg_tcache_max, opt_lg_tcache_max, ssize_t)
CTL_RO_NL_CGEN(config_prof, opt_prof, opt_prof, bool)
CTL_RO_NL_CGEN(config_prof, opt_prof_prefix, opt_prof_prefix, const char*)
CTL_RO_NL_CGEN(config_prof, opt_prof_active, opt_prof_active, bool)
CTL_RO_NL_CGEN(config_prof, opt_prof_thread_active_init,
  opt_prof_thread_active_init, bool)
CTL_RO_NL_CGEN(config_prof, opt_lg_prof_sample, opt_lg_prof_sample, size_t)
CTL_RO_NL_CGEN(config_prof, opt_prof_accum, opt_prof_accum, bool)
CTL_RO_NL_CGEN(
  config_prof, opt_lg_prof_interval, opt_lg_prof_interval, ssize_t)
CTL_RO_NL_CGEN(config_prof, opt_prof_gdump, opt_prof_gdump, bool)
CTL_RO_NL_CGEN(config_prof, opt_prof_final, opt_prof_final, bool)
CTL_RO_NL_CGEN(config_prof, opt_prof_leak, opt_prof_leak, bool)

/******************************************************************************/

static int thread_arena_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  arena_t* oldarena;
  unsigned newind, oldind;

  oldarena = arena_choose(tsd, NULL);
  if (oldarena == NULL) { return EAGAIN; }
  newind = oldind = arena_ind_get(oldarena);
  WRITE(newind, unsigned);
  READ(oldind, unsigned);

  if (newind != oldind) {
    arena_t* newarena;

    if (newind >= narenas_total_get()) {
      /* New arena index is out of range. */
      ret = EFAULT;
      goto label_return;
    }

    if (have_percpu_arena && PERCPU_ARENA_ENABLED(opt_percpu_arena)) {
      if (newind < percpu_arena_ind_limit(opt_percpu_arena)) {
        /*
         * If perCPU arena is enabled, thread_arena
         * control is not allowed for the auto arena
         * range.
         */
        ret = EPERM;
        goto label_return;
      }
    }

    /* Initialize arena if necessary. */
    newarena = arena_get(tsd_tsdn(tsd), newind, true);
    if (newarena == NULL) {
      ret = EAGAIN;
      goto label_return;
    }
    /* Set new arena/tcache associations. */
    arena_migrate(tsd, oldind, newind);
    if (tcache_available(tsd)) {
      tcache_arena_reassociate(
        tsd_tsdn(tsd), tsd_tcachep_get(tsd), newarena);
    }
  }

  ret = 0;
label_return:
  return ret;
}

CTL_TSD_RO_NL_CGEN(
  config_stats, thread_allocated, tsd_thread_allocated_get, uint64_t)
CTL_TSD_RO_NL_CGEN(
  config_stats, thread_allocatedp, tsd_thread_allocatedp_get, uint64_t*)
CTL_TSD_RO_NL_CGEN(
  config_stats, thread_deallocated, tsd_thread_deallocated_get, uint64_t)
CTL_TSD_RO_NL_CGEN(
  config_stats, thread_deallocatedp, tsd_thread_deallocatedp_get, uint64_t*)

static int thread_tcache_enabled_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  oldval = tcache_enabled_get(tsd);
  if (newp != NULL) {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    tcache_enabled_set(tsd, *(bool*)newp);
  }
  READ(oldval, bool);

  ret = 0;
label_return:
  return ret;
}

static int thread_tcache_flush_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;

  if (!tcache_available(tsd)) {
    ret = EFAULT;
    goto label_return;
  }

  READONLY();
  WRITEONLY();

  tcache_flush(tsd);

  ret = 0;
label_return:
  return ret;
}

static int thread_prof_name_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;

  if (!config_prof) { return ENOENT; }

  READ_XOR_WRITE();

  if (newp != NULL) {
    if (newlen != sizeof(const char*)) {
      ret = EINVAL;
      goto label_return;
    }

    if ((ret = prof_thread_name_set(tsd, *(const char**)newp)) != 0) {
      goto label_return;
    }
  } else {
    const char* oldname = prof_thread_name_get(tsd);
    READ(oldname, const char*);
  }

  ret = 0;
label_return:
  return ret;
}

static int thread_prof_active_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  if (!config_prof) { return ENOENT; }

  oldval = prof_thread_active_get(tsd);
  if (newp != NULL) {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    if (prof_thread_active_set(tsd, *(bool*)newp)) {
      ret = EAGAIN;
      goto label_return;
    }
  }
  READ(oldval, bool);

  ret = 0;
label_return:
  return ret;
}

/******************************************************************************/

static int tcache_create_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned tcache_ind;

  READONLY();
  if (tcaches_create(tsd, &tcache_ind)) {
    ret = EFAULT;
    goto label_return;
  }
  READ(tcache_ind, unsigned);

  ret = 0;
label_return:
  return ret;
}

static int tcache_flush_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned tcache_ind;

  WRITEONLY();
  tcache_ind = UINT_MAX;
  WRITE(tcache_ind, unsigned);
  if (tcache_ind == UINT_MAX) {
    ret = EFAULT;
    goto label_return;
  }
  tcaches_flush(tsd, tcache_ind);

  ret = 0;
label_return:
  return ret;
}

static int tcache_destroy_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned tcache_ind;

  WRITEONLY();
  tcache_ind = UINT_MAX;
  WRITE(tcache_ind, unsigned);
  if (tcache_ind == UINT_MAX) {
    ret = EFAULT;
    goto label_return;
  }
  tcaches_destroy(tsd, tcache_ind);

  ret = 0;
label_return:
  return ret;
}

/******************************************************************************/

static int arena_i_initialized_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  tsdn_t* tsdn = tsd_tsdn(tsd);
  unsigned arena_ind;
  bool initialized;

  READONLY();
  MIB_UNSIGNED(arena_ind, 1);

  malloc_mutex_lock(tsdn, &ctl_mtx);
  initialized = arenas_i(arena_ind)->initialized;
  malloc_mutex_unlock(tsdn, &ctl_mtx);

  READ(initialized, bool);

  ret = 0;
label_return:
  return ret;
}

static void arena_i_decay(tsdn_t* tsdn, unsigned arena_ind, bool all) {
  malloc_mutex_lock(tsdn, &ctl_mtx);
  {
    unsigned narenas = ctl_arenas->narenas;

    /*
     * Access via index narenas is deprecated, and scheduled for
     * removal in 6.0.0.
     */
    if (arena_ind == MALLCTL_ARENAS_ALL || arena_ind == narenas) {
      unsigned i;
      VARIABLE_ARRAY(arena_t*, tarenas, narenas);

      for (i = 0; i < narenas; i++) {
        tarenas[i] = arena_get(tsdn, i, false);
      }

      /*
       * No further need to hold ctl_mtx, since narenas and
       * tarenas contain everything needed below.
       */
      malloc_mutex_unlock(tsdn, &ctl_mtx);

      for (i = 0; i < narenas; i++) {
        if (tarenas[i] != NULL) {
          arena_decay(tsdn, tarenas[i], false, all);
        }
      }
    } else {
      arena_t* tarena;

      assert(arena_ind < narenas);

      tarena = arena_get(tsdn, arena_ind, false);

      /* No further need to hold ctl_mtx. */
      malloc_mutex_unlock(tsdn, &ctl_mtx);

      if (tarena != NULL) { arena_decay(tsdn, tarena, false, all); }
    }
  }
}

static int arena_i_decay_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;

  READONLY();
  WRITEONLY();
  MIB_UNSIGNED(arena_ind, 1);
  arena_i_decay(tsd_tsdn(tsd), arena_ind, false);

  ret = 0;
label_return:
  return ret;
}

static int arena_i_purge_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;

  READONLY();
  WRITEONLY();
  MIB_UNSIGNED(arena_ind, 1);
  arena_i_decay(tsd_tsdn(tsd), arena_ind, true);

  ret = 0;
label_return:
  return ret;
}

static int arena_i_reset_destroy_helper(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen,
  unsigned* arena_ind, arena_t** arena) {
  int ret;

  READONLY();
  WRITEONLY();
  MIB_UNSIGNED(*arena_ind, 1);

  *arena = arena_get(tsd_tsdn(tsd), *arena_ind, false);
  if (*arena == NULL || arena_is_auto(*arena)) {
    ret = EFAULT;
    goto label_return;
  }

  ret = 0;
label_return:
  return ret;
}

static void arena_reset_prepare_background_thread(
  tsd_t* tsd, unsigned arena_ind) {
  /* Temporarily disable the background thread during arena reset. */
  if (have_background_thread) {
    malloc_mutex_lock(tsd_tsdn(tsd), &background_thread_lock);
    if (background_thread_enabled()) {
      background_thread_info_t* info
        = background_thread_info_get(arena_ind);
      assert(info->state == background_thread_started);
      malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
      info->state = background_thread_paused;
      malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
    }
  }
}

static void arena_reset_finish_background_thread(
  tsd_t* tsd, unsigned arena_ind) {
  if (have_background_thread) {
    if (background_thread_enabled()) {
      background_thread_info_t* info
        = background_thread_info_get(arena_ind);
      assert(info->state == background_thread_paused);
      malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
      info->state = background_thread_started;
      malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
    }
    malloc_mutex_unlock(tsd_tsdn(tsd), &background_thread_lock);
  }
}

static int arena_i_reset_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;
  arena_t* arena;

  ret = arena_i_reset_destroy_helper(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, &arena_ind, &arena);
  if (ret != 0) { return ret; }

  arena_reset_prepare_background_thread(tsd, arena_ind);
  arena_reset(tsd, arena);
  arena_reset_finish_background_thread(tsd, arena_ind);

  return ret;
}

static int arena_i_destroy_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;
  arena_t* arena;
  ctl_arena_t *ctl_darena, *ctl_arena;

  ret = arena_i_reset_destroy_helper(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, &arena_ind, &arena);
  if (ret != 0) { goto label_return; }

  if (arena_nthreads_get(arena, false) != 0
    || arena_nthreads_get(arena, true) != 0) {
    ret = EFAULT;
    goto label_return;
  }

  arena_reset_prepare_background_thread(tsd, arena_ind);
  /* Merge stats after resetting and purging arena. */
  arena_reset(tsd, arena);
  arena_decay(tsd_tsdn(tsd), arena, false, true);
  ctl_darena = arenas_i(MALLCTL_ARENAS_DESTROYED);
  ctl_darena->initialized = true;
  ctl_arena_refresh(tsd_tsdn(tsd), arena, ctl_darena, arena_ind, true);
  /* Destroy arena. */
  arena_destroy(tsd, arena);
  ctl_arena = arenas_i(arena_ind);
  ctl_arena->initialized = false;
  /* Record arena index for later recycling via arenas.create. */
  ql_elm_new(ctl_arena, destroyed_link);
  ql_tail_insert(&ctl_arenas->destroyed, ctl_arena, destroyed_link);
  arena_reset_finish_background_thread(tsd, arena_ind);

  assert(ret == 0);
label_return:
  return ret;
}

static int arena_i_dss_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  const char* dss = NULL;
  unsigned arena_ind;
  dss_prec_t dss_prec_old = dss_prec_limit;
  dss_prec_t dss_prec = dss_prec_limit;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  WRITE(dss, const char*);
  MIB_UNSIGNED(arena_ind, 1);
  if (dss != NULL) {
    int i;
    bool match = false;

    for (i = 0; i < dss_prec_limit; i++) {
      if (strcmp(dss_prec_names[i], dss) == 0) {
        dss_prec = i;
        match = true;
        break;
      }
    }

    if (!match) {
      ret = EINVAL;
      goto label_return;
    }
  }

  /*
   * Access via index narenas is deprecated, and scheduled for removal in
   * 6.0.0.
   */
  if (arena_ind == MALLCTL_ARENAS_ALL || arena_ind == ctl_arenas->narenas) {
    if (dss_prec != dss_prec_limit && extent_dss_prec_set(dss_prec)) {
      ret = EFAULT;
      goto label_return;
    }
    dss_prec_old = extent_dss_prec_get();
  } else {
    arena_t* arena = arena_get(tsd_tsdn(tsd), arena_ind, false);
    if (arena == NULL
      || (dss_prec != dss_prec_limit
        && arena_dss_prec_set(arena, dss_prec))) {
      ret = EFAULT;
      goto label_return;
    }
    dss_prec_old = arena_dss_prec_get(arena);
  }

  dss = dss_prec_names[dss_prec_old];
  READ(dss, const char*);

  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static int arena_i_decay_ms_ctl_impl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen,
  bool dirty) {
  int ret;
  unsigned arena_ind;
  arena_t* arena;

  MIB_UNSIGNED(arena_ind, 1);
  arena = arena_get(tsd_tsdn(tsd), arena_ind, false);
  if (arena == NULL) {
    ret = EFAULT;
    goto label_return;
  }

  if (oldp != NULL && oldlenp != NULL) {
    size_t oldval = dirty ? arena_dirty_decay_ms_get(arena)
                          : arena_muzzy_decay_ms_get(arena);
    READ(oldval, ssize_t);
  }
  if (newp != NULL) {
    if (newlen != sizeof(ssize_t)) {
      ret = EINVAL;
      goto label_return;
    }
    if (arena_is_huge(arena_ind) && *(ssize_t*)newp > 0) {
      /*
       * By default the huge arena purges eagerly.  If it is
       * set to non-zero decay time afterwards, background
       * thread might be needed.
       */
      if (background_thread_create(tsd, arena_ind)) {
        ret = EFAULT;
        goto label_return;
      }
    }
    if (dirty
        ? arena_dirty_decay_ms_set(tsd_tsdn(tsd), arena, *(ssize_t*)newp)
        : arena_muzzy_decay_ms_set(tsd_tsdn(tsd), arena, *(ssize_t*)newp)) {
      ret = EFAULT;
      goto label_return;
    }
  }

  ret = 0;
label_return:
  return ret;
}

static int arena_i_dirty_decay_ms_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  return arena_i_decay_ms_ctl_impl(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, true);
}

static int arena_i_muzzy_decay_ms_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  return arena_i_decay_ms_ctl_impl(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, false);
}

static int arena_i_extent_hooks_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;
  arena_t* arena;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  MIB_UNSIGNED(arena_ind, 1);
  if (arena_ind < narenas_total_get()) {
    extent_hooks_t* old_extent_hooks;
    arena = arena_get(tsd_tsdn(tsd), arena_ind, false);
    if (arena == NULL) {
      if (arena_ind >= narenas_auto) {
        ret = EFAULT;
        goto label_return;
      }
      old_extent_hooks = (extent_hooks_t*)&extent_hooks_default;
      READ(old_extent_hooks, extent_hooks_t*);
      if (newp != NULL) {
        /* Initialize a new arena as a side effect. */
        extent_hooks_t* new_extent_hooks JEMALLOC_CC_SILENCE_INIT(NULL);
        WRITE(new_extent_hooks, extent_hooks_t*);
        arena = arena_init(tsd_tsdn(tsd), arena_ind, new_extent_hooks);
        if (arena == NULL) {
          ret = EFAULT;
          goto label_return;
        }
      }
    } else {
      if (newp != NULL) {
        extent_hooks_t* new_extent_hooks JEMALLOC_CC_SILENCE_INIT(NULL);
        WRITE(new_extent_hooks, extent_hooks_t*);
        old_extent_hooks = extent_hooks_set(tsd, arena, new_extent_hooks);
        READ(old_extent_hooks, extent_hooks_t*);
      } else {
        old_extent_hooks = extent_hooks_get(arena);
        READ(old_extent_hooks, extent_hooks_t*);
      }
    }
  } else {
    ret = EFAULT;
    goto label_return;
  }
  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static int arena_i_retain_grow_limit_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;
  arena_t* arena;

  if (!opt_retain) {
    /* Only relevant when retain is enabled. */
    return ENOENT;
  }

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  MIB_UNSIGNED(arena_ind, 1);
  if (arena_ind < narenas_total_get()
    && (arena = arena_get(tsd_tsdn(tsd), arena_ind, false)) != NULL) {
    size_t old_limit, new_limit;
    if (newp != NULL) { WRITE(new_limit, size_t); }
    bool err = arena_retain_grow_limit_get_set(
      tsd, arena, &old_limit, newp != NULL ? &new_limit : NULL);
    if (!err) {
      READ(old_limit, size_t);
      ret = 0;
    } else {
      ret = EFAULT;
    }
  } else {
    ret = EFAULT;
  }
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static const ctl_named_node_t* arena_i_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i) {
  const ctl_named_node_t* ret;

  malloc_mutex_lock(tsdn, &ctl_mtx);
  switch (i) {
  case MALLCTL_ARENAS_ALL:
  case MALLCTL_ARENAS_DESTROYED: break;
  default:
    if (i > ctl_arenas->narenas) {
      ret = NULL;
      goto label_return;
    }
    break;
  }

  ret = super_arena_i_node;
label_return:
  malloc_mutex_unlock(tsdn, &ctl_mtx);
  return ret;
}

/******************************************************************************/

static int arenas_narenas_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned narenas;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  READONLY();
  if (*oldlenp != sizeof(unsigned)) {
    ret = EINVAL;
    goto label_return;
  }
  narenas = ctl_arenas->narenas;
  READ(narenas, unsigned);

  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static int arenas_decay_ms_ctl_impl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen,
  bool dirty) {
  int ret;

  if (oldp != NULL && oldlenp != NULL) {
    size_t oldval = (dirty ? arena_dirty_decay_ms_default_get()
                           : arena_muzzy_decay_ms_default_get());
    READ(oldval, ssize_t);
  }
  if (newp != NULL) {
    if (newlen != sizeof(ssize_t)) {
      ret = EINVAL;
      goto label_return;
    }
    if (dirty ? arena_dirty_decay_ms_default_set(*(ssize_t*)newp)
              : arena_muzzy_decay_ms_default_set(*(ssize_t*)newp)) {
      ret = EFAULT;
      goto label_return;
    }
  }

  ret = 0;
label_return:
  return ret;
}

static int arenas_dirty_decay_ms_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  return arenas_decay_ms_ctl_impl(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, true);
}

static int arenas_muzzy_decay_ms_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  return arenas_decay_ms_ctl_impl(
    tsd, mib, miblen, oldp, oldlenp, newp, newlen, false);
}

CTL_RO_NL_GEN(arenas_quantum, QUANTUM, size_t)
CTL_RO_NL_GEN(arenas_page, PAGE, size_t)
CTL_RO_NL_GEN(arenas_tcache_max, tcache_maxclass, size_t)
CTL_RO_NL_GEN(arenas_nbins, SC_NBINS, unsigned)
CTL_RO_NL_GEN(arenas_nhbins, nhbins, unsigned)
CTL_RO_NL_GEN(arenas_bin_i_size, bin_infos[mib[2]].reg_size, size_t)
CTL_RO_NL_GEN(arenas_bin_i_nregs, bin_infos[mib[2]].nregs, uint32_t)
CTL_RO_NL_GEN(arenas_bin_i_slab_size, bin_infos[mib[2]].slab_size, size_t)
CTL_RO_NL_GEN(arenas_bin_i_nshards, bin_infos[mib[2]].n_shards, uint32_t)
static const ctl_named_node_t* arenas_bin_i_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i) {
  if (i > SC_NBINS) { return NULL; }
  return super_arenas_bin_i_node;
}

CTL_RO_NL_GEN(arenas_nlextents, SC_NSIZES - SC_NBINS, unsigned)
CTL_RO_NL_GEN(
  arenas_lextent_i_size, sz_index2size(SC_NBINS + (szind_t)mib[2]), size_t)
static const ctl_named_node_t* arenas_lextent_i_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i) {
  if (i > SC_NSIZES - SC_NBINS) { return NULL; }
  return super_arenas_lextent_i_node;
}

static int arenas_create_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  extent_hooks_t* extent_hooks;
  unsigned arena_ind;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);

  extent_hooks = (extent_hooks_t*)&extent_hooks_default;
  WRITE(extent_hooks, extent_hooks_t*);
  if ((arena_ind = ctl_arena_init(tsd, extent_hooks)) == UINT_MAX) {
    ret = EAGAIN;
    goto label_return;
  }
  READ(arena_ind, unsigned);

  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

static int arenas_lookup_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  unsigned arena_ind;
  void* ptr;
  extent_t* extent;
  arena_t* arena;

  ptr = NULL;
  ret = EINVAL;
  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  WRITE(ptr, void*);
  extent = iealloc(tsd_tsdn(tsd), ptr);
  if (extent == NULL) goto label_return;

  arena = extent_arena_get(extent);
  if (arena == NULL) goto label_return;

  arena_ind = arena_ind_get(arena);
  READ(arena_ind, unsigned);

  ret = 0;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

/******************************************************************************/

static int prof_thread_active_init_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  if (!config_prof) { return ENOENT; }

  if (newp != NULL) {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    oldval = prof_thread_active_init_set(tsd_tsdn(tsd), *(bool*)newp);
  } else {
    oldval = prof_thread_active_init_get(tsd_tsdn(tsd));
  }
  READ(oldval, bool);

  ret = 0;
label_return:
  return ret;
}

static int prof_active_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  if (!config_prof) { return ENOENT; }

  if (newp != NULL) {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    oldval = prof_active_set(tsd_tsdn(tsd), *(bool*)newp);
  } else {
    oldval = prof_active_get(tsd_tsdn(tsd));
  }
  READ(oldval, bool);

  ret = 0;
label_return:
  return ret;
}

static int prof_dump_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  const char* filename = NULL;

  if (!config_prof) { return ENOENT; }

  WRITEONLY();
  WRITE(filename, const char*);

  if (prof_mdump(tsd, filename)) {
    ret = EFAULT;
    goto label_return;
  }

  ret = 0;
label_return:
  return ret;
}

static int prof_gdump_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  bool oldval;

  if (!config_prof) { return ENOENT; }

  if (newp != NULL) {
    if (newlen != sizeof(bool)) {
      ret = EINVAL;
      goto label_return;
    }
    oldval = prof_gdump_set(tsd_tsdn(tsd), *(bool*)newp);
  } else {
    oldval = prof_gdump_get(tsd_tsdn(tsd));
  }
  READ(oldval, bool);

  ret = 0;
label_return:
  return ret;
}

static int prof_reset_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  size_t lg_sample = lg_prof_sample;

  if (!config_prof) { return ENOENT; }

  WRITEONLY();
  WRITE(lg_sample, size_t);
  if (lg_sample >= (sizeof(uint64_t) << 3)) {
    lg_sample = (sizeof(uint64_t) << 3) - 1;
  }

  prof_reset(tsd, lg_sample);

  ret = 0;
label_return:
  return ret;
}

CTL_RO_NL_CGEN(config_prof, prof_interval, prof_interval, uint64_t)
CTL_RO_NL_CGEN(config_prof, lg_prof_sample, lg_prof_sample, size_t)

static int prof_log_start_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;

  const char* filename = NULL;

  if (!config_prof) { return ENOENT; }

  WRITEONLY();
  WRITE(filename, const char*);

  if (prof_log_start(tsd_tsdn(tsd), filename)) {
    ret = EFAULT;
    goto label_return;
  }

  ret = 0;
label_return:
  return ret;
}

static int prof_log_stop_ctl(tsd_t* tsd, const size_t* mib, size_t miblen,
  void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  if (!config_prof) { return ENOENT; }

  if (prof_log_stop(tsd_tsdn(tsd))) { return EFAULT; }

  return 0;
}

/******************************************************************************/

CTL_RO_CGEN(config_stats, stats_allocated, ctl_stats->allocated, size_t)
CTL_RO_CGEN(config_stats, stats_active, ctl_stats->active, size_t)
CTL_RO_CGEN(config_stats, stats_metadata, ctl_stats->metadata, size_t)
CTL_RO_CGEN(
  config_stats, stats_metadata_thp, ctl_stats->metadata_thp, size_t)
CTL_RO_CGEN(config_stats, stats_resident, ctl_stats->resident, size_t)
CTL_RO_CGEN(config_stats, stats_mapped, ctl_stats->mapped, size_t)
CTL_RO_CGEN(config_stats, stats_retained, ctl_stats->retained, size_t)

CTL_RO_CGEN(config_stats, stats_background_thread_num_threads,
  ctl_stats->background_thread.num_threads, size_t)
CTL_RO_CGEN(config_stats, stats_background_thread_num_runs,
  ctl_stats->background_thread.num_runs, uint64_t)
CTL_RO_CGEN(config_stats, stats_background_thread_run_interval,
  nstime_ns(&ctl_stats->background_thread.run_interval), uint64_t)

CTL_RO_GEN(stats_arenas_i_dss, arenas_i(mib[2])->dss, const char*)
CTL_RO_GEN(
  stats_arenas_i_dirty_decay_ms, arenas_i(mib[2])->dirty_decay_ms, ssize_t)
CTL_RO_GEN(
  stats_arenas_i_muzzy_decay_ms, arenas_i(mib[2])->muzzy_decay_ms, ssize_t)
CTL_RO_GEN(stats_arenas_i_nthreads, arenas_i(mib[2])->nthreads, unsigned)
CTL_RO_GEN(stats_arenas_i_uptime,
  nstime_ns(&arenas_i(mib[2])->astats->astats.uptime), uint64_t)
CTL_RO_GEN(stats_arenas_i_pactive, arenas_i(mib[2])->pactive, size_t)
CTL_RO_GEN(stats_arenas_i_pdirty, arenas_i(mib[2])->pdirty, size_t)
CTL_RO_GEN(stats_arenas_i_pmuzzy, arenas_i(mib[2])->pmuzzy, size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_mapped,
  atomic_load_zu(&arenas_i(mib[2])->astats->astats.mapped, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_retained,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.retained, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_extent_avail,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.extent_avail, ATOMIC_RELAXED),
  size_t)

CTL_RO_CGEN(config_stats, stats_arenas_i_dirty_npurge,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_dirty.npurge),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_dirty_nmadvise,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_dirty.nmadvise),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_dirty_purged,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_dirty.purged),
  uint64_t)

CTL_RO_CGEN(config_stats, stats_arenas_i_muzzy_npurge,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_muzzy.npurge),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_muzzy_nmadvise,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_muzzy.nmadvise),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_muzzy_purged,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.decay_muzzy.purged),
  uint64_t)

CTL_RO_CGEN(config_stats, stats_arenas_i_base,
  atomic_load_zu(&arenas_i(mib[2])->astats->astats.base, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_internal,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.internal, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_metadata_thp,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.metadata_thp, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_tcache_bytes,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.tcache_bytes, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_resident,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.resident, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_abandoned_vm,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.abandoned_vm, ATOMIC_RELAXED),
  size_t)

CTL_RO_CGEN(config_stats, stats_arenas_i_small_allocated,
  arenas_i(mib[2])->astats->allocated_small, size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_small_nmalloc,
  arenas_i(mib[2])->astats->nmalloc_small, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_small_ndalloc,
  arenas_i(mib[2])->astats->ndalloc_small, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_small_nrequests,
  arenas_i(mib[2])->astats->nrequests_small, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_small_nfills,
  arenas_i(mib[2])->astats->nfills_small, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_small_nflushes,
  arenas_i(mib[2])->astats->nflushes_small, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_large_allocated,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->astats.allocated_large, ATOMIC_RELAXED),
  size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_large_nmalloc,
  ctl_arena_stats_read_u64(&arenas_i(mib[2])->astats->astats.nmalloc_large),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_large_ndalloc,
  ctl_arena_stats_read_u64(&arenas_i(mib[2])->astats->astats.ndalloc_large),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_large_nrequests,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.nrequests_large),
  uint64_t)
/*
 * Note: "nmalloc_large" here instead of "nfills" in the read.  This is
 * intentional (large has no batch fill).
 */
CTL_RO_CGEN(config_stats, stats_arenas_i_large_nfills,
  ctl_arena_stats_read_u64(&arenas_i(mib[2])->astats->astats.nmalloc_large),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_large_nflushes,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->astats.nflushes_large),
  uint64_t)

/* Lock profiling related APIs below. */
#define RO_MUTEX_CTL_GEN(n, l)                                             \
  CTL_RO_CGEN(config_stats, stats_##n##_num_ops, l.n_lock_ops, uint64_t)   \
  CTL_RO_CGEN(                                                             \
    config_stats, stats_##n##_num_wait, l.n_wait_times, uint64_t)          \
  CTL_RO_CGEN(                                                             \
    config_stats, stats_##n##_num_spin_acq, l.n_spin_acquired, uint64_t)   \
  CTL_RO_CGEN(config_stats, stats_##n##_num_owner_switch,                  \
    l.n_owner_switches, uint64_t)                                          \
  CTL_RO_CGEN(config_stats, stats_##n##_total_wait_time,                   \
    nstime_ns(&l.tot_wait_time), uint64_t)                                 \
  CTL_RO_CGEN(config_stats, stats_##n##_max_wait_time,                     \
    nstime_ns(&l.max_wait_time), uint64_t)                                 \
  CTL_RO_CGEN(                                                             \
    config_stats, stats_##n##_max_num_thds, l.max_n_thds, uint32_t)

/* Global mutexes. */
#define OP(mtx)                                                            \
  RO_MUTEX_CTL_GEN(                                                        \
    mutexes_##mtx, ctl_stats->mutex_prof_data[global_prof_mutex_##mtx])
MUTEX_PROF_GLOBAL_MUTEXES
#undef OP

/* Per arena mutexes */
#define OP(mtx)                                                            \
  RO_MUTEX_CTL_GEN(arenas_i_mutexes_##mtx,                                 \
    arenas_i(mib[2])                                                       \
      ->astats->astats.mutex_prof_data[arena_prof_mutex_##mtx])
MUTEX_PROF_ARENA_MUTEXES
#undef OP

/* tcache bin mutex */
RO_MUTEX_CTL_GEN(arenas_i_bins_j_mutex,
  arenas_i(mib[2])->astats->bstats[mib[4]].mutex_data)
#undef RO_MUTEX_CTL_GEN

/* Resets all mutex stats, including global, arena and bin mutexes. */
static int stats_mutexes_reset_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  if (!config_stats) { return ENOENT; }

  tsdn_t* tsdn = tsd_tsdn(tsd);

#define MUTEX_PROF_RESET(mtx)                                              \
  malloc_mutex_lock(tsdn, &mtx);                                           \
  malloc_mutex_prof_data_reset(tsdn, &mtx);                                \
  malloc_mutex_unlock(tsdn, &mtx);

  /* Global mutexes: ctl and prof. */
  MUTEX_PROF_RESET(ctl_mtx);
  if (have_background_thread) { MUTEX_PROF_RESET(background_thread_lock); }
  if (config_prof && opt_prof) { MUTEX_PROF_RESET(bt2gctx_mtx); }

  /* Per arena mutexes. */
  unsigned n = narenas_total_get();

  for (unsigned i = 0; i < n; i++) {
    arena_t* arena = arena_get(tsdn, i, false);
    if (!arena) { continue; }
    MUTEX_PROF_RESET(arena->large_mtx);
    MUTEX_PROF_RESET(arena->extent_avail_mtx);
    MUTEX_PROF_RESET(arena->extents_dirty.mtx);
    MUTEX_PROF_RESET(arena->extents_muzzy.mtx);
    MUTEX_PROF_RESET(arena->extents_retained.mtx);
    MUTEX_PROF_RESET(arena->decay_dirty.mtx);
    MUTEX_PROF_RESET(arena->decay_muzzy.mtx);
    MUTEX_PROF_RESET(arena->tcache_ql_mtx);
    MUTEX_PROF_RESET(arena->base->mtx);

    for (szind_t i = 0; i < SC_NBINS; i++) {
      for (unsigned j = 0; j < bin_infos[i].n_shards; j++) {
        bin_t* bin = &arena->bins[i].bin_shards[j];
        MUTEX_PROF_RESET(bin->lock);
      }
    }
  }
#undef MUTEX_PROF_RESET
  return 0;
}

CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nmalloc,
  arenas_i(mib[2])->astats->bstats[mib[4]].nmalloc, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_ndalloc,
  arenas_i(mib[2])->astats->bstats[mib[4]].ndalloc, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nrequests,
  arenas_i(mib[2])->astats->bstats[mib[4]].nrequests, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_curregs,
  arenas_i(mib[2])->astats->bstats[mib[4]].curregs, size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nfills,
  arenas_i(mib[2])->astats->bstats[mib[4]].nfills, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nflushes,
  arenas_i(mib[2])->astats->bstats[mib[4]].nflushes, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nslabs,
  arenas_i(mib[2])->astats->bstats[mib[4]].nslabs, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nreslabs,
  arenas_i(mib[2])->astats->bstats[mib[4]].reslabs, uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_curslabs,
  arenas_i(mib[2])->astats->bstats[mib[4]].curslabs, size_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_bins_j_nonfull_slabs,
  arenas_i(mib[2])->astats->bstats[mib[4]].nonfull_slabs, size_t)

static const ctl_named_node_t* stats_arenas_i_bins_j_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t j) {
  if (j > SC_NBINS) { return NULL; }
  return super_stats_arenas_i_bins_j_node;
}

CTL_RO_CGEN(config_stats, stats_arenas_i_lextents_j_nmalloc,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->lstats[mib[4]].nmalloc),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_lextents_j_ndalloc,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->lstats[mib[4]].ndalloc),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_lextents_j_nrequests,
  ctl_arena_stats_read_u64(
    &arenas_i(mib[2])->astats->lstats[mib[4]].nrequests),
  uint64_t)
CTL_RO_CGEN(config_stats, stats_arenas_i_lextents_j_curlextents,
  arenas_i(mib[2])->astats->lstats[mib[4]].curlextents, size_t)

static const ctl_named_node_t* stats_arenas_i_lextents_j_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t j) {
  if (j > SC_NSIZES - SC_NBINS) { return NULL; }
  return super_stats_arenas_i_lextents_j_node;
}

CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_ndirty,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->estats[mib[4]].ndirty, ATOMIC_RELAXED),
  size_t);
CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_nmuzzy,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->estats[mib[4]].nmuzzy, ATOMIC_RELAXED),
  size_t);
CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_nretained,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->estats[mib[4]].nretained, ATOMIC_RELAXED),
  size_t);
CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_dirty_bytes,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->estats[mib[4]].dirty_bytes, ATOMIC_RELAXED),
  size_t);
CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_muzzy_bytes,
  atomic_load_zu(
    &arenas_i(mib[2])->astats->estats[mib[4]].muzzy_bytes, ATOMIC_RELAXED),
  size_t);
CTL_RO_CGEN(config_stats, stats_arenas_i_extents_j_retained_bytes,
  atomic_load_zu(&arenas_i(mib[2])->astats->estats[mib[4]].retained_bytes,
    ATOMIC_RELAXED),
  size_t);

static const ctl_named_node_t* stats_arenas_i_extents_j_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t j) {
  if (j >= SC_NPSIZES) { return NULL; }
  return super_stats_arenas_i_extents_j_node;
}

static bool ctl_arenas_i_verify(size_t i) {
  size_t a = arenas_i2a_impl(i, true, true);
  if (a == UINT_MAX || !ctl_arenas->arenas[a]->initialized) { return true; }

  return false;
}

static const ctl_named_node_t* stats_arenas_i_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i) {
  const ctl_named_node_t* ret;

  malloc_mutex_lock(tsdn, &ctl_mtx);
  if (ctl_arenas_i_verify(i)) {
    ret = NULL;
    goto label_return;
  }

  ret = super_stats_arenas_i_node;
label_return:
  malloc_mutex_unlock(tsdn, &ctl_mtx);
  return ret;
}

static int experimental_hooks_install_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  if (oldp == NULL || oldlenp == NULL || newp == NULL) {
    ret = EINVAL;
    goto label_return;
  }
  /*
   * Note: this is a *private* struct.  This is an experimental interface;
   * forcing the user to know the jemalloc internals well enough to
   * extract the ABI hopefully ensures nobody gets too comfortable with
   * this API, which can change at a moment's notice.
   */
  hooks_t hooks;
  WRITE(hooks, hooks_t);
  void* handle = hook_install(tsd_tsdn(tsd), &hooks);
  if (handle == NULL) {
    ret = EAGAIN;
    goto label_return;
  }
  READ(handle, void*);

  ret = 0;
label_return:
  return ret;
}

static int experimental_hooks_remove_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;
  WRITEONLY();
  void* handle = NULL;
  WRITE(handle, void*);
  if (handle == NULL) {
    ret = EINVAL;
    goto label_return;
  }
  hook_remove(tsd_tsdn(tsd), handle);
  ret = 0;
label_return:
  return ret;
}

/*
 * Output six memory utilization entries for an input pointer, the first one
 * of type (void *) and the remaining five of type size_t, describing the
 * following (in the same order):
 *
 * (a) memory address of the extent a potential reallocation would go into,
 * == the five fields below describe about the extent the pointer resides in
 * == (b) number of free regions in the extent, (c) number of regions in the
 * extent, (d) size of the extent in terms of bytes, (e) total number of
 * free regions in the bin the extent belongs to, and (f) total number of
 * regions in the bin the extent belongs to.
 *
 * Note that "(e)" and "(f)" are only available when stats are enabled;
 * otherwise their values are undefined.
 *
 * This API is mainly intended for small class allocations, where extents
 * are used as slab.
 *
 * In case of large class allocations, "(a)" will be NULL, and "(e)" and
 * "(f)" will be zero (if stats are enabled; otherwise undefined).  The
 * other three fields will be properly set though the values are trivial:
 * "(b)" will be 0,
 * "(c)" will be 1, and "(d)" will be the usable size.
 *
 * The input pointer and size are respectively passed in by newp and newlen,
 * and the output fields and size are respectively oldp and *oldlenp.
 *
 * It can be beneficial to define the following macros to make it easier to
 * access the output:
 *
 * #define SLABCUR_READ(out) (*(void **)out)
 * #define COUNTS(out) ((size_t *)((void **)out + 1))
 * #define NFREE_READ(out) COUNTS(out)[0]
 * #define NREGS_READ(out) COUNTS(out)[1]
 * #define SIZE_READ(out) COUNTS(out)[2]
 * #define BIN_NFREE_READ(out) COUNTS(out)[3]
 * #define BIN_NREGS_READ(out) COUNTS(out)[4]
 *
 * and then write e.g. NFREE_READ(oldp) to fetch the output.  See the unit
 * test test_query in test/unit/extent_util.c for an example.
 *
 * For a typical defragmentation workflow making use of this API for
 * understanding the fragmentation level, please refer to the comment for
 * experimental_utilization_batch_query_ctl.
 *
 * It's up to the application how to determine the significance of
 * fragmentation relying on the outputs returned.  Possible choices are:
 *
 * (a) if extent utilization ratio is below certain threshold,
 * (b) if extent memory consumption is above certain threshold,
 * (c) if extent utilization ratio is significantly below bin utilization
 * ratio, (d) if input pointer deviates a lot from potential reallocation
 * address, or (e) some selection/combination of the above.
 *
 * The caller needs to make sure that the input/output arguments are valid,
 * in particular, that the size of the output is correct, i.e.:
 *
 *     *oldlenp = sizeof(void *) + sizeof(size_t) * 5
 *
 * Otherwise, the function immediately returns EINVAL without touching
 * anything.
 *
 * In the rare case where there's no associated extent found for the input
 * pointer, the function zeros out all output fields and return.  Please
 * refer to the comment for experimental_utilization_batch_query_ctl to
 * understand the motivation from C++.
 */
static int experimental_utilization_query_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  int ret;

  assert(sizeof(extent_util_stats_verbose_t)
    == sizeof(void*) + sizeof(size_t) * 5);

  if (oldp == NULL || oldlenp == NULL
    || *oldlenp != sizeof(extent_util_stats_verbose_t) || newp == NULL) {
    ret = EINVAL;
    goto label_return;
  }

  void* ptr = NULL;
  WRITE(ptr, void*);
  extent_util_stats_verbose_t* util_stats
    = (extent_util_stats_verbose_t*)oldp;
  extent_util_stats_verbose_get(tsd_tsdn(tsd), ptr, &util_stats->nfree,
    &util_stats->nregs, &util_stats->size, &util_stats->bin_nfree,
    &util_stats->bin_nregs, &util_stats->slabcur_addr);
  ret = 0;

label_return:
  return ret;
}

/*
 * Given an input array of pointers, output three memory utilization entries
 *of type size_t for each input pointer about the extent it resides in:
 *
 * (a) number of free regions in the extent,
 * (b) number of regions in the extent, and
 * (c) size of the extent in terms of bytes.
 *
 * This API is mainly intended for small class allocations, where extents
 *are used as slab.  In case of large class allocations, the outputs are
 *trivial:
 * "(a)" will be 0, "(b)" will be 1, and "(c)" will be the usable size.
 *
 * Note that multiple input pointers may reside on a same extent so the
 *output fields may contain duplicates.
 *
 * The format of the input/output looks like:
 *
 * input[0]:  1st_pointer_to_query	|  output[0]:
 *1st_extent_n_free_regions |  output[1]: 1st_extent_n_regions |  output[2]:
 *1st_extent_size input[1]:  2nd_pointer_to_query	|  output[3]:
 *2nd_extent_n_free_regions |  output[4]: 2nd_extent_n_regions |  output[5]:
 *2nd_extent_size
 * ...					|  ...
 *
 * The input array and size are respectively passed in by newp and newlen,
 *and the output array and size are respectively oldp and *oldlenp.
 *
 * It can be beneficial to define the following macros to make it easier to
 * access the output:
 *
 * #define NFREE_READ(out, i) out[(i) * 3]
 * #define NREGS_READ(out, i) out[(i) * 3 + 1]
 * #define SIZE_READ(out, i) out[(i) * 3 + 2]
 *
 * and then write e.g. NFREE_READ(oldp, i) to fetch the output.  See the
 *unit test test_batch in test/unit/extent_util.c for a concrete example.
 *
 * A typical workflow would be composed of the following steps:
 *
 * (1) flush tcache: mallctl("thread.tcache.flush", ...)
 * (2) initialize input array of pointers to query fragmentation
 * (3) allocate output array to hold utilization statistics
 * (4) query utilization: mallctl("experimental.utilization.batch_query",
 *...) (5) (optional) decide if it's worthwhile to defragment; otherwise
 *stop here (6) disable tcache: mallctl("thread.tcache.enabled", ...) (7)
 *defragment allocations with significant fragmentation, e.g.: for each
 *allocation { if it's fragmented { malloc(...); jet_memcpy(...); free(...);
 *             }
 *         }
 * (8) enable tcache: mallctl("thread.tcache.enabled", ...)
 *
 * The application can determine the significance of fragmentation
 *themselves relying on the statistics returned, both at the overall level
 *i.e. step "(5)" and at individual allocation level i.e. within step "(7)".
 *Possible choices are:
 *
 * (a) whether memory utilization ratio is below certain threshold,
 * (b) whether memory consumption is above certain threshold, or
 * (c) some combination of the two.
 *
 * The caller needs to make sure that the input/output arrays are valid and
 * their sizes are proper as well as matched, meaning:
 *
 * (a) newlen = n_pointers * sizeof(const void *)
 * (b) *oldlenp = n_pointers * sizeof(size_t) * 3
 * (c) n_pointers > 0
 *
 * Otherwise, the function immediately returns EINVAL without touching
 *anything.
 *
 * In the rare case where there's no associated extent found for some
 *pointers, rather than immediately terminating the computation and raising
 *an error, the function simply zeros out the corresponding output fields
 *and continues the computation until all input pointers are handled.  The
 *motivations of such a design are as follows:
 *
 * (a) The function always either processes nothing or processes everything,
 *and never leaves the output half touched and half untouched.
 *
 * (b) It facilitates usage needs especially common in C++.  A vast variety
 *of C++ objects are instantiated with multiple dynamic memory allocations.
 *For example, std::string and std::vector typically use at least two
 *allocations, one for the metadata and one for the actual content.  Other
 *types may use even more allocations.  When inquiring about utilization
 *statistics, the caller often wants to examine into all such allocations,
 *especially internal one(s), rather than just the topmost one.  The issue
 *comes when some implementations do certain optimizations to
 *reduce/aggregate some internal allocations, e.g. putting short strings
 *directly into the metadata, and such decisions are not known to the
 *caller.  Therefore, we permit pointers to memory usages that may not be
 *returned by previous malloc calls, and we provide the caller a convenient
 *way to identify such cases.
 */
static int experimental_utilization_batch_query_ctl(tsd_t* tsd,
  const size_t* mib, size_t miblen, void* oldp, size_t* oldlenp, void* newp,
  size_t newlen) {
  int ret;

  assert(sizeof(extent_util_stats_t) == sizeof(size_t) * 3);

  const size_t len = newlen / sizeof(const void*);
  if (oldp == NULL || oldlenp == NULL || newp == NULL || newlen == 0
    || newlen != len * sizeof(const void*)
    || *oldlenp != len * sizeof(extent_util_stats_t)) {
    ret = EINVAL;
    goto label_return;
  }

  void** ptrs = (void**)newp;
  extent_util_stats_t* util_stats = (extent_util_stats_t*)oldp;
  size_t i;
  for (i = 0; i < len; ++i) {
    extent_util_stats_get(tsd_tsdn(tsd), ptrs[i], &util_stats[i].nfree,
      &util_stats[i].nregs, &util_stats[i].size);
  }
  ret = 0;

label_return:
  return ret;
}

static const ctl_named_node_t* experimental_arenas_i_index(
  tsdn_t* tsdn, const size_t* mib, size_t miblen, size_t i) {
  const ctl_named_node_t* ret;

  malloc_mutex_lock(tsdn, &ctl_mtx);
  if (ctl_arenas_i_verify(i)) {
    ret = NULL;
    goto label_return;
  }
  ret = super_experimental_arenas_i_node;
label_return:
  malloc_mutex_unlock(tsdn, &ctl_mtx);
  return ret;
}

static int experimental_arenas_i_pactivep_ctl(tsd_t* tsd, const size_t* mib,
  size_t miblen, void* oldp, size_t* oldlenp, void* newp, size_t newlen) {
  if (!config_stats) { return ENOENT; }
  if (oldp == NULL || oldlenp == NULL || *oldlenp != sizeof(size_t*)) {
    return EINVAL;
  }

  unsigned arena_ind;
  arena_t* arena;
  int ret;
  size_t* pactivep;

  malloc_mutex_lock(tsd_tsdn(tsd), &ctl_mtx);
  READONLY();
  MIB_UNSIGNED(arena_ind, 2);
  if (arena_ind < narenas_total_get()
    && (arena = arena_get(tsd_tsdn(tsd), arena_ind, false)) != NULL) {
#if defined(JEMALLOC_GCC_ATOMIC_ATOMICS)                                   \
  || defined(JEMALLOC_GCC_SYNC_ATOMICS) || defined(_MSC_VER)
    /* Expose the underlying counter for fast read. */
    pactivep = (size_t*)&(arena->nactive.repr);
    READ(pactivep, size_t*);
    ret = 0;
#else
    ret = EFAULT;
#endif
  } else {
    ret = EFAULT;
  }
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &ctl_mtx);
  return ret;
}

#include "jemalloc/internal/div.h"

/*
 * Suppose we have n = q * d, all integers. We know n and d, and want q = n
 * / d.
 *
 * For any k, we have (here, all division is exact; not C-style rounding):
 * floor(ceil(2^k / d) * n / 2^k) = floor((2^k + r) / d * n / 2^k), where
 * r = (-2^k) mod d.
 *
 * Expanding this out:
 * ... = floor(2^k / d * n / 2^k + r / d * n / 2^k)
 *     = floor(n / d + (r / d) * (n / 2^k)).
 *
 * The fractional part of n / d is 0 (because of the assumption that d
 * divides n exactly), so we have:
 * ... = n / d + floor((r / d) * (n / 2^k))
 *
 * So that our initial expression is equal to the quantity we seek, so long
 * as (r / d) * (n / 2^k) < 1.
 *
 * r is a remainder mod d, so r < d and r / d < 1 always. We can make
 * n / 2 ^ k < 1 by setting k = 32. This gets us a value of magic that
 * works.
 */

void div_init(div_info_t* div_info, size_t d) {
  /* Nonsensical. */
  assert(d != 0);
  /*
   * This would make the value of magic too high to fit into a uint32_t
   * (we would want magic = 2^32 exactly). This would mess with code gen
   * on 32-bit machines.
   */
  assert(d != 1);

  uint64_t two_to_k = ((uint64_t)1 << 32);
  uint32_t magic = (uint32_t)(two_to_k / d);

  /*
   * We want magic = ceil(2^k / d), but C gives us floor. We have to
   * increment it unless the result was exact (i.e. unless d is a power of
   * two).
   */
  if (two_to_k % d != 0) { magic++; }
  div_info->magic = magic;
#ifdef JEMALLOC_DEBUG
  div_info->d = d;
#endif
}
#define JEMALLOC_EXTENT_C_

#include "jemalloc/internal/extent_dss.h"
#include "jemalloc/internal/extent_mmap.h"
#include "jemalloc/internal/ph.h"

#include "jemalloc/internal/mutex_pool.h"

/******************************************************************************/
/* Data. */

rtree_t extents_rtree;
/* Keyed by the address of the extent_t being protected. */
mutex_pool_t extent_mutex_pool;

size_t opt_lg_extent_max_active_fit = LG_EXTENT_MAX_ACTIVE_FIT_DEFAULT;

static const bitmap_info_t extents_bitmap_info
  = BITMAP_INFO_INITIALIZER(SC_NPSIZES + 1);

static void* extent_alloc_default(extent_hooks_t* extent_hooks,
  void* new_addr, size_t size, size_t alignment, bool* zero, bool* commit,
  unsigned arena_ind);
static bool extent_dalloc_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, bool committed, unsigned arena_ind);
static void extent_destroy_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, bool committed, unsigned arena_ind);
static bool extent_commit_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, size_t offset, size_t length, unsigned arena_ind);
static bool extent_commit_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained);
static bool extent_decommit_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind);
#ifdef PAGES_CAN_PURGE_LAZY
static bool extent_purge_lazy_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind);
#endif
static bool extent_purge_lazy_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained);
#ifdef PAGES_CAN_PURGE_FORCED
static bool extent_purge_forced_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind);
#endif
static bool extent_purge_forced_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained);
static bool extent_split_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, size_t size_a, size_t size_b, bool committed,
  unsigned arena_ind);
static extent_t* extent_split_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t size_a,
  szind_t szind_a, bool slab_a, size_t size_b, szind_t szind_b, bool slab_b,
  bool growing_retained);
static bool extent_merge_default(extent_hooks_t* extent_hooks, void* addr_a,
  size_t size_a, void* addr_b, size_t size_b, bool committed,
  unsigned arena_ind);
static bool extent_merge_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* a, extent_t* b,
  bool growing_retained);

const extent_hooks_t extent_hooks_default
  = { extent_alloc_default, extent_dalloc_default, extent_destroy_default,
      extent_commit_default, extent_decommit_default
#ifdef PAGES_CAN_PURGE_LAZY
      ,
      extent_purge_lazy_default
#else
      ,
      NULL
#endif
#ifdef PAGES_CAN_PURGE_FORCED
      ,
      extent_purge_forced_default
#else
      ,
      NULL
#endif
      ,
      extent_split_default, extent_merge_default };

/* Used exclusively for gdump triggering. */
static atomic_zu_t curpages;
static atomic_zu_t highpages;

/******************************************************************************/
/*
 * Function prototypes for static functions that are referenced prior to
 * definition.
 */

static void extent_deregister(tsdn_t* tsdn, extent_t* extent);
static extent_t* extent_recycle(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, void* new_addr,
  size_t usize, size_t pad, size_t alignment, bool slab, szind_t szind,
  bool* zero, bool* commit, bool growing_retained);
static extent_t* extent_try_coalesce(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, extent_t* extent, bool* coalesced,
  bool growing_retained);
static void extent_record(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, extent_t* extent,
  bool growing_retained);

/******************************************************************************/

#define ATTR_NONE /* does nothing */

ph_gen(ATTR_NONE, extent_avail_, extent_tree_t, extent_t, ph_link,
  extent_esnead_comp)

#undef ATTR_NONE

  typedef enum {
    lock_result_success,
    lock_result_failure,
    lock_result_no_extent
  } lock_result_t;

static lock_result_t extent_rtree_leaf_elm_try_lock(tsdn_t* tsdn,
  rtree_leaf_elm_t* elm, extent_t** result, bool inactive_only) {
  extent_t* extent1
    = rtree_leaf_elm_extent_read(tsdn, &extents_rtree, elm, true);

  /* Slab implies active extents and should be skipped. */
  if (extent1 == NULL
    || (inactive_only
      && rtree_leaf_elm_slab_read(tsdn, &extents_rtree, elm, true))) {
    return lock_result_no_extent;
  }

  /*
   * It's possible that the extent changed out from under us, and with it
   * the leaf->extent mapping.  We have to recheck while holding the lock.
   */
  extent_lock(tsdn, extent1);
  extent_t* extent2
    = rtree_leaf_elm_extent_read(tsdn, &extents_rtree, elm, true);

  if (extent1 == extent2) {
    *result = extent1;
    return lock_result_success;
  } else {
    extent_unlock(tsdn, extent1);
    return lock_result_failure;
  }
}

/*
 * Returns a pool-locked extent_t * if there's one associated with the given
 * address, and NULL otherwise.
 */
static extent_t* extent_lock_from_addr(
  tsdn_t* tsdn, rtree_ctx_t* rtree_ctx, void* addr, bool inactive_only) {
  extent_t* ret = NULL;
  rtree_leaf_elm_t* elm = rtree_leaf_elm_lookup(
    tsdn, &extents_rtree, rtree_ctx, (uintptr_t)addr, false, false);
  if (elm == NULL) { return NULL; }
  lock_result_t lock_result;
  do {
    lock_result
      = extent_rtree_leaf_elm_try_lock(tsdn, elm, &ret, inactive_only);
  } while (lock_result == lock_result_failure);
  return ret;
}

extent_t* extent_alloc(tsdn_t* tsdn, arena_t* arena) {
  malloc_mutex_lock(tsdn, &arena->extent_avail_mtx);
  extent_t* extent = extent_avail_first(&arena->extent_avail);
  if (extent == NULL) {
    malloc_mutex_unlock(tsdn, &arena->extent_avail_mtx);
    return base_alloc_extent(tsdn, arena->base);
  }
  extent_avail_remove(&arena->extent_avail, extent);
  atomic_fetch_sub_zu(&arena->extent_avail_cnt, 1, ATOMIC_RELAXED);
  malloc_mutex_unlock(tsdn, &arena->extent_avail_mtx);
  return extent;
}

void extent_dalloc(tsdn_t* tsdn, arena_t* arena, extent_t* extent) {
  malloc_mutex_lock(tsdn, &arena->extent_avail_mtx);
  extent_avail_insert(&arena->extent_avail, extent);
  atomic_fetch_add_zu(&arena->extent_avail_cnt, 1, ATOMIC_RELAXED);
  malloc_mutex_unlock(tsdn, &arena->extent_avail_mtx);
}

extent_hooks_t* extent_hooks_get(arena_t* arena) {
  return base_extent_hooks_get(arena->base);
}

extent_hooks_t* extent_hooks_set(
  tsd_t* tsd, arena_t* arena, extent_hooks_t* extent_hooks) {
  background_thread_info_t* info;
  if (have_background_thread) {
    info = arena_background_thread_info_get(arena);
    malloc_mutex_lock(tsd_tsdn(tsd), &info->mtx);
  }
  extent_hooks_t* ret = base_extent_hooks_set(arena->base, extent_hooks);
  if (have_background_thread) {
    malloc_mutex_unlock(tsd_tsdn(tsd), &info->mtx);
  }

  return ret;
}

static void extent_hooks_assure_initialized(
  arena_t* arena, extent_hooks_t** r_extent_hooks) {
  if (*r_extent_hooks == EXTENT_HOOKS_INITIALIZER) {
    *r_extent_hooks = extent_hooks_get(arena);
  }
}

#ifndef JEMALLOC_JET
static
#endif
  size_t
  extent_size_quantize_floor(size_t size) {
  size_t ret;
  pszind_t pind;

  assert(size > 0);
  assert((size & PAGE_MASK) == 0);

  pind = sz_psz2ind(size - sz_large_pad + 1);
  if (pind == 0) {
    /*
     * Avoid underflow.  This short-circuit would also do the right
     * thing for all sizes in the range for which there are
     * PAGE-spaced size classes, but it's simplest to just handle
     * the one case that would cause erroneous results.
     */
    return size;
  }
  ret = sz_pind2sz(pind - 1) + sz_large_pad;
  assert(ret <= size);
  return ret;
}

#ifndef JEMALLOC_JET
static
#endif
  size_t
  extent_size_quantize_ceil(size_t size) {
  size_t ret;

  assert(size > 0);
  assert(size - sz_large_pad <= SC_LARGE_MAXCLASS);
  assert((size & PAGE_MASK) == 0);

  ret = extent_size_quantize_floor(size);
  if (ret < size) {
    /*
     * Skip a quantization that may have an adequately large extent,
     * because under-sized extents may be mixed in.  This only
     * happens when an unusual size is requested, i.e. for aligned
     * allocation, and is just one of several places where linear
     * search would potentially find sufficiently aligned available
     * memory somewhere lower.
     */
    ret = sz_pind2sz(sz_psz2ind(ret - sz_large_pad + 1)) + sz_large_pad;
  }
  return ret;
}

/* Generate pairing heap functions. */
ph_gen(, extent_heap_, extent_heap_t, extent_t, ph_link, extent_snad_comp)

  bool extents_init(tsdn_t* tsdn, extents_t* extents, extent_state_t state,
    bool delay_coalesce) {
  if (malloc_mutex_init(&extents->mtx, "extents", WITNESS_RANK_EXTENTS,
        malloc_mutex_rank_exclusive)) {
    return true;
  }
  for (unsigned i = 0; i < SC_NPSIZES + 1; i++) {
    extent_heap_new(&extents->heaps[i]);
  }
  bitmap_init(extents->bitmap, &extents_bitmap_info, true);
  extent_list_init(&extents->lru);
  atomic_store_zu(&extents->npages, 0, ATOMIC_RELAXED);
  extents->state = state;
  extents->delay_coalesce = delay_coalesce;
  return false;
}

extent_state_t extents_state_get(const extents_t* extents) {
  return extents->state;
}

size_t extents_npages_get(extents_t* extents) {
  return atomic_load_zu(&extents->npages, ATOMIC_RELAXED);
}

size_t extents_nextents_get(extents_t* extents, pszind_t pind) {
  return atomic_load_zu(&extents->nextents[pind], ATOMIC_RELAXED);
}

size_t extents_nbytes_get(extents_t* extents, pszind_t pind) {
  return atomic_load_zu(&extents->nbytes[pind], ATOMIC_RELAXED);
}

static void extents_stats_add(extents_t* extent, pszind_t pind, size_t sz) {
  size_t cur = atomic_load_zu(&extent->nextents[pind], ATOMIC_RELAXED);
  atomic_store_zu(&extent->nextents[pind], cur + 1, ATOMIC_RELAXED);
  cur = atomic_load_zu(&extent->nbytes[pind], ATOMIC_RELAXED);
  atomic_store_zu(&extent->nbytes[pind], cur + sz, ATOMIC_RELAXED);
}

static void extents_stats_sub(extents_t* extent, pszind_t pind, size_t sz) {
  size_t cur = atomic_load_zu(&extent->nextents[pind], ATOMIC_RELAXED);
  atomic_store_zu(&extent->nextents[pind], cur - 1, ATOMIC_RELAXED);
  cur = atomic_load_zu(&extent->nbytes[pind], ATOMIC_RELAXED);
  atomic_store_zu(&extent->nbytes[pind], cur - sz, ATOMIC_RELAXED);
}

static void extents_insert_locked(
  tsdn_t* tsdn, extents_t* extents, extent_t* extent) {
  malloc_mutex_assert_owner(tsdn, &extents->mtx);
  assert(extent_state_get(extent) == extents->state);

  size_t size = extent_size_get(extent);
  size_t psz = extent_size_quantize_floor(size);
  pszind_t pind = sz_psz2ind(psz);
  if (extent_heap_empty(&extents->heaps[pind])) {
    bitmap_unset(extents->bitmap, &extents_bitmap_info, (size_t)pind);
  }
  extent_heap_insert(&extents->heaps[pind], extent);

  if (config_stats) { extents_stats_add(extents, pind, size); }

  extent_list_append(&extents->lru, extent);
  size_t npages = size >> LG_PAGE;
  /*
   * All modifications to npages hold the mutex (as asserted above), so we
   * don't need an atomic fetch-add; we can get by with a load followed by
   * a store.
   */
  size_t cur_extents_npages
    = atomic_load_zu(&extents->npages, ATOMIC_RELAXED);
  atomic_store_zu(
    &extents->npages, cur_extents_npages + npages, ATOMIC_RELAXED);
}

static void extents_remove_locked(
  tsdn_t* tsdn, extents_t* extents, extent_t* extent) {
  malloc_mutex_assert_owner(tsdn, &extents->mtx);
  assert(extent_state_get(extent) == extents->state);

  size_t size = extent_size_get(extent);
  size_t psz = extent_size_quantize_floor(size);
  pszind_t pind = sz_psz2ind(psz);
  extent_heap_remove(&extents->heaps[pind], extent);

  if (config_stats) { extents_stats_sub(extents, pind, size); }

  if (extent_heap_empty(&extents->heaps[pind])) {
    bitmap_set(extents->bitmap, &extents_bitmap_info, (size_t)pind);
  }
  extent_list_remove(&extents->lru, extent);
  size_t npages = size >> LG_PAGE;
  /*
   * As in extents_insert_locked, we hold extents->mtx and so don't need
   * atomic operations for updating extents->npages.
   */
  size_t cur_extents_npages
    = atomic_load_zu(&extents->npages, ATOMIC_RELAXED);
  assert(cur_extents_npages >= npages);
  atomic_store_zu(&extents->npages, cur_extents_npages - (size >> LG_PAGE),
    ATOMIC_RELAXED);
}

/*
 * Find an extent with size [min_size, max_size) to satisfy the alignment
 * requirement.  For each size, try only the first extent in the heap.
 */
static extent_t* extents_fit_alignment(
  extents_t* extents, size_t min_size, size_t max_size, size_t alignment) {
  pszind_t pind = sz_psz2ind(extent_size_quantize_ceil(min_size));
  pszind_t pind_max = sz_psz2ind(extent_size_quantize_ceil(max_size));

  for (pszind_t i = (pszind_t)bitmap_ffu(
         extents->bitmap, &extents_bitmap_info, (size_t)pind);
       i < pind_max; i = (pszind_t)bitmap_ffu(extents->bitmap,
                       &extents_bitmap_info, (size_t)i + 1)) {
    assert(i < SC_NPSIZES);
    assert(!extent_heap_empty(&extents->heaps[i]));
    extent_t* extent = extent_heap_first(&extents->heaps[i]);
    uintptr_t base = (uintptr_t)extent_base_get(extent);
    size_t candidate_size = extent_size_get(extent);
    assert(candidate_size >= min_size);

    uintptr_t next_align
      = ALIGNMENT_CEILING((uintptr_t)base, PAGE_CEILING(alignment));
    if (base > next_align || base + candidate_size <= next_align) {
      /* Overflow or not crossing the next alignment. */
      continue;
    }

    size_t leadsize = next_align - base;
    if (candidate_size - leadsize >= min_size) { return extent; }
  }

  return NULL;
}

/*
 * Do first-fit extent selection, i.e. select the oldest/lowest extent that
 * is large enough.
 */
static extent_t* extents_first_fit_locked(
  tsdn_t* tsdn, arena_t* arena, extents_t* extents, size_t size) {
  extent_t* ret = NULL;

  pszind_t pind = sz_psz2ind(extent_size_quantize_ceil(size));

  if (!maps_coalesce && !opt_retain) {
    /*
     * No split / merge allowed (Windows w/o retain). Try exact fit
     * only.
     */
    return extent_heap_empty(&extents->heaps[pind])
      ? NULL
      : extent_heap_first(&extents->heaps[pind]);
  }

  for (pszind_t i = (pszind_t)bitmap_ffu(
         extents->bitmap, &extents_bitmap_info, (size_t)pind);
       i < SC_NPSIZES + 1; i = (pszind_t)bitmap_ffu(extents->bitmap,
                             &extents_bitmap_info, (size_t)i + 1)) {
    assert(!extent_heap_empty(&extents->heaps[i]));
    extent_t* extent = extent_heap_first(&extents->heaps[i]);
    assert(extent_size_get(extent) >= size);
    /*
     * In order to reduce fragmentation, avoid reusing and splitting
     * large extents for much smaller sizes.
     *
     * Only do check for dirty extents (delay_coalesce).
     */
    if (extents->delay_coalesce
      && (sz_pind2sz(i) >> opt_lg_extent_max_active_fit) > size) {
      break;
    }
    if (ret == NULL || extent_snad_comp(extent, ret) < 0) { ret = extent; }
    if (i == SC_NPSIZES) { break; }
    assert(i < SC_NPSIZES);
  }

  return ret;
}

/*
 * Do first-fit extent selection, where the selection policy choice is
 * based on extents->delay_coalesce.
 */
static extent_t* extents_fit_locked(tsdn_t* tsdn, arena_t* arena,
  extents_t* extents, size_t esize, size_t alignment) {
  malloc_mutex_assert_owner(tsdn, &extents->mtx);

  size_t max_size = esize + PAGE_CEILING(alignment) - PAGE;
  /* Beware size_t wrap-around. */
  if (max_size < esize) { return NULL; }

  extent_t* extent
    = extents_first_fit_locked(tsdn, arena, extents, max_size);

  if (alignment > PAGE && extent == NULL) {
    /*
     * max_size guarantees the alignment requirement but is rather
     * pessimistic.  Next we try to satisfy the aligned allocation
     * with sizes in [esize, max_size).
     */
    extent = extents_fit_alignment(extents, esize, max_size, alignment);
  }

  return extent;
}

static bool extent_try_delayed_coalesce(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, extent_t* extent) {
  extent_state_set(extent, extent_state_active);
  bool coalesced;
  extent = extent_try_coalesce(tsdn, arena, r_extent_hooks, rtree_ctx,
    extents, extent, &coalesced, false);
  extent_state_set(extent, extents_state_get(extents));

  if (!coalesced) { return true; }
  extents_insert_locked(tsdn, extents, extent);
  return false;
}

extent_t* extents_alloc(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, void* new_addr,
  size_t size, size_t pad, size_t alignment, bool slab, szind_t szind,
  bool* zero, bool* commit) {
  assert(size + pad != 0);
  assert(alignment != 0);
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_t* extent = extent_recycle(tsdn, arena, r_extent_hooks, extents,
    new_addr, size, pad, alignment, slab, szind, zero, commit, false);
  assert(extent == NULL || extent_dumpable_get(extent));
  return extent;
}

void extents_dalloc(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, extent_t* extent) {
  assert(extent_base_get(extent) != NULL);
  assert(extent_size_get(extent) != 0);
  assert(extent_dumpable_get(extent));
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_addr_set(extent, extent_base_get(extent));
  extent_zeroed_set(extent, false);

  extent_record(tsdn, arena, r_extent_hooks, extents, extent, false);
}

extent_t* extents_evict(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, size_t npages_min) {
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  malloc_mutex_lock(tsdn, &extents->mtx);

  /*
   * Get the LRU coalesced extent, if any.  If coalescing was delayed,
   * the loop will iterate until the LRU extent is fully coalesced.
   */
  extent_t* extent;
  while (true) {
    /* Get the LRU extent, if any. */
    extent = extent_list_first(&extents->lru);
    if (extent == NULL) { goto label_return; }
    /* Check the eviction limit. */
    size_t extents_npages
      = atomic_load_zu(&extents->npages, ATOMIC_RELAXED);
    if (extents_npages <= npages_min) {
      extent = NULL;
      goto label_return;
    }
    extents_remove_locked(tsdn, extents, extent);
    if (!extents->delay_coalesce) { break; }
    /* Try to coalesce. */
    if (extent_try_delayed_coalesce(
          tsdn, arena, r_extent_hooks, rtree_ctx, extents, extent)) {
      break;
    }
    /*
     * The LRU extent was just coalesced and the result placed in
     * the LRU at its neighbor's position.  Start over.
     */
  }

  /*
   * Either mark the extent active or deregister it to protect against
   * concurrent operations.
   */
  switch (extents_state_get(extents)) {
  case extent_state_active: not_reached();
  case extent_state_dirty:
  case extent_state_muzzy:
    extent_state_set(extent, extent_state_active);
    break;
  case extent_state_retained: extent_deregister(tsdn, extent); break;
  default: not_reached();
  }

label_return:
  malloc_mutex_unlock(tsdn, &extents->mtx);
  return extent;
}

/*
 * This can only happen when we fail to allocate a new extent struct (which
 * indicates OOM), e.g. when trying to split an existing extent.
 */
static void extents_abandon_vm(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, extent_t* extent,
  bool growing_retained) {
  size_t sz = extent_size_get(extent);
  if (config_stats) {
    arena_stats_accum_zu(&arena->stats.abandoned_vm, sz);
  }
  /*
   * Leak extent after making sure its pages have already been purged, so
   * that this is only a virtual memory leak.
   */
  if (extents_state_get(extents) == extent_state_dirty) {
    if (extent_purge_lazy_impl(
          tsdn, arena, r_extent_hooks, extent, 0, sz, growing_retained)) {
      extent_purge_forced_impl(tsdn, arena, r_extent_hooks, extent, 0,
        extent_size_get(extent), growing_retained);
    }
  }
  extent_dalloc(tsdn, arena, extent);
}

void extents_prefork(tsdn_t* tsdn, extents_t* extents) {
  malloc_mutex_prefork(tsdn, &extents->mtx);
}

void extents_postfork_parent(tsdn_t* tsdn, extents_t* extents) {
  malloc_mutex_postfork_parent(tsdn, &extents->mtx);
}

void extents_postfork_child(tsdn_t* tsdn, extents_t* extents) {
  malloc_mutex_postfork_child(tsdn, &extents->mtx);
}

static void extent_deactivate_locked(
  tsdn_t* tsdn, arena_t* arena, extents_t* extents, extent_t* extent) {
  assert(extent_arena_get(extent) == arena);
  assert(extent_state_get(extent) == extent_state_active);

  extent_state_set(extent, extents_state_get(extents));
  extents_insert_locked(tsdn, extents, extent);
}

static void extent_deactivate(
  tsdn_t* tsdn, arena_t* arena, extents_t* extents, extent_t* extent) {
  malloc_mutex_lock(tsdn, &extents->mtx);
  extent_deactivate_locked(tsdn, arena, extents, extent);
  malloc_mutex_unlock(tsdn, &extents->mtx);
}

static void extent_activate_locked(
  tsdn_t* tsdn, arena_t* arena, extents_t* extents, extent_t* extent) {
  assert(extent_arena_get(extent) == arena);
  assert(extent_state_get(extent) == extents_state_get(extents));

  extents_remove_locked(tsdn, extents, extent);
  extent_state_set(extent, extent_state_active);
}

static bool extent_rtree_leaf_elms_lookup(tsdn_t* tsdn,
  rtree_ctx_t* rtree_ctx, const extent_t* extent, bool dependent,
  bool init_missing, rtree_leaf_elm_t** r_elm_a,
  rtree_leaf_elm_t** r_elm_b) {
  *r_elm_a = rtree_leaf_elm_lookup(tsdn, &extents_rtree, rtree_ctx,
    (uintptr_t)extent_base_get(extent), dependent, init_missing);
  if (!dependent && *r_elm_a == NULL) { return true; }
  assert(*r_elm_a != NULL);

  *r_elm_b = rtree_leaf_elm_lookup(tsdn, &extents_rtree, rtree_ctx,
    (uintptr_t)extent_last_get(extent), dependent, init_missing);
  if (!dependent && *r_elm_b == NULL) { return true; }
  assert(*r_elm_b != NULL);

  return false;
}

static void extent_rtree_write_acquired(tsdn_t* tsdn,
  rtree_leaf_elm_t* elm_a, rtree_leaf_elm_t* elm_b, extent_t* extent,
  szind_t szind, bool slab) {
  rtree_leaf_elm_write(tsdn, &extents_rtree, elm_a, extent, szind, slab);
  if (elm_b != NULL) {
    rtree_leaf_elm_write(tsdn, &extents_rtree, elm_b, extent, szind, slab);
  }
}

static void extent_interior_register(
  tsdn_t* tsdn, rtree_ctx_t* rtree_ctx, extent_t* extent, szind_t szind) {
  assert(extent_slab_get(extent));

  /* Register interior. */
  for (size_t i = 1; i < (extent_size_get(extent) >> LG_PAGE) - 1; i++) {
    rtree_write(tsdn, &extents_rtree, rtree_ctx,
      (uintptr_t)extent_base_get(extent) + (uintptr_t)(i << LG_PAGE),
      extent, szind, true);
  }
}

static void extent_gdump_add(tsdn_t* tsdn, const extent_t* extent) {
  cassert(config_prof);
  /* prof_gdump() requirement. */
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  if (opt_prof && extent_state_get(extent) == extent_state_active) {
    size_t nadd = extent_size_get(extent) >> LG_PAGE;
    size_t cur
      = atomic_fetch_add_zu(&curpages, nadd, ATOMIC_RELAXED) + nadd;
    size_t high = atomic_load_zu(&highpages, ATOMIC_RELAXED);
    while (cur > high
      && !atomic_compare_exchange_weak_zu(
        &highpages, &high, cur, ATOMIC_RELAXED, ATOMIC_RELAXED)) {
      /*
       * Don't refresh cur, because it may have decreased
       * since this thread lost the highpages update race.
       * Note that high is updated in case of CAS failure.
       */
    }
    if (cur > high && prof_gdump_get_unlocked()) { prof_gdump(tsdn); }
  }
}

static void extent_gdump_sub(tsdn_t* tsdn, const extent_t* extent) {
  cassert(config_prof);

  if (opt_prof && extent_state_get(extent) == extent_state_active) {
    size_t nsub = extent_size_get(extent) >> LG_PAGE;
    assert(atomic_load_zu(&curpages, ATOMIC_RELAXED) >= nsub);
    atomic_fetch_sub_zu(&curpages, nsub, ATOMIC_RELAXED);
  }
}

static bool extent_register_impl(
  tsdn_t* tsdn, extent_t* extent, bool gdump_add) {
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  rtree_leaf_elm_t *elm_a, *elm_b;

  /*
   * We need to hold the lock to protect against a concurrent coalesce
   * operation that sees us in a partial state.
   */
  extent_lock(tsdn, extent);

  if (extent_rtree_leaf_elms_lookup(
        tsdn, rtree_ctx, extent, false, true, &elm_a, &elm_b)) {
    extent_unlock(tsdn, extent);
    return true;
  }

  szind_t szind = extent_szind_get_maybe_invalid(extent);
  bool slab = extent_slab_get(extent);
  extent_rtree_write_acquired(tsdn, elm_a, elm_b, extent, szind, slab);
  if (slab) { extent_interior_register(tsdn, rtree_ctx, extent, szind); }

  extent_unlock(tsdn, extent);

  if (config_prof && gdump_add) { extent_gdump_add(tsdn, extent); }

  return false;
}

static bool extent_register(tsdn_t* tsdn, extent_t* extent) {
  return extent_register_impl(tsdn, extent, true);
}

static bool extent_register_no_gdump_add(tsdn_t* tsdn, extent_t* extent) {
  return extent_register_impl(tsdn, extent, false);
}

static void extent_reregister(tsdn_t* tsdn, extent_t* extent) {
  bool err = extent_register(tsdn, extent);
  assert(!err);
}

/*
 * Removes all pointers to the given extent from the global rtree indices
 * for its interior.  This is relevant for slab extents, for which we need
 * to do metadata lookups at places other than the head of the extent.  We
 * deregister on the interior, then, when an extent moves from being an
 * active slab to an inactive state.
 */
static void extent_interior_deregister(
  tsdn_t* tsdn, rtree_ctx_t* rtree_ctx, extent_t* extent) {
  size_t i;

  assert(extent_slab_get(extent));

  for (i = 1; i < (extent_size_get(extent) >> LG_PAGE) - 1; i++) {
    rtree_clear(tsdn, &extents_rtree, rtree_ctx,
      (uintptr_t)extent_base_get(extent) + (uintptr_t)(i << LG_PAGE));
  }
}

/*
 * Removes all pointers to the given extent from the global rtree.
 */
static void extent_deregister_impl(
  tsdn_t* tsdn, extent_t* extent, bool gdump) {
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  rtree_leaf_elm_t *elm_a, *elm_b;
  extent_rtree_leaf_elms_lookup(
    tsdn, rtree_ctx, extent, true, false, &elm_a, &elm_b);

  extent_lock(tsdn, extent);

  extent_rtree_write_acquired(tsdn, elm_a, elm_b, NULL, SC_NSIZES, false);
  if (extent_slab_get(extent)) {
    extent_interior_deregister(tsdn, rtree_ctx, extent);
    extent_slab_set(extent, false);
  }

  extent_unlock(tsdn, extent);

  if (config_prof && gdump) { extent_gdump_sub(tsdn, extent); }
}

static void extent_deregister(tsdn_t* tsdn, extent_t* extent) {
  extent_deregister_impl(tsdn, extent, true);
}

static void extent_deregister_no_gdump_sub(tsdn_t* tsdn, extent_t* extent) {
  extent_deregister_impl(tsdn, extent, false);
}

/*
 * Tries to find and remove an extent from extents that can be used for the
 * given allocation request.
 */
static extent_t* extent_recycle_extract(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, void* new_addr, size_t size, size_t pad,
  size_t alignment, bool slab, bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);
  assert(alignment > 0);
  if (config_debug && new_addr != NULL) {
    /*
     * Non-NULL new_addr has two use cases:
     *
     *   1) Recycle a known-extant extent, e.g. during purging.
     *   2) Perform in-place expanding reallocation.
     *
     * Regardless of use case, new_addr must either refer to a
     * non-existing extent, or to the base of an extant extent,
     * since only active slabs support interior lookups (which of
     * course cannot be recycled).
     */
    assert(PAGE_ADDR2BASE(new_addr) == new_addr);
    assert(pad == 0);
    assert(alignment <= PAGE);
  }

  size_t esize = size + pad;
  malloc_mutex_lock(tsdn, &extents->mtx);
  extent_hooks_assure_initialized(arena, r_extent_hooks);
  extent_t* extent;
  if (new_addr != NULL) {
    extent = extent_lock_from_addr(tsdn, rtree_ctx, new_addr, false);
    if (extent != NULL) {
      /*
       * We might null-out extent to report an error, but we
       * still need to unlock the associated mutex after.
       */
      extent_t* unlock_extent = extent;
      assert(extent_base_get(extent) == new_addr);
      if (extent_arena_get(extent) != arena
        || extent_size_get(extent) < esize
        || extent_state_get(extent) != extents_state_get(extents)) {
        extent = NULL;
      }
      extent_unlock(tsdn, unlock_extent);
    }
  } else {
    extent = extents_fit_locked(tsdn, arena, extents, esize, alignment);
  }
  if (extent == NULL) {
    malloc_mutex_unlock(tsdn, &extents->mtx);
    return NULL;
  }

  extent_activate_locked(tsdn, arena, extents, extent);
  malloc_mutex_unlock(tsdn, &extents->mtx);

  return extent;
}

/*
 * Given an allocation request and an extent guaranteed to be able to
 * satisfy it, this splits off lead and trail extents, leaving extent
 * pointing to an extent satisfying the allocation. This function doesn't
 * put lead or trail into any extents_t; it's the caller's job to ensure
 * that they can be reused.
 */
typedef enum {
  /*
   * Split successfully.  lead, extent, and trail, are modified to extents
   * describing the ranges before, in, and after the given allocation.
   */
  extent_split_interior_ok,
  /*
   * The extent can't satisfy the given allocation request.  None of the
   * input extent_t *s are touched.
   */
  extent_split_interior_cant_alloc,
  /*
   * In a potentially invalid state.  Must leak (if *to_leak is non-NULL),
   * and salvage what's still salvageable (if *to_salvage is non-NULL).
   * None of lead, extent, or trail are valid.
   */
  extent_split_interior_error
} extent_split_interior_result_t;

static extent_split_interior_result_t extent_split_interior(tsdn_t* tsdn,
  arena_t* arena, extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  /* The result of splitting, in case of success. */
  extent_t** extent, extent_t** lead, extent_t** trail,
  /* The mess to clean up, in case of error. */
  extent_t** to_leak, extent_t** to_salvage, void* new_addr, size_t size,
  size_t pad, size_t alignment, bool slab, szind_t szind,
  bool growing_retained) {
  size_t esize = size + pad;
  size_t leadsize = ALIGNMENT_CEILING((uintptr_t)extent_base_get(*extent),
                      PAGE_CEILING(alignment))
    - (uintptr_t)extent_base_get(*extent);
  assert(new_addr == NULL || leadsize == 0);
  if (extent_size_get(*extent) < leadsize + esize) {
    return extent_split_interior_cant_alloc;
  }
  size_t trailsize = extent_size_get(*extent) - leadsize - esize;

  *lead = NULL;
  *trail = NULL;
  *to_leak = NULL;
  *to_salvage = NULL;

  /* Split the lead. */
  if (leadsize != 0) {
    *lead = *extent;
    *extent
      = extent_split_impl(tsdn, arena, r_extent_hooks, *lead, leadsize,
        SC_NSIZES, false, esize + trailsize, szind, slab, growing_retained);
    if (*extent == NULL) {
      *to_leak = *lead;
      *lead = NULL;
      return extent_split_interior_error;
    }
  }

  /* Split the trail. */
  if (trailsize != 0) {
    *trail = extent_split_impl(tsdn, arena, r_extent_hooks, *extent, esize,
      szind, slab, trailsize, SC_NSIZES, false, growing_retained);
    if (*trail == NULL) {
      *to_leak = *extent;
      *to_salvage = *lead;
      *lead = NULL;
      *extent = NULL;
      return extent_split_interior_error;
    }
  }

  if (leadsize == 0 && trailsize == 0) {
    /*
     * Splitting causes szind to be set as a side effect, but no
     * splitting occurred.
     */
    extent_szind_set(*extent, szind);
    if (szind != SC_NSIZES) {
      rtree_szind_slab_update(tsdn, &extents_rtree, rtree_ctx,
        (uintptr_t)extent_addr_get(*extent), szind, slab);
      if (slab && extent_size_get(*extent) > PAGE) {
        rtree_szind_slab_update(tsdn, &extents_rtree, rtree_ctx,
          (uintptr_t)extent_past_get(*extent) - (uintptr_t)PAGE, szind,
          slab);
      }
    }
  }

  return extent_split_interior_ok;
}

/*
 * This fulfills the indicated allocation request out of the given extent
 * (which the caller should have ensured was big enough).  If there's any
 * unused space before or after the resulting allocation, that space is
 * given its own extent and put back into extents.
 */
static extent_t* extent_recycle_split(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, void* new_addr, size_t size, size_t pad,
  size_t alignment, bool slab, szind_t szind, extent_t* extent,
  bool growing_retained) {
  extent_t* lead;
  extent_t* trail;
  extent_t* to_leak;
  extent_t* to_salvage;

  extent_split_interior_result_t result
    = extent_split_interior(tsdn, arena, r_extent_hooks, rtree_ctx, &extent,
      &lead, &trail, &to_leak, &to_salvage, new_addr, size, pad, alignment,
      slab, szind, growing_retained);

  if (!maps_coalesce && result != extent_split_interior_ok && !opt_retain) {
    /*
     * Split isn't supported (implies Windows w/o retain).  Avoid
     * leaking the extents.
     */
    assert(to_leak != NULL && lead == NULL && trail == NULL);
    extent_deactivate(tsdn, arena, extents, to_leak);
    return NULL;
  }

  if (result == extent_split_interior_ok) {
    if (lead != NULL) { extent_deactivate(tsdn, arena, extents, lead); }
    if (trail != NULL) { extent_deactivate(tsdn, arena, extents, trail); }
    return extent;
  } else {
    /*
     * We should have picked an extent that was large enough to
     * fulfill our allocation request.
     */
    assert(result == extent_split_interior_error);
    if (to_salvage != NULL) { extent_deregister(tsdn, to_salvage); }
    if (to_leak != NULL) {
      void* leak = extent_base_get(to_leak);
      extent_deregister_no_gdump_sub(tsdn, to_leak);
      extents_abandon_vm(
        tsdn, arena, r_extent_hooks, extents, to_leak, growing_retained);
      assert(extent_lock_from_addr(tsdn, rtree_ctx, leak, false) == NULL);
    }
    return NULL;
  }
  unreachable();
}

static bool extent_need_manual_zero(arena_t* arena) {
  /*
   * Need to manually zero the extent on repopulating if either; 1) non
   * default extent hooks installed (in which case the purge semantics may
   * change); or 2) transparent huge pages enabled.
   */
  return (!arena_has_default_hooks(arena) || (opt_thp == thp_mode_always));
}

/*
 * Tries to satisfy the given allocation request by reusing one of the
 * extents in the given extents_t.
 */
static extent_t* extent_recycle(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, void* new_addr,
  size_t size, size_t pad, size_t alignment, bool slab, szind_t szind,
  bool* zero, bool* commit, bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);
  assert(new_addr == NULL || !slab);
  assert(pad == 0 || !slab);
  assert(!*zero || !slab);

  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  extent_t* extent
    = extent_recycle_extract(tsdn, arena, r_extent_hooks, rtree_ctx,
      extents, new_addr, size, pad, alignment, slab, growing_retained);
  if (extent == NULL) { return NULL; }

  extent = extent_recycle_split(tsdn, arena, r_extent_hooks, rtree_ctx,
    extents, new_addr, size, pad, alignment, slab, szind, extent,
    growing_retained);
  if (extent == NULL) { return NULL; }

  if (*commit && !extent_committed_get(extent)) {
    if (extent_commit_impl(tsdn, arena, r_extent_hooks, extent, 0,
          extent_size_get(extent), growing_retained)) {
      extent_record(
        tsdn, arena, r_extent_hooks, extents, extent, growing_retained);
      return NULL;
    }
    if (!extent_need_manual_zero(arena)) {
      extent_zeroed_set(extent, true);
    }
  }

  if (extent_committed_get(extent)) { *commit = true; }
  if (extent_zeroed_get(extent)) { *zero = true; }

  if (pad != 0) { extent_addr_randomize(tsdn, extent, alignment); }
  assert(extent_state_get(extent) == extent_state_active);
  if (slab) {
    extent_slab_set(extent, slab);
    extent_interior_register(tsdn, rtree_ctx, extent, szind);
  }

  if (*zero) {
    void* addr = extent_base_get(extent);
    if (!extent_zeroed_get(extent)) {
      size_t size = extent_size_get(extent);
      if (extent_need_manual_zero(arena)
        || pages_purge_forced(addr, size)) {
        memset(addr, 0, size);
      }
    } else if (config_debug) {
      size_t* p = (size_t*)(uintptr_t)addr;
      /* Check the first page only. */
      for (size_t i = 0; i < PAGE / sizeof(size_t); i++) {
        assert(p[i] == 0);
      }
    }
  }
  return extent;
}

/*
 * If the caller specifies (!*zero), it is still possible to receive zeroed
 * memory, in which case *zero is toggled to true.  arena_extent_alloc()
 * takes advantage of this to avoid demanding zeroed extents, but taking
 * advantage of them if they are returned.
 */
static void* extent_alloc_core(tsdn_t* tsdn, arena_t* arena, void* new_addr,
  size_t size, size_t alignment, bool* zero, bool* commit,
  dss_prec_t dss_prec) {
  void* ret;

  assert(size != 0);
  assert(alignment != 0);

  /* "primary" dss. */
  if (have_dss && dss_prec == dss_prec_primary
    && (ret = extent_alloc_dss(
          tsdn, arena, new_addr, size, alignment, zero, commit))
      != NULL) {
    return ret;
  }
  /* mmap. */
  if ((ret = extent_alloc_mmap(new_addr, size, alignment, zero, commit))
    != NULL) {
    return ret;
  }
  /* "secondary" dss. */
  if (have_dss && dss_prec == dss_prec_secondary
    && (ret = extent_alloc_dss(
          tsdn, arena, new_addr, size, alignment, zero, commit))
      != NULL) {
    return ret;
  }

  /* All strategies for allocation failed. */
  return NULL;
}

static void* extent_alloc_default_impl(tsdn_t* tsdn, arena_t* arena,
  void* new_addr, size_t size, size_t alignment, bool* zero, bool* commit) {
  void* ret
    = extent_alloc_core(tsdn, arena, new_addr, size, alignment, zero,
      commit, (dss_prec_t)atomic_load_u(&arena->dss_prec, ATOMIC_RELAXED));
  if (have_madvise_huge && ret) { pages_set_thp_state(ret, size); }
  return ret;
}

static void* extent_alloc_default(extent_hooks_t* extent_hooks,
  void* new_addr, size_t size, size_t alignment, bool* zero, bool* commit,
  unsigned arena_ind) {
  tsdn_t* tsdn;
  arena_t* arena;

  tsdn = tsdn_fetch();
  arena = arena_get(tsdn, arena_ind, false);
  /*
   * The arena we're allocating on behalf of must have been initialized
   * already.
   */
  assert(arena != NULL);

  return extent_alloc_default_impl(tsdn, arena, new_addr, size,
    ALIGNMENT_CEILING(alignment, PAGE), zero, commit);
}

static void extent_hook_pre_reentrancy(tsdn_t* tsdn, arena_t* arena) {
  tsd_t* tsd = tsdn_null(tsdn) ? tsd_fetch() : tsdn_tsd(tsdn);
  if (arena == arena_get(tsd_tsdn(tsd), 0, false)) {
    /*
     * The only legitimate case of customized extent hooks for a0 is
     * hooks with no allocation activities.  One such example is to
     * place metadata on pre-allocated resources such as huge pages.
     * In that case, rely on reentrancy_level checks to catch
     * infinite recursions.
     */
    pre_reentrancy(tsd, NULL);
  } else {
    pre_reentrancy(tsd, arena);
  }
}

static void extent_hook_post_reentrancy(tsdn_t* tsdn) {
  tsd_t* tsd = tsdn_null(tsdn) ? tsd_fetch() : tsdn_tsd(tsdn);
  post_reentrancy(tsd);
}

/*
 * If virtual memory is retained, create increasingly larger extents from
 * which to split requested extents in order to limit the total number of
 * disjoint virtual memory ranges retained by each arena.
 */
static extent_t* extent_grow_retained(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, size_t size, size_t pad,
  size_t alignment, bool slab, szind_t szind, bool* zero, bool* commit) {
  malloc_mutex_assert_owner(tsdn, &arena->extent_grow_mtx);
  assert(pad == 0 || !slab);
  assert(!*zero || !slab);

  size_t esize = size + pad;
  size_t alloc_size_min = esize + PAGE_CEILING(alignment) - PAGE;
  /* Beware size_t wrap-around. */
  if (alloc_size_min < esize) { goto label_err; }
  /*
   * Find the next extent size in the series that would be large enough to
   * satisfy this request.
   */
  pszind_t egn_skip = 0;
  size_t alloc_size = sz_pind2sz(arena->extent_grow_next + egn_skip);
  while (alloc_size < alloc_size_min) {
    egn_skip++;
    if (arena->extent_grow_next + egn_skip
      >= sz_psz2ind(SC_LARGE_MAXCLASS)) {
      /* Outside legal range. */
      goto label_err;
    }
    alloc_size = sz_pind2sz(arena->extent_grow_next + egn_skip);
  }

  extent_t* extent = extent_alloc(tsdn, arena);
  if (extent == NULL) { goto label_err; }
  bool zeroed = false;
  bool committed = false;

  void* ptr;
  if (*r_extent_hooks == &extent_hooks_default) {
    ptr = extent_alloc_default_impl(
      tsdn, arena, NULL, alloc_size, PAGE, &zeroed, &committed);
  } else {
    extent_hook_pre_reentrancy(tsdn, arena);
    ptr = (*r_extent_hooks)
            ->alloc(*r_extent_hooks, NULL, alloc_size, PAGE, &zeroed,
              &committed, arena_ind_get(arena));
    extent_hook_post_reentrancy(tsdn);
  }

  extent_init(extent, arena, ptr, alloc_size, false, SC_NSIZES,
    arena_extent_sn_next(arena), extent_state_active, zeroed, committed,
    true, EXTENT_IS_HEAD);
  if (ptr == NULL) {
    extent_dalloc(tsdn, arena, extent);
    goto label_err;
  }

  if (extent_register_no_gdump_add(tsdn, extent)) {
    extent_dalloc(tsdn, arena, extent);
    goto label_err;
  }

  if (extent_zeroed_get(extent) && extent_committed_get(extent)) {
    *zero = true;
  }
  if (extent_committed_get(extent)) { *commit = true; }

  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  extent_t* lead;
  extent_t* trail;
  extent_t* to_leak;
  extent_t* to_salvage;
  extent_split_interior_result_t result = extent_split_interior(tsdn, arena,
    r_extent_hooks, rtree_ctx, &extent, &lead, &trail, &to_leak,
    &to_salvage, NULL, size, pad, alignment, slab, szind, true);

  if (result == extent_split_interior_ok) {
    if (lead != NULL) {
      extent_record(
        tsdn, arena, r_extent_hooks, &arena->extents_retained, lead, true);
    }
    if (trail != NULL) {
      extent_record(
        tsdn, arena, r_extent_hooks, &arena->extents_retained, trail, true);
    }
  } else {
    /*
     * We should have allocated a sufficiently large extent; the
     * cant_alloc case should not occur.
     */
    assert(result == extent_split_interior_error);
    if (to_salvage != NULL) {
      if (config_prof) { extent_gdump_add(tsdn, to_salvage); }
      extent_record(tsdn, arena, r_extent_hooks, &arena->extents_retained,
        to_salvage, true);
    }
    if (to_leak != NULL) {
      extent_deregister_no_gdump_sub(tsdn, to_leak);
      extents_abandon_vm(tsdn, arena, r_extent_hooks,
        &arena->extents_retained, to_leak, true);
    }
    goto label_err;
  }

  if (*commit && !extent_committed_get(extent)) {
    if (extent_commit_impl(tsdn, arena, r_extent_hooks, extent, 0,
          extent_size_get(extent), true)) {
      extent_record(tsdn, arena, r_extent_hooks, &arena->extents_retained,
        extent, true);
      goto label_err;
    }
    if (!extent_need_manual_zero(arena)) {
      extent_zeroed_set(extent, true);
    }
  }

  /*
   * Increment extent_grow_next if doing so wouldn't exceed the allowed
   * range.
   */
  if (arena->extent_grow_next + egn_skip + 1 <= arena->retain_grow_limit) {
    arena->extent_grow_next += egn_skip + 1;
  } else {
    arena->extent_grow_next = arena->retain_grow_limit;
  }
  /* All opportunities for failure are past. */
  malloc_mutex_unlock(tsdn, &arena->extent_grow_mtx);

  if (config_prof) {
    /* Adjust gdump stats now that extent is final size. */
    extent_gdump_add(tsdn, extent);
  }
  if (pad != 0) { extent_addr_randomize(tsdn, extent, alignment); }
  if (slab) {
    rtree_ctx_t rtree_ctx_fallback;
    rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

    extent_slab_set(extent, true);
    extent_interior_register(tsdn, rtree_ctx, extent, szind);
  }
  if (*zero && !extent_zeroed_get(extent)) {
    void* addr = extent_base_get(extent);
    size_t size = extent_size_get(extent);
    if (extent_need_manual_zero(arena) || pages_purge_forced(addr, size)) {
      memset(addr, 0, size);
    }
  }

  return extent;
label_err:
  malloc_mutex_unlock(tsdn, &arena->extent_grow_mtx);
  return NULL;
}

static extent_t* extent_alloc_retained(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, void* new_addr, size_t size, size_t pad,
  size_t alignment, bool slab, szind_t szind, bool* zero, bool* commit) {
  assert(size != 0);
  assert(alignment != 0);

  malloc_mutex_lock(tsdn, &arena->extent_grow_mtx);

  extent_t* extent
    = extent_recycle(tsdn, arena, r_extent_hooks, &arena->extents_retained,
      new_addr, size, pad, alignment, slab, szind, zero, commit, true);
  if (extent != NULL) {
    malloc_mutex_unlock(tsdn, &arena->extent_grow_mtx);
    if (config_prof) { extent_gdump_add(tsdn, extent); }
  } else if (opt_retain && new_addr == NULL) {
    extent = extent_grow_retained(tsdn, arena, r_extent_hooks, size, pad,
      alignment, slab, szind, zero, commit);
    /* extent_grow_retained() always releases extent_grow_mtx. */
  } else {
    malloc_mutex_unlock(tsdn, &arena->extent_grow_mtx);
  }
  malloc_mutex_assert_not_owner(tsdn, &arena->extent_grow_mtx);

  return extent;
}

static extent_t* extent_alloc_wrapper_hard(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, void* new_addr, size_t size, size_t pad,
  size_t alignment, bool slab, szind_t szind, bool* zero, bool* commit) {
  size_t esize = size + pad;
  extent_t* extent = extent_alloc(tsdn, arena);
  if (extent == NULL) { return NULL; }
  void* addr;
  size_t palignment = ALIGNMENT_CEILING(alignment, PAGE);
  if (*r_extent_hooks == &extent_hooks_default) {
    /* Call directly to propagate tsdn. */
    addr = extent_alloc_default_impl(
      tsdn, arena, new_addr, esize, palignment, zero, commit);
  } else {
    extent_hook_pre_reentrancy(tsdn, arena);
    addr = (*r_extent_hooks)
             ->alloc(*r_extent_hooks, new_addr, esize, palignment, zero,
               commit, arena_ind_get(arena));
    extent_hook_post_reentrancy(tsdn);
  }
  if (addr == NULL) {
    extent_dalloc(tsdn, arena, extent);
    return NULL;
  }
  extent_init(extent, arena, addr, esize, slab, szind,
    arena_extent_sn_next(arena), extent_state_active, *zero, *commit, true,
    EXTENT_NOT_HEAD);
  if (pad != 0) { extent_addr_randomize(tsdn, extent, alignment); }
  if (extent_register(tsdn, extent)) {
    extent_dalloc(tsdn, arena, extent);
    return NULL;
  }

  return extent;
}

extent_t* extent_alloc_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, void* new_addr, size_t size, size_t pad,
  size_t alignment, bool slab, szind_t szind, bool* zero, bool* commit) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  extent_t* extent = extent_alloc_retained(tsdn, arena, r_extent_hooks,
    new_addr, size, pad, alignment, slab, szind, zero, commit);
  if (extent == NULL) {
    if (opt_retain && new_addr != NULL) {
      /*
       * When retain is enabled and new_addr is set, we do not
       * attempt extent_alloc_wrapper_hard which does mmap
       * that is very unlikely to succeed (unless it happens
       * to be at the end).
       */
      return NULL;
    }
    extent = extent_alloc_wrapper_hard(tsdn, arena, r_extent_hooks,
      new_addr, size, pad, alignment, slab, szind, zero, commit);
  }

  assert(extent == NULL || extent_dumpable_get(extent));
  return extent;
}

static bool extent_can_coalesce(arena_t* arena, extents_t* extents,
  const extent_t* inner, const extent_t* outer) {
  assert(extent_arena_get(inner) == arena);
  if (extent_arena_get(outer) != arena) { return false; }

  assert(extent_state_get(inner) == extent_state_active);
  if (extent_state_get(outer) != extents->state) { return false; }

  if (extent_committed_get(inner) != extent_committed_get(outer)) {
    return false;
  }

  return true;
}

static bool extent_coalesce(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, extent_t* inner,
  extent_t* outer, bool forward, bool growing_retained) {
  assert(extent_can_coalesce(arena, extents, inner, outer));

  extent_activate_locked(tsdn, arena, extents, outer);

  malloc_mutex_unlock(tsdn, &extents->mtx);
  bool err = extent_merge_impl(tsdn, arena, r_extent_hooks,
    forward ? inner : outer, forward ? outer : inner, growing_retained);
  malloc_mutex_lock(tsdn, &extents->mtx);

  if (err) { extent_deactivate_locked(tsdn, arena, extents, outer); }

  return err;
}

static extent_t* extent_try_coalesce_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, extent_t* extent, bool* coalesced,
  bool growing_retained, bool inactive_only) {
  /*
   * We avoid checking / locking inactive neighbors for large size
   * classes, since they are eagerly coalesced on deallocation which can
   * cause lock contention.
   */
  /*
   * Continue attempting to coalesce until failure, to protect against
   * races with other threads that are thwarted by this one.
   */
  bool again;
  do {
    again = false;

    /* Try to coalesce forward. */
    extent_t* next = extent_lock_from_addr(
      tsdn, rtree_ctx, extent_past_get(extent), inactive_only);
    if (next != NULL) {
      /*
       * extents->mtx only protects against races for
       * like-state extents, so call extent_can_coalesce()
       * before releasing next's pool lock.
       */
      bool can_coalesce = extent_can_coalesce(arena, extents, extent, next);

      extent_unlock(tsdn, next);

      if (can_coalesce
        && !extent_coalesce(tsdn, arena, r_extent_hooks, extents, extent,
          next, true, growing_retained)) {
        if (extents->delay_coalesce) {
          /* Do minimal coalescing. */
          *coalesced = true;
          return extent;
        }
        again = true;
      }
    }

    /* Try to coalesce backward. */
    extent_t* prev = extent_lock_from_addr(
      tsdn, rtree_ctx, extent_before_get(extent), inactive_only);
    if (prev != NULL) {
      bool can_coalesce = extent_can_coalesce(arena, extents, extent, prev);
      extent_unlock(tsdn, prev);

      if (can_coalesce
        && !extent_coalesce(tsdn, arena, r_extent_hooks, extents, extent,
          prev, false, growing_retained)) {
        extent = prev;
        if (extents->delay_coalesce) {
          /* Do minimal coalescing. */
          *coalesced = true;
          return extent;
        }
        again = true;
      }
    }
  } while (again);

  if (extents->delay_coalesce) { *coalesced = false; }
  return extent;
}

static extent_t* extent_try_coalesce(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, extent_t* extent, bool* coalesced,
  bool growing_retained) {
  return extent_try_coalesce_impl(tsdn, arena, r_extent_hooks, rtree_ctx,
    extents, extent, coalesced, growing_retained, false);
}

static extent_t* extent_try_coalesce_large(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, rtree_ctx_t* rtree_ctx,
  extents_t* extents, extent_t* extent, bool* coalesced,
  bool growing_retained) {
  return extent_try_coalesce_impl(tsdn, arena, r_extent_hooks, rtree_ctx,
    extents, extent, coalesced, growing_retained, true);
}

/*
 * Does the metadata management portions of putting an unused extent into
 * the given extents_t (coalesces, deregisters slab interiors, the heap
 * operations).
 */
static void extent_record(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extents_t* extents, extent_t* extent,
  bool growing_retained) {
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  assert((extents_state_get(extents) != extent_state_dirty
           && extents_state_get(extents) != extent_state_muzzy)
    || !extent_zeroed_get(extent));

  malloc_mutex_lock(tsdn, &extents->mtx);
  extent_hooks_assure_initialized(arena, r_extent_hooks);

  extent_szind_set(extent, SC_NSIZES);
  if (extent_slab_get(extent)) {
    extent_interior_deregister(tsdn, rtree_ctx, extent);
    extent_slab_set(extent, false);
  }

  assert(rtree_extent_read(tsdn, &extents_rtree, rtree_ctx,
           (uintptr_t)extent_base_get(extent), true)
    == extent);

  if (!extents->delay_coalesce) {
    extent = extent_try_coalesce(tsdn, arena, r_extent_hooks, rtree_ctx,
      extents, extent, NULL, growing_retained);
  } else if (extent_size_get(extent) >= SC_LARGE_MINCLASS) {
    assert(extents == &arena->extents_dirty);
    /* Always coalesce large extents eagerly. */
    bool coalesced;
    do {
      assert(extent_state_get(extent) == extent_state_active);
      extent = extent_try_coalesce_large(tsdn, arena, r_extent_hooks,
        rtree_ctx, extents, extent, &coalesced, growing_retained);
    } while (coalesced);
    if (extent_size_get(extent) >= oversize_threshold) {
      /* Shortcut to purge the oversize extent eagerly. */
      malloc_mutex_unlock(tsdn, &extents->mtx);
      arena_decay_extent(tsdn, arena, r_extent_hooks, extent);
      return;
    }
  }
  extent_deactivate_locked(tsdn, arena, extents, extent);

  malloc_mutex_unlock(tsdn, &extents->mtx);
}

void extent_dalloc_gap(tsdn_t* tsdn, arena_t* arena, extent_t* extent) {
  extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;

  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  if (extent_register(tsdn, extent)) {
    extent_dalloc(tsdn, arena, extent);
    return;
  }
  extent_dalloc_wrapper(tsdn, arena, &extent_hooks, extent);
}

static bool extent_may_dalloc(void) {
  /* With retain enabled, the default dalloc always fails. */
  return !opt_retain;
}

static bool extent_dalloc_default_impl(void* addr, size_t size) {
  if (!have_dss || !extent_in_dss(addr)) {
    return extent_dalloc_mmap(addr, size);
  }
  return true;
}

static bool extent_dalloc_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, bool committed, unsigned arena_ind) {
  return extent_dalloc_default_impl(addr, size);
}

static bool extent_dalloc_wrapper_try(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent) {
  bool err;

  assert(extent_base_get(extent) != NULL);
  assert(extent_size_get(extent) != 0);
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_addr_set(extent, extent_base_get(extent));

  extent_hooks_assure_initialized(arena, r_extent_hooks);
  /* Try to deallocate. */
  if (*r_extent_hooks == &extent_hooks_default) {
    /* Call directly to propagate tsdn. */
    err = extent_dalloc_default_impl(
      extent_base_get(extent), extent_size_get(extent));
  } else {
    extent_hook_pre_reentrancy(tsdn, arena);
    err = ((*r_extent_hooks)->dalloc == NULL
      || (*r_extent_hooks)
           ->dalloc(*r_extent_hooks, extent_base_get(extent),
             extent_size_get(extent), extent_committed_get(extent),
             arena_ind_get(arena)));
    extent_hook_post_reentrancy(tsdn);
  }

  if (!err) { extent_dalloc(tsdn, arena, extent); }

  return err;
}

void extent_dalloc_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent) {
  assert(extent_dumpable_get(extent));
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  /* Avoid calling the default extent_dalloc unless have to. */
  if (*r_extent_hooks != &extent_hooks_default || extent_may_dalloc()) {
    /*
     * Deregister first to avoid a race with other allocating
     * threads, and reregister if deallocation fails.
     */
    extent_deregister(tsdn, extent);
    if (!extent_dalloc_wrapper_try(tsdn, arena, r_extent_hooks, extent)) {
      return;
    }
    extent_reregister(tsdn, extent);
  }

  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  /* Try to decommit; purge if that fails. */
  bool zeroed;
  if (!extent_committed_get(extent)) {
    zeroed = true;
  } else if (!extent_decommit_wrapper(tsdn, arena, r_extent_hooks, extent,
               0, extent_size_get(extent))) {
    zeroed = true;
  } else if ((*r_extent_hooks)->purge_forced != NULL
    && !(*r_extent_hooks)
          ->purge_forced(*r_extent_hooks, extent_base_get(extent),
            extent_size_get(extent), 0, extent_size_get(extent),
            arena_ind_get(arena))) {
    zeroed = true;
  } else if (extent_state_get(extent) == extent_state_muzzy
    || ((*r_extent_hooks)->purge_lazy != NULL
      && !(*r_extent_hooks)
            ->purge_lazy(*r_extent_hooks, extent_base_get(extent),
              extent_size_get(extent), 0, extent_size_get(extent),
              arena_ind_get(arena)))) {
    zeroed = false;
  } else {
    zeroed = false;
  }
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }
  extent_zeroed_set(extent, zeroed);

  if (config_prof) { extent_gdump_sub(tsdn, extent); }

  extent_record(
    tsdn, arena, r_extent_hooks, &arena->extents_retained, extent, false);
}

static void extent_destroy_default_impl(void* addr, size_t size) {
  if (!have_dss || !extent_in_dss(addr)) { pages_unmap(addr, size); }
}

static void extent_destroy_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, bool committed, unsigned arena_ind) {
  extent_destroy_default_impl(addr, size);
}

void extent_destroy_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent) {
  assert(extent_base_get(extent) != NULL);
  assert(extent_size_get(extent) != 0);
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  /* Deregister first to avoid a race with other allocating threads. */
  extent_deregister(tsdn, extent);

  extent_addr_set(extent, extent_base_get(extent));

  extent_hooks_assure_initialized(arena, r_extent_hooks);
  /* Try to destroy; silently fail otherwise. */
  if (*r_extent_hooks == &extent_hooks_default) {
    /* Call directly to propagate tsdn. */
    extent_destroy_default_impl(
      extent_base_get(extent), extent_size_get(extent));
  } else if ((*r_extent_hooks)->destroy != NULL) {
    extent_hook_pre_reentrancy(tsdn, arena);
    (*r_extent_hooks)
      ->destroy(*r_extent_hooks, extent_base_get(extent),
        extent_size_get(extent), extent_committed_get(extent),
        arena_ind_get(arena));
    extent_hook_post_reentrancy(tsdn);
  }

  extent_dalloc(tsdn, arena, extent);
}

static bool extent_commit_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, size_t offset, size_t length, unsigned arena_ind) {
  return pages_commit((void*)((uintptr_t)addr + (uintptr_t)offset), length);
}

static bool extent_commit_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  bool err = ((*r_extent_hooks)->commit == NULL
    || (*r_extent_hooks)
         ->commit(*r_extent_hooks, extent_base_get(extent),
           extent_size_get(extent), offset, length, arena_ind_get(arena)));
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }
  extent_committed_set(extent, extent_committed_get(extent) || !err);
  return err;
}

bool extent_commit_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length) {
  return extent_commit_impl(
    tsdn, arena, r_extent_hooks, extent, offset, length, false);
}

static bool extent_decommit_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind) {
  return pages_decommit(
    (void*)((uintptr_t)addr + (uintptr_t)offset), length);
}

bool extent_decommit_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length) {
  witness_assert_depth_to_rank(
    tsdn_witness_tsdp_get(tsdn), WITNESS_RANK_CORE, 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  bool err = ((*r_extent_hooks)->decommit == NULL
    || (*r_extent_hooks)
         ->decommit(*r_extent_hooks, extent_base_get(extent),
           extent_size_get(extent), offset, length, arena_ind_get(arena)));
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }
  extent_committed_set(extent, extent_committed_get(extent) && err);
  return err;
}

#ifdef PAGES_CAN_PURGE_LAZY
static bool extent_purge_lazy_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind) {
  assert(addr != NULL);
  assert((offset & PAGE_MASK) == 0);
  assert(length != 0);
  assert((length & PAGE_MASK) == 0);

  return pages_purge_lazy(
    (void*)((uintptr_t)addr + (uintptr_t)offset), length);
}
#endif

static bool extent_purge_lazy_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  if ((*r_extent_hooks)->purge_lazy == NULL) { return true; }
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  bool err
    = (*r_extent_hooks)
        ->purge_lazy(*r_extent_hooks, extent_base_get(extent),
          extent_size_get(extent), offset, length, arena_ind_get(arena));
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }

  return err;
}

bool extent_purge_lazy_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length) {
  return extent_purge_lazy_impl(
    tsdn, arena, r_extent_hooks, extent, offset, length, false);
}

#ifdef PAGES_CAN_PURGE_FORCED
static bool extent_purge_forced_default(extent_hooks_t* extent_hooks,
  void* addr, size_t size, size_t offset, size_t length,
  unsigned arena_ind) {
  assert(addr != NULL);
  assert((offset & PAGE_MASK) == 0);
  assert(length != 0);
  assert((length & PAGE_MASK) == 0);

  return pages_purge_forced(
    (void*)((uintptr_t)addr + (uintptr_t)offset), length);
}
#endif

static bool extent_purge_forced_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length, bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  if ((*r_extent_hooks)->purge_forced == NULL) { return true; }
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  bool err
    = (*r_extent_hooks)
        ->purge_forced(*r_extent_hooks, extent_base_get(extent),
          extent_size_get(extent), offset, length, arena_ind_get(arena));
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }
  return err;
}

bool extent_purge_forced_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t offset,
  size_t length) {
  return extent_purge_forced_impl(
    tsdn, arena, r_extent_hooks, extent, offset, length, false);
}

static bool extent_split_default(extent_hooks_t* extent_hooks, void* addr,
  size_t size, size_t size_a, size_t size_b, bool committed,
  unsigned arena_ind) {
  if (!maps_coalesce) {
    /*
     * Without retain, only whole regions can be purged (required by
     * MEM_RELEASE on Windows) -- therefore disallow splitting.  See
     * comments in extent_head_no_merge().
     */
    return !opt_retain;
  }

  return false;
}

/*
 * Accepts the extent to split, and the characteristics of each side of the
 * split.  The 'a' parameters go with the 'lead' of the resulting pair of
 * extents (the lower addressed portion of the split), and the 'b'
 * parameters go with the trail (the higher addressed portion).  This makes
 * 'extent' the lead, and returns the trail (except in case of error).
 */
static extent_t* extent_split_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t size_a,
  szind_t szind_a, bool slab_a, size_t size_b, szind_t szind_b, bool slab_b,
  bool growing_retained) {
  assert(extent_size_get(extent) == size_a + size_b);
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  if ((*r_extent_hooks)->split == NULL) { return NULL; }

  extent_t* trail = extent_alloc(tsdn, arena);
  if (trail == NULL) { goto label_error_a; }

  extent_init(trail, arena,
    (void*)((uintptr_t)extent_base_get(extent) + size_a), size_b, slab_b,
    szind_b, extent_sn_get(extent), extent_state_get(extent),
    extent_zeroed_get(extent), extent_committed_get(extent),
    extent_dumpable_get(extent), EXTENT_NOT_HEAD);

  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  rtree_leaf_elm_t *lead_elm_a, *lead_elm_b;
  {
    extent_t lead;

    extent_init(&lead, arena, extent_addr_get(extent), size_a, slab_a,
      szind_a, extent_sn_get(extent), extent_state_get(extent),
      extent_zeroed_get(extent), extent_committed_get(extent),
      extent_dumpable_get(extent), EXTENT_NOT_HEAD);

    extent_rtree_leaf_elms_lookup(
      tsdn, rtree_ctx, &lead, false, true, &lead_elm_a, &lead_elm_b);
  }
  rtree_leaf_elm_t *trail_elm_a, *trail_elm_b;
  extent_rtree_leaf_elms_lookup(
    tsdn, rtree_ctx, trail, false, true, &trail_elm_a, &trail_elm_b);

  if (lead_elm_a == NULL || lead_elm_b == NULL || trail_elm_a == NULL
    || trail_elm_b == NULL) {
    goto label_error_b;
  }

  extent_lock2(tsdn, extent, trail);

  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_pre_reentrancy(tsdn, arena);
  }
  bool err = (*r_extent_hooks)
               ->split(*r_extent_hooks, extent_base_get(extent),
                 size_a + size_b, size_a, size_b,
                 extent_committed_get(extent), arena_ind_get(arena));
  if (*r_extent_hooks != &extent_hooks_default) {
    extent_hook_post_reentrancy(tsdn);
  }
  if (err) { goto label_error_c; }

  extent_size_set(extent, size_a);
  extent_szind_set(extent, szind_a);

  extent_rtree_write_acquired(
    tsdn, lead_elm_a, lead_elm_b, extent, szind_a, slab_a);
  extent_rtree_write_acquired(
    tsdn, trail_elm_a, trail_elm_b, trail, szind_b, slab_b);

  extent_unlock2(tsdn, extent, trail);

  return trail;
label_error_c:
  extent_unlock2(tsdn, extent, trail);
label_error_b:
  extent_dalloc(tsdn, arena, trail);
label_error_a:
  return NULL;
}

extent_t* extent_split_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* extent, size_t size_a,
  szind_t szind_a, bool slab_a, size_t size_b, szind_t szind_b,
  bool slab_b) {
  return extent_split_impl(tsdn, arena, r_extent_hooks, extent, size_a,
    szind_a, slab_a, size_b, szind_b, slab_b, false);
}

static bool extent_merge_default_impl(void* addr_a, void* addr_b) {
  if (!maps_coalesce && !opt_retain) { return true; }
  if (have_dss && !extent_dss_mergeable(addr_a, addr_b)) { return true; }

  return false;
}

/*
 * Returns true if the given extents can't be merged because of their head
 * bit settings.  Assumes the second extent has the higher address.
 */
static bool extent_head_no_merge(extent_t* a, extent_t* b) {
  assert(extent_base_get(a) < extent_base_get(b));
  /*
   * When coalesce is not always allowed (Windows), only merge extents
   * from the same VirtualAlloc region under opt.retain (in which case
   * MEM_DECOMMIT is utilized for purging).
   */
  if (maps_coalesce) { return false; }
  if (!opt_retain) { return true; }
  /* If b is a head extent, disallow the cross-region merge. */
  if (extent_is_head_get(b)) {
    /*
     * Additionally, sn should not overflow with retain; sanity
     * check that different regions have unique sn.
     */
    assert(extent_sn_comp(a, b) != 0);
    return true;
  }
  assert(extent_sn_comp(a, b) == 0);

  return false;
}

static bool extent_merge_default(extent_hooks_t* extent_hooks, void* addr_a,
  size_t size_a, void* addr_b, size_t size_b, bool committed,
  unsigned arena_ind) {
  if (!maps_coalesce) {
    tsdn_t* tsdn = tsdn_fetch();
    extent_t* a = iealloc(tsdn, addr_a);
    extent_t* b = iealloc(tsdn, addr_b);
    if (extent_head_no_merge(a, b)) { return true; }
  }
  return extent_merge_default_impl(addr_a, addr_b);
}

static bool extent_merge_impl(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* a, extent_t* b,
  bool growing_retained) {
  witness_assert_depth_to_rank(tsdn_witness_tsdp_get(tsdn),
    WITNESS_RANK_CORE, growing_retained ? 1 : 0);
  assert(extent_base_get(a) < extent_base_get(b));

  extent_hooks_assure_initialized(arena, r_extent_hooks);

  if ((*r_extent_hooks)->merge == NULL || extent_head_no_merge(a, b)) {
    return true;
  }

  bool err;
  if (*r_extent_hooks == &extent_hooks_default) {
    /* Call directly to propagate tsdn. */
    err = extent_merge_default_impl(extent_base_get(a), extent_base_get(b));
  } else {
    extent_hook_pre_reentrancy(tsdn, arena);
    err = (*r_extent_hooks)
            ->merge(*r_extent_hooks, extent_base_get(a), extent_size_get(a),
              extent_base_get(b), extent_size_get(b),
              extent_committed_get(a), arena_ind_get(arena));
    extent_hook_post_reentrancy(tsdn);
  }

  if (err) { return true; }

  /*
   * The rtree writes must happen while all the relevant elements are
   * owned, so the following code uses decomposed helper functions rather
   * than extent_{,de}register() to do things in the right order.
   */
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  rtree_leaf_elm_t *a_elm_a, *a_elm_b, *b_elm_a, *b_elm_b;
  extent_rtree_leaf_elms_lookup(
    tsdn, rtree_ctx, a, true, false, &a_elm_a, &a_elm_b);
  extent_rtree_leaf_elms_lookup(
    tsdn, rtree_ctx, b, true, false, &b_elm_a, &b_elm_b);

  extent_lock2(tsdn, a, b);

  if (a_elm_b != NULL) {
    rtree_leaf_elm_write(
      tsdn, &extents_rtree, a_elm_b, NULL, SC_NSIZES, false);
  }
  if (b_elm_b != NULL) {
    rtree_leaf_elm_write(
      tsdn, &extents_rtree, b_elm_a, NULL, SC_NSIZES, false);
  } else {
    b_elm_b = b_elm_a;
  }

  extent_size_set(a, extent_size_get(a) + extent_size_get(b));
  extent_szind_set(a, SC_NSIZES);
  extent_sn_set(a,
    (extent_sn_get(a) < extent_sn_get(b)) ? extent_sn_get(a)
                                          : extent_sn_get(b));
  extent_zeroed_set(a, extent_zeroed_get(a) && extent_zeroed_get(b));

  extent_rtree_write_acquired(tsdn, a_elm_a, b_elm_b, a, SC_NSIZES, false);

  extent_unlock2(tsdn, a, b);

  extent_dalloc(tsdn, extent_arena_get(b), b);

  return false;
}

bool extent_merge_wrapper(tsdn_t* tsdn, arena_t* arena,
  extent_hooks_t** r_extent_hooks, extent_t* a, extent_t* b) {
  return extent_merge_impl(tsdn, arena, r_extent_hooks, a, b, false);
}

bool extent_boot(void) {
  if (rtree_new(&extents_rtree, true)) { return true; }

  if (mutex_pool_init(&extent_mutex_pool, "extent_mutex_pool",
        WITNESS_RANK_EXTENT_POOL)) {
    return true;
  }

  if (have_dss) { extent_dss_boot(); }

  return false;
}

void extent_util_stats_get(tsdn_t* tsdn, const void* ptr, size_t* nfree,
  size_t* nregs, size_t* size) {
  assert(ptr != NULL && nfree != NULL && nregs != NULL && size != NULL);

  const extent_t* extent = iealloc(tsdn, ptr);
  if (unlikely(extent == NULL)) {
    *nfree = *nregs = *size = 0;
    return;
  }

  *size = extent_size_get(extent);
  if (!extent_slab_get(extent)) {
    *nfree = 0;
    *nregs = 1;
  } else {
    *nfree = extent_nfree_get(extent);
    *nregs = bin_infos[extent_szind_get(extent)].nregs;
    assert(*nfree <= *nregs);
    assert(*nfree * extent_usize_get(extent) <= *size);
  }
}

void extent_util_stats_verbose_get(tsdn_t* tsdn, const void* ptr,
  size_t* nfree, size_t* nregs, size_t* size, size_t* bin_nfree,
  size_t* bin_nregs, void** slabcur_addr) {
  assert(ptr != NULL && nfree != NULL && nregs != NULL && size != NULL
    && bin_nfree != NULL && bin_nregs != NULL && slabcur_addr != NULL);

  const extent_t* extent = iealloc(tsdn, ptr);
  if (unlikely(extent == NULL)) {
    *nfree = *nregs = *size = *bin_nfree = *bin_nregs = 0;
    *slabcur_addr = NULL;
    return;
  }

  *size = extent_size_get(extent);
  if (!extent_slab_get(extent)) {
    *nfree = *bin_nfree = *bin_nregs = 0;
    *nregs = 1;
    *slabcur_addr = NULL;
    return;
  }

  *nfree = extent_nfree_get(extent);
  const szind_t szind = extent_szind_get(extent);
  *nregs = bin_infos[szind].nregs;
  assert(*nfree <= *nregs);
  assert(*nfree * extent_usize_get(extent) <= *size);

  const arena_t* arena = extent_arena_get(extent);
  assert(arena != NULL);
  const unsigned binshard = extent_binshard_get(extent);
  bin_t* bin = &arena->bins[szind].bin_shards[binshard];

  malloc_mutex_lock(tsdn, &bin->lock);
  if (config_stats) {
    *bin_nregs = *nregs * bin->stats.curslabs;
    assert(*bin_nregs >= bin->stats.curregs);
    *bin_nfree = *bin_nregs - bin->stats.curregs;
  } else {
    *bin_nfree = *bin_nregs = 0;
  }
  *slabcur_addr = extent_addr_get(bin->slabcur);
  assert(*slabcur_addr != NULL);
  malloc_mutex_unlock(tsdn, &bin->lock);
}
#define JEMALLOC_EXTENT_DSS_C_

#include "jemalloc/internal/extent_dss.h"
#include "jemalloc/internal/spin.h"

/******************************************************************************/
/* Data. */

const char* opt_dss = DSS_DEFAULT;

const char* dss_prec_names[]
  = { "disabled", "primary", "secondary", "N/A" };

/*
 * Current dss precedence default, used when creating new arenas.  NB: This
 * is stored as unsigned rather than dss_prec_t because in principle there's
 * no guarantee that sizeof(dss_prec_t) is the same as sizeof(unsigned), and
 * we use atomic operations to synchronize the setting.
 */
static atomic_u_t dss_prec_default
  = ATOMIC_INIT((unsigned)DSS_PREC_DEFAULT);

/* Base address of the DSS. */
static void* dss_base;
/* Atomic boolean indicating whether a thread is currently extending DSS. */
static atomic_b_t dss_extending;
/* Atomic boolean indicating whether the DSS is exhausted. */
static atomic_b_t dss_exhausted;
/* Atomic current upper limit on DSS addresses. */
static atomic_p_t dss_max;

/******************************************************************************/

static void* extent_dss_sbrk(intptr_t increment) {
#ifdef JEMALLOC_DSS
  return sbrk(increment);
#else
  not_implemented();
  return NULL;
#endif
}

dss_prec_t extent_dss_prec_get(void) {
  dss_prec_t ret;

  if (!have_dss) { return dss_prec_disabled; }
  ret = (dss_prec_t)atomic_load_u(&dss_prec_default, ATOMIC_ACQUIRE);
  return ret;
}

bool extent_dss_prec_set(dss_prec_t dss_prec) {
  if (!have_dss) { return (dss_prec != dss_prec_disabled); }
  atomic_store_u(&dss_prec_default, (unsigned)dss_prec, ATOMIC_RELEASE);
  return false;
}

static void extent_dss_extending_start(void) {
  spin_t spinner = SPIN_INITIALIZER;
  while (true) {
    bool expected = false;
    if (atomic_compare_exchange_weak_b(&dss_extending, &expected, true,
          ATOMIC_ACQ_REL, ATOMIC_RELAXED)) {
      break;
    }
    spin_adaptive(&spinner);
  }
}

static void extent_dss_extending_finish(void) {
  assert(atomic_load_b(&dss_extending, ATOMIC_RELAXED));

  atomic_store_b(&dss_extending, false, ATOMIC_RELEASE);
}

static void* extent_dss_max_update(void* new_addr) {
  /*
   * Get the current end of the DSS as max_cur and assure that dss_max is
   * up to date.
   */
  void* max_cur = extent_dss_sbrk(0);
  if (max_cur == (void*)-1) { return NULL; }
  atomic_store_p(&dss_max, max_cur, ATOMIC_RELEASE);
  /* Fixed new_addr can only be supported if it is at the edge of DSS. */
  if (new_addr != NULL && max_cur != new_addr) { return NULL; }
  return max_cur;
}

void* extent_alloc_dss(tsdn_t* tsdn, arena_t* arena, void* new_addr,
  size_t size, size_t alignment, bool* zero, bool* commit) {
  extent_t* gap;

  cassert(have_dss);
  assert(size > 0);
  assert(alignment == ALIGNMENT_CEILING(alignment, PAGE));

  /*
   * sbrk() uses a signed increment argument, so take care not to
   * interpret a large allocation request as a negative increment.
   */
  if ((intptr_t)size < 0) { return NULL; }

  gap = extent_alloc(tsdn, arena);
  if (gap == NULL) { return NULL; }

  extent_dss_extending_start();
  if (!atomic_load_b(&dss_exhausted, ATOMIC_ACQUIRE)) {
    /*
     * The loop is necessary to recover from races with other
     * threads that are using the DSS for something other than
     * malloc.
     */
    while (true) {
      void* max_cur = extent_dss_max_update(new_addr);
      if (max_cur == NULL) { goto label_oom; }

      /*
       * Compute how much page-aligned gap space (if any) is
       * necessary to satisfy alignment.  This space can be
       * recycled for later use.
       */
      void* gap_addr_page = (void*)(PAGE_CEILING((uintptr_t)max_cur));
      void* ret
        = (void*)ALIGNMENT_CEILING((uintptr_t)gap_addr_page, alignment);
      size_t gap_size_page = (uintptr_t)ret - (uintptr_t)gap_addr_page;
      if (gap_size_page != 0) {
        extent_init(gap, arena, gap_addr_page, gap_size_page, false,
          SC_NSIZES, arena_extent_sn_next(arena), extent_state_active,
          false, true, true, EXTENT_NOT_HEAD);
      }
      /*
       * Compute the address just past the end of the desired
       * allocation space.
       */
      void* dss_next = (void*)((uintptr_t)ret + size);
      if ((uintptr_t)ret < (uintptr_t)max_cur
        || (uintptr_t)dss_next < (uintptr_t)max_cur) {
        goto label_oom; /* Wrap-around. */
      }
      /* Compute the increment, including subpage bytes. */
      void* gap_addr_subpage = max_cur;
      size_t gap_size_subpage
        = (uintptr_t)ret - (uintptr_t)gap_addr_subpage;
      intptr_t incr = gap_size_subpage + size;

      assert((uintptr_t)max_cur + incr == (uintptr_t)ret + size);

      /* Try to allocate. */
      void* dss_prev = extent_dss_sbrk(incr);
      if (dss_prev == max_cur) {
        /* Success. */
        atomic_store_p(&dss_max, dss_next, ATOMIC_RELEASE);
        extent_dss_extending_finish();

        if (gap_size_page != 0) {
          extent_dalloc_gap(tsdn, arena, gap);
        } else {
          extent_dalloc(tsdn, arena, gap);
        }
        if (!*commit) { *commit = pages_decommit(ret, size); }
        if (*zero && *commit) {
          extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;
          extent_t extent;

          extent_init(&extent, arena, ret, size, size, false, SC_NSIZES,
            extent_state_active, false, true, true, EXTENT_NOT_HEAD);
          if (extent_purge_forced_wrapper(
                tsdn, arena, &extent_hooks, &extent, 0, size)) {
            memset(ret, 0, size);
          }
        }
        return ret;
      }
      /*
       * Failure, whether due to OOM or a race with a raw
       * sbrk() call from outside the allocator.
       */
      if (dss_prev == (void*)-1) {
        /* OOM. */
        atomic_store_b(&dss_exhausted, true, ATOMIC_RELEASE);
        goto label_oom;
      }
    }
  }
label_oom:
  extent_dss_extending_finish();
  extent_dalloc(tsdn, arena, gap);
  return NULL;
}

static bool extent_in_dss_helper(void* addr, void* max) {
  return ((uintptr_t)addr >= (uintptr_t)dss_base
    && (uintptr_t)addr < (uintptr_t)max);
}

bool extent_in_dss(void* addr) {
  cassert(have_dss);

  return extent_in_dss_helper(
    addr, atomic_load_p(&dss_max, ATOMIC_ACQUIRE));
}

bool extent_dss_mergeable(void* addr_a, void* addr_b) {
  void* max;

  cassert(have_dss);

  if ((uintptr_t)addr_a < (uintptr_t)dss_base
    && (uintptr_t)addr_b < (uintptr_t)dss_base) {
    return true;
  }

  max = atomic_load_p(&dss_max, ATOMIC_ACQUIRE);
  return (
    extent_in_dss_helper(addr_a, max) == extent_in_dss_helper(addr_b, max));
}

void extent_dss_boot(void) {
  cassert(have_dss);

  dss_base = extent_dss_sbrk(0);
  atomic_store_b(&dss_extending, false, ATOMIC_RELAXED);
  atomic_store_b(&dss_exhausted, dss_base == (void*)-1, ATOMIC_RELAXED);
  atomic_store_p(&dss_max, dss_base, ATOMIC_RELAXED);
}

/******************************************************************************/
#define JEMALLOC_EXTENT_MMAP_C_

#include "jemalloc/internal/extent_mmap.h"

/******************************************************************************/
/* Data. */

bool opt_retain =
#ifdef JEMALLOC_RETAIN
  true
#else
  false
#endif
  ;

/******************************************************************************/

void* extent_alloc_mmap(
  void* new_addr, size_t size, size_t alignment, bool* zero, bool* commit) {
  assert(alignment == ALIGNMENT_CEILING(alignment, PAGE));
  void* ret = pages_map(new_addr, size, alignment, commit);
  if (ret == NULL) { return NULL; }
  assert(ret != NULL);
  if (*commit) { *zero = true; }
  return ret;
}

bool extent_dalloc_mmap(void* addr, size_t size) {
  if (!opt_retain) { pages_unmap(addr, size); }
  return opt_retain;
}
#define JEMALLOC_HASH_C_

#include "jemalloc/internal/hook.h"

#include "jemalloc/internal/seq.h"

typedef struct hooks_internal_s hooks_internal_t;
struct hooks_internal_s {
  hooks_t hooks;
  bool in_use;
};

seq_define(hooks_internal_t, hooks)

  static atomic_u_t nhooks = ATOMIC_INIT(0);
static seq_hooks_t hooks[HOOK_MAX];
static malloc_mutex_t hooks_mu;

bool hook_boot() {
  return malloc_mutex_init(
    &hooks_mu, "hooks", WITNESS_RANK_HOOK, malloc_mutex_rank_exclusive);
}

static void* hook_install_locked(hooks_t* to_install) {
  hooks_internal_t hooks_internal;
  for (int i = 0; i < HOOK_MAX; i++) {
    bool success = seq_try_load_hooks(&hooks_internal, &hooks[i]);
    /* We hold mu; no concurrent access. */
    assert(success);
    if (!hooks_internal.in_use) {
      hooks_internal.hooks = *to_install;
      hooks_internal.in_use = true;
      seq_store_hooks(&hooks[i], &hooks_internal);
      atomic_store_u(&nhooks, atomic_load_u(&nhooks, ATOMIC_RELAXED) + 1,
        ATOMIC_RELAXED);
      return &hooks[i];
    }
  }
  return NULL;
}

void* hook_install(tsdn_t* tsdn, hooks_t* to_install) {
  malloc_mutex_lock(tsdn, &hooks_mu);
  void* ret = hook_install_locked(to_install);
  if (ret != NULL) { tsd_global_slow_inc(tsdn); }
  malloc_mutex_unlock(tsdn, &hooks_mu);
  return ret;
}

static void hook_remove_locked(seq_hooks_t* to_remove) {
  hooks_internal_t hooks_internal;
  bool success = seq_try_load_hooks(&hooks_internal, to_remove);
  /* We hold mu; no concurrent access. */
  assert(success);
  /* Should only remove hooks that were added. */
  assert(hooks_internal.in_use);
  hooks_internal.in_use = false;
  seq_store_hooks(to_remove, &hooks_internal);
  atomic_store_u(
    &nhooks, atomic_load_u(&nhooks, ATOMIC_RELAXED) - 1, ATOMIC_RELAXED);
}

void hook_remove(tsdn_t* tsdn, void* opaque) {
  if (config_debug) {
    char* hooks_begin = (char*)&hooks[0];
    char* hooks_end = (char*)&hooks[HOOK_MAX];
    char* hook = (char*)opaque;
    assert(hooks_begin <= hook && hook < hooks_end
      && (hook - hooks_begin) % sizeof(seq_hooks_t) == 0);
  }
  malloc_mutex_lock(tsdn, &hooks_mu);
  hook_remove_locked((seq_hooks_t*)opaque);
  tsd_global_slow_dec(tsdn);
  malloc_mutex_unlock(tsdn, &hooks_mu);
}

#define FOR_EACH_HOOK_BEGIN(hooks_internal_ptr)                            \
  for (int for_each_hook_counter = 0; for_each_hook_counter < HOOK_MAX;    \
       for_each_hook_counter++) {                                          \
    bool for_each_hook_success = seq_try_load_hooks(                       \
      (hooks_internal_ptr), &hooks[for_each_hook_counter]);                \
    if (!for_each_hook_success) { continue; }                              \
    if (!(hooks_internal_ptr)->in_use) { continue; }
#define FOR_EACH_HOOK_END }

static bool* hook_reentrantp() {
  /*
   * We prevent user reentrancy within hooks.  This is basically just a
   * thread-local bool that triggers an early-exit.
   *
   * We don't fold in_hook into reentrancy.  There are two reasons for
   * this:
   * - Right now, we turn on reentrancy during things like extent hook
   *   execution.  Allocating during extent hooks is not officially
   *   supported, but we don't want to break it for the time being.  These
   *   sorts of allocations should probably still be hooked, though.
   * - If a hook allocates, we may want it to be relatively fast (after
   *   all, it executes on every allocator operation).  Turning on
   *   reentrancy is a fairly heavyweight mode (disabling tcache,
   *   redirecting to arena 0, etc.).  It's possible we may one day want
   *   to turn on reentrant mode here, if it proves too difficult to keep
   *   this working.  But that's fairly easy for us to see; OTOH, people
   *   not using hooks because they're too slow is easy for us to miss.
   *
   * The tricky part is
   * that this code might get invoked even if we don't have access to tsd.
   * This function mimics getting a pointer to thread-local data, except
   * that it might secretly return a pointer to some global data if we
   * know that the caller will take the early-exit path.
   * If we return a bool that indicates that we are reentrant, then the
   * caller will go down the early exit path, leaving the global
   * untouched.
   */
  static bool in_hook_global = true;
  tsdn_t* tsdn = tsdn_fetch();
  tcache_t* tcache = tsdn_tcachep_get(tsdn);
  if (tcache != NULL) { return &tcache->in_hook; }
  return &in_hook_global;
}

#define HOOK_PROLOGUE                                                      \
  if (likely(atomic_load_u(&nhooks, ATOMIC_RELAXED) == 0)) { return; }     \
  bool* in_hook = hook_reentrantp();                                       \
  if (*in_hook) { return; }                                                \
  *in_hook = true;

#define HOOK_EPILOGUE *in_hook = false;

void hook_invoke_alloc(hook_alloc_t type, void* result,
  uintptr_t result_raw, uintptr_t args_raw[3]) {
  HOOK_PROLOGUE

  hooks_internal_t hook;
  FOR_EACH_HOOK_BEGIN(&hook)
  hook_alloc h = hook.hooks.alloc_hook;
  if (h != NULL) {
    h(hook.hooks.extra, type, result, result_raw, args_raw);
  }
  FOR_EACH_HOOK_END

  HOOK_EPILOGUE
}

void hook_invoke_dalloc(
  hook_dalloc_t type, void* address, uintptr_t args_raw[3]) {
  HOOK_PROLOGUE
  hooks_internal_t hook;
  FOR_EACH_HOOK_BEGIN(&hook)
  hook_dalloc h = hook.hooks.dalloc_hook;
  if (h != NULL) { h(hook.hooks.extra, type, address, args_raw); }
  FOR_EACH_HOOK_END
  HOOK_EPILOGUE
}

void hook_invoke_expand(hook_expand_t type, void* address, size_t old_usize,
  size_t new_usize, uintptr_t result_raw, uintptr_t args_raw[4]) {
  HOOK_PROLOGUE
  hooks_internal_t hook;
  FOR_EACH_HOOK_BEGIN(&hook)
  hook_expand h = hook.hooks.expand_hook;
  if (h != NULL) {
    h(hook.hooks.extra, type, address, old_usize, new_usize, result_raw,
      args_raw);
  }
  FOR_EACH_HOOK_END
  HOOK_EPILOGUE
}
#define JEMALLOC_LARGE_C_

#include "jemalloc/internal/extent_mmap.h"

/******************************************************************************/

void* large_malloc(tsdn_t* tsdn, arena_t* arena, size_t usize, bool zero) {
  assert(usize == sz_s2u(usize));

  return large_palloc(tsdn, arena, usize, CACHELINE, zero);
}

void* large_palloc(
  tsdn_t* tsdn, arena_t* arena, size_t usize, size_t alignment, bool zero) {
  size_t ausize;
  extent_t* extent;
  bool is_zeroed;
  UNUSED bool idump JEMALLOC_CC_SILENCE_INIT(false);

  assert(!tsdn_null(tsdn) || arena != NULL);

  ausize = sz_sa2u(usize, alignment);
  if (unlikely(ausize == 0 || ausize > SC_LARGE_MAXCLASS)) { return NULL; }

  if (config_fill && unlikely(opt_zero)) { zero = true; }
  /*
   * Copy zero into is_zeroed and pass the copy when allocating the
   * extent, so that it is possible to make correct junk/zero fill
   * decisions below, even if is_zeroed ends up true when zero is false.
   */
  is_zeroed = zero;
  if (likely(!tsdn_null(tsdn))) {
    arena = arena_choose_maybe_huge(tsdn_tsd(tsdn), arena, usize);
  }
  if (unlikely(arena == NULL)
    || (extent = arena_extent_alloc_large(
          tsdn, arena, usize, alignment, &is_zeroed))
      == NULL) {
    return NULL;
  }

  /* See comments in arena_bin_slabs_full_insert(). */
  if (!arena_is_auto(arena)) {
    /* Insert extent into large. */
    malloc_mutex_lock(tsdn, &arena->large_mtx);
    extent_list_append(&arena->large, extent);
    malloc_mutex_unlock(tsdn, &arena->large_mtx);
  }
  if (config_prof && arena_prof_accum(tsdn, arena, usize)) {
    prof_idump(tsdn);
  }

  if (zero) {
    assert(is_zeroed);
  } else if (config_fill && unlikely(opt_junk_alloc)) {
    memset(extent_addr_get(extent), JEMALLOC_ALLOC_JUNK,
      extent_usize_get(extent));
  }

  arena_decay_tick(tsdn, arena);
  return extent_addr_get(extent);
}

static void large_dalloc_junk_impl(void* ptr, size_t size) {
  memset(ptr, JEMALLOC_FREE_JUNK, size);
}
large_dalloc_junk_t* JET_MUTABLE large_dalloc_junk = large_dalloc_junk_impl;

static void large_dalloc_maybe_junk_impl(void* ptr, size_t size) {
  if (config_fill && have_dss && unlikely(opt_junk_free)) {
    /*
     * Only bother junk filling if the extent isn't about to be
     * unmapped.
     */
    if (opt_retain || (have_dss && extent_in_dss(ptr))) {
      large_dalloc_junk(ptr, size);
    }
  }
}
large_dalloc_maybe_junk_t* JET_MUTABLE large_dalloc_maybe_junk
  = large_dalloc_maybe_junk_impl;

static bool large_ralloc_no_move_shrink(
  tsdn_t* tsdn, extent_t* extent, size_t usize) {
  arena_t* arena = extent_arena_get(extent);
  size_t oldusize = extent_usize_get(extent);
  extent_hooks_t* extent_hooks = extent_hooks_get(arena);
  size_t diff = extent_size_get(extent) - (usize + sz_large_pad);

  assert(oldusize > usize);

  if (extent_hooks->split == NULL) { return true; }

  /* Split excess pages. */
  if (diff != 0) {
    extent_t* trail = extent_split_wrapper(tsdn, arena, &extent_hooks,
      extent, usize + sz_large_pad, sz_size2index(usize), false, diff,
      SC_NSIZES, false);
    if (trail == NULL) { return true; }

    if (config_fill && unlikely(opt_junk_free)) {
      large_dalloc_maybe_junk(
        extent_addr_get(trail), extent_size_get(trail));
    }

    arena_extents_dirty_dalloc(tsdn, arena, &extent_hooks, trail);
  }

  arena_extent_ralloc_large_shrink(tsdn, arena, extent, oldusize);

  return false;
}

static bool large_ralloc_no_move_expand(
  tsdn_t* tsdn, extent_t* extent, size_t usize, bool zero) {
  arena_t* arena = extent_arena_get(extent);
  size_t oldusize = extent_usize_get(extent);
  extent_hooks_t* extent_hooks = extent_hooks_get(arena);
  size_t trailsize = usize - oldusize;

  if (extent_hooks->merge == NULL) { return true; }

  if (config_fill && unlikely(opt_zero)) { zero = true; }
  /*
   * Copy zero into is_zeroed_trail and pass the copy when allocating the
   * extent, so that it is possible to make correct junk/zero fill
   * decisions below, even if is_zeroed_trail ends up true when zero is
   * false.
   */
  bool is_zeroed_trail = zero;
  bool commit = true;
  extent_t* trail;
  bool new_mapping;
  if ((trail = extents_alloc(tsdn, arena, &extent_hooks,
         &arena->extents_dirty, extent_past_get(extent), trailsize, 0,
         CACHELINE, false, SC_NSIZES, &is_zeroed_trail, &commit))
      != NULL
    || (trail = extents_alloc(tsdn, arena, &extent_hooks,
          &arena->extents_muzzy, extent_past_get(extent), trailsize, 0,
          CACHELINE, false, SC_NSIZES, &is_zeroed_trail, &commit))
      != NULL) {
    if (config_stats) { new_mapping = false; }
  } else {
    if ((trail = extent_alloc_wrapper(tsdn, arena, &extent_hooks,
           extent_past_get(extent), trailsize, 0, CACHELINE, false,
           SC_NSIZES, &is_zeroed_trail, &commit))
      == NULL) {
      return true;
    }
    if (config_stats) { new_mapping = true; }
  }

  if (extent_merge_wrapper(tsdn, arena, &extent_hooks, extent, trail)) {
    extent_dalloc_wrapper(tsdn, arena, &extent_hooks, trail);
    return true;
  }
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);
  szind_t szind = sz_size2index(usize);
  extent_szind_set(extent, szind);
  rtree_szind_slab_update(tsdn, &extents_rtree, rtree_ctx,
    (uintptr_t)extent_addr_get(extent), szind, false);

  if (config_stats && new_mapping) {
    arena_stats_mapped_add(tsdn, &arena->stats, trailsize);
  }

  if (zero) {
    if (config_cache_oblivious) {
      /*
       * Zero the trailing bytes of the original allocation's
       * last page, since they are in an indeterminate state.
       * There will always be trailing bytes, because ptr's
       * offset from the beginning of the extent is a multiple
       * of CACHELINE in [0 .. PAGE).
       */
      void* zbase = (void*)((uintptr_t)extent_addr_get(extent) + oldusize);
      void* zpast = PAGE_ADDR2BASE((void*)((uintptr_t)zbase + PAGE));
      size_t nzero = (uintptr_t)zpast - (uintptr_t)zbase;
      assert(nzero > 0);
      memset(zbase, 0, nzero);
    }
    assert(is_zeroed_trail);
  } else if (config_fill && unlikely(opt_junk_alloc)) {
    memset((void*)((uintptr_t)extent_addr_get(extent) + oldusize),
      JEMALLOC_ALLOC_JUNK, usize - oldusize);
  }

  arena_extent_ralloc_large_expand(tsdn, arena, extent, oldusize);

  return false;
}

bool large_ralloc_no_move(tsdn_t* tsdn, extent_t* extent, size_t usize_min,
  size_t usize_max, bool zero) {
  size_t oldusize = extent_usize_get(extent);

  /* The following should have been caught by callers. */
  assert(usize_min > 0 && usize_max <= SC_LARGE_MAXCLASS);
  /* Both allocation sizes must be large to avoid a move. */
  assert(oldusize >= SC_LARGE_MINCLASS && usize_max >= SC_LARGE_MINCLASS);

  if (usize_max > oldusize) {
    /* Attempt to expand the allocation in-place. */
    if (!large_ralloc_no_move_expand(tsdn, extent, usize_max, zero)) {
      arena_decay_tick(tsdn, extent_arena_get(extent));
      return false;
    }
    /* Try again, this time with usize_min. */
    if (usize_min < usize_max && usize_min > oldusize
      && large_ralloc_no_move_expand(tsdn, extent, usize_min, zero)) {
      arena_decay_tick(tsdn, extent_arena_get(extent));
      return false;
    }
  }

  /*
   * Avoid moving the allocation if the existing extent size accommodates
   * the new size.
   */
  if (oldusize >= usize_min && oldusize <= usize_max) {
    arena_decay_tick(tsdn, extent_arena_get(extent));
    return false;
  }

  /* Attempt to shrink the allocation in-place. */
  if (oldusize > usize_max) {
    if (!large_ralloc_no_move_shrink(tsdn, extent, usize_max)) {
      arena_decay_tick(tsdn, extent_arena_get(extent));
      return false;
    }
  }
  return true;
}

static void* large_ralloc_move_helper(
  tsdn_t* tsdn, arena_t* arena, size_t usize, size_t alignment, bool zero) {
  if (alignment <= CACHELINE) {
    return large_malloc(tsdn, arena, usize, zero);
  }
  return large_palloc(tsdn, arena, usize, alignment, zero);
}

void* large_ralloc(tsdn_t* tsdn, arena_t* arena, void* ptr, size_t usize,
  size_t alignment, bool zero, tcache_t* tcache,
  hook_ralloc_args_t* hook_args) {
  extent_t* extent = iealloc(tsdn, ptr);

  size_t oldusize = extent_usize_get(extent);
  /* The following should have been caught by callers. */
  assert(usize > 0 && usize <= SC_LARGE_MAXCLASS);
  /* Both allocation sizes must be large to avoid a move. */
  assert(oldusize >= SC_LARGE_MINCLASS && usize >= SC_LARGE_MINCLASS);

  /* Try to avoid moving the allocation. */
  if (!large_ralloc_no_move(tsdn, extent, usize, usize, zero)) {
    hook_invoke_expand(
      hook_args->is_realloc ? hook_expand_realloc : hook_expand_rallocx,
      ptr, oldusize, usize, (uintptr_t)ptr, hook_args->args);
    return extent_addr_get(extent);
  }

  /*
   * usize and old size are different enough that we need to use a
   * different size class.  In that case, fall back to allocating new
   * space and copying.
   */
  void* ret = large_ralloc_move_helper(tsdn, arena, usize, alignment, zero);
  if (ret == NULL) { return NULL; }

  hook_invoke_alloc(
    hook_args->is_realloc ? hook_alloc_realloc : hook_alloc_rallocx, ret,
    (uintptr_t)ret, hook_args->args);
  hook_invoke_dalloc(
    hook_args->is_realloc ? hook_dalloc_realloc : hook_dalloc_rallocx, ptr,
    hook_args->args);

  size_t copysize = (usize < oldusize) ? usize : oldusize;
  jet_memcpy(ret, extent_addr_get(extent), copysize);
  isdalloct(tsdn, extent_addr_get(extent), oldusize, tcache, NULL, true);
  return ret;
}

/*
 * junked_locked indicates whether the extent's data have been junk-filled,
 * and whether the arena's large_mtx is currently held.
 */
static void large_dalloc_prep_impl(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent, bool junked_locked) {
  if (!junked_locked) {
    /* See comments in arena_bin_slabs_full_insert(). */
    if (!arena_is_auto(arena)) {
      malloc_mutex_lock(tsdn, &arena->large_mtx);
      extent_list_remove(&arena->large, extent);
      malloc_mutex_unlock(tsdn, &arena->large_mtx);
    }
    large_dalloc_maybe_junk(
      extent_addr_get(extent), extent_usize_get(extent));
  } else {
    /* Only hold the large_mtx if necessary. */
    if (!arena_is_auto(arena)) {
      malloc_mutex_assert_owner(tsdn, &arena->large_mtx);
      extent_list_remove(&arena->large, extent);
    }
  }
  arena_extent_dalloc_large_prep(tsdn, arena, extent);
}

static void large_dalloc_finish_impl(
  tsdn_t* tsdn, arena_t* arena, extent_t* extent) {
  extent_hooks_t* extent_hooks = EXTENT_HOOKS_INITIALIZER;
  arena_extents_dirty_dalloc(tsdn, arena, &extent_hooks, extent);
}

void large_dalloc_prep_junked_locked(tsdn_t* tsdn, extent_t* extent) {
  large_dalloc_prep_impl(tsdn, extent_arena_get(extent), extent, true);
}

void large_dalloc_finish(tsdn_t* tsdn, extent_t* extent) {
  large_dalloc_finish_impl(tsdn, extent_arena_get(extent), extent);
}

void large_dalloc(tsdn_t* tsdn, extent_t* extent) {
  arena_t* arena = extent_arena_get(extent);
  large_dalloc_prep_impl(tsdn, arena, extent, false);
  large_dalloc_finish_impl(tsdn, arena, extent);
  arena_decay_tick(tsdn, arena);
}

size_t large_salloc(tsdn_t* tsdn, const extent_t* extent) {
  return extent_usize_get(extent);
}

prof_tctx_t* large_prof_tctx_get(tsdn_t* tsdn, const extent_t* extent) {
  return extent_prof_tctx_get(extent);
}

void large_prof_tctx_set(
  tsdn_t* tsdn, extent_t* extent, prof_tctx_t* tctx) {
  extent_prof_tctx_set(extent, tctx);
}

void large_prof_tctx_reset(tsdn_t* tsdn, extent_t* extent) {
  large_prof_tctx_set(tsdn, extent, (prof_tctx_t*)(uintptr_t)1U);
}

nstime_t large_prof_alloc_time_get(const extent_t* extent) {
  return extent_prof_alloc_time_get(extent);
}

void large_prof_alloc_time_set(extent_t* extent, nstime_t t) {
  extent_prof_alloc_time_set(extent, t);
}

#include "jemalloc/internal/log.h"

char log_var_names[JEMALLOC_LOG_VAR_BUFSIZE];
atomic_b_t log_init_done = ATOMIC_INIT(false);

/*
 * Returns true if we were able to pick out a segment.  Fills in
 * r_segment_end with a pointer to the first character after the end of the
 * string.
 */
static const char* log_var_extract_segment(const char* segment_begin) {
  const char* end;
  for (end = segment_begin; *end != '\0' && *end != '|'; end++) { }
  return end;
}

static bool log_var_matches_segment(const char* segment_begin,
  const char* segment_end, const char* log_var_begin,
  const char* log_var_end) {
  assert(segment_begin <= segment_end);
  assert(log_var_begin < log_var_end);

  ptrdiff_t segment_len = segment_end - segment_begin;
  ptrdiff_t log_var_len = log_var_end - log_var_begin;
  /* The special '.' segment matches everything. */
  if (segment_len == 1 && *segment_begin == '.') { return true; }
  if (segment_len == log_var_len) {
    return strncmp(segment_begin, log_var_begin, segment_len) == 0;
  } else if (segment_len < log_var_len) {
    return strncmp(segment_begin, log_var_begin, segment_len) == 0
      && log_var_begin[segment_len] == '.';
  } else {
    return false;
  }
}

unsigned log_var_update_state(log_var_t* log_var) {
  const char* log_var_begin = log_var->name;
  const char* log_var_end = log_var->name + cstr_len(log_var->name);

  /* Pointer to one before the beginning of the current segment. */
  const char* segment_begin = log_var_names;

  /*
   * If log_init done is false, we haven't parsed the malloc conf yet.  To
   * avoid log-spew, we default to not displaying anything.
   */
  if (!atomic_load_b(&log_init_done, ATOMIC_ACQUIRE)) {
    return LOG_INITIALIZED_NOT_ENABLED;
  }

  while (true) {
    const char* segment_end = log_var_extract_segment(segment_begin);
    assert(segment_end < log_var_names + JEMALLOC_LOG_VAR_BUFSIZE);
    if (log_var_matches_segment(
          segment_begin, segment_end, log_var_begin, log_var_end)) {
      atomic_store_u(&log_var->state, LOG_ENABLED, ATOMIC_RELAXED);
      return LOG_ENABLED;
    }
    if (*segment_end == '\0') {
      /* Hit the end of the segment string with no match. */
      atomic_store_u(
        &log_var->state, LOG_INITIALIZED_NOT_ENABLED, ATOMIC_RELAXED);
      return LOG_INITIALIZED_NOT_ENABLED;
    }
    /* Otherwise, skip the delimiter and continue. */
    segment_begin = segment_end + 1;
  }
}
#define JEMALLOC_MALLOC_IO_C_

#include "jemalloc/internal/malloc_io.h"

#ifdef assert
#undef assert
#endif
#ifdef not_reached
#undef not_reached
#endif
#ifdef not_implemented
#undef not_implemented
#endif
#ifdef assert_not_implemented
#undef assert_not_implemented
#endif

/*
 * Define simple versions of assertion macros that won't recurse in case
 * of assertion failures in malloc_*printf().
 */
#define assert(e)                                                          \
  do {                                                                     \
    if (config_debug && !(e)) {                                            \
      malloc_write("<jemalloc>: Failed assertion\n");                      \
      abort();                                                             \
    }                                                                      \
  } while (0)

#define not_reached()                                                      \
  do {                                                                     \
    if (config_debug) {                                                    \
      malloc_write("<jemalloc>: Unreachable code reached\n");              \
      abort();                                                             \
    }                                                                      \
    unreachable();                                                         \
  } while (0)

#define not_implemented()                                                  \
  do {                                                                     \
    if (config_debug) {                                                    \
      malloc_write("<jemalloc>: Not implemented\n");                       \
      abort();                                                             \
    }                                                                      \
  } while (0)

#define assert_not_implemented(e)                                          \
  do {                                                                     \
    if (unlikely(config_debug && !(e))) { not_implemented(); }             \
  } while (0)

/******************************************************************************/
/* Function prototypes for non-inline static functions. */

static void wrtmessage(void* cbopaque, const char* s);
#define U2S_BUFSIZE ((1U << (LG_SIZEOF_INTMAX_T + 3)) + 1)
static char* u2s(
  uintmax_t x, unsigned base, bool uppercase, char* s, size_t* slen_p);
#define D2S_BUFSIZE (1 + U2S_BUFSIZE)
static char* d2s(intmax_t x, char sign, char* s, size_t* slen_p);
#define O2S_BUFSIZE (1 + U2S_BUFSIZE)
static char* o2s(uintmax_t x, bool alt_form, char* s, size_t* slen_p);
#define X2S_BUFSIZE (2 + U2S_BUFSIZE)
static char* x2s(
  uintmax_t x, bool alt_form, bool uppercase, char* s, size_t* slen_p);

/******************************************************************************/

/* malloc_message() setup. */
static void wrtmessage(void* cbopaque, const char* s) {
  malloc_write_fd(STDERR_FILENO, s, cstr_len(s));
}

JEMALLOC_EXPORT void (*je_malloc_message)(void*, const char* s);

/*
 * Wrapper around malloc_message() that avoids the need for
 * je_malloc_message(...) throughout the code.
 */
void malloc_write(const char* s) {
  if (je_malloc_message != NULL) {
    je_malloc_message(NULL, s);
  } else {
    wrtmessage(NULL, s);
  }
}

/*
 * glibc provides a non-standard strerror_r() when _GNU_SOURCE is defined,
 * so provide a wrapper.
 */
int buferror(int err, char* buf, size_t buflen) {
#ifdef _WIN32
  FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM, NULL, err, 0, (LPSTR)buf,
    (DWORD)buflen, NULL);
  return 0;
#elif defined(JEMALLOC_STRERROR_R_RETURNS_CHAR_WITH_GNU_SOURCE)            \
  && defined(_GNU_SOURCE)
  char* b = strerror_r(err, buf, buflen);
  if (b != buf) {
    strncpy(buf, b, buflen);
    buf[buflen - 1] = '\0';
  }
  return 0;
#else
  return strerror_r(err, buf, buflen);
#endif
}

uintmax_t malloc_strtoumax(
  const char* restrict nptr, char** restrict endptr, int base) {
  uintmax_t ret, digit;
  unsigned b;
  bool neg;
  const char *p, *ns;

  p = nptr;
  if (base < 0 || base == 1 || base > 36) {
    ns = p;
    set_errno(EINVAL);
    ret = UINTMAX_MAX;
    goto label_return;
  }
  b = base;

  /* Swallow leading whitespace and get sign, if any. */
  neg = false;
  while (true) {
    switch (*p) {
    case '\t':
    case '\n':
    case '\v':
    case '\f':
    case '\r':
    case ' ': p++; break;
    case '-':
      neg = true;
      /* Fall through. */
    case '+':
      p++;
      /* Fall through. */
    default: goto label_prefix;
    }
  }

/* Get prefix, if any. */
label_prefix:
  /*
   * Note where the first non-whitespace/sign character is so that it is
   * possible to tell whether any digits are consumed (e.g., "  0" vs.
   * "  -x").
   */
  ns = p;
  if (*p == '0') {
    switch (p[1]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      if (b == 0) { b = 8; }
      if (b == 8) { p++; }
      break;
    case 'X':
    case 'x':
      switch (p[2]) {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
        if (b == 0) { b = 16; }
        if (b == 16) { p += 2; }
        break;
      default: break;
      }
      break;
    default:
      p++;
      ret = 0;
      goto label_return;
    }
  }
  if (b == 0) { b = 10; }

  /* Convert. */
  ret = 0;
  while ((*p >= '0' && *p <= '9' && (digit = *p - '0') < b)
    || (*p >= 'A' && *p <= 'Z' && (digit = 10 + *p - 'A') < b)
    || (*p >= 'a' && *p <= 'z' && (digit = 10 + *p - 'a') < b)) {
    uintmax_t pret = ret;
    ret *= b;
    ret += digit;
    if (ret < pret) {
      /* Overflow. */
      set_errno(ERANGE);
      ret = UINTMAX_MAX;
      goto label_return;
    }
    p++;
  }
  if (neg) { ret = (uintmax_t)(-((intmax_t)ret)); }

  if (p == ns) {
    /* No conversion performed. */
    set_errno(EINVAL);
    ret = UINTMAX_MAX;
    goto label_return;
  }

label_return:
  if (endptr != NULL) {
    if (p == ns) {
      /* No characters were converted. */
      *endptr = (char*)nptr;
    } else {
      *endptr = (char*)p;
    }
  }
  return ret;
}

static char* u2s(
  uintmax_t x, unsigned base, bool uppercase, char* s, size_t* slen_p) {
  unsigned i;

  i = U2S_BUFSIZE - 1;
  s[i] = '\0';
  switch (base) {
  case 10:
    do {
      i--;
      s[i] = "0123456789"[x % (uint64_t)10];
      x /= (uint64_t)10;
    } while (x > 0);
    break;
  case 16: {
    const char* digits
      = (uppercase) ? "0123456789ABCDEF" : "0123456789abcdef";

    do {
      i--;
      s[i] = digits[x & 0xf];
      x >>= 4;
    } while (x > 0);
    break;
  }
  default: {
    const char* digits = (uppercase)
      ? "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      : "0123456789abcdefghijklmnopqrstuvwxyz";

    assert(base >= 2 && base <= 36);
    do {
      i--;
      s[i] = digits[x % (uint64_t)base];
      x /= (uint64_t)base;
    } while (x > 0);
  }
  }

  *slen_p = U2S_BUFSIZE - 1 - i;
  return &s[i];
}

static char* d2s(intmax_t x, char sign, char* s, size_t* slen_p) {
  bool neg;

  if ((neg = (x < 0))) { x = -x; }
  s = u2s(x, 10, false, s, slen_p);
  if (neg) { sign = '-'; }
  switch (sign) {
  case '-':
    if (!neg) { break; }
    /* Fall through. */
  case ' ':
  case '+':
    s--;
    (*slen_p)++;
    *s = sign;
    break;
  default: not_reached();
  }
  return s;
}

static char* o2s(uintmax_t x, bool alt_form, char* s, size_t* slen_p) {
  s = u2s(x, 8, false, s, slen_p);
  if (alt_form && *s != '0') {
    s--;
    (*slen_p)++;
    *s = '0';
  }
  return s;
}

static char* x2s(
  uintmax_t x, bool alt_form, bool uppercase, char* s, size_t* slen_p) {
  s = u2s(x, 16, uppercase, s, slen_p);
  if (alt_form) {
    s -= 2;
    (*slen_p) += 2;
    jet_memcpy(s, uppercase ? "0X" : "0x", 2);
  }
  return s;
}

size_t malloc_vsnprintf(
  char* str, size_t size, const char* format, va_list ap) {
  size_t i;
  const char* f;

#define APPEND_C(c)                                                        \
  do {                                                                     \
    if (i < size) { str[i] = (c); }                                        \
    i++;                                                                   \
  } while (0)
#define APPEND_S(s, slen)                                                  \
  do {                                                                     \
    if (i < size) {                                                        \
      size_t cpylen = (slen <= size - i) ? slen : size - i;                \
      jet_memcpy(&str[i], s, cpylen);                                      \
    }                                                                      \
    i += slen;                                                             \
  } while (0)
#define APPEND_PADDED_S(s, slen, width, left_justify)                      \
  do {                                                                     \
    /* Left padding. */                                                    \
    size_t pad_len = (width == -1)                                         \
      ? 0                                                                  \
      : ((slen < (size_t)width) ? (size_t)width - slen : 0);               \
    if (!left_justify && pad_len != 0) {                                   \
      size_t j;                                                            \
      for (j = 0; j < pad_len; j++) { APPEND_C(' '); }                     \
    }                                                                      \
    /* Value. */                                                           \
    APPEND_S(s, slen);                                                     \
    /* Right padding. */                                                   \
    if (left_justify && pad_len != 0) {                                    \
      size_t j;                                                            \
      for (j = 0; j < pad_len; j++) { APPEND_C(' '); }                     \
    }                                                                      \
  } while (0)
#define GET_ARG_NUMERIC(val, len)                                          \
  do {                                                                     \
    switch ((unsigned char)len) {                                          \
    case '?': val = va_arg(ap, int); break;                                \
    case '?' | 0x80: val = va_arg(ap, unsigned int); break;                \
    case 'l': val = va_arg(ap, long); break;                               \
    case 'l' | 0x80: val = va_arg(ap, unsigned long); break;               \
    case 'q': val = va_arg(ap, long long); break;                          \
    case 'q' | 0x80: val = va_arg(ap, unsigned long long); break;          \
    case 'j': val = va_arg(ap, intmax_t); break;                           \
    case 'j' | 0x80: val = va_arg(ap, uintmax_t); break;                   \
    case 't': val = va_arg(ap, ptrdiff_t); break;                          \
    case 'z': val = va_arg(ap, ssize_t); break;                            \
    case 'z' | 0x80: val = va_arg(ap, size_t); break;                      \
    case 'p': /* Synthetic; used for %p. */                                \
      val = va_arg(ap, uintptr_t);                                         \
      break;                                                               \
    default: not_reached(); val = 0;                                       \
    }                                                                      \
  } while (0)

  i = 0;
  f = format;
  while (true) {
    switch (*f) {
    case '\0': goto label_out;
    case '%': {
      bool alt_form = false;
      bool left_justify = false;
      bool plus_space = false;
      bool plus_plus = false;
      int prec = -1;
      int width = -1;
      unsigned char len = '?';
      char* s;
      size_t slen;

      f++;
      /* Flags. */
      while (true) {
        switch (*f) {
        case '#':
          assert(!alt_form);
          alt_form = true;
          break;
        case '-':
          assert(!left_justify);
          left_justify = true;
          break;
        case ' ':
          assert(!plus_space);
          plus_space = true;
          break;
        case '+':
          assert(!plus_plus);
          plus_plus = true;
          break;
        default: goto label_width;
        }
        f++;
      }
    /* Width. */
    label_width:
      switch (*f) {
      case '*':
        width = va_arg(ap, int);
        f++;
        if (width < 0) {
          left_justify = true;
          width = -width;
        }
        break;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': {
        uintmax_t uwidth;
        set_errno(0);
        uwidth = malloc_strtoumax(f, (char**)&f, 10);
        assert(uwidth != UINTMAX_MAX || get_errno() != ERANGE);
        width = (int)uwidth;
        break;
      }
      default: break;
      }
      /* Width/precision separator. */
      if (*f == '.') {
        f++;
      } else {
        goto label_length;
      }
      /* Precision. */
      switch (*f) {
      case '*':
        prec = va_arg(ap, int);
        f++;
        break;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': {
        uintmax_t uprec;
        set_errno(0);
        uprec = malloc_strtoumax(f, (char**)&f, 10);
        assert(uprec != UINTMAX_MAX || get_errno() != ERANGE);
        prec = (int)uprec;
        break;
      }
      default: break;
      }
    /* Length. */
    label_length:
      switch (*f) {
      case 'l':
        f++;
        if (*f == 'l') {
          len = 'q';
          f++;
        } else {
          len = 'l';
        }
        break;
      case 'q':
      case 'j':
      case 't':
      case 'z':
        len = *f;
        f++;
        break;
      default: break;
      }
      /* Conversion specifier. */
      switch (*f) {
      case '%':
        /* %% */
        APPEND_C(*f);
        f++;
        break;
      case 'd':
      case 'i': {
        intmax_t val JEMALLOC_CC_SILENCE_INIT(0);
        char buf[D2S_BUFSIZE];

        GET_ARG_NUMERIC(val, len);
        s = d2s(
          val, (plus_plus ? '+' : (plus_space ? ' ' : '-')), buf, &slen);
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      }
      case 'o': {
        uintmax_t val JEMALLOC_CC_SILENCE_INIT(0);
        char buf[O2S_BUFSIZE];

        GET_ARG_NUMERIC(val, len | 0x80);
        s = o2s(val, alt_form, buf, &slen);
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      }
      case 'u': {
        uintmax_t val JEMALLOC_CC_SILENCE_INIT(0);
        char buf[U2S_BUFSIZE];

        GET_ARG_NUMERIC(val, len | 0x80);
        s = u2s(val, 10, false, buf, &slen);
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      }
      case 'x':
      case 'X': {
        uintmax_t val JEMALLOC_CC_SILENCE_INIT(0);
        char buf[X2S_BUFSIZE];

        GET_ARG_NUMERIC(val, len | 0x80);
        s = x2s(val, alt_form, *f == 'X', buf, &slen);
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      }
      case 'c': {
        unsigned char val;
        char buf[2];

        assert(len == '?' || len == 'l');
        assert_not_implemented(len != 'l');
        val = va_arg(ap, int);
        buf[0] = val;
        buf[1] = '\0';
        APPEND_PADDED_S(buf, 1, width, left_justify);
        f++;
        break;
      }
      case 's':
        assert(len == '?' || len == 'l');
        assert_not_implemented(len != 'l');
        s = va_arg(ap, char*);
        slen = (prec < 0) ? cstr_len(s) : (size_t)prec;
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      case 'p': {
        uintmax_t val;
        char buf[X2S_BUFSIZE];

        GET_ARG_NUMERIC(val, 'p');
        s = x2s(val, true, false, buf, &slen);
        APPEND_PADDED_S(s, slen, width, left_justify);
        f++;
        break;
      }
      default: not_reached();
      }
      break;
    }
    default: {
      APPEND_C(*f);
      f++;
      break;
    }
    }
  }
label_out:
  if (i < size) {
    str[i] = '\0';
  } else {
    str[size - 1] = '\0';
  }

#undef APPEND_C
#undef APPEND_S
#undef APPEND_PADDED_S
#undef GET_ARG_NUMERIC
  return i;
}

JEMALLOC_FORMAT_PRINTF(3, 4)
size_t malloc_snprintf(char* str, size_t size, const char* format, ...) {
  size_t ret;
  va_list ap;

  va_start(ap, format);
  ret = malloc_vsnprintf(str, size, format, ap);
  va_end(ap);

  return ret;
}

void malloc_vcprintf(void (*write_cb)(void*, const char*), void* cbopaque,
  const char* format, va_list ap) {
  char buf[MALLOC_PRINTF_BUFSIZE];

  if (write_cb == NULL) {
    /*
     * The caller did not provide an alternate write_cb callback
     * function, so use the default one.  malloc_write() is an
     * inline function, so use malloc_message() directly here.
     */
    write_cb = (je_malloc_message != NULL) ? je_malloc_message : wrtmessage;
  }

  malloc_vsnprintf(buf, sizeof(buf), format, ap);
  write_cb(cbopaque, buf);
}

/*
 * Print to a callback function in such a way as to (hopefully) avoid memory
 * allocation.
 */
JEMALLOC_FORMAT_PRINTF(3, 4)
void malloc_cprintf(void (*write_cb)(void*, const char*), void* cbopaque,
  const char* format, ...) {
  va_list ap;

  va_start(ap, format);
  malloc_vcprintf(write_cb, cbopaque, format, ap);
  va_end(ap);
}

/* Print to stderr in such a way as to avoid memory allocation. */
JEMALLOC_FORMAT_PRINTF(1, 2)
void malloc_printf(const char* format, ...) {
  va_list ap;

  va_start(ap, format);
  malloc_vcprintf(NULL, NULL, format, ap);
  va_end(ap);
}

/*
 * Restore normal assertion macros, in order to make it possible to compile
 * all C files as a single concatenation.
 */
// #undef assert
// #undef not_reached
// #undef not_implemented
// #undef assert_not_implemented
#define JEMALLOC_MUTEX_C_

#include "jemalloc/internal/malloc_io.h"
#include "jemalloc/internal/spin.h"

#ifndef _CRT_SPINCOUNT
#define _CRT_SPINCOUNT 4000
#endif

/******************************************************************************/
/* Data. */

#ifdef JEMALLOC_LAZY_LOCK
bool isthreaded = false;
#endif
#ifdef JEMALLOC_MUTEX_INIT_CB
static bool postpone_init = true;
static malloc_mutex_t* postponed_mutexes = NULL;
#endif

/******************************************************************************/
/*
 * We intercept pthread_create() calls in order to toggle isthreaded if the
 * process goes multi-threaded.
 */

#if defined(JEMALLOC_LAZY_LOCK) && !defined(_WIN32)
JEMALLOC_EXPORT int pthread_create(pthread_t* __restrict thread,
  const pthread_attr_t* __restrict attr, void* (*start_routine)(void*),
  void* __restrict arg) {
  return pthread_create_wrapper(thread, attr, start_routine, arg);
}
#endif

/******************************************************************************/

#ifdef JEMALLOC_MUTEX_INIT_CB
JEMALLOC_EXPORT int _pthread_mutex_init_calloc_cb(
  pthread_mutex_t* mutex, void*(calloc_cb)(size_t, size_t));
#endif

void malloc_mutex_lock_slow(malloc_mutex_t* mutex) {
  mutex_prof_data_t* data = &mutex->prof_data;
  nstime_t before = NSTIME_ZERO_INITIALIZER;

  if (ncpus == 1) { goto label_spin_done; }

  int cnt = 0, max_cnt = MALLOC_MUTEX_MAX_SPIN;
  do {
    spin_cpu_spinwait();
    if (!atomic_load_b(&mutex->locked, ATOMIC_RELAXED)
      && !malloc_mutex_trylock_final(mutex)) {
      data->n_spin_acquired++;
      return;
    }
  } while (cnt++ < max_cnt);

  if (!config_stats) {
    /* Only spin is useful when stats is off. */
    malloc_mutex_lock_final(mutex);
    return;
  }
label_spin_done:
  nstime_update(&before);
  /* Copy before to after to avoid clock skews. */
  nstime_t after;
  nstime_copy(&after, &before);
  uint32_t n_thds
    = atomic_fetch_add_u32(&data->n_waiting_thds, 1, ATOMIC_RELAXED) + 1;
  /* One last try as above two calls may take quite some cycles. */
  if (!malloc_mutex_trylock_final(mutex)) {
    atomic_fetch_sub_u32(&data->n_waiting_thds, 1, ATOMIC_RELAXED);
    data->n_spin_acquired++;
    return;
  }

  /* True slow path. */
  malloc_mutex_lock_final(mutex);
  /* Update more slow-path only counters. */
  atomic_fetch_sub_u32(&data->n_waiting_thds, 1, ATOMIC_RELAXED);
  nstime_update(&after);

  nstime_t delta;
  nstime_copy(&delta, &after);
  nstime_subtract(&delta, &before);

  data->n_wait_times++;
  nstime_add(&data->tot_wait_time, &delta);
  if (nstime_compare(&data->max_wait_time, &delta) < 0) {
    nstime_copy(&data->max_wait_time, &delta);
  }
  if (n_thds > data->max_n_thds) { data->max_n_thds = n_thds; }
}

static void mutex_prof_data_init(mutex_prof_data_t* data) {
  memset(data, 0, sizeof(mutex_prof_data_t));
  nstime_init(&data->max_wait_time, 0);
  nstime_init(&data->tot_wait_time, 0);
  data->prev_owner = NULL;
}

void malloc_mutex_prof_data_reset(tsdn_t* tsdn, malloc_mutex_t* mutex) {
  malloc_mutex_assert_owner(tsdn, mutex);
  mutex_prof_data_init(&mutex->prof_data);
}

static int mutex_addr_comp(const witness_t* witness1, void* mutex1,
  const witness_t* witness2, void* mutex2) {
  assert(mutex1 != NULL);
  assert(mutex2 != NULL);
  uintptr_t mu1int = (uintptr_t)mutex1;
  uintptr_t mu2int = (uintptr_t)mutex2;
  if (mu1int < mu2int) {
    return -1;
  } else if (mu1int == mu2int) {
    return 0;
  } else {
    return 1;
  }
}

bool malloc_mutex_init(malloc_mutex_t* mutex, const char* name,
  witness_rank_t rank, malloc_mutex_lock_order_t lock_order) {
  mutex_prof_data_init(&mutex->prof_data);
#ifdef _WIN32
#if _WIN32_WINNT >= 0x0600
  InitializeSRWLock(&mutex->lock);
#else
  if (!InitializeCriticalSectionAndSpinCount(
        &mutex->lock, _CRT_SPINCOUNT)) {
    return true;
  }
#endif
#elif (defined(JEMALLOC_OS_UNFAIR_LOCK))
  mutex->lock = OS_UNFAIR_LOCK_INIT;
#elif (defined(JEMALLOC_MUTEX_INIT_CB))
  if (postpone_init) {
    mutex->postponed_next = postponed_mutexes;
    postponed_mutexes = mutex;
  } else {
    if (_pthread_mutex_init_calloc_cb(&mutex->lock, bootstrap_calloc)
      != 0) {
      return true;
    }
  }
#else
  pthread_mutexattr_t attr;

  if (pthread_mutexattr_init(&attr) != 0) { return true; }
  pthread_mutexattr_settype(&attr, MALLOC_MUTEX_TYPE);
  if (pthread_mutex_init(&mutex->lock, &attr) != 0) {
    pthread_mutexattr_destroy(&attr);
    return true;
  }
  pthread_mutexattr_destroy(&attr);
#endif
  if (config_debug) {
    mutex->lock_order = lock_order;
    if (lock_order == malloc_mutex_address_ordered) {
      witness_init(&mutex->witness, name, rank, mutex_addr_comp, mutex);
    } else {
      witness_init(&mutex->witness, name, rank, NULL, NULL);
    }
  }
  return false;
}

void malloc_mutex_prefork(tsdn_t* tsdn, malloc_mutex_t* mutex) {
  malloc_mutex_lock(tsdn, mutex);
}

void malloc_mutex_postfork_parent(tsdn_t* tsdn, malloc_mutex_t* mutex) {
  malloc_mutex_unlock(tsdn, mutex);
}

void malloc_mutex_postfork_child(tsdn_t* tsdn, malloc_mutex_t* mutex) {
#ifdef JEMALLOC_MUTEX_INIT_CB
  malloc_mutex_unlock(tsdn, mutex);
#else
  if (malloc_mutex_init(mutex, mutex->witness.name, mutex->witness.rank,
        mutex->lock_order)) {
    malloc_printf("<jemalloc>: Error re-initializing mutex in "
                  "child\n");
    if (opt_abort) { abort(); }
  }
#endif
}

bool malloc_mutex_boot(void) {
#ifdef JEMALLOC_MUTEX_INIT_CB
  postpone_init = false;
  while (postponed_mutexes != NULL) {
    if (_pthread_mutex_init_calloc_cb(
          &postponed_mutexes->lock, bootstrap_calloc)
      != 0) {
      return true;
    }
    postponed_mutexes = postponed_mutexes->postponed_next;
  }
#endif
  return false;
}
#define JEMALLOC_MUTEX_POOL_C_

#include "jemalloc/internal/mutex_pool.h"

bool mutex_pool_init(
  mutex_pool_t* pool, const char* name, witness_rank_t rank) {
  for (int i = 0; i < MUTEX_POOL_SIZE; ++i) {
    if (malloc_mutex_init(
          &pool->mutexes[i], name, rank, malloc_mutex_address_ordered)) {
      return true;
    }
  }
  return false;
}

#include "jemalloc/internal/nstime.h"

#define BILLION UINT64_C(1000000000)
#define MILLION UINT64_C(1000000)

void nstime_init(nstime_t* time, uint64_t ns) { time->ns = ns; }

void nstime_init2(nstime_t* time, uint64_t sec, uint64_t nsec) {
  time->ns = sec * BILLION + nsec;
}

uint64_t nstime_ns(const nstime_t* time) { return time->ns; }

uint64_t nstime_msec(const nstime_t* time) { return time->ns / MILLION; }

uint64_t nstime_sec(const nstime_t* time) { return time->ns / BILLION; }

uint64_t nstime_nsec(const nstime_t* time) { return time->ns % BILLION; }

void nstime_copy(nstime_t* time, const nstime_t* source) {
  *time = *source;
}

int nstime_compare(const nstime_t* a, const nstime_t* b) {
  return (a->ns > b->ns) - (a->ns < b->ns);
}

void nstime_add(nstime_t* time, const nstime_t* addend) {
  assert(UINT64_MAX - time->ns >= addend->ns);

  time->ns += addend->ns;
}

void nstime_iadd(nstime_t* time, uint64_t addend) {
  assert(UINT64_MAX - time->ns >= addend);

  time->ns += addend;
}

void nstime_subtract(nstime_t* time, const nstime_t* subtrahend) {
  assert(nstime_compare(time, subtrahend) >= 0);

  time->ns -= subtrahend->ns;
}

void nstime_isubtract(nstime_t* time, uint64_t subtrahend) {
  assert(time->ns >= subtrahend);

  time->ns -= subtrahend;
}

void nstime_imultiply(nstime_t* time, uint64_t multiplier) {
  assert(
    (((time->ns | multiplier) & (UINT64_MAX << (sizeof(uint64_t) << 2)))
      == 0)
    || ((time->ns * multiplier) / multiplier == time->ns));

  time->ns *= multiplier;
}

void nstime_idivide(nstime_t* time, uint64_t divisor) {
  assert(divisor != 0);

  time->ns /= divisor;
}

uint64_t nstime_divide(const nstime_t* time, const nstime_t* divisor) {
  assert(divisor->ns != 0);

  return time->ns / divisor->ns;
}

#ifdef _WIN32
#define NSTIME_MONOTONIC true
static void nstime_get(nstime_t* time) {
  FILETIME ft;
  uint64_t ticks_100ns;

  GetSystemTimeAsFileTime(&ft);
  ticks_100ns = (((uint64_t)ft.dwHighDateTime) << 32) | ft.dwLowDateTime;

  nstime_init(time, ticks_100ns * 100);
}
#elif defined(JEMALLOC_HAVE_CLOCK_MONOTONIC_COARSE)
#define NSTIME_MONOTONIC true
static void nstime_get(nstime_t* time) {
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC_COARSE, &ts);
  nstime_init2(time, ts.tv_sec, ts.tv_nsec);
}
#elif defined(JEMALLOC_HAVE_CLOCK_MONOTONIC)
#define NSTIME_MONOTONIC true
static void nstime_get(nstime_t* time) {
  struct timespec ts;

  clock_gettime(CLOCK_MONOTONIC, &ts);
  nstime_init2(time, ts.tv_sec, ts.tv_nsec);
}
#elif defined(JEMALLOC_HAVE_MACH_ABSOLUTE_TIME)
#define NSTIME_MONOTONIC true
static void nstime_get(nstime_t* time) {
  nstime_init(time, mach_absolute_time());
}
#else
#define NSTIME_MONOTONIC false
static void nstime_get(nstime_t* time) {
  struct timeval tv;

  gettimeofday(&tv, NULL);
  nstime_init2(time, tv.tv_sec, tv.tv_usec * 1000);
}
#endif

static bool nstime_monotonic_impl(void) {
  return NSTIME_MONOTONIC;
#undef NSTIME_MONOTONIC
}
nstime_monotonic_t* JET_MUTABLE nstime_monotonic = nstime_monotonic_impl;

static bool nstime_update_impl(nstime_t* time) {
  nstime_t old_time;

  nstime_copy(&old_time, time);
  nstime_get(time);

  /* Handle non-monotonic clocks. */
  if (unlikely(nstime_compare(&old_time, time) > 0)) {
    nstime_copy(time, &old_time);
    return true;
  }

  return false;
}
nstime_update_t* JET_MUTABLE nstime_update = nstime_update_impl;
#define JEMALLOC_PAGES_C_

#include "jemalloc/internal/pages.h"

#include "jemalloc/internal/malloc_io.h"

#ifdef JEMALLOC_SYSCTL_VM_OVERCOMMIT
#include <sys/sysctl.h>
#ifdef __FreeBSD__
#include <vm/vm_param.h>
#endif
#endif

/******************************************************************************/
/* Data. */

/* Actual operating system page size, detected during bootstrap, <= PAGE. */
static size_t os_page;

#ifndef _WIN32
#define PAGES_PROT_COMMIT (PROT_READ | PROT_WRITE)
#define PAGES_PROT_DECOMMIT (PROT_NONE)
static int mmap_flags;
#endif
static bool os_overcommits;

const char* thp_mode_names[]
  = { "default", "always", "never", "not supported" };
thp_mode_t opt_thp = THP_MODE_DEFAULT;
thp_mode_t init_system_thp_mode;

/* Runtime support for lazy purge. Irrelevant when !pages_can_purge_lazy. */
static bool pages_can_purge_lazy_runtime = true;

/******************************************************************************/
/*
 * Function prototypes for static functions that are referenced prior to
 * definition.
 */

static void os_pages_unmap(void* addr, size_t size);

/******************************************************************************/

static void* os_pages_map(
  void* addr, size_t size, size_t alignment, bool* commit) {
  assert(ALIGNMENT_ADDR2BASE(addr, os_page) == addr);
  assert(ALIGNMENT_CEILING(size, os_page) == size);
  assert(size != 0);

  if (os_overcommits) { *commit = true; }

  void* ret;
#ifdef _WIN32
  /*
   * If VirtualAlloc can't allocate at the given address when one is
   * given, it fails and returns NULL.
   */
  ret = VirtualAlloc(
    addr, size, MEM_RESERVE | (*commit ? MEM_COMMIT : 0), PAGE_READWRITE);
#else
  /*
   * We don't use MAP_FIXED here, because it can cause the *replacement*
   * of existing mappings, and we only want to create new mappings.
   */
  {
    int prot = *commit ? PAGES_PROT_COMMIT : PAGES_PROT_DECOMMIT;

    ret = mmap(addr, size, prot, mmap_flags, -1, 0);
  }
  assert(ret != NULL);

  if (ret == MAP_FAILED) {
    ret = NULL;
  } else if (addr != NULL && ret != addr) {
    /*
     * We succeeded in mapping memory, but not in the right place.
     */
    os_pages_unmap(ret, size);
    ret = NULL;
  }
#endif
  assert(ret == NULL || (addr == NULL && ret != addr)
    || (addr != NULL && ret == addr));
  return ret;
}

static void* os_pages_trim(void* addr, size_t alloc_size, size_t leadsize,
  size_t size, bool* commit) {
  void* ret = (void*)((uintptr_t)addr + leadsize);

  assert(alloc_size >= leadsize + size);
#ifdef _WIN32
  os_pages_unmap(addr, alloc_size);
  void* new_addr = os_pages_map(ret, size, PAGE, commit);
  if (new_addr == ret) { return ret; }
  if (new_addr != NULL) { os_pages_unmap(new_addr, size); }
  return NULL;
#else
  size_t trailsize = alloc_size - leadsize - size;

  if (leadsize != 0) { os_pages_unmap(addr, leadsize); }
  if (trailsize != 0) {
    os_pages_unmap((void*)((uintptr_t)ret + size), trailsize);
  }
  return ret;
#endif
}

static void os_pages_unmap(void* addr, size_t size) {
  assert(ALIGNMENT_ADDR2BASE(addr, os_page) == addr);
  assert(ALIGNMENT_CEILING(size, os_page) == size);

#ifdef _WIN32
  if (VirtualFree(addr, 0, MEM_RELEASE) == 0)
#else
  if (munmap(addr, size) == -1)
#endif
  {
    char buf[BUFERROR_BUF];

    buferror(get_errno(), buf, sizeof(buf));
    malloc_printf("<jemalloc>: Error in "
#ifdef _WIN32
                  "VirtualFree"
#else
                  "munmap"
#endif
                  "(): %s\n",
      buf);
    if (opt_abort) { abort(); }
  }
}

static void* pages_map_slow(size_t size, size_t alignment, bool* commit) {
  size_t alloc_size = size + alignment - os_page;
  /* Beware size_t wrap-around. */
  if (alloc_size < size) { return NULL; }

  void* ret;
  do {
    void* pages = os_pages_map(NULL, alloc_size, alignment, commit);
    if (pages == NULL) { return NULL; }
    size_t leadsize
      = ALIGNMENT_CEILING((uintptr_t)pages, alignment) - (uintptr_t)pages;
    ret = os_pages_trim(pages, alloc_size, leadsize, size, commit);
  } while (ret == NULL);

  assert(ret != NULL);
  assert(PAGE_ADDR2BASE(ret) == ret);
  return ret;
}

void* pages_map(void* addr, size_t size, size_t alignment, bool* commit) {
  assert(alignment >= PAGE);
  assert(ALIGNMENT_ADDR2BASE(addr, alignment) == addr);

#if defined(__FreeBSD__) && defined(MAP_EXCL)
  /*
   * FreeBSD has mechanisms both to mmap at specific address without
   * touching existing mappings, and to mmap with specific alignment.
   */
  {
    if (os_overcommits) { *commit = true; }

    int prot = *commit ? PAGES_PROT_COMMIT : PAGES_PROT_DECOMMIT;
    int flags = mmap_flags;

    if (addr != NULL) {
      flags |= MAP_FIXED | MAP_EXCL;
    } else {
      unsigned alignment_bits = ffs_zu(alignment);
      assert(alignment_bits > 1);
      flags |= MAP_ALIGNED(alignment_bits - 1);
    }

    void* ret = mmap(addr, size, prot, flags, -1, 0);
    if (ret == MAP_FAILED) { ret = NULL; }

    return ret;
  }
#endif
  /*
   * Ideally, there would be a way to specify alignment to mmap() (like
   * NetBSD has), but in the absence of such a feature, we have to work
   * hard to efficiently create aligned mappings.  The reliable, but
   * slow method is to create a mapping that is over-sized, then trim the
   * excess.  However, that always results in one or two calls to
   * os_pages_unmap(), and it can leave holes in the process's virtual
   * memory map if memory grows downward.
   *
   * Optimistically try mapping precisely the right amount before falling
   * back to the slow method, with the expectation that the optimistic
   * approach works most of the time.
   */

  void* ret = os_pages_map(addr, size, os_page, commit);
  if (ret == NULL || ret == addr) { return ret; }
  assert(addr == NULL);
  if (ALIGNMENT_ADDR2OFFSET(ret, alignment) != 0) {
    os_pages_unmap(ret, size);
    return pages_map_slow(size, alignment, commit);
  }

  assert(PAGE_ADDR2BASE(ret) == ret);
  return ret;
}

void pages_unmap(void* addr, size_t size) {
  assert(PAGE_ADDR2BASE(addr) == addr);
  assert(PAGE_CEILING(size) == size);

  os_pages_unmap(addr, size);
}

static bool pages_commit_impl(void* addr, size_t size, bool commit) {
  assert(PAGE_ADDR2BASE(addr) == addr);
  assert(PAGE_CEILING(size) == size);

  if (os_overcommits) { return true; }

#ifdef _WIN32
  return (commit
      ? (addr != VirtualAlloc(addr, size, MEM_COMMIT, PAGE_READWRITE))
      : (!VirtualFree(addr, size, MEM_DECOMMIT)));
#else
  {
    int prot = commit ? PAGES_PROT_COMMIT : PAGES_PROT_DECOMMIT;
    void* result = mmap(addr, size, prot, mmap_flags | MAP_FIXED, -1, 0);
    if (result == MAP_FAILED) { return true; }
    if (result != addr) {
      /*
       * We succeeded in mapping memory, but not in the right
       * place.
       */
      os_pages_unmap(result, size);
      return true;
    }
    return false;
  }
#endif
}

bool pages_commit(void* addr, size_t size) {
  return pages_commit_impl(addr, size, true);
}

bool pages_decommit(void* addr, size_t size) {
  return pages_commit_impl(addr, size, false);
}

bool pages_purge_lazy(void* addr, size_t size) {
  assert(ALIGNMENT_ADDR2BASE(addr, os_page) == addr);
  assert(PAGE_CEILING(size) == size);

  if (!pages_can_purge_lazy) { return true; }
  if (!pages_can_purge_lazy_runtime) {
    /*
     * Built with lazy purge enabled, but detected it was not
     * supported on the current system.
     */
    return true;
  }

#ifdef _WIN32
  VirtualAlloc(addr, size, MEM_RESET, PAGE_READWRITE);
  return false;
#elif defined(JEMALLOC_PURGE_MADVISE_FREE)
  return (madvise(addr, size,
#ifdef MADV_FREE
            MADV_FREE
#else
            JEMALLOC_MADV_FREE
#endif
            )
    != 0);
#elif defined(JEMALLOC_PURGE_MADVISE_DONTNEED)                             \
  && !defined(JEMALLOC_PURGE_MADVISE_DONTNEED_ZEROS)
  return (madvise(addr, size, MADV_DONTNEED) != 0);
#else
  not_reached();
#endif
}

bool pages_purge_forced(void* addr, size_t size) {
  assert(PAGE_ADDR2BASE(addr) == addr);
  assert(PAGE_CEILING(size) == size);

  if (!pages_can_purge_forced) { return true; }

#if defined(JEMALLOC_PURGE_MADVISE_DONTNEED)                               \
  && defined(JEMALLOC_PURGE_MADVISE_DONTNEED_ZEROS)
  return (madvise(addr, size, MADV_DONTNEED) != 0);
#elif defined(JEMALLOC_MAPS_COALESCE)
  /* Try to overlay a new demand-zeroed mapping. */
  return pages_commit(addr, size);
#else
  not_reached();
#endif
}

static bool pages_huge_impl(void* addr, size_t size, bool aligned) {
  if (aligned) {
    assert(HUGEPAGE_ADDR2BASE(addr) == addr);
    assert(HUGEPAGE_CEILING(size) == size);
  }
#ifdef JEMALLOC_HAVE_MADVISE_HUGE
  return (madvise(addr, size, MADV_HUGEPAGE) != 0);
#else
  return true;
#endif
}

bool pages_huge(void* addr, size_t size) {
  return pages_huge_impl(addr, size, true);
}

static bool pages_huge_unaligned(void* addr, size_t size) {
  return pages_huge_impl(addr, size, false);
}

static bool pages_nohuge_impl(void* addr, size_t size, bool aligned) {
  if (aligned) {
    assert(HUGEPAGE_ADDR2BASE(addr) == addr);
    assert(HUGEPAGE_CEILING(size) == size);
  }

#ifdef JEMALLOC_HAVE_MADVISE_HUGE
  return (madvise(addr, size, MADV_NOHUGEPAGE) != 0);
#else
  return false;
#endif
}

bool pages_nohuge(void* addr, size_t size) {
  return pages_nohuge_impl(addr, size, true);
}

static bool pages_nohuge_unaligned(void* addr, size_t size) {
  return pages_nohuge_impl(addr, size, false);
}

bool pages_dontdump(void* addr, size_t size) {
  assert(PAGE_ADDR2BASE(addr) == addr);
  assert(PAGE_CEILING(size) == size);
#ifdef JEMALLOC_MADVISE_DONTDUMP
  return madvise(addr, size, MADV_DONTDUMP) != 0;
#else
  return false;
#endif
}

bool pages_dodump(void* addr, size_t size) {
  assert(PAGE_ADDR2BASE(addr) == addr);
  assert(PAGE_CEILING(size) == size);
#ifdef JEMALLOC_MADVISE_DONTDUMP
  return madvise(addr, size, MADV_DODUMP) != 0;
#else
  return false;
#endif
}

static size_t os_page_detect(void) {
#ifdef _WIN32
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
#elif defined(__FreeBSD__)
  /*
   * This returns the value obtained from
   * the auxv vector, avoiding a syscall.
   */
  return getpagesize();
#else
  long result = sysconf(_SC_PAGESIZE);
  if (result == -1) { return LG_PAGE; }
  return (size_t)result;
#endif
}

#ifdef JEMALLOC_SYSCTL_VM_OVERCOMMIT
static bool os_overcommits_sysctl(void) {
  int vm_overcommit;
  size_t sz;

  sz = sizeof(vm_overcommit);
#if defined(__FreeBSD__) && defined(VM_OVERCOMMIT)
  int mib[2];

  mib[0] = CTL_VM;
  mib[1] = VM_OVERCOMMIT;
  if (sysctl(mib, 2, &vm_overcommit, &sz, NULL, 0) != 0) {
    return false; /* Error. */
  }
#else
  if (sysctlbyname("vm.overcommit", &vm_overcommit, &sz, NULL, 0) != 0) {
    return false; /* Error. */
  }
#endif

  return ((vm_overcommit & 0x3) == 0);
}
#endif

#ifdef JEMALLOC_PROC_SYS_VM_OVERCOMMIT_MEMORY
/*
 * Use syscall(2) rather than {open,read,close}(2) when possible to avoid
 * reentry during bootstrapping if another library has interposed system
 * call wrappers.
 */
static bool os_overcommits_proc(void) {
  int fd;
  char buf[1];

#if defined(JEMALLOC_USE_SYSCALL) && defined(SYS_open)
#if defined(O_CLOEXEC)
  fd = (int)syscall(
    SYS_open, "/proc/sys/vm/overcommit_memory", O_RDONLY | O_CLOEXEC);
#else
  fd = (int)syscall(SYS_open, "/proc/sys/vm/overcommit_memory", O_RDONLY);
  if (fd != -1) { fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC); }
#endif
#elif defined(JEMALLOC_USE_SYSCALL) && defined(SYS_openat)
#if defined(O_CLOEXEC)
  fd = (int)syscall(SYS_openat, AT_FDCWD, "/proc/sys/vm/overcommit_memory",
    O_RDONLY | O_CLOEXEC);
#else
  fd = (int)syscall(
    SYS_openat, AT_FDCWD, "/proc/sys/vm/overcommit_memory", O_RDONLY);
  if (fd != -1) { fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC); }
#endif
#else
#if defined(O_CLOEXEC)
  fd = open("/proc/sys/vm/overcommit_memory", O_RDONLY | O_CLOEXEC);
#else
  fd = open("/proc/sys/vm/overcommit_memory", O_RDONLY);
  if (fd != -1) { fcntl(fd, F_SETFD, fcntl(fd, F_GETFD) | FD_CLOEXEC); }
#endif
#endif

  if (fd == -1) { return false; /* Error. */ }

  ssize_t nread = malloc_read_fd(fd, &buf, sizeof(buf));
#if defined(JEMALLOC_USE_SYSCALL) && defined(SYS_close)
  syscall(SYS_close, fd);
#else
  close(fd);
#endif

  if (nread < 1) { return false; /* Error. */ }
  /*
   * /proc/sys/vm/overcommit_memory meanings:
   * 0: Heuristic overcommit.
   * 1: Always overcommit.
   * 2: Never overcommit.
   */
  return (buf[0] == '0' || buf[0] == '1');
}
#endif

void pages_set_thp_state(void* ptr, size_t size) {
  if (opt_thp == thp_mode_default || opt_thp == init_system_thp_mode) {
    return;
  }
  assert(opt_thp != thp_mode_not_supported
    && init_system_thp_mode != thp_mode_not_supported);

  if (opt_thp == thp_mode_always
    && init_system_thp_mode != thp_mode_never) {
    assert(init_system_thp_mode == thp_mode_default);
    pages_huge_unaligned(ptr, size);
  } else if (opt_thp == thp_mode_never) {
    assert(init_system_thp_mode == thp_mode_default
      || init_system_thp_mode == thp_mode_always);
    pages_nohuge_unaligned(ptr, size);
  }
}

static void init_thp_state(void) {
  if (!have_madvise_huge) {
    if (metadata_thp_enabled() && opt_abort) {
      malloc_write("<jemalloc>: no MADV_HUGEPAGE support\n");
      abort();
    }
    goto label_error;
  }

  static const char sys_state_madvise[] = "always [madvise] never\n";
  static const char sys_state_always[] = "[always] madvise never\n";
  static const char sys_state_never[] = "always madvise [never]\n";
  char buf[sizeof(sys_state_madvise)];

#if defined(JEMALLOC_USE_SYSCALL) && defined(SYS_open)
  int fd = (int)syscall(
    SYS_open, "/sys/kernel/mm/transparent_hugepage/enabled", O_RDONLY);
#else
  int fd = open("/sys/kernel/mm/transparent_hugepage/enabled", O_RDONLY);
#endif
  if (fd == -1) { goto label_error; }

  ssize_t nread = malloc_read_fd(fd, &buf, sizeof(buf));
#if defined(JEMALLOC_USE_SYSCALL) && defined(SYS_close)
  syscall(SYS_close, fd);
#else
  close(fd);
#endif

  if (nread < 0) { goto label_error; }

  if (strncmp(buf, sys_state_madvise, (size_t)nread) == 0) {
    init_system_thp_mode = thp_mode_default;
  } else if (strncmp(buf, sys_state_always, (size_t)nread) == 0) {
    init_system_thp_mode = thp_mode_always;
  } else if (strncmp(buf, sys_state_never, (size_t)nread) == 0) {
    init_system_thp_mode = thp_mode_never;
  } else {
    goto label_error;
  }
  return;
label_error:
  opt_thp = init_system_thp_mode = thp_mode_not_supported;
}

bool pages_boot(void) {
  os_page = os_page_detect();
  if (os_page > PAGE) {
    malloc_write("<jemalloc>: Unsupported system page size\n");
    if (opt_abort) { abort(); }
    return true;
  }

#ifndef _WIN32
  mmap_flags = MAP_PRIVATE | MAP_ANON;
#endif

#ifdef JEMALLOC_SYSCTL_VM_OVERCOMMIT
  os_overcommits = os_overcommits_sysctl();
#elif defined(JEMALLOC_PROC_SYS_VM_OVERCOMMIT_MEMORY)
  os_overcommits = os_overcommits_proc();
#ifdef MAP_NORESERVE
  if (os_overcommits) { mmap_flags |= MAP_NORESERVE; }
#endif
#else
  os_overcommits = false;
#endif

  init_thp_state();

#ifdef __FreeBSD__
  /*
   * FreeBSD doesn't need the check; madvise(2) is known to work.
   */
#else
  /* Detect lazy purge runtime support. */
  if (pages_can_purge_lazy) {
    bool committed = false;
    void* madv_free_page = os_pages_map(NULL, PAGE, PAGE, &committed);
    if (madv_free_page == NULL) { return true; }
    assert(pages_can_purge_lazy_runtime);
    if (pages_purge_lazy(madv_free_page, PAGE)) {
      pages_can_purge_lazy_runtime = false;
    }
    os_pages_unmap(madv_free_page, PAGE);
  }
#endif

  return false;
}
#define JEMALLOC_PRNG_C_
#define JEMALLOC_PROF_C_

#include "jemalloc/internal/ckh.h"
#include "jemalloc/internal/hash.h"
#include "jemalloc/internal/malloc_io.h"

#include "jemalloc/internal/emitter.h"

/******************************************************************************/

#ifdef JEMALLOC_PROF_LIBUNWIND
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#endif

#ifdef JEMALLOC_PROF_LIBGCC
/*
 * We have a circular dependency -- jemalloc_internal.h tells us if we
 * should use libgcc's unwinding functionality, but after we've included
 * that, we've already hooked _Unwind_Backtrace.  We'll temporarily disable
 * hooking.
 */
#undef _Unwind_Backtrace
#include <unwind.h>
#define _Unwind_Backtrace                                                  \
  JEMALLOC_HOOK(_Unwind_Backtrace, test_hooks_libc_hook)
#endif

/******************************************************************************/
/* Data. */

bool opt_prof = false;
bool opt_prof_active = true;
bool opt_prof_thread_active_init = true;
size_t opt_lg_prof_sample = LG_PROF_SAMPLE_DEFAULT;
ssize_t opt_lg_prof_interval = LG_PROF_INTERVAL_DEFAULT;
bool opt_prof_gdump = false;
bool opt_prof_final = false;
bool opt_prof_leak = false;
bool opt_prof_accum = false;
bool opt_prof_log = false;
char opt_prof_prefix[
/* Minimize memory bloat for non-prof builds. */
#ifdef JEMALLOC_PROF
  PATH_MAX +
#endif
  1];

/*
 * Initialized as opt_prof_active, and accessed via
 * prof_active_[gs]et{_unlocked,}().
 */
bool prof_active;
static malloc_mutex_t prof_active_mtx;

/*
 * Initialized as opt_prof_thread_active_init, and accessed via
 * prof_thread_active_init_[gs]et().
 */
static bool prof_thread_active_init;
static malloc_mutex_t prof_thread_active_init_mtx;

/*
 * Initialized as opt_prof_gdump, and accessed via
 * prof_gdump_[gs]et{_unlocked,}().
 */
bool prof_gdump_val;
static malloc_mutex_t prof_gdump_mtx;

uint64_t prof_interval = 0;

size_t lg_prof_sample;

typedef enum prof_logging_state_e prof_logging_state_t;
enum prof_logging_state_e {
  prof_logging_state_stopped,
  prof_logging_state_started,
  prof_logging_state_dumping
};

/*
 * - stopped: log_start never called, or previous log_stop has completed.
 * - started: log_start called, log_stop not called yet. Allocations are
 * logged.
 * - dumping: log_stop called but not finished; samples are not logged
 * anymore.
 */
prof_logging_state_t prof_logging_state = prof_logging_state_stopped;

#ifdef JEMALLOC_JET
static bool prof_log_dummy = false;
#endif

/* Incremented for every log file that is output. */
static uint64_t log_seq = 0;
static char log_filename[
/* Minimize memory bloat for non-prof builds. */
#ifdef JEMALLOC_PROF
  PATH_MAX +
#endif
  1];

/* Timestamp for most recent call to log_start(). */
static nstime_t log_start_timestamp = NSTIME_ZERO_INITIALIZER;

/* Increment these when adding to the log_bt and log_thr linked lists. */
static size_t log_bt_index = 0;
static size_t log_thr_index = 0;

/* Linked list node definitions. These are only used in prof.c. */
typedef struct prof_bt_node_s prof_bt_node_t;

struct prof_bt_node_s {
  prof_bt_node_t* next;
  size_t index;
  prof_bt_t bt;
  /* Variable size backtrace vector pointed to by bt. */
  void* vec[1];
};

typedef struct prof_thr_node_s prof_thr_node_t;

struct prof_thr_node_s {
  prof_thr_node_t* next;
  size_t index;
  uint64_t thr_uid;
  /* Variable size based on thr_name_sz. */
  char name[1];
};

typedef struct prof_alloc_node_s prof_alloc_node_t;

/* This is output when logging sampled allocations. */
struct prof_alloc_node_s {
  prof_alloc_node_t* next;
  /* Indices into an array of thread data. */
  size_t alloc_thr_ind;
  size_t free_thr_ind;

  /* Indices into an array of backtraces. */
  size_t alloc_bt_ind;
  size_t free_bt_ind;

  uint64_t alloc_time_ns;
  uint64_t free_time_ns;

  size_t usize;
};

/*
 * Created on the first call to prof_log_start and deleted on prof_log_stop.
 * These are the backtraces and threads that have already been logged by an
 * allocation.
 */
static bool log_tables_initialized = false;
static ckh_t log_bt_node_set;
static ckh_t log_thr_node_set;

/* Store linked lists for logged data. */
static prof_bt_node_t* log_bt_first = NULL;
static prof_bt_node_t* log_bt_last = NULL;
static prof_thr_node_t* log_thr_first = NULL;
static prof_thr_node_t* log_thr_last = NULL;
static prof_alloc_node_t* log_alloc_first = NULL;
static prof_alloc_node_t* log_alloc_last = NULL;

/* Protects the prof_logging_state and any log_{...} variable. */
static malloc_mutex_t log_mtx;

/*
 * Table of mutexes that are shared among gctx's.  These are leaf locks, so
 * there is no problem with using them for more than one gctx at the same
 * time. The primary motivation for this sharing though is that gctx's are
 * ephemeral, and destroying mutexes causes complications for systems that
 * allocate when creating/destroying mutexes.
 */
static malloc_mutex_t* gctx_locks;
static atomic_u_t cum_gctxs; /* Atomic counter. */

/*
 * Table of mutexes that are shared among tdata's.  No operations require
 * holding multiple tdata locks, so there is no problem with using them for
 * more than one tdata at the same time, even though a gctx lock may be
 * acquired while holding a tdata lock.
 */
static malloc_mutex_t* tdata_locks;

/*
 * Global hash of (prof_bt_t *)-->(prof_gctx_t *).  This is the master data
 * structure that knows about all backtraces currently captured.
 */
static ckh_t bt2gctx;
/* Non static to enable profiling. */
malloc_mutex_t bt2gctx_mtx;

/*
 * Tree of all extant prof_tdata_t structures, regardless of state,
 * {attached,detached,expired}.
 */
static prof_tdata_tree_t tdatas;
static malloc_mutex_t tdatas_mtx;

static uint64_t next_thr_uid;
static malloc_mutex_t next_thr_uid_mtx;

static malloc_mutex_t prof_dump_seq_mtx;
static uint64_t prof_dump_seq;
static uint64_t prof_dump_iseq;
static uint64_t prof_dump_mseq;
static uint64_t prof_dump_useq;

/*
 * This buffer is rather large for stack allocation, so use a single buffer
 * for all profile dumps.
 */
static malloc_mutex_t prof_dump_mtx;
static char prof_dump_buf[
/* Minimize memory bloat for non-prof builds. */
#ifdef JEMALLOC_PROF
  PROF_DUMP_BUFSIZE
#else
  1
#endif
];
static size_t prof_dump_buf_end;
static int prof_dump_fd;

/* Do not dump any profiles until bootstrapping is complete. */
static bool prof_booted = false;

/******************************************************************************/
/*
 * Function prototypes for static functions that are referenced prior to
 * definition.
 */

static bool prof_tctx_should_destroy(tsdn_t* tsdn, prof_tctx_t* tctx);
static void prof_tctx_destroy(tsd_t* tsd, prof_tctx_t* tctx);
static bool prof_tdata_should_destroy(
  tsdn_t* tsdn, prof_tdata_t* tdata, bool even_if_attached);
static void prof_tdata_destroy(
  tsd_t* tsd, prof_tdata_t* tdata, bool even_if_attached);
static char* prof_thread_name_alloc(tsdn_t* tsdn, const char* thread_name);

/* Hashtable functions for log_bt_node_set and log_thr_node_set. */
static void prof_thr_node_hash(const void* key, size_t r_hash[2]);
static bool prof_thr_node_keycomp(const void* k1, const void* k2);
static void prof_bt_node_hash(const void* key, size_t r_hash[2]);
static bool prof_bt_node_keycomp(const void* k1, const void* k2);

/******************************************************************************/
/* Red-black trees. */

static int prof_tctx_comp(const prof_tctx_t* a, const prof_tctx_t* b) {
  uint64_t a_thr_uid = a->thr_uid;
  uint64_t b_thr_uid = b->thr_uid;
  int ret = (a_thr_uid > b_thr_uid) - (a_thr_uid < b_thr_uid);
  if (ret == 0) {
    uint64_t a_thr_discrim = a->thr_discrim;
    uint64_t b_thr_discrim = b->thr_discrim;
    ret = (a_thr_discrim > b_thr_discrim) - (a_thr_discrim < b_thr_discrim);
    if (ret == 0) {
      uint64_t a_tctx_uid = a->tctx_uid;
      uint64_t b_tctx_uid = b->tctx_uid;
      ret = (a_tctx_uid > b_tctx_uid) - (a_tctx_uid < b_tctx_uid);
    }
  }
  return ret;
}

rb_gen(static UNUSED, tctx_tree_, prof_tctx_tree_t, prof_tctx_t, tctx_link,
  prof_tctx_comp)

  static int prof_gctx_comp(const prof_gctx_t* a, const prof_gctx_t* b) {
  unsigned a_len = a->bt.len;
  unsigned b_len = b->bt.len;
  unsigned comp_len = (a_len < b_len) ? a_len : b_len;
  int ret = memcmp(a->bt.vec, b->bt.vec, comp_len * sizeof(void*));
  if (ret == 0) { ret = (a_len > b_len) - (a_len < b_len); }
  return ret;
}

rb_gen(static UNUSED, gctx_tree_, prof_gctx_tree_t, prof_gctx_t, dump_link,
  prof_gctx_comp)

  static int prof_tdata_comp(const prof_tdata_t* a, const prof_tdata_t* b) {
  int ret;
  uint64_t a_uid = a->thr_uid;
  uint64_t b_uid = b->thr_uid;

  ret = ((a_uid > b_uid) - (a_uid < b_uid));
  if (ret == 0) {
    uint64_t a_discrim = a->thr_discrim;
    uint64_t b_discrim = b->thr_discrim;

    ret = ((a_discrim > b_discrim) - (a_discrim < b_discrim));
  }
  return ret;
}

rb_gen(static UNUSED, tdata_tree_, prof_tdata_tree_t, prof_tdata_t,
  tdata_link, prof_tdata_comp)

  /******************************************************************************/

  void prof_alloc_rollback(tsd_t* tsd, prof_tctx_t* tctx, bool updated) {
  prof_tdata_t* tdata;

  cassert(config_prof);

  if (updated) {
    /*
     * Compute a new sample threshold.  This isn't very important in
     * practice, because this function is rarely executed, so the
     * potential for sample bias is minimal except in contrived
     * programs.
     */
    tdata = prof_tdata_get(tsd, true);
    if (tdata != NULL) { prof_sample_threshold_update(tdata); }
  }

  if ((uintptr_t)tctx > (uintptr_t)1U) {
    malloc_mutex_lock(tsd_tsdn(tsd), tctx->tdata->lock);
    tctx->prepared = false;
    if (prof_tctx_should_destroy(tsd_tsdn(tsd), tctx)) {
      prof_tctx_destroy(tsd, tctx);
    } else {
      malloc_mutex_unlock(tsd_tsdn(tsd), tctx->tdata->lock);
    }
  }
}

void prof_malloc_sample_object(
  tsdn_t* tsdn, const void* ptr, size_t usize, prof_tctx_t* tctx) {
  prof_tctx_set(tsdn, ptr, usize, NULL, tctx);

  /* Get the current time and set this in the extent_t. We'll read this
   * when free() is called. */
  nstime_t t = NSTIME_ZERO_INITIALIZER;
  nstime_update(&t);
  prof_alloc_time_set(tsdn, ptr, NULL, t);

  malloc_mutex_lock(tsdn, tctx->tdata->lock);
  tctx->cnts.curobjs++;
  tctx->cnts.curbytes += usize;
  if (opt_prof_accum) {
    tctx->cnts.accumobjs++;
    tctx->cnts.accumbytes += usize;
  }
  tctx->prepared = false;
  malloc_mutex_unlock(tsdn, tctx->tdata->lock);
}

static size_t prof_log_bt_index(tsd_t* tsd, prof_bt_t* bt) {
  assert(prof_logging_state == prof_logging_state_started);
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &log_mtx);

  prof_bt_node_t dummy_node;
  dummy_node.bt = *bt;
  prof_bt_node_t* node;

  /* See if this backtrace is already cached in the table. */
  if (ckh_search(
        &log_bt_node_set, (void*)(&dummy_node), (void**)(&node), NULL)) {
    size_t sz = offsetof(prof_bt_node_t, vec) + (bt->len * sizeof(void*));
    prof_bt_node_t* new_node
      = (prof_bt_node_t*)iallocztm(tsd_tsdn(tsd), sz, sz_size2index(sz),
        false, NULL, true, arena_get(TSDN_NULL, 0, true), true);
    if (log_bt_first == NULL) {
      log_bt_first = new_node;
      log_bt_last = new_node;
    } else {
      log_bt_last->next = new_node;
      log_bt_last = new_node;
    }

    new_node->next = NULL;
    new_node->index = log_bt_index;
    /*
     * Copy the backtrace: bt is inside a tdata or gctx, which
     * might die before prof_log_stop is called.
     */
    new_node->bt.len = bt->len;
    jet_memcpy(new_node->vec, bt->vec, bt->len * sizeof(void*));
    new_node->bt.vec = new_node->vec;

    log_bt_index++;
    ckh_insert(tsd, &log_bt_node_set, (void*)new_node, NULL);
    return new_node->index;
  } else {
    return node->index;
  }
}
static size_t prof_log_thr_index(
  tsd_t* tsd, uint64_t thr_uid, const char* name) {
  assert(prof_logging_state == prof_logging_state_started);
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &log_mtx);

  prof_thr_node_t dummy_node;
  dummy_node.thr_uid = thr_uid;
  prof_thr_node_t* node;

  /* See if this thread is already cached in the table. */
  if (ckh_search(
        &log_thr_node_set, (void*)(&dummy_node), (void**)(&node), NULL)) {
    size_t sz = offsetof(prof_thr_node_t, name) + cstr_len(name) + 1;
    prof_thr_node_t* new_node
      = (prof_thr_node_t*)iallocztm(tsd_tsdn(tsd), sz, sz_size2index(sz),
        false, NULL, true, arena_get(TSDN_NULL, 0, true), true);
    if (log_thr_first == NULL) {
      log_thr_first = new_node;
      log_thr_last = new_node;
    } else {
      log_thr_last->next = new_node;
      log_thr_last = new_node;
    }

    new_node->next = NULL;
    new_node->index = log_thr_index;
    new_node->thr_uid = thr_uid;
    strcpy(new_node->name, name);

    log_thr_index++;
    ckh_insert(tsd, &log_thr_node_set, (void*)new_node, NULL);
    return new_node->index;
  } else {
    return node->index;
  }
}

static void prof_try_log(
  tsd_t* tsd, const void* ptr, size_t usize, prof_tctx_t* tctx) {
  malloc_mutex_assert_owner(tsd_tsdn(tsd), tctx->tdata->lock);

  prof_tdata_t* cons_tdata = prof_tdata_get(tsd, false);
  if (cons_tdata == NULL) {
    /*
     * We decide not to log these allocations. cons_tdata will be
     * NULL only when the current thread is in a weird state (e.g.
     * it's being destroyed).
     */
    return;
  }

  malloc_mutex_lock(tsd_tsdn(tsd), &log_mtx);

  if (prof_logging_state != prof_logging_state_started) { goto label_done; }

  if (!log_tables_initialized) {
    bool err1 = ckh_new(tsd, &log_bt_node_set, PROF_CKH_MINITEMS,
      prof_bt_node_hash, prof_bt_node_keycomp);
    bool err2 = ckh_new(tsd, &log_thr_node_set, PROF_CKH_MINITEMS,
      prof_thr_node_hash, prof_thr_node_keycomp);
    if (err1 || err2) { goto label_done; }
    log_tables_initialized = true;
  }

  nstime_t alloc_time
    = prof_alloc_time_get(tsd_tsdn(tsd), ptr, (alloc_ctx_t*)NULL);
  nstime_t free_time = NSTIME_ZERO_INITIALIZER;
  nstime_update(&free_time);

  size_t sz = sizeof(prof_alloc_node_t);
  prof_alloc_node_t* new_node
    = (prof_alloc_node_t*)iallocztm(tsd_tsdn(tsd), sz, sz_size2index(sz),
      false, NULL, true, arena_get(TSDN_NULL, 0, true), true);

  const char* prod_thr_name
    = (tctx->tdata->thread_name == NULL) ? "" : tctx->tdata->thread_name;
  const char* cons_thr_name = prof_thread_name_get(tsd);

  prof_bt_t bt;
  /* Initialize the backtrace, using the buffer in tdata to store it. */
  bt_init(&bt, cons_tdata->vec);
  prof_backtrace(&bt);
  prof_bt_t* cons_bt = &bt;

  /* We haven't destroyed tctx yet, so gctx should be good to read. */
  prof_bt_t* prod_bt = &tctx->gctx->bt;

  new_node->next = NULL;
  new_node->alloc_thr_ind
    = prof_log_thr_index(tsd, tctx->tdata->thr_uid, prod_thr_name);
  new_node->free_thr_ind
    = prof_log_thr_index(tsd, cons_tdata->thr_uid, cons_thr_name);
  new_node->alloc_bt_ind = prof_log_bt_index(tsd, prod_bt);
  new_node->free_bt_ind = prof_log_bt_index(tsd, cons_bt);
  new_node->alloc_time_ns = nstime_ns(&alloc_time);
  new_node->free_time_ns = nstime_ns(&free_time);
  new_node->usize = usize;

  if (log_alloc_first == NULL) {
    log_alloc_first = new_node;
    log_alloc_last = new_node;
  } else {
    log_alloc_last->next = new_node;
    log_alloc_last = new_node;
  }

label_done:
  malloc_mutex_unlock(tsd_tsdn(tsd), &log_mtx);
}

void prof_free_sampled_object(
  tsd_t* tsd, const void* ptr, size_t usize, prof_tctx_t* tctx) {
  malloc_mutex_lock(tsd_tsdn(tsd), tctx->tdata->lock);

  assert(tctx->cnts.curobjs > 0);
  assert(tctx->cnts.curbytes >= usize);
  tctx->cnts.curobjs--;
  tctx->cnts.curbytes -= usize;

  prof_try_log(tsd, ptr, usize, tctx);

  if (prof_tctx_should_destroy(tsd_tsdn(tsd), tctx)) {
    prof_tctx_destroy(tsd, tctx);
  } else {
    malloc_mutex_unlock(tsd_tsdn(tsd), tctx->tdata->lock);
  }
}

void bt_init(prof_bt_t* bt, void** vec) {
  cassert(config_prof);

  bt->vec = vec;
  bt->len = 0;
}

static void prof_enter(tsd_t* tsd, prof_tdata_t* tdata) {
  cassert(config_prof);
  assert(tdata == prof_tdata_get(tsd, false));

  if (tdata != NULL) {
    assert(!tdata->enq);
    tdata->enq = true;
  }

  malloc_mutex_lock(tsd_tsdn(tsd), &bt2gctx_mtx);
}

static void prof_leave(tsd_t* tsd, prof_tdata_t* tdata) {
  cassert(config_prof);
  assert(tdata == prof_tdata_get(tsd, false));

  malloc_mutex_unlock(tsd_tsdn(tsd), &bt2gctx_mtx);

  if (tdata != NULL) {
    bool idump, gdump;

    assert(tdata->enq);
    tdata->enq = false;
    idump = tdata->enq_idump;
    tdata->enq_idump = false;
    gdump = tdata->enq_gdump;
    tdata->enq_gdump = false;

    if (idump) { prof_idump(tsd_tsdn(tsd)); }
    if (gdump) { prof_gdump(tsd_tsdn(tsd)); }
  }
}

#ifdef JEMALLOC_PROF_LIBUNWIND
void prof_backtrace(prof_bt_t* bt) {
  int nframes;

  cassert(config_prof);
  assert(bt->len == 0);
  assert(bt->vec != NULL);

  nframes = unw_backtrace(bt->vec, PROF_BT_MAX);
  if (nframes <= 0) { return; }
  bt->len = nframes;
}
#elif (defined(JEMALLOC_PROF_LIBGCC))
static _Unwind_Reason_Code prof_unwind_init_callback(
  struct _Unwind_Context* context, void* arg) {
  cassert(config_prof);

  return _URC_NO_REASON;
}

static _Unwind_Reason_Code prof_unwind_callback(
  struct _Unwind_Context* context, void* arg) {
  prof_unwind_data_t* data = (prof_unwind_data_t*)arg;
  void* ip;

  cassert(config_prof);

  ip = (void*)_Unwind_GetIP(context);
  if (ip == NULL) { return _URC_END_OF_STACK; }
  data->bt->vec[data->bt->len] = ip;
  data->bt->len++;
  if (data->bt->len == data->max) { return _URC_END_OF_STACK; }

  return _URC_NO_REASON;
}

void prof_backtrace(prof_bt_t* bt) {
  prof_unwind_data_t data = { bt, PROF_BT_MAX };

  cassert(config_prof);

  _Unwind_Backtrace(prof_unwind_callback, &data);
}
#elif (defined(JEMALLOC_PROF_GCC))
void prof_backtrace(prof_bt_t* bt) {
#define BT_FRAME(i)                                                        \
  if ((i) < PROF_BT_MAX) {                                                 \
    void* p;                                                               \
    if (__builtin_frame_address(i) == 0) { return; }                       \
    p = __builtin_return_address(i);                                       \
    if (p == NULL) { return; }                                             \
    bt->vec[(i)] = p;                                                      \
    bt->len = (i) + 1;                                                     \
  } else {                                                                 \
    return;                                                                \
  }

  cassert(config_prof);

  BT_FRAME(0)
  BT_FRAME(1)
  BT_FRAME(2)
  BT_FRAME(3)
  BT_FRAME(4)
  BT_FRAME(5)
  BT_FRAME(6)
  BT_FRAME(7)
  BT_FRAME(8)
  BT_FRAME(9)

  BT_FRAME(10)
  BT_FRAME(11)
  BT_FRAME(12)
  BT_FRAME(13)
  BT_FRAME(14)
  BT_FRAME(15)
  BT_FRAME(16)
  BT_FRAME(17)
  BT_FRAME(18)
  BT_FRAME(19)

  BT_FRAME(20)
  BT_FRAME(21)
  BT_FRAME(22)
  BT_FRAME(23)
  BT_FRAME(24)
  BT_FRAME(25)
  BT_FRAME(26)
  BT_FRAME(27)
  BT_FRAME(28)
  BT_FRAME(29)

  BT_FRAME(30)
  BT_FRAME(31)
  BT_FRAME(32)
  BT_FRAME(33)
  BT_FRAME(34)
  BT_FRAME(35)
  BT_FRAME(36)
  BT_FRAME(37)
  BT_FRAME(38)
  BT_FRAME(39)

  BT_FRAME(40)
  BT_FRAME(41)
  BT_FRAME(42)
  BT_FRAME(43)
  BT_FRAME(44)
  BT_FRAME(45)
  BT_FRAME(46)
  BT_FRAME(47)
  BT_FRAME(48)
  BT_FRAME(49)

  BT_FRAME(50)
  BT_FRAME(51)
  BT_FRAME(52)
  BT_FRAME(53)
  BT_FRAME(54)
  BT_FRAME(55)
  BT_FRAME(56)
  BT_FRAME(57)
  BT_FRAME(58)
  BT_FRAME(59)

  BT_FRAME(60)
  BT_FRAME(61)
  BT_FRAME(62)
  BT_FRAME(63)
  BT_FRAME(64)
  BT_FRAME(65)
  BT_FRAME(66)
  BT_FRAME(67)
  BT_FRAME(68)
  BT_FRAME(69)

  BT_FRAME(70)
  BT_FRAME(71)
  BT_FRAME(72)
  BT_FRAME(73)
  BT_FRAME(74)
  BT_FRAME(75)
  BT_FRAME(76)
  BT_FRAME(77)
  BT_FRAME(78)
  BT_FRAME(79)

  BT_FRAME(80)
  BT_FRAME(81)
  BT_FRAME(82)
  BT_FRAME(83)
  BT_FRAME(84)
  BT_FRAME(85)
  BT_FRAME(86)
  BT_FRAME(87)
  BT_FRAME(88)
  BT_FRAME(89)

  BT_FRAME(90)
  BT_FRAME(91)
  BT_FRAME(92)
  BT_FRAME(93)
  BT_FRAME(94)
  BT_FRAME(95)
  BT_FRAME(96)
  BT_FRAME(97)
  BT_FRAME(98)
  BT_FRAME(99)

  BT_FRAME(100)
  BT_FRAME(101)
  BT_FRAME(102)
  BT_FRAME(103)
  BT_FRAME(104)
  BT_FRAME(105)
  BT_FRAME(106)
  BT_FRAME(107)
  BT_FRAME(108)
  BT_FRAME(109)

  BT_FRAME(110)
  BT_FRAME(111)
  BT_FRAME(112)
  BT_FRAME(113)
  BT_FRAME(114)
  BT_FRAME(115)
  BT_FRAME(116)
  BT_FRAME(117)
  BT_FRAME(118)
  BT_FRAME(119)

  BT_FRAME(120)
  BT_FRAME(121)
  BT_FRAME(122)
  BT_FRAME(123)
  BT_FRAME(124)
  BT_FRAME(125)
  BT_FRAME(126)
  BT_FRAME(127)
#undef BT_FRAME
}
#else
void prof_backtrace(prof_bt_t* bt) {
  cassert(config_prof);
  not_reached();
}
#endif

static malloc_mutex_t* prof_gctx_mutex_choose(void) {
  unsigned ngctxs = atomic_fetch_add_u(&cum_gctxs, 1, ATOMIC_RELAXED);

  return &gctx_locks[(ngctxs - 1) % PROF_NCTX_LOCKS];
}

static malloc_mutex_t* prof_tdata_mutex_choose(uint64_t thr_uid) {
  return &tdata_locks[thr_uid % PROF_NTDATA_LOCKS];
}

static prof_gctx_t* prof_gctx_create(tsdn_t* tsdn, prof_bt_t* bt) {
  /*
   * Create a single allocation that has space for vec of length bt->len.
   */
  size_t size = offsetof(prof_gctx_t, vec) + (bt->len * sizeof(void*));
  prof_gctx_t* gctx
    = (prof_gctx_t*)iallocztm(tsdn, size, sz_size2index(size), false, NULL,
      true, arena_get(TSDN_NULL, 0, true), true);
  if (gctx == NULL) { return NULL; }
  gctx->lock = prof_gctx_mutex_choose();
  /*
   * Set nlimbo to 1, in order to avoid a race condition with
   * prof_tctx_destroy()/prof_gctx_try_destroy().
   */
  gctx->nlimbo = 1;
  tctx_tree_new(&gctx->tctxs);
  /* Duplicate bt. */
  jet_memcpy(gctx->vec, bt->vec, bt->len * sizeof(void*));
  gctx->bt.vec = gctx->vec;
  gctx->bt.len = bt->len;
  return gctx;
}

static void prof_gctx_try_destroy(tsd_t* tsd, prof_tdata_t* tdata_self,
  prof_gctx_t* gctx, prof_tdata_t* tdata) {
  cassert(config_prof);

  /*
   * Check that gctx is still unused by any thread cache before destroying
   * it.  prof_lookup() increments gctx->nlimbo in order to avoid a race
   * condition with this function, as does prof_tctx_destroy() in order to
   * avoid a race between the main body of prof_tctx_destroy() and entry
   * into this function.
   */
  prof_enter(tsd, tdata_self);
  malloc_mutex_lock(tsd_tsdn(tsd), gctx->lock);
  assert(gctx->nlimbo != 0);
  if (tctx_tree_empty(&gctx->tctxs) && gctx->nlimbo == 1) {
    /* Remove gctx from bt2gctx. */
    if (ckh_remove(tsd, &bt2gctx, &gctx->bt, NULL, NULL)) { not_reached(); }
    prof_leave(tsd, tdata_self);
    /* Destroy gctx. */
    malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
    idalloctm(tsd_tsdn(tsd), gctx, NULL, NULL, true, true);
  } else {
    /*
     * Compensate for increment in prof_tctx_destroy() or
     * prof_lookup().
     */
    gctx->nlimbo--;
    malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
    prof_leave(tsd, tdata_self);
  }
}

static bool prof_tctx_should_destroy(tsdn_t* tsdn, prof_tctx_t* tctx) {
  malloc_mutex_assert_owner(tsdn, tctx->tdata->lock);

  if (opt_prof_accum) { return false; }
  if (tctx->cnts.curobjs != 0) { return false; }
  if (tctx->prepared) { return false; }
  return true;
}

static bool prof_gctx_should_destroy(prof_gctx_t* gctx) {
  if (opt_prof_accum) { return false; }
  if (!tctx_tree_empty(&gctx->tctxs)) { return false; }
  if (gctx->nlimbo != 0) { return false; }
  return true;
}

static void prof_tctx_destroy(tsd_t* tsd, prof_tctx_t* tctx) {
  prof_tdata_t* tdata = tctx->tdata;
  prof_gctx_t* gctx = tctx->gctx;
  bool destroy_tdata, destroy_tctx, destroy_gctx;

  malloc_mutex_assert_owner(tsd_tsdn(tsd), tctx->tdata->lock);

  assert(tctx->cnts.curobjs == 0);
  assert(tctx->cnts.curbytes == 0);
  assert(!opt_prof_accum);
  assert(tctx->cnts.accumobjs == 0);
  assert(tctx->cnts.accumbytes == 0);

  ckh_remove(tsd, &tdata->bt2tctx, &gctx->bt, NULL, NULL);
  destroy_tdata = prof_tdata_should_destroy(tsd_tsdn(tsd), tdata, false);
  malloc_mutex_unlock(tsd_tsdn(tsd), tdata->lock);

  malloc_mutex_lock(tsd_tsdn(tsd), gctx->lock);
  switch (tctx->state) {
  case prof_tctx_state_nominal:
    tctx_tree_remove(&gctx->tctxs, tctx);
    destroy_tctx = true;
    if (prof_gctx_should_destroy(gctx)) {
      /*
       * Increment gctx->nlimbo in order to keep another
       * thread from winning the race to destroy gctx while
       * this one has gctx->lock dropped.  Without this, it
       * would be possible for another thread to:
       *
       * 1) Sample an allocation associated with gctx.
       * 2) Deallocate the sampled object.
       * 3) Successfully prof_gctx_try_destroy(gctx).
       *
       * The result would be that gctx no longer exists by the
       * time this thread accesses it in
       * prof_gctx_try_destroy().
       */
      gctx->nlimbo++;
      destroy_gctx = true;
    } else {
      destroy_gctx = false;
    }
    break;
  case prof_tctx_state_dumping:
    /*
     * A dumping thread needs tctx to remain valid until dumping
     * has finished.  Change state such that the dumping thread will
     * complete destruction during a late dump iteration phase.
     */
    tctx->state = prof_tctx_state_purgatory;
    destroy_tctx = false;
    destroy_gctx = false;
    break;
  default:
    not_reached();
    destroy_tctx = false;
    destroy_gctx = false;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
  if (destroy_gctx) {
    prof_gctx_try_destroy(tsd, prof_tdata_get(tsd, false), gctx, tdata);
  }

  malloc_mutex_assert_not_owner(tsd_tsdn(tsd), tctx->tdata->lock);

  if (destroy_tdata) { prof_tdata_destroy(tsd, tdata, false); }

  if (destroy_tctx) {
    idalloctm(tsd_tsdn(tsd), tctx, NULL, NULL, true, true);
  }
}

static bool prof_lookup_global(tsd_t* tsd, prof_bt_t* bt,
  prof_tdata_t* tdata, void** p_btkey, prof_gctx_t** p_gctx,
  bool* p_new_gctx) {
  union {
    prof_gctx_t* p;
    void* v;
  } gctx, tgctx;
  union {
    prof_bt_t* p;
    void* v;
  } btkey;
  bool new_gctx;

  prof_enter(tsd, tdata);
  if (ckh_search(&bt2gctx, bt, &btkey.v, &gctx.v)) {
    /* bt has never been seen before.  Insert it. */
    prof_leave(tsd, tdata);
    tgctx.p = prof_gctx_create(tsd_tsdn(tsd), bt);
    if (tgctx.v == NULL) { return true; }
    prof_enter(tsd, tdata);
    if (ckh_search(&bt2gctx, bt, &btkey.v, &gctx.v)) {
      gctx.p = tgctx.p;
      btkey.p = &gctx.p->bt;
      if (ckh_insert(tsd, &bt2gctx, btkey.v, gctx.v)) {
        /* OOM. */
        prof_leave(tsd, tdata);
        idalloctm(tsd_tsdn(tsd), gctx.v, NULL, NULL, true, true);
        return true;
      }
      new_gctx = true;
    } else {
      new_gctx = false;
    }
  } else {
    tgctx.v = NULL;
    new_gctx = false;
  }

  if (!new_gctx) {
    /*
     * Increment nlimbo, in order to avoid a race condition with
     * prof_tctx_destroy()/prof_gctx_try_destroy().
     */
    malloc_mutex_lock(tsd_tsdn(tsd), gctx.p->lock);
    gctx.p->nlimbo++;
    malloc_mutex_unlock(tsd_tsdn(tsd), gctx.p->lock);
    new_gctx = false;

    if (tgctx.v != NULL) {
      /* Lost race to insert. */
      idalloctm(tsd_tsdn(tsd), tgctx.v, NULL, NULL, true, true);
    }
  }
  prof_leave(tsd, tdata);

  *p_btkey = btkey.v;
  *p_gctx = gctx.p;
  *p_new_gctx = new_gctx;
  return false;
}

prof_tctx_t* prof_lookup(tsd_t* tsd, prof_bt_t* bt) {
  union {
    prof_tctx_t* p;
    void* v;
  } ret;
  prof_tdata_t* tdata;
  bool not_found;

  cassert(config_prof);

  tdata = prof_tdata_get(tsd, false);
  if (tdata == NULL) { return NULL; }

  malloc_mutex_lock(tsd_tsdn(tsd), tdata->lock);
  not_found = ckh_search(&tdata->bt2tctx, bt, NULL, &ret.v);
  if (!not_found) { /* Note double negative! */
    ret.p->prepared = true;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), tdata->lock);
  if (not_found) {
    void* btkey;
    prof_gctx_t* gctx;
    bool new_gctx, error;

    /*
     * This thread's cache lacks bt.  Look for it in the global
     * cache.
     */
    if (prof_lookup_global(tsd, bt, tdata, &btkey, &gctx, &new_gctx)) {
      return NULL;
    }

    /* Link a prof_tctx_t into gctx for this thread. */
    ret.v = iallocztm(tsd_tsdn(tsd), sizeof(prof_tctx_t),
      sz_size2index(sizeof(prof_tctx_t)), false, NULL, true,
      arena_ichoose(tsd, NULL), true);
    if (ret.p == NULL) {
      if (new_gctx) { prof_gctx_try_destroy(tsd, tdata, gctx, tdata); }
      return NULL;
    }
    ret.p->tdata = tdata;
    ret.p->thr_uid = tdata->thr_uid;
    ret.p->thr_discrim = tdata->thr_discrim;
    memset(&ret.p->cnts, 0, sizeof(prof_cnt_t));
    ret.p->gctx = gctx;
    ret.p->tctx_uid = tdata->tctx_uid_next++;
    ret.p->prepared = true;
    ret.p->state = prof_tctx_state_initializing;
    malloc_mutex_lock(tsd_tsdn(tsd), tdata->lock);
    error = ckh_insert(tsd, &tdata->bt2tctx, btkey, ret.v);
    malloc_mutex_unlock(tsd_tsdn(tsd), tdata->lock);
    if (error) {
      if (new_gctx) { prof_gctx_try_destroy(tsd, tdata, gctx, tdata); }
      idalloctm(tsd_tsdn(tsd), ret.v, NULL, NULL, true, true);
      return NULL;
    }
    malloc_mutex_lock(tsd_tsdn(tsd), gctx->lock);
    ret.p->state = prof_tctx_state_nominal;
    tctx_tree_insert(&gctx->tctxs, ret.p);
    gctx->nlimbo--;
    malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
  }

  return ret.p;
}

/*
 * The bodies of this function and prof_leakcheck() are compiled out unless
 * heap profiling is enabled, so that it is possible to compile jemalloc
 * with floating point support completely disabled.  Avoiding floating point
 * code is important on memory-constrained systems, but it also enables a
 * workaround for versions of glibc that don't properly save/restore
 * floating point registers during dynamic lazy symbol loading (which
 * internally calls into whatever malloc implementation happens to be
 * integrated into the application).  Note that some compilers (e.g.
 * gcc 4.8) may use floating point registers for fast memory moves, so
 * jemalloc must be compiled with such optimizations disabled (e.g.
 * -mno-sse) in order for the workaround to be complete.
 */
void prof_sample_threshold_update(prof_tdata_t* tdata) {
#ifdef JEMALLOC_PROF
  if (!config_prof) { return; }

  if (lg_prof_sample == 0) {
    tsd_bytes_until_sample_set(tsd_fetch(), 0);
    return;
  }

  /*
   * Compute sample interval as a geometrically distributed random
   * variable with mean (2^lg_prof_sample).
   *
   *                             __        __
   *                             |  log(u)  |                     1
   * tdata->bytes_until_sample = | -------- |, where p = ---------------
   *                             | log(1-p) |             lg_prof_sample
   *                                                     2
   *
   * For more information on the math, see:
   *
   *   Non-Uniform Random Variate Generation
   *   Luc Devroye
   *   Springer-Verlag, New York, 1986
   *   pp 500
   *   (http://luc.devroye.org/rnbookindex.html)
   */
  uint64_t r = prng_lg_range_u64(&tdata->prng_state, 53);
  double u = (double)r * (1.0 / 9007199254740992.0L);
  uint64_t bytes_until_sample
    = (uint64_t)(log(u)
        / log(1.0 - (1.0 / (double)((uint64_t)1U << lg_prof_sample))))
    + (uint64_t)1U;
  if (bytes_until_sample > SSIZE_MAX) { bytes_until_sample = SSIZE_MAX; }
  tsd_bytes_until_sample_set(tsd_fetch(), bytes_until_sample);

#endif
}

#ifdef JEMALLOC_JET
static prof_tdata_t* prof_tdata_count_iter(
  prof_tdata_tree_t* tdatas, prof_tdata_t* tdata, void* arg) {
  size_t* tdata_count = (size_t*)arg;

  (*tdata_count)++;

  return NULL;
}

size_t prof_tdata_count(void) {
  size_t tdata_count = 0;
  tsdn_t* tsdn;

  tsdn = tsdn_fetch();
  malloc_mutex_lock(tsdn, &tdatas_mtx);
  tdata_tree_iter(
    &tdatas, NULL, prof_tdata_count_iter, (void*)&tdata_count);
  malloc_mutex_unlock(tsdn, &tdatas_mtx);

  return tdata_count;
}

size_t prof_bt_count(void) {
  size_t bt_count;
  tsd_t* tsd;
  prof_tdata_t* tdata;

  tsd = tsd_fetch();
  tdata = prof_tdata_get(tsd, false);
  if (tdata == NULL) { return 0; }

  malloc_mutex_lock(tsd_tsdn(tsd), &bt2gctx_mtx);
  bt_count = ckh_count(&bt2gctx);
  malloc_mutex_unlock(tsd_tsdn(tsd), &bt2gctx_mtx);

  return bt_count;
}
#endif

static int prof_dump_open_impl(bool propagate_err, const char* filename) {
  int fd;

  fd = creat(filename, 0644);
  if (fd == -1 && !propagate_err) {
    malloc_printf("<jemalloc>: creat(\"%s\"), 0644) failed\n", filename);
    if (opt_abort) { abort(); }
  }

  return fd;
}
prof_dump_open_t* JET_MUTABLE prof_dump_open = prof_dump_open_impl;

static bool prof_dump_flush(bool propagate_err) {
  bool ret = false;
  ssize_t err;

  cassert(config_prof);

  err = malloc_write_fd(prof_dump_fd, prof_dump_buf, prof_dump_buf_end);
  if (err == -1) {
    if (!propagate_err) {
      malloc_write("<jemalloc>: write() failed during heap "
                   "profile flush\n");
      if (opt_abort) { abort(); }
    }
    ret = true;
  }
  prof_dump_buf_end = 0;

  return ret;
}

static bool prof_dump_close(bool propagate_err) {
  bool ret;

  assert(prof_dump_fd != -1);
  ret = prof_dump_flush(propagate_err);
  close(prof_dump_fd);
  prof_dump_fd = -1;

  return ret;
}

static bool prof_dump_write(bool propagate_err, const char* s) {
  size_t i, slen, n;

  cassert(config_prof);

  i = 0;
  slen = cstr_len(s);
  while (i < slen) {
    /* Flush the buffer if it is full. */
    if (prof_dump_buf_end == PROF_DUMP_BUFSIZE) {
      if (prof_dump_flush(propagate_err) && propagate_err) { return true; }
    }

    if (prof_dump_buf_end + slen - i <= PROF_DUMP_BUFSIZE) {
      /* Finish writing. */
      n = slen - i;
    } else {
      /* Write as much of s as will fit. */
      n = PROF_DUMP_BUFSIZE - prof_dump_buf_end;
    }
    jet_memcpy(&prof_dump_buf[prof_dump_buf_end], &s[i], n);
    prof_dump_buf_end += n;
    i += n;
  }
  assert(i == slen);

  return false;
}

JEMALLOC_FORMAT_PRINTF(2, 3)
static bool prof_dump_printf(bool propagate_err, const char* format, ...) {
  bool ret;
  va_list ap;
  char buf[PROF_PRINTF_BUFSIZE];

  va_start(ap, format);
  malloc_vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);
  ret = prof_dump_write(propagate_err, buf);

  return ret;
}

static void prof_tctx_merge_tdata(
  tsdn_t* tsdn, prof_tctx_t* tctx, prof_tdata_t* tdata) {
  malloc_mutex_assert_owner(tsdn, tctx->tdata->lock);

  malloc_mutex_lock(tsdn, tctx->gctx->lock);

  switch (tctx->state) {
  case prof_tctx_state_initializing:
    malloc_mutex_unlock(tsdn, tctx->gctx->lock);
    return;
  case prof_tctx_state_nominal:
    tctx->state = prof_tctx_state_dumping;
    malloc_mutex_unlock(tsdn, tctx->gctx->lock);

    jet_memcpy(&tctx->dump_cnts, &tctx->cnts, sizeof(prof_cnt_t));

    tdata->cnt_summed.curobjs += tctx->dump_cnts.curobjs;
    tdata->cnt_summed.curbytes += tctx->dump_cnts.curbytes;
    if (opt_prof_accum) {
      tdata->cnt_summed.accumobjs += tctx->dump_cnts.accumobjs;
      tdata->cnt_summed.accumbytes += tctx->dump_cnts.accumbytes;
    }
    break;
  case prof_tctx_state_dumping:
  case prof_tctx_state_purgatory: not_reached();
  }
}

static void prof_tctx_merge_gctx(
  tsdn_t* tsdn, prof_tctx_t* tctx, prof_gctx_t* gctx) {
  malloc_mutex_assert_owner(tsdn, gctx->lock);

  gctx->cnt_summed.curobjs += tctx->dump_cnts.curobjs;
  gctx->cnt_summed.curbytes += tctx->dump_cnts.curbytes;
  if (opt_prof_accum) {
    gctx->cnt_summed.accumobjs += tctx->dump_cnts.accumobjs;
    gctx->cnt_summed.accumbytes += tctx->dump_cnts.accumbytes;
  }
}

static prof_tctx_t* prof_tctx_merge_iter(
  prof_tctx_tree_t* tctxs, prof_tctx_t* tctx, void* arg) {
  tsdn_t* tsdn = (tsdn_t*)arg;

  malloc_mutex_assert_owner(tsdn, tctx->gctx->lock);

  switch (tctx->state) {
  case prof_tctx_state_nominal:
    /* New since dumping started; ignore. */
    break;
  case prof_tctx_state_dumping:
  case prof_tctx_state_purgatory:
    prof_tctx_merge_gctx(tsdn, tctx, tctx->gctx);
    break;
  default: not_reached();
  }

  return NULL;
}

struct prof_tctx_dump_iter_arg_s {
  tsdn_t* tsdn;
  bool propagate_err;
};

static prof_tctx_t* prof_tctx_dump_iter(
  prof_tctx_tree_t* tctxs, prof_tctx_t* tctx, void* opaque) {
  struct prof_tctx_dump_iter_arg_s* arg
    = (struct prof_tctx_dump_iter_arg_s*)opaque;

  malloc_mutex_assert_owner(arg->tsdn, tctx->gctx->lock);

  switch (tctx->state) {
  case prof_tctx_state_initializing:
  case prof_tctx_state_nominal:
    /* Not captured by this dump. */
    break;
  case prof_tctx_state_dumping:
  case prof_tctx_state_purgatory:
    if (prof_dump_printf(arg->propagate_err,
          "  t%" FMTu64 ": %" FMTu64 ": %" FMTu64 " [%" FMTu64 ": "
          "%" FMTu64 "]\n",
          tctx->thr_uid, tctx->dump_cnts.curobjs, tctx->dump_cnts.curbytes,
          tctx->dump_cnts.accumobjs, tctx->dump_cnts.accumbytes)) {
      return tctx;
    }
    break;
  default: not_reached();
  }
  return NULL;
}

static prof_tctx_t* prof_tctx_finish_iter(
  prof_tctx_tree_t* tctxs, prof_tctx_t* tctx, void* arg) {
  tsdn_t* tsdn = (tsdn_t*)arg;
  prof_tctx_t* ret;

  malloc_mutex_assert_owner(tsdn, tctx->gctx->lock);

  switch (tctx->state) {
  case prof_tctx_state_nominal:
    /* New since dumping started; ignore. */
    break;
  case prof_tctx_state_dumping:
    tctx->state = prof_tctx_state_nominal;
    break;
  case prof_tctx_state_purgatory: ret = tctx; goto label_return;
  default: not_reached();
  }

  ret = NULL;
label_return:
  return ret;
}

static void prof_dump_gctx_prep(
  tsdn_t* tsdn, prof_gctx_t* gctx, prof_gctx_tree_t* gctxs) {
  cassert(config_prof);

  malloc_mutex_lock(tsdn, gctx->lock);

  /*
   * Increment nlimbo so that gctx won't go away before dump.
   * Additionally, link gctx into the dump list so that it is included in
   * prof_dump()'s second pass.
   */
  gctx->nlimbo++;
  gctx_tree_insert(gctxs, gctx);

  memset(&gctx->cnt_summed, 0, sizeof(prof_cnt_t));

  malloc_mutex_unlock(tsdn, gctx->lock);
}

struct prof_gctx_merge_iter_arg_s {
  tsdn_t* tsdn;
  size_t leak_ngctx;
};

static prof_gctx_t* prof_gctx_merge_iter(
  prof_gctx_tree_t* gctxs, prof_gctx_t* gctx, void* opaque) {
  struct prof_gctx_merge_iter_arg_s* arg
    = (struct prof_gctx_merge_iter_arg_s*)opaque;

  malloc_mutex_lock(arg->tsdn, gctx->lock);
  tctx_tree_iter(
    &gctx->tctxs, NULL, prof_tctx_merge_iter, (void*)arg->tsdn);
  if (gctx->cnt_summed.curobjs != 0) { arg->leak_ngctx++; }
  malloc_mutex_unlock(arg->tsdn, gctx->lock);

  return NULL;
}

static void prof_gctx_finish(tsd_t* tsd, prof_gctx_tree_t* gctxs) {
  prof_tdata_t* tdata = prof_tdata_get(tsd, false);
  prof_gctx_t* gctx;

  /*
   * Standard tree iteration won't work here, because as soon as we
   * decrement gctx->nlimbo and unlock gctx, another thread can
   * concurrently destroy it, which will corrupt the tree.  Therefore,
   * tear down the tree one node at a time during iteration.
   */
  while ((gctx = gctx_tree_first(gctxs)) != NULL) {
    gctx_tree_remove(gctxs, gctx);
    malloc_mutex_lock(tsd_tsdn(tsd), gctx->lock);
    {
      prof_tctx_t* next;

      next = NULL;
      do {
        prof_tctx_t* to_destroy = tctx_tree_iter(
          &gctx->tctxs, next, prof_tctx_finish_iter, (void*)tsd_tsdn(tsd));
        if (to_destroy != NULL) {
          next = tctx_tree_next(&gctx->tctxs, to_destroy);
          tctx_tree_remove(&gctx->tctxs, to_destroy);
          idalloctm(tsd_tsdn(tsd), to_destroy, NULL, NULL, true, true);
        } else {
          next = NULL;
        }
      } while (next != NULL);
    }
    gctx->nlimbo--;
    if (prof_gctx_should_destroy(gctx)) {
      gctx->nlimbo++;
      malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
      prof_gctx_try_destroy(tsd, tdata, gctx, tdata);
    } else {
      malloc_mutex_unlock(tsd_tsdn(tsd), gctx->lock);
    }
  }
}

struct prof_tdata_merge_iter_arg_s {
  tsdn_t* tsdn;
  prof_cnt_t cnt_all;
};

static prof_tdata_t* prof_tdata_merge_iter(
  prof_tdata_tree_t* tdatas, prof_tdata_t* tdata, void* opaque) {
  struct prof_tdata_merge_iter_arg_s* arg
    = (struct prof_tdata_merge_iter_arg_s*)opaque;

  malloc_mutex_lock(arg->tsdn, tdata->lock);
  if (!tdata->expired) {
    size_t tabind;
    union {
      prof_tctx_t* p;
      void* v;
    } tctx;

    tdata->dumping = true;
    memset(&tdata->cnt_summed, 0, sizeof(prof_cnt_t));
    for (tabind = 0; !ckh_iter(&tdata->bt2tctx, &tabind, NULL, &tctx.v);) {
      prof_tctx_merge_tdata(arg->tsdn, tctx.p, tdata);
    }

    arg->cnt_all.curobjs += tdata->cnt_summed.curobjs;
    arg->cnt_all.curbytes += tdata->cnt_summed.curbytes;
    if (opt_prof_accum) {
      arg->cnt_all.accumobjs += tdata->cnt_summed.accumobjs;
      arg->cnt_all.accumbytes += tdata->cnt_summed.accumbytes;
    }
  } else {
    tdata->dumping = false;
  }
  malloc_mutex_unlock(arg->tsdn, tdata->lock);

  return NULL;
}

static prof_tdata_t* prof_tdata_dump_iter(
  prof_tdata_tree_t* tdatas, prof_tdata_t* tdata, void* arg) {
  bool propagate_err = *(bool*)arg;

  if (!tdata->dumping) { return NULL; }

  if (prof_dump_printf(propagate_err,
        "  t%" FMTu64 ": %" FMTu64 ": %" FMTu64 " [%" FMTu64 ": %" FMTu64
        "]%s%s\n",
        tdata->thr_uid, tdata->cnt_summed.curobjs,
        tdata->cnt_summed.curbytes, tdata->cnt_summed.accumobjs,
        tdata->cnt_summed.accumbytes,
        (tdata->thread_name != NULL) ? " " : "",
        (tdata->thread_name != NULL) ? tdata->thread_name : "")) {
    return tdata;
  }
  return NULL;
}

static bool prof_dump_header_impl(
  tsdn_t* tsdn, bool propagate_err, const prof_cnt_t* cnt_all) {
  bool ret;

  if (prof_dump_printf(propagate_err,
        "heap_v2/%" FMTu64 "\n"
        "  t*: %" FMTu64 ": %" FMTu64 " [%" FMTu64 ": %" FMTu64 "]\n",
        ((uint64_t)1U << lg_prof_sample), cnt_all->curobjs,
        cnt_all->curbytes, cnt_all->accumobjs, cnt_all->accumbytes)) {
    return true;
  }

  malloc_mutex_lock(tsdn, &tdatas_mtx);
  ret = (tdata_tree_iter(
           &tdatas, NULL, prof_tdata_dump_iter, (void*)&propagate_err)
    != NULL);
  malloc_mutex_unlock(tsdn, &tdatas_mtx);
  return ret;
}
prof_dump_header_t* JET_MUTABLE prof_dump_header = prof_dump_header_impl;

static bool prof_dump_gctx(tsdn_t* tsdn, bool propagate_err,
  prof_gctx_t* gctx, const prof_bt_t* bt, prof_gctx_tree_t* gctxs) {
  bool ret;
  unsigned i;
  struct prof_tctx_dump_iter_arg_s prof_tctx_dump_iter_arg;

  cassert(config_prof);
  malloc_mutex_assert_owner(tsdn, gctx->lock);

  /* Avoid dumping such gctx's that have no useful data. */
  if ((!opt_prof_accum && gctx->cnt_summed.curobjs == 0)
    || (opt_prof_accum && gctx->cnt_summed.accumobjs == 0)) {
    assert(gctx->cnt_summed.curobjs == 0);
    assert(gctx->cnt_summed.curbytes == 0);
    assert(gctx->cnt_summed.accumobjs == 0);
    assert(gctx->cnt_summed.accumbytes == 0);
    ret = false;
    goto label_return;
  }

  if (prof_dump_printf(propagate_err, "@")) {
    ret = true;
    goto label_return;
  }
  for (i = 0; i < bt->len; i++) {
    if (prof_dump_printf(
          propagate_err, " %#" FMTxPTR, (uintptr_t)bt->vec[i])) {
      ret = true;
      goto label_return;
    }
  }

  if (prof_dump_printf(propagate_err,
        "\n"
        "  t*: %" FMTu64 ": %" FMTu64 " [%" FMTu64 ": %" FMTu64 "]\n",
        gctx->cnt_summed.curobjs, gctx->cnt_summed.curbytes,
        gctx->cnt_summed.accumobjs, gctx->cnt_summed.accumbytes)) {
    ret = true;
    goto label_return;
  }

  prof_tctx_dump_iter_arg.tsdn = tsdn;
  prof_tctx_dump_iter_arg.propagate_err = propagate_err;
  if (tctx_tree_iter(&gctx->tctxs, NULL, prof_tctx_dump_iter,
        (void*)&prof_tctx_dump_iter_arg)
    != NULL) {
    ret = true;
    goto label_return;
  }

  ret = false;
label_return:
  return ret;
}

#ifndef _WIN32
JEMALLOC_FORMAT_PRINTF(1, 2)
static int prof_open_maps(const char* format, ...) {
  int mfd;
  va_list ap;
  char filename[PATH_MAX + 1];

  va_start(ap, format);
  malloc_vsnprintf(filename, sizeof(filename), format, ap);
  va_end(ap);

#if defined(O_CLOEXEC)
  mfd = open(filename, O_RDONLY | O_CLOEXEC);
#else
  mfd = open(filename, O_RDONLY);
  if (mfd != -1) { fcntl(mfd, F_SETFD, fcntl(mfd, F_GETFD) | FD_CLOEXEC); }
#endif

  return mfd;
}
#endif

static int prof_getpid(void) {
#ifdef _WIN32
  return GetCurrentProcessId();
#else
  return getpid();
#endif
}

static bool prof_dump_maps(bool propagate_err) {
  bool ret;
  int mfd;

  cassert(config_prof);
#ifdef __FreeBSD__
  mfd = prof_open_maps("/proc/curproc/map");
#elif defined(_WIN32)
  mfd = -1; // Not implemented
#else
  {
    int pid = prof_getpid();

    mfd = prof_open_maps("/proc/%d/task/%d/maps", pid, pid);
    if (mfd == -1) { mfd = prof_open_maps("/proc/%d/maps", pid); }
  }
#endif
  if (mfd != -1) {
    ssize_t nread;

    if (prof_dump_write(propagate_err, "\nMAPPED_LIBRARIES:\n")
      && propagate_err) {
      ret = true;
      goto label_return;
    }
    nread = 0;
    do {
      prof_dump_buf_end += nread;
      if (prof_dump_buf_end == PROF_DUMP_BUFSIZE) {
        /* Make space in prof_dump_buf before read(). */
        if (prof_dump_flush(propagate_err) && propagate_err) {
          ret = true;
          goto label_return;
        }
      }
      nread = malloc_read_fd(mfd, &prof_dump_buf[prof_dump_buf_end],
        PROF_DUMP_BUFSIZE - prof_dump_buf_end);
    } while (nread > 0);
  } else {
    ret = true;
    goto label_return;
  }

  ret = false;
label_return:
  if (mfd != -1) { close(mfd); }
  return ret;
}

/*
 * See prof_sample_threshold_update() comment for why the body of this
 * function is conditionally compiled.
 */
static void prof_leakcheck(
  const prof_cnt_t* cnt_all, size_t leak_ngctx, const char* filename) {
#ifdef JEMALLOC_PROF
  /*
   * Scaling is equivalent AdjustSamples() in jeprof, but the result may
   * differ slightly from what jeprof reports, because here we scale the
   * summary values, whereas jeprof scales each context individually and
   * reports the sums of the scaled values.
   */
  if (cnt_all->curbytes != 0) {
    double sample_period = (double)((uint64_t)1 << lg_prof_sample);
    double ratio = (((double)cnt_all->curbytes) / (double)cnt_all->curobjs)
      / sample_period;
    double scale_factor = 1.0 / (1.0 - exp(-ratio));
    uint64_t curbytes
      = (uint64_t)round(((double)cnt_all->curbytes) * scale_factor);
    uint64_t curobjs
      = (uint64_t)round(((double)cnt_all->curobjs) * scale_factor);

    malloc_printf("<jemalloc>: Leak approximation summary: ~%" FMTu64
                  " byte%s, ~%" FMTu64 " object%s, >= %zu context%s\n",
      curbytes, (curbytes != 1) ? "s" : "", curobjs,
      (curobjs != 1) ? "s" : "", leak_ngctx, (leak_ngctx != 1) ? "s" : "");
    malloc_printf(
      "<jemalloc>: Run jeprof on \"%s\" for leak detail\n", filename);
  }
#endif
}

struct prof_gctx_dump_iter_arg_s {
  tsdn_t* tsdn;
  bool propagate_err;
};

static prof_gctx_t* prof_gctx_dump_iter(
  prof_gctx_tree_t* gctxs, prof_gctx_t* gctx, void* opaque) {
  prof_gctx_t* ret;
  struct prof_gctx_dump_iter_arg_s* arg
    = (struct prof_gctx_dump_iter_arg_s*)opaque;

  malloc_mutex_lock(arg->tsdn, gctx->lock);

  if (prof_dump_gctx(
        arg->tsdn, arg->propagate_err, gctx, &gctx->bt, gctxs)) {
    ret = gctx;
    goto label_return;
  }

  ret = NULL;
label_return:
  malloc_mutex_unlock(arg->tsdn, gctx->lock);
  return ret;
}

static void prof_dump_prep(tsd_t* tsd, prof_tdata_t* tdata,
  struct prof_tdata_merge_iter_arg_s* prof_tdata_merge_iter_arg,
  struct prof_gctx_merge_iter_arg_s* prof_gctx_merge_iter_arg,
  prof_gctx_tree_t* gctxs) {
  size_t tabind;
  union {
    prof_gctx_t* p;
    void* v;
  } gctx;

  prof_enter(tsd, tdata);

  /*
   * Put gctx's in limbo and clear their counters in preparation for
   * summing.
   */
  gctx_tree_new(gctxs);
  for (tabind = 0; !ckh_iter(&bt2gctx, &tabind, NULL, &gctx.v);) {
    prof_dump_gctx_prep(tsd_tsdn(tsd), gctx.p, gctxs);
  }

  /*
   * Iterate over tdatas, and for the non-expired ones snapshot their tctx
   * stats and merge them into the associated gctx's.
   */
  prof_tdata_merge_iter_arg->tsdn = tsd_tsdn(tsd);
  memset(&prof_tdata_merge_iter_arg->cnt_all, 0, sizeof(prof_cnt_t));
  malloc_mutex_lock(tsd_tsdn(tsd), &tdatas_mtx);
  tdata_tree_iter(
    &tdatas, NULL, prof_tdata_merge_iter, (void*)prof_tdata_merge_iter_arg);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tdatas_mtx);

  /* Merge tctx stats into gctx's. */
  prof_gctx_merge_iter_arg->tsdn = tsd_tsdn(tsd);
  prof_gctx_merge_iter_arg->leak_ngctx = 0;
  gctx_tree_iter(
    gctxs, NULL, prof_gctx_merge_iter, (void*)prof_gctx_merge_iter_arg);

  prof_leave(tsd, tdata);
}

static bool prof_dump_file(tsd_t* tsd, bool propagate_err,
  const char* filename, bool leakcheck, prof_tdata_t* tdata,
  struct prof_tdata_merge_iter_arg_s* prof_tdata_merge_iter_arg,
  struct prof_gctx_merge_iter_arg_s* prof_gctx_merge_iter_arg,
  struct prof_gctx_dump_iter_arg_s* prof_gctx_dump_iter_arg,
  prof_gctx_tree_t* gctxs) {
  /* Create dump file. */
  if ((prof_dump_fd = prof_dump_open(propagate_err, filename)) == -1) {
    return true;
  }

  /* Dump profile header. */
  if (prof_dump_header(tsd_tsdn(tsd), propagate_err,
        &prof_tdata_merge_iter_arg->cnt_all)) {
    goto label_write_error;
  }

  /* Dump per gctx profile stats. */
  prof_gctx_dump_iter_arg->tsdn = tsd_tsdn(tsd);
  prof_gctx_dump_iter_arg->propagate_err = propagate_err;
  if (gctx_tree_iter(
        gctxs, NULL, prof_gctx_dump_iter, (void*)prof_gctx_dump_iter_arg)
    != NULL) {
    goto label_write_error;
  }

  /* Dump /proc/<pid>/maps if possible. */
  if (prof_dump_maps(propagate_err)) { goto label_write_error; }

  if (prof_dump_close(propagate_err)) { return true; }

  return false;
label_write_error:
  prof_dump_close(propagate_err);
  return true;
}

static bool prof_dump(
  tsd_t* tsd, bool propagate_err, const char* filename, bool leakcheck) {
  cassert(config_prof);
  assert(tsd_reentrancy_level_get(tsd) == 0);

  prof_tdata_t* tdata = prof_tdata_get(tsd, true);
  if (tdata == NULL) { return true; }

  pre_reentrancy(tsd, NULL);
  malloc_mutex_lock(tsd_tsdn(tsd), &prof_dump_mtx);

  prof_gctx_tree_t gctxs;
  struct prof_tdata_merge_iter_arg_s prof_tdata_merge_iter_arg;
  struct prof_gctx_merge_iter_arg_s prof_gctx_merge_iter_arg;
  struct prof_gctx_dump_iter_arg_s prof_gctx_dump_iter_arg;
  prof_dump_prep(tsd, tdata, &prof_tdata_merge_iter_arg,
    &prof_gctx_merge_iter_arg, &gctxs);
  bool err = prof_dump_file(tsd, propagate_err, filename, leakcheck, tdata,
    &prof_tdata_merge_iter_arg, &prof_gctx_merge_iter_arg,
    &prof_gctx_dump_iter_arg, &gctxs);
  prof_gctx_finish(tsd, &gctxs);

  malloc_mutex_unlock(tsd_tsdn(tsd), &prof_dump_mtx);
  post_reentrancy(tsd);

  if (err) { return true; }

  if (leakcheck) {
    prof_leakcheck(&prof_tdata_merge_iter_arg.cnt_all,
      prof_gctx_merge_iter_arg.leak_ngctx, filename);
  }
  return false;
}

#ifdef JEMALLOC_JET
void prof_cnt_all(uint64_t* curobjs, uint64_t* curbytes,
  uint64_t* accumobjs, uint64_t* accumbytes) {
  tsd_t* tsd;
  prof_tdata_t* tdata;
  struct prof_tdata_merge_iter_arg_s prof_tdata_merge_iter_arg;
  struct prof_gctx_merge_iter_arg_s prof_gctx_merge_iter_arg;
  prof_gctx_tree_t gctxs;

  tsd = tsd_fetch();
  tdata = prof_tdata_get(tsd, false);
  if (tdata == NULL) {
    if (curobjs != NULL) { *curobjs = 0; }
    if (curbytes != NULL) { *curbytes = 0; }
    if (accumobjs != NULL) { *accumobjs = 0; }
    if (accumbytes != NULL) { *accumbytes = 0; }
    return;
  }

  prof_dump_prep(tsd, tdata, &prof_tdata_merge_iter_arg,
    &prof_gctx_merge_iter_arg, &gctxs);
  prof_gctx_finish(tsd, &gctxs);

  if (curobjs != NULL) {
    *curobjs = prof_tdata_merge_iter_arg.cnt_all.curobjs;
  }
  if (curbytes != NULL) {
    *curbytes = prof_tdata_merge_iter_arg.cnt_all.curbytes;
  }
  if (accumobjs != NULL) {
    *accumobjs = prof_tdata_merge_iter_arg.cnt_all.accumobjs;
  }
  if (accumbytes != NULL) {
    *accumbytes = prof_tdata_merge_iter_arg.cnt_all.accumbytes;
  }
}
#endif

#define DUMP_FILENAME_BUFSIZE (PATH_MAX + 1)
#define VSEQ_INVALID UINT64_C(0xffffffffffffffff)
static void prof_dump_filename(char* filename, char v, uint64_t vseq) {
  cassert(config_prof);

  if (vseq != VSEQ_INVALID) {
    /* "<prefix>.<pid>.<seq>.v<vseq>.heap" */
    malloc_snprintf(filename, DUMP_FILENAME_BUFSIZE,
      "%s.%d.%" FMTu64 ".%c%" FMTu64 ".heap", opt_prof_prefix,
      prof_getpid(), prof_dump_seq, v, vseq);
  } else {
    /* "<prefix>.<pid>.<seq>.<v>.heap" */
    malloc_snprintf(filename, DUMP_FILENAME_BUFSIZE,
      "%s.%d.%" FMTu64 ".%c.heap", opt_prof_prefix, prof_getpid(),
      prof_dump_seq, v);
  }
  prof_dump_seq++;
}

static void prof_fdump(void) {
  tsd_t* tsd;
  char filename[DUMP_FILENAME_BUFSIZE];

  cassert(config_prof);
  assert(opt_prof_final);
  assert(opt_prof_prefix[0] != '\0');

  if (!prof_booted) { return; }
  tsd = tsd_fetch();
  assert(tsd_reentrancy_level_get(tsd) == 0);

  malloc_mutex_lock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
  prof_dump_filename(filename, 'f', VSEQ_INVALID);
  malloc_mutex_unlock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
  prof_dump(tsd, false, filename, opt_prof_leak);
}

bool prof_accum_init(tsdn_t* tsdn, prof_accum_t* prof_accum) {
  cassert(config_prof);

#ifndef JEMALLOC_ATOMIC_U64
  if (malloc_mutex_init(&prof_accum->mtx, "prof_accum",
        WITNESS_RANK_PROF_ACCUM, malloc_mutex_rank_exclusive)) {
    return true;
  }
  prof_accum->accumbytes = 0;
#else
  atomic_store_u64(&prof_accum->accumbytes, 0, ATOMIC_RELAXED);
#endif
  return false;
}

void prof_idump(tsdn_t* tsdn) {
  tsd_t* tsd;
  prof_tdata_t* tdata;

  cassert(config_prof);

  if (!prof_booted || tsdn_null(tsdn) || !prof_active_get_unlocked()) {
    return;
  }
  tsd = tsdn_tsd(tsdn);
  if (tsd_reentrancy_level_get(tsd) > 0) { return; }

  tdata = prof_tdata_get(tsd, false);
  if (tdata == NULL) { return; }
  if (tdata->enq) {
    tdata->enq_idump = true;
    return;
  }

  if (opt_prof_prefix[0] != '\0') {
    char filename[PATH_MAX + 1];
    malloc_mutex_lock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
    prof_dump_filename(filename, 'i', prof_dump_iseq);
    prof_dump_iseq++;
    malloc_mutex_unlock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
    prof_dump(tsd, false, filename, false);
  }
}

bool prof_mdump(tsd_t* tsd, const char* filename) {
  cassert(config_prof);
  assert(tsd_reentrancy_level_get(tsd) == 0);

  if (!opt_prof || !prof_booted) { return true; }
  char filename_buf[DUMP_FILENAME_BUFSIZE];
  if (filename == NULL) {
    /* No filename specified, so automatically generate one. */
    if (opt_prof_prefix[0] == '\0') { return true; }
    malloc_mutex_lock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
    prof_dump_filename(filename_buf, 'm', prof_dump_mseq);
    prof_dump_mseq++;
    malloc_mutex_unlock(tsd_tsdn(tsd), &prof_dump_seq_mtx);
    filename = filename_buf;
  }
  return prof_dump(tsd, true, filename, false);
}

void prof_gdump(tsdn_t* tsdn) {
  tsd_t* tsd;
  prof_tdata_t* tdata;

  cassert(config_prof);

  if (!prof_booted || tsdn_null(tsdn) || !prof_active_get_unlocked()) {
    return;
  }
  tsd = tsdn_tsd(tsdn);
  if (tsd_reentrancy_level_get(tsd) > 0) { return; }

  tdata = prof_tdata_get(tsd, false);
  if (tdata == NULL) { return; }
  if (tdata->enq) {
    tdata->enq_gdump = true;
    return;
  }

  if (opt_prof_prefix[0] != '\0') {
    char filename[DUMP_FILENAME_BUFSIZE];
    malloc_mutex_lock(tsdn, &prof_dump_seq_mtx);
    prof_dump_filename(filename, 'u', prof_dump_useq);
    prof_dump_useq++;
    malloc_mutex_unlock(tsdn, &prof_dump_seq_mtx);
    prof_dump(tsd, false, filename, false);
  }
}

static void prof_bt_hash(const void* key, size_t r_hash[2]) {
  prof_bt_t* bt = (prof_bt_t*)key;

  cassert(config_prof);

  hash(bt->vec, bt->len * sizeof(void*), 0x94122f33U, r_hash);
}

static bool prof_bt_keycomp(const void* k1, const void* k2) {
  const prof_bt_t* bt1 = (prof_bt_t*)k1;
  const prof_bt_t* bt2 = (prof_bt_t*)k2;

  cassert(config_prof);

  if (bt1->len != bt2->len) { return false; }
  return (memcmp(bt1->vec, bt2->vec, bt1->len * sizeof(void*)) == 0);
}

static void prof_bt_node_hash(const void* key, size_t r_hash[2]) {
  const prof_bt_node_t* bt_node = (prof_bt_node_t*)key;
  prof_bt_hash((void*)(&bt_node->bt), r_hash);
}

static bool prof_bt_node_keycomp(const void* k1, const void* k2) {
  const prof_bt_node_t* bt_node1 = (prof_bt_node_t*)k1;
  const prof_bt_node_t* bt_node2 = (prof_bt_node_t*)k2;
  return prof_bt_keycomp((void*)(&bt_node1->bt), (void*)(&bt_node2->bt));
}

static void prof_thr_node_hash(const void* key, size_t r_hash[2]) {
  const prof_thr_node_t* thr_node = (prof_thr_node_t*)key;
  hash(&thr_node->thr_uid, sizeof(uint64_t), 0x94122f35U, r_hash);
}

static bool prof_thr_node_keycomp(const void* k1, const void* k2) {
  const prof_thr_node_t* thr_node1 = (prof_thr_node_t*)k1;
  const prof_thr_node_t* thr_node2 = (prof_thr_node_t*)k2;
  return thr_node1->thr_uid == thr_node2->thr_uid;
}

static uint64_t prof_thr_uid_alloc(tsdn_t* tsdn) {
  uint64_t thr_uid;

  malloc_mutex_lock(tsdn, &next_thr_uid_mtx);
  thr_uid = next_thr_uid;
  next_thr_uid++;
  malloc_mutex_unlock(tsdn, &next_thr_uid_mtx);

  return thr_uid;
}

static prof_tdata_t* prof_tdata_init_impl(tsd_t* tsd, uint64_t thr_uid,
  uint64_t thr_discrim, char* thread_name, bool active) {
  prof_tdata_t* tdata;

  cassert(config_prof);

  /* Initialize an empty cache for this thread. */
  tdata = (prof_tdata_t*)iallocztm(tsd_tsdn(tsd), sizeof(prof_tdata_t),
    sz_size2index(sizeof(prof_tdata_t)), false, NULL, true,
    arena_get(TSDN_NULL, 0, true), true);
  if (tdata == NULL) { return NULL; }

  tdata->lock = prof_tdata_mutex_choose(thr_uid);
  tdata->thr_uid = thr_uid;
  tdata->thr_discrim = thr_discrim;
  tdata->thread_name = thread_name;
  tdata->attached = true;
  tdata->expired = false;
  tdata->tctx_uid_next = 0;

  if (ckh_new(tsd, &tdata->bt2tctx, PROF_CKH_MINITEMS, prof_bt_hash,
        prof_bt_keycomp)) {
    idalloctm(tsd_tsdn(tsd), tdata, NULL, NULL, true, true);
    return NULL;
  }

  tdata->prng_state = (uint64_t)(uintptr_t)tdata;
  prof_sample_threshold_update(tdata);

  tdata->enq = false;
  tdata->enq_idump = false;
  tdata->enq_gdump = false;

  tdata->dumping = false;
  tdata->active = active;

  malloc_mutex_lock(tsd_tsdn(tsd), &tdatas_mtx);
  tdata_tree_insert(&tdatas, tdata);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tdatas_mtx);

  return tdata;
}

prof_tdata_t* prof_tdata_init(tsd_t* tsd) {
  return prof_tdata_init_impl(tsd, prof_thr_uid_alloc(tsd_tsdn(tsd)), 0,
    NULL, prof_thread_active_init_get(tsd_tsdn(tsd)));
}

static bool prof_tdata_should_destroy_unlocked(
  prof_tdata_t* tdata, bool even_if_attached) {
  if (tdata->attached && !even_if_attached) { return false; }
  if (ckh_count(&tdata->bt2tctx) != 0) { return false; }
  return true;
}

static bool prof_tdata_should_destroy(
  tsdn_t* tsdn, prof_tdata_t* tdata, bool even_if_attached) {
  malloc_mutex_assert_owner(tsdn, tdata->lock);

  return prof_tdata_should_destroy_unlocked(tdata, even_if_attached);
}

static void prof_tdata_destroy_locked(
  tsd_t* tsd, prof_tdata_t* tdata, bool even_if_attached) {
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &tdatas_mtx);

  tdata_tree_remove(&tdatas, tdata);

  assert(prof_tdata_should_destroy_unlocked(tdata, even_if_attached));

  if (tdata->thread_name != NULL) {
    idalloctm(tsd_tsdn(tsd), tdata->thread_name, NULL, NULL, true, true);
  }
  ckh_delete(tsd, &tdata->bt2tctx);
  idalloctm(tsd_tsdn(tsd), tdata, NULL, NULL, true, true);
}

static void prof_tdata_destroy(
  tsd_t* tsd, prof_tdata_t* tdata, bool even_if_attached) {
  malloc_mutex_lock(tsd_tsdn(tsd), &tdatas_mtx);
  prof_tdata_destroy_locked(tsd, tdata, even_if_attached);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tdatas_mtx);
}

static void prof_tdata_detach(tsd_t* tsd, prof_tdata_t* tdata) {
  bool destroy_tdata;

  malloc_mutex_lock(tsd_tsdn(tsd), tdata->lock);
  if (tdata->attached) {
    destroy_tdata = prof_tdata_should_destroy(tsd_tsdn(tsd), tdata, true);
    /*
     * Only detach if !destroy_tdata, because detaching would allow
     * another thread to win the race to destroy tdata.
     */
    if (!destroy_tdata) { tdata->attached = false; }
    tsd_prof_tdata_set(tsd, NULL);
  } else {
    destroy_tdata = false;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), tdata->lock);
  if (destroy_tdata) { prof_tdata_destroy(tsd, tdata, true); }
}

prof_tdata_t* prof_tdata_reinit(tsd_t* tsd, prof_tdata_t* tdata) {
  uint64_t thr_uid = tdata->thr_uid;
  uint64_t thr_discrim = tdata->thr_discrim + 1;
  char* thread_name = (tdata->thread_name != NULL)
    ? prof_thread_name_alloc(tsd_tsdn(tsd), tdata->thread_name)
    : NULL;
  bool active = tdata->active;

  prof_tdata_detach(tsd, tdata);
  return prof_tdata_init_impl(
    tsd, thr_uid, thr_discrim, thread_name, active);
}

static bool prof_tdata_expire(tsdn_t* tsdn, prof_tdata_t* tdata) {
  bool destroy_tdata;

  malloc_mutex_lock(tsdn, tdata->lock);
  if (!tdata->expired) {
    tdata->expired = true;
    destroy_tdata = tdata->attached
      ? false
      : prof_tdata_should_destroy(tsdn, tdata, false);
  } else {
    destroy_tdata = false;
  }
  malloc_mutex_unlock(tsdn, tdata->lock);

  return destroy_tdata;
}

static prof_tdata_t* prof_tdata_reset_iter(
  prof_tdata_tree_t* tdatas, prof_tdata_t* tdata, void* arg) {
  tsdn_t* tsdn = (tsdn_t*)arg;

  return (prof_tdata_expire(tsdn, tdata) ? tdata : NULL);
}

void prof_reset(tsd_t* tsd, size_t lg_sample) {
  prof_tdata_t* next;

  assert(lg_sample < (sizeof(uint64_t) << 3));

  malloc_mutex_lock(tsd_tsdn(tsd), &prof_dump_mtx);
  malloc_mutex_lock(tsd_tsdn(tsd), &tdatas_mtx);

  lg_prof_sample = lg_sample;

  next = NULL;
  do {
    prof_tdata_t* to_destroy
      = tdata_tree_iter(&tdatas, next, prof_tdata_reset_iter, (void*)tsd);
    if (to_destroy != NULL) {
      next = tdata_tree_next(&tdatas, to_destroy);
      prof_tdata_destroy_locked(tsd, to_destroy, false);
    } else {
      next = NULL;
    }
  } while (next != NULL);

  malloc_mutex_unlock(tsd_tsdn(tsd), &tdatas_mtx);
  malloc_mutex_unlock(tsd_tsdn(tsd), &prof_dump_mtx);
}

void prof_tdata_cleanup(tsd_t* tsd) {
  prof_tdata_t* tdata;

  if (!config_prof) { return; }

  tdata = tsd_prof_tdata_get(tsd);
  if (tdata != NULL) { prof_tdata_detach(tsd, tdata); }
}

bool prof_active_get(tsdn_t* tsdn) {
  bool prof_active_current;

  malloc_mutex_lock(tsdn, &prof_active_mtx);
  prof_active_current = prof_active;
  malloc_mutex_unlock(tsdn, &prof_active_mtx);
  return prof_active_current;
}

bool prof_active_set(tsdn_t* tsdn, bool active) {
  bool prof_active_old;

  malloc_mutex_lock(tsdn, &prof_active_mtx);
  prof_active_old = prof_active;
  prof_active = active;
  malloc_mutex_unlock(tsdn, &prof_active_mtx);
  return prof_active_old;
}

#ifdef JEMALLOC_JET
size_t prof_log_bt_count(void) {
  size_t cnt = 0;
  prof_bt_node_t* node = log_bt_first;
  while (node != NULL) {
    cnt++;
    node = node->next;
  }
  return cnt;
}

size_t prof_log_alloc_count(void) {
  size_t cnt = 0;
  prof_alloc_node_t* node = log_alloc_first;
  while (node != NULL) {
    cnt++;
    node = node->next;
  }
  return cnt;
}

size_t prof_log_thr_count(void) {
  size_t cnt = 0;
  prof_thr_node_t* node = log_thr_first;
  while (node != NULL) {
    cnt++;
    node = node->next;
  }
  return cnt;
}

bool prof_log_is_logging(void) {
  return prof_logging_state == prof_logging_state_started;
}

bool prof_log_rep_check(void) {
  if (prof_logging_state == prof_logging_state_stopped
    && log_tables_initialized) {
    return true;
  }

  if (log_bt_last != NULL && log_bt_last->next != NULL) { return true; }
  if (log_thr_last != NULL && log_thr_last->next != NULL) { return true; }
  if (log_alloc_last != NULL && log_alloc_last->next != NULL) {
    return true;
  }

  size_t bt_count = prof_log_bt_count();
  size_t thr_count = prof_log_thr_count();
  size_t alloc_count = prof_log_alloc_count();

  if (prof_logging_state == prof_logging_state_stopped) {
    if (bt_count != 0 || thr_count != 0 || alloc_count || 0) {
      return true;
    }
  }

  prof_alloc_node_t* node = log_alloc_first;
  while (node != NULL) {
    if (node->alloc_bt_ind >= bt_count) { return true; }
    if (node->free_bt_ind >= bt_count) { return true; }
    if (node->alloc_thr_ind >= thr_count) { return true; }
    if (node->free_thr_ind >= thr_count) { return true; }
    if (node->alloc_time_ns > node->free_time_ns) { return true; }
    node = node->next;
  }

  return false;
}

void prof_log_dummy_set(bool new_value) { prof_log_dummy = new_value; }
#endif

bool prof_log_start(tsdn_t* tsdn, const char* filename) {
  if (!opt_prof || !prof_booted) { return true; }

  bool ret = false;
  size_t buf_size = PATH_MAX + 1;

  malloc_mutex_lock(tsdn, &log_mtx);

  if (prof_logging_state != prof_logging_state_stopped) {
    ret = true;
  } else if (filename == NULL) {
    /* Make default name. */
    malloc_snprintf(log_filename, buf_size, "%s.%d.%" FMTu64 ".json",
      opt_prof_prefix, prof_getpid(), log_seq);
    log_seq++;
    prof_logging_state = prof_logging_state_started;
  } else if (cstr_len(filename) >= buf_size) {
    ret = true;
  } else {
    strcpy(log_filename, filename);
    prof_logging_state = prof_logging_state_started;
  }

  if (!ret) { nstime_update(&log_start_timestamp); }

  malloc_mutex_unlock(tsdn, &log_mtx);

  return ret;
}

/* Used as an atexit function to stop logging on exit. */
static void prof_log_stop_final(void) {
  tsd_t* tsd = tsd_fetch();
  prof_log_stop(tsd_tsdn(tsd));
}

struct prof_emitter_cb_arg_s {
  int fd;
  ssize_t ret;
};

static void prof_emitter_write_cb(void* opaque, const char* to_write) {
  struct prof_emitter_cb_arg_s* arg = (struct prof_emitter_cb_arg_s*)opaque;
  size_t bytes = cstr_len(to_write);
#ifdef JEMALLOC_JET
  if (prof_log_dummy) { return; }
#endif
  arg->ret = write(arg->fd, (void*)to_write, bytes);
}

/*
 * prof_log_emit_{...} goes through the appropriate linked list, emitting
 * each node to the json and deallocating it.
 */
static void prof_log_emit_threads(tsd_t* tsd, emitter_t* emitter) {
  emitter_json_array_kv_begin(emitter, "threads");
  prof_thr_node_t* thr_node = log_thr_first;
  prof_thr_node_t* thr_old_node;
  while (thr_node != NULL) {
    emitter_json_object_begin(emitter);

    emitter_json_kv(
      emitter, "thr_uid", emitter_type_uint64, &thr_node->thr_uid);

    char* thr_name = thr_node->name;

    emitter_json_kv(emitter, "thr_name", emitter_type_string, &thr_name);

    emitter_json_object_end(emitter);
    thr_old_node = thr_node;
    thr_node = thr_node->next;
    idalloc(tsd, thr_old_node);
  }
  emitter_json_array_end(emitter);
}

static void prof_log_emit_traces(tsd_t* tsd, emitter_t* emitter) {
  emitter_json_array_kv_begin(emitter, "stack_traces");
  prof_bt_node_t* bt_node = log_bt_first;
  prof_bt_node_t* bt_old_node;
  /*
   * Calculate how many hex digits we need: twice number of bytes, two for
   * "0x", and then one more for terminating '\0'.
   */
  char buf[2 * sizeof(intptr_t) + 3];
  size_t buf_sz = sizeof(buf);
  while (bt_node != NULL) {
    emitter_json_array_begin(emitter);
    size_t i;
    for (i = 0; i < bt_node->bt.len; i++) {
      malloc_snprintf(buf, buf_sz, "%p", bt_node->bt.vec[i]);
      char* trace_str = buf;
      emitter_json_value(emitter, emitter_type_string, &trace_str);
    }
    emitter_json_array_end(emitter);

    bt_old_node = bt_node;
    bt_node = bt_node->next;
    idalloc(tsd, bt_old_node);
  }
  emitter_json_array_end(emitter);
}

static void prof_log_emit_allocs(tsd_t* tsd, emitter_t* emitter) {
  emitter_json_array_kv_begin(emitter, "allocations");
  prof_alloc_node_t* alloc_node = log_alloc_first;
  prof_alloc_node_t* alloc_old_node;
  while (alloc_node != NULL) {
    emitter_json_object_begin(emitter);

    emitter_json_kv(emitter, "alloc_thread", emitter_type_size,
      &alloc_node->alloc_thr_ind);

    emitter_json_kv(
      emitter, "free_thread", emitter_type_size, &alloc_node->free_thr_ind);

    emitter_json_kv(
      emitter, "alloc_trace", emitter_type_size, &alloc_node->alloc_bt_ind);

    emitter_json_kv(
      emitter, "free_trace", emitter_type_size, &alloc_node->free_bt_ind);

    emitter_json_kv(emitter, "alloc_timestamp", emitter_type_uint64,
      &alloc_node->alloc_time_ns);

    emitter_json_kv(emitter, "free_timestamp", emitter_type_uint64,
      &alloc_node->free_time_ns);

    emitter_json_kv(
      emitter, "usize", emitter_type_uint64, &alloc_node->usize);

    emitter_json_object_end(emitter);

    alloc_old_node = alloc_node;
    alloc_node = alloc_node->next;
    idalloc(tsd, alloc_old_node);
  }
  emitter_json_array_end(emitter);
}

static void prof_log_emit_metadata(emitter_t* emitter) {
  emitter_json_object_kv_begin(emitter, "info");

  nstime_t now = NSTIME_ZERO_INITIALIZER;

  nstime_update(&now);
  uint64_t ns = nstime_ns(&now) - nstime_ns(&log_start_timestamp);
  emitter_json_kv(emitter, "duration", emitter_type_uint64, &ns);

  char* vers = JEMALLOC_VERSION;
  emitter_json_kv(emitter, "version", emitter_type_string, &vers);

  emitter_json_kv(
    emitter, "lg_sample_rate", emitter_type_int, &lg_prof_sample);

  int pid = prof_getpid();
  emitter_json_kv(emitter, "pid", emitter_type_int, &pid);

  emitter_json_object_end(emitter);
}

bool prof_log_stop(tsdn_t* tsdn) {
  if (!opt_prof || !prof_booted) { return true; }

  tsd_t* tsd = tsdn_tsd(tsdn);
  malloc_mutex_lock(tsdn, &log_mtx);

  if (prof_logging_state != prof_logging_state_started) {
    malloc_mutex_unlock(tsdn, &log_mtx);
    return true;
  }

  /*
   * Set the state to dumping. We'll set it to stopped when we're done.
   * Since other threads won't be able to start/stop/log when the state is
   * dumping, we don't have to hold the lock during the whole method.
   */
  prof_logging_state = prof_logging_state_dumping;
  malloc_mutex_unlock(tsdn, &log_mtx);

  emitter_t emitter;

  /* Create a file. */

  int fd;
#ifdef JEMALLOC_JET
  if (prof_log_dummy) {
    fd = 0;
  } else {
    fd = creat(log_filename, 0644);
  }
#else
  fd = creat(log_filename, 0644);
#endif

  if (fd == -1) {
    malloc_printf("<jemalloc>: creat() for log file \"%s\" "
                  " failed with %d\n",
      log_filename, errno);
    if (opt_abort) { abort(); }
    return true;
  }

  /* Emit to json. */
  struct prof_emitter_cb_arg_s arg;
  arg.fd = fd;
  emitter_init(
    &emitter, emitter_output_json, &prof_emitter_write_cb, (void*)(&arg));

  emitter_begin(&emitter);
  prof_log_emit_metadata(&emitter);
  prof_log_emit_threads(tsd, &emitter);
  prof_log_emit_traces(tsd, &emitter);
  prof_log_emit_allocs(tsd, &emitter);
  emitter_end(&emitter);

  /* Reset global state. */
  if (log_tables_initialized) {
    ckh_delete(tsd, &log_bt_node_set);
    ckh_delete(tsd, &log_thr_node_set);
  }
  log_tables_initialized = false;
  log_bt_index = 0;
  log_thr_index = 0;
  log_bt_first = NULL;
  log_bt_last = NULL;
  log_thr_first = NULL;
  log_thr_last = NULL;
  log_alloc_first = NULL;
  log_alloc_last = NULL;

  malloc_mutex_lock(tsdn, &log_mtx);
  prof_logging_state = prof_logging_state_stopped;
  malloc_mutex_unlock(tsdn, &log_mtx);

#ifdef JEMALLOC_JET
  if (prof_log_dummy) { return false; }
#endif
  return close(fd);
}

const char* prof_thread_name_get(tsd_t* tsd) {
  prof_tdata_t* tdata;

  tdata = prof_tdata_get(tsd, true);
  if (tdata == NULL) { return ""; }
  return (tdata->thread_name != NULL ? tdata->thread_name : "");
}

static char* prof_thread_name_alloc(tsdn_t* tsdn, const char* thread_name) {
  char* ret;
  size_t size;

  if (thread_name == NULL) { return NULL; }

  size = cstr_len(thread_name) + 1;
  if (size == 1) { return ""; }

  ret = iallocztm(tsdn, size, sz_size2index(size), false, NULL, true,
    arena_get(TSDN_NULL, 0, true), true);
  if (ret == NULL) { return NULL; }
  jet_memcpy(ret, thread_name, size);
  return ret;
}

int prof_thread_name_set(tsd_t* tsd, const char* thread_name) {
  prof_tdata_t* tdata;
  unsigned i;
  char* s;

  tdata = prof_tdata_get(tsd, true);
  if (tdata == NULL) { return EAGAIN; }

  /* Validate input. */
  if (thread_name == NULL) { return EFAULT; }
  for (i = 0; thread_name[i] != '\0'; i++) {
    char c = thread_name[i];
    if (!isgraph(c) && !isblank(c)) { return EFAULT; }
  }

  s = prof_thread_name_alloc(tsd_tsdn(tsd), thread_name);
  if (s == NULL) { return EAGAIN; }

  if (tdata->thread_name != NULL) {
    idalloctm(tsd_tsdn(tsd), tdata->thread_name, NULL, NULL, true, true);
    tdata->thread_name = NULL;
  }
  if (cstr_len(s) > 0) { tdata->thread_name = s; }
  return 0;
}

bool prof_thread_active_get(tsd_t* tsd) {
  prof_tdata_t* tdata;

  tdata = prof_tdata_get(tsd, true);
  if (tdata == NULL) { return false; }
  return tdata->active;
}

bool prof_thread_active_set(tsd_t* tsd, bool active) {
  prof_tdata_t* tdata;

  tdata = prof_tdata_get(tsd, true);
  if (tdata == NULL) { return true; }
  tdata->active = active;
  return false;
}

bool prof_thread_active_init_get(tsdn_t* tsdn) {
  bool active_init;

  malloc_mutex_lock(tsdn, &prof_thread_active_init_mtx);
  active_init = prof_thread_active_init;
  malloc_mutex_unlock(tsdn, &prof_thread_active_init_mtx);
  return active_init;
}

bool prof_thread_active_init_set(tsdn_t* tsdn, bool active_init) {
  bool active_init_old;

  malloc_mutex_lock(tsdn, &prof_thread_active_init_mtx);
  active_init_old = prof_thread_active_init;
  prof_thread_active_init = active_init;
  malloc_mutex_unlock(tsdn, &prof_thread_active_init_mtx);
  return active_init_old;
}

bool prof_gdump_get(tsdn_t* tsdn) {
  bool prof_gdump_current;

  malloc_mutex_lock(tsdn, &prof_gdump_mtx);
  prof_gdump_current = prof_gdump_val;
  malloc_mutex_unlock(tsdn, &prof_gdump_mtx);
  return prof_gdump_current;
}

bool prof_gdump_set(tsdn_t* tsdn, bool gdump) {
  bool prof_gdump_old;

  malloc_mutex_lock(tsdn, &prof_gdump_mtx);
  prof_gdump_old = prof_gdump_val;
  prof_gdump_val = gdump;
  malloc_mutex_unlock(tsdn, &prof_gdump_mtx);
  return prof_gdump_old;
}

void prof_boot0(void) {
  cassert(config_prof);

  jet_memcpy(
    opt_prof_prefix, PROF_PREFIX_DEFAULT, sizeof(PROF_PREFIX_DEFAULT));
}

void prof_boot1(void) {
  cassert(config_prof);

  /*
   * opt_prof must be in its final state before any arenas are
   * initialized, so this function must be executed early.
   */

  if (opt_prof_leak && !opt_prof) {
    /*
     * Enable opt_prof, but in such a way that profiles are never
     * automatically dumped.
     */
    opt_prof = true;
    opt_prof_gdump = false;
  } else if (opt_prof) {
    if (opt_lg_prof_interval >= 0) {
      prof_interval = (((uint64_t)1U) << opt_lg_prof_interval);
    }
  }
}

bool prof_boot2(tsd_t* tsd) {
  cassert(config_prof);

  if (opt_prof) {
    unsigned i;

    lg_prof_sample = opt_lg_prof_sample;

    prof_active = opt_prof_active;
    if (malloc_mutex_init(&prof_active_mtx, "prof_active",
          WITNESS_RANK_PROF_ACTIVE, malloc_mutex_rank_exclusive)) {
      return true;
    }

    prof_gdump_val = opt_prof_gdump;
    if (malloc_mutex_init(&prof_gdump_mtx, "prof_gdump",
          WITNESS_RANK_PROF_GDUMP, malloc_mutex_rank_exclusive)) {
      return true;
    }

    prof_thread_active_init = opt_prof_thread_active_init;
    if (malloc_mutex_init(&prof_thread_active_init_mtx,
          "prof_thread_active_init", WITNESS_RANK_PROF_THREAD_ACTIVE_INIT,
          malloc_mutex_rank_exclusive)) {
      return true;
    }

    if (ckh_new(tsd, &bt2gctx, PROF_CKH_MINITEMS, prof_bt_hash,
          prof_bt_keycomp)) {
      return true;
    }
    if (malloc_mutex_init(&bt2gctx_mtx, "prof_bt2gctx",
          WITNESS_RANK_PROF_BT2GCTX, malloc_mutex_rank_exclusive)) {
      return true;
    }

    tdata_tree_new(&tdatas);
    if (malloc_mutex_init(&tdatas_mtx, "prof_tdatas",
          WITNESS_RANK_PROF_TDATAS, malloc_mutex_rank_exclusive)) {
      return true;
    }

    next_thr_uid = 0;
    if (malloc_mutex_init(&next_thr_uid_mtx, "prof_next_thr_uid",
          WITNESS_RANK_PROF_NEXT_THR_UID, malloc_mutex_rank_exclusive)) {
      return true;
    }

    if (malloc_mutex_init(&prof_dump_seq_mtx, "prof_dump_seq",
          WITNESS_RANK_PROF_DUMP_SEQ, malloc_mutex_rank_exclusive)) {
      return true;
    }
    if (malloc_mutex_init(&prof_dump_mtx, "prof_dump",
          WITNESS_RANK_PROF_DUMP, malloc_mutex_rank_exclusive)) {
      return true;
    }

    if (opt_prof_final && opt_prof_prefix[0] != '\0'
      && atexit(prof_fdump) != 0) {
      malloc_write("<jemalloc>: Error in atexit()\n");
      if (opt_abort) { abort(); }
    }

    if (opt_prof_log) { prof_log_start(tsd_tsdn(tsd), NULL); }

    if (atexit(prof_log_stop_final) != 0) {
      malloc_write("<jemalloc>: Error in atexit() "
                   "for logging\n");
      if (opt_abort) { abort(); }
    }

    if (malloc_mutex_init(&log_mtx, "prof_log", WITNESS_RANK_PROF_LOG,
          malloc_mutex_rank_exclusive)) {
      return true;
    }

    if (ckh_new(tsd, &log_bt_node_set, PROF_CKH_MINITEMS, prof_bt_node_hash,
          prof_bt_node_keycomp)) {
      return true;
    }

    if (ckh_new(tsd, &log_thr_node_set, PROF_CKH_MINITEMS,
          prof_thr_node_hash, prof_thr_node_keycomp)) {
      return true;
    }

    log_tables_initialized = true;

    gctx_locks = (malloc_mutex_t*)base_alloc(tsd_tsdn(tsd), b0get(),
      PROF_NCTX_LOCKS * sizeof(malloc_mutex_t), CACHELINE);
    if (gctx_locks == NULL) { return true; }
    for (i = 0; i < PROF_NCTX_LOCKS; i++) {
      if (malloc_mutex_init(&gctx_locks[i], "prof_gctx",
            WITNESS_RANK_PROF_GCTX, malloc_mutex_rank_exclusive)) {
        return true;
      }
    }

    tdata_locks = (malloc_mutex_t*)base_alloc(tsd_tsdn(tsd), b0get(),
      PROF_NTDATA_LOCKS * sizeof(malloc_mutex_t), CACHELINE);
    if (tdata_locks == NULL) { return true; }
    for (i = 0; i < PROF_NTDATA_LOCKS; i++) {
      if (malloc_mutex_init(&tdata_locks[i], "prof_tdata",
            WITNESS_RANK_PROF_TDATA, malloc_mutex_rank_exclusive)) {
        return true;
      }
    }
#ifdef JEMALLOC_PROF_LIBGCC
    /*
     * Cause the backtracing machinery to allocate its internal
     * state before enabling profiling.
     */
    _Unwind_Backtrace(prof_unwind_init_callback, NULL);
#endif
  }
  prof_booted = true;

  return false;
}

void prof_prefork0(tsdn_t* tsdn) {
  if (config_prof && opt_prof) {
    unsigned i;

    malloc_mutex_prefork(tsdn, &prof_dump_mtx);
    malloc_mutex_prefork(tsdn, &bt2gctx_mtx);
    malloc_mutex_prefork(tsdn, &tdatas_mtx);
    for (i = 0; i < PROF_NTDATA_LOCKS; i++) {
      malloc_mutex_prefork(tsdn, &tdata_locks[i]);
    }
    for (i = 0; i < PROF_NCTX_LOCKS; i++) {
      malloc_mutex_prefork(tsdn, &gctx_locks[i]);
    }
  }
}

void prof_prefork1(tsdn_t* tsdn) {
  if (config_prof && opt_prof) {
    malloc_mutex_prefork(tsdn, &prof_active_mtx);
    malloc_mutex_prefork(tsdn, &prof_dump_seq_mtx);
    malloc_mutex_prefork(tsdn, &prof_gdump_mtx);
    malloc_mutex_prefork(tsdn, &next_thr_uid_mtx);
    malloc_mutex_prefork(tsdn, &prof_thread_active_init_mtx);
  }
}

void prof_postfork_parent(tsdn_t* tsdn) {
  if (config_prof && opt_prof) {
    unsigned i;

    malloc_mutex_postfork_parent(tsdn, &prof_thread_active_init_mtx);
    malloc_mutex_postfork_parent(tsdn, &next_thr_uid_mtx);
    malloc_mutex_postfork_parent(tsdn, &prof_gdump_mtx);
    malloc_mutex_postfork_parent(tsdn, &prof_dump_seq_mtx);
    malloc_mutex_postfork_parent(tsdn, &prof_active_mtx);
    for (i = 0; i < PROF_NCTX_LOCKS; i++) {
      malloc_mutex_postfork_parent(tsdn, &gctx_locks[i]);
    }
    for (i = 0; i < PROF_NTDATA_LOCKS; i++) {
      malloc_mutex_postfork_parent(tsdn, &tdata_locks[i]);
    }
    malloc_mutex_postfork_parent(tsdn, &tdatas_mtx);
    malloc_mutex_postfork_parent(tsdn, &bt2gctx_mtx);
    malloc_mutex_postfork_parent(tsdn, &prof_dump_mtx);
  }
}

void prof_postfork_child(tsdn_t* tsdn) {
  if (config_prof && opt_prof) {
    unsigned i;

    malloc_mutex_postfork_child(tsdn, &prof_thread_active_init_mtx);
    malloc_mutex_postfork_child(tsdn, &next_thr_uid_mtx);
    malloc_mutex_postfork_child(tsdn, &prof_gdump_mtx);
    malloc_mutex_postfork_child(tsdn, &prof_dump_seq_mtx);
    malloc_mutex_postfork_child(tsdn, &prof_active_mtx);
    for (i = 0; i < PROF_NCTX_LOCKS; i++) {
      malloc_mutex_postfork_child(tsdn, &gctx_locks[i]);
    }
    for (i = 0; i < PROF_NTDATA_LOCKS; i++) {
      malloc_mutex_postfork_child(tsdn, &tdata_locks[i]);
    }
    malloc_mutex_postfork_child(tsdn, &tdatas_mtx);
    malloc_mutex_postfork_child(tsdn, &bt2gctx_mtx);
    malloc_mutex_postfork_child(tsdn, &prof_dump_mtx);
  }
}

/******************************************************************************/
#define JEMALLOC_RTREE_C_

/*
 * Only the most significant bits of keys passed to rtree_{read,write}() are
 * used.
 */
bool rtree_new(rtree_t* rtree, bool zeroed) {
#ifdef JEMALLOC_JET
  if (!zeroed) { memset(rtree, 0, sizeof(rtree_t)); /* Clear root. */ }
#else
  assert(zeroed);
#endif

  if (malloc_mutex_init(&rtree->init_lock, "rtree", WITNESS_RANK_RTREE,
        malloc_mutex_rank_exclusive)) {
    return true;
  }

  return false;
}

static rtree_node_elm_t* rtree_node_alloc_impl(
  tsdn_t* tsdn, rtree_t* rtree, size_t nelms) {
  return (rtree_node_elm_t*)base_alloc(
    tsdn, b0get(), nelms * sizeof(rtree_node_elm_t), CACHELINE);
}
rtree_node_alloc_t* JET_MUTABLE rtree_node_alloc = rtree_node_alloc_impl;

static void rtree_node_dalloc_impl(
  tsdn_t* tsdn, rtree_t* rtree, rtree_node_elm_t* node) {
  /* Nodes are never deleted during normal operation. */
  not_reached();
}
static rtree_node_dalloc_t* JET_MUTABLE rtree_node_dalloc
  = rtree_node_dalloc_impl;

static rtree_leaf_elm_t* rtree_leaf_alloc_impl(
  tsdn_t* tsdn, rtree_t* rtree, size_t nelms) {
  return (rtree_leaf_elm_t*)base_alloc(
    tsdn, b0get(), nelms * sizeof(rtree_leaf_elm_t), CACHELINE);
}
rtree_leaf_alloc_t* JET_MUTABLE rtree_leaf_alloc = rtree_leaf_alloc_impl;

static void rtree_leaf_dalloc_impl(
  tsdn_t* tsdn, rtree_t* rtree, rtree_leaf_elm_t* leaf) {
  /* Leaves are never deleted during normal operation. */
  not_reached();
}
static rtree_leaf_dalloc_t* JET_MUTABLE rtree_leaf_dalloc
  = rtree_leaf_dalloc_impl;

#ifdef JEMALLOC_JET
#if RTREE_HEIGHT > 1
static void rtree_delete_subtree(
  tsdn_t* tsdn, rtree_t* rtree, rtree_node_elm_t* subtree, unsigned level) {
  size_t nchildren = ZU(1) << rtree_levels[level].bits;
  if (level + 2 < RTREE_HEIGHT) {
    for (size_t i = 0; i < nchildren; i++) {
      rtree_node_elm_t* node = (rtree_node_elm_t*)atomic_load_p(
        &subtree[i].child, ATOMIC_RELAXED);
      if (node != NULL) {
        rtree_delete_subtree(tsdn, rtree, node, level + 1);
      }
    }
  } else {
    for (size_t i = 0; i < nchildren; i++) {
      rtree_leaf_elm_t* leaf = (rtree_leaf_elm_t*)atomic_load_p(
        &subtree[i].child, ATOMIC_RELAXED);
      if (leaf != NULL) { rtree_leaf_dalloc(tsdn, rtree, leaf); }
    }
  }

  if (subtree != rtree->root) { rtree_node_dalloc(tsdn, rtree, subtree); }
}
#endif

void rtree_delete(tsdn_t* tsdn, rtree_t* rtree) {
#if RTREE_HEIGHT > 1
  rtree_delete_subtree(tsdn, rtree, rtree->root, 0);
#endif
}
#endif

static rtree_node_elm_t* rtree_node_init(
  tsdn_t* tsdn, rtree_t* rtree, unsigned level, atomic_p_t* elmp) {
  malloc_mutex_lock(tsdn, &rtree->init_lock);
  /*
   * If *elmp is non-null, then it was initialized with the init lock
   * held, so we can get by with 'relaxed' here.
   */
  rtree_node_elm_t* node = atomic_load_p(elmp, ATOMIC_RELAXED);
  if (node == NULL) {
    node = rtree_node_alloc(tsdn, rtree, ZU(1) << rtree_levels[level].bits);
    if (node == NULL) {
      malloc_mutex_unlock(tsdn, &rtree->init_lock);
      return NULL;
    }
    /*
     * Even though we hold the lock, a later reader might not; we
     * need release semantics.
     */
    atomic_store_p(elmp, node, ATOMIC_RELEASE);
  }
  malloc_mutex_unlock(tsdn, &rtree->init_lock);

  return node;
}

static rtree_leaf_elm_t* rtree_leaf_init(
  tsdn_t* tsdn, rtree_t* rtree, atomic_p_t* elmp) {
  malloc_mutex_lock(tsdn, &rtree->init_lock);
  /*
   * If *elmp is non-null, then it was initialized with the init lock
   * held, so we can get by with 'relaxed' here.
   */
  rtree_leaf_elm_t* leaf = atomic_load_p(elmp, ATOMIC_RELAXED);
  if (leaf == NULL) {
    leaf = rtree_leaf_alloc(
      tsdn, rtree, ZU(1) << rtree_levels[RTREE_HEIGHT - 1].bits);
    if (leaf == NULL) {
      malloc_mutex_unlock(tsdn, &rtree->init_lock);
      return NULL;
    }
    /*
     * Even though we hold the lock, a later reader might not; we
     * need release semantics.
     */
    atomic_store_p(elmp, leaf, ATOMIC_RELEASE);
  }
  malloc_mutex_unlock(tsdn, &rtree->init_lock);

  return leaf;
}

static bool rtree_node_valid(rtree_node_elm_t* node) {
  return ((uintptr_t)node != (uintptr_t)0);
}

static bool rtree_leaf_valid(rtree_leaf_elm_t* leaf) {
  return ((uintptr_t)leaf != (uintptr_t)0);
}

static rtree_node_elm_t* rtree_child_node_tryread(
  rtree_node_elm_t* elm, bool dependent) {
  rtree_node_elm_t* node;

  if (dependent) {
    node = (rtree_node_elm_t*)atomic_load_p(&elm->child, ATOMIC_RELAXED);
  } else {
    node = (rtree_node_elm_t*)atomic_load_p(&elm->child, ATOMIC_ACQUIRE);
  }

  assert(!dependent || node != NULL);
  return node;
}

static rtree_node_elm_t* rtree_child_node_read(tsdn_t* tsdn, rtree_t* rtree,
  rtree_node_elm_t* elm, unsigned level, bool dependent) {
  rtree_node_elm_t* node;

  node = rtree_child_node_tryread(elm, dependent);
  if (!dependent && unlikely(!rtree_node_valid(node))) {
    node = rtree_node_init(tsdn, rtree, level + 1, &elm->child);
  }
  assert(!dependent || node != NULL);
  return node;
}

static rtree_leaf_elm_t* rtree_child_leaf_tryread(
  rtree_node_elm_t* elm, bool dependent) {
  rtree_leaf_elm_t* leaf;

  if (dependent) {
    leaf = (rtree_leaf_elm_t*)atomic_load_p(&elm->child, ATOMIC_RELAXED);
  } else {
    leaf = (rtree_leaf_elm_t*)atomic_load_p(&elm->child, ATOMIC_ACQUIRE);
  }

  assert(!dependent || leaf != NULL);
  return leaf;
}

static rtree_leaf_elm_t* rtree_child_leaf_read(tsdn_t* tsdn, rtree_t* rtree,
  rtree_node_elm_t* elm, unsigned level, bool dependent) {
  rtree_leaf_elm_t* leaf;

  leaf = rtree_child_leaf_tryread(elm, dependent);
  if (!dependent && unlikely(!rtree_leaf_valid(leaf))) {
    leaf = rtree_leaf_init(tsdn, rtree, &elm->child);
  }
  assert(!dependent || leaf != NULL);
  return leaf;
}

rtree_leaf_elm_t* rtree_leaf_elm_lookup_hard(tsdn_t* tsdn, rtree_t* rtree,
  rtree_ctx_t* rtree_ctx, uintptr_t key, bool dependent,
  bool init_missing) {
  rtree_node_elm_t* node;
  rtree_leaf_elm_t* leaf;
#if RTREE_HEIGHT > 1
  node = rtree->root;
#else
  leaf = rtree->root;
#endif

  if (config_debug) {
    uintptr_t leafkey = rtree_leafkey(key);
    for (unsigned i = 0; i < RTREE_CTX_NCACHE; i++) {
      assert(rtree_ctx->cache[i].leafkey != leafkey);
    }
    for (unsigned i = 0; i < RTREE_CTX_NCACHE_L2; i++) {
      assert(rtree_ctx->l2_cache[i].leafkey != leafkey);
    }
  }

#define RTREE_GET_CHILD(level)                                             \
  {                                                                        \
    assert(level < RTREE_HEIGHT - 1);                                      \
    if (level != 0 && !dependent && unlikely(!rtree_node_valid(node))) {   \
      return NULL;                                                         \
    }                                                                      \
    uintptr_t subkey = rtree_subkey(key, level);                           \
    if (level + 2 < RTREE_HEIGHT) {                                        \
      node = init_missing                                                  \
        ? rtree_child_node_read(                                           \
          tsdn, rtree, &node[subkey], level, dependent)                    \
        : rtree_child_node_tryread(&node[subkey], dependent);              \
    } else {                                                               \
      leaf = init_missing                                                  \
        ? rtree_child_leaf_read(                                           \
          tsdn, rtree, &node[subkey], level, dependent)                    \
        : rtree_child_leaf_tryread(&node[subkey], dependent);              \
    }                                                                      \
  }
  /*
   * Cache replacement upon hard lookup (i.e. L1 & L2 rtree cache miss):
   * (1) evict last entry in L2 cache; (2) move the collision slot from L1
   * cache down to L2; and 3) fill L1.
   */
#define RTREE_GET_LEAF(level)                                              \
  {                                                                        \
    assert(level == RTREE_HEIGHT - 1);                                     \
    if (!dependent && unlikely(!rtree_leaf_valid(leaf))) { return NULL; }  \
    if (RTREE_CTX_NCACHE_L2 > 1) {                                         \
      memmove(&rtree_ctx->l2_cache[1], &rtree_ctx->l2_cache[0],            \
        sizeof(rtree_ctx_cache_elm_t) * (RTREE_CTX_NCACHE_L2 - 1));        \
    }                                                                      \
    size_t slot = rtree_cache_direct_map(key);                             \
    rtree_ctx->l2_cache[0].leafkey = rtree_ctx->cache[slot].leafkey;       \
    rtree_ctx->l2_cache[0].leaf = rtree_ctx->cache[slot].leaf;             \
    uintptr_t leafkey = rtree_leafkey(key);                                \
    rtree_ctx->cache[slot].leafkey = leafkey;                              \
    rtree_ctx->cache[slot].leaf = leaf;                                    \
    uintptr_t subkey = rtree_subkey(key, level);                           \
    return &leaf[subkey];                                                  \
  }
  if (RTREE_HEIGHT > 1) { RTREE_GET_CHILD(0) }
  if (RTREE_HEIGHT > 2) { RTREE_GET_CHILD(1) }
  if (RTREE_HEIGHT > 3) {
    for (unsigned i = 2; i < RTREE_HEIGHT - 1; i++) { RTREE_GET_CHILD(i) }
  }
  RTREE_GET_LEAF(RTREE_HEIGHT - 1)
#undef RTREE_GET_CHILD
#undef RTREE_GET_LEAF
  not_reached();
}

void rtree_ctx_data_init(rtree_ctx_t* ctx) {
  for (unsigned i = 0; i < RTREE_CTX_NCACHE; i++) {
    rtree_ctx_cache_elm_t* cache = &ctx->cache[i];
    cache->leafkey = RTREE_LEAFKEY_INVALID;
    cache->leaf = NULL;
  }
  for (unsigned i = 0; i < RTREE_CTX_NCACHE_L2; i++) {
    rtree_ctx_cache_elm_t* cache = &ctx->l2_cache[i];
    cache->leafkey = RTREE_LEAFKEY_INVALID;
    cache->leaf = NULL;
  }
}

static void (*safety_check_abort)(const char* message);

void safety_check_set_abort(void (*abort_fn)(const char*)) {
  safety_check_abort = abort_fn;
}

void safety_check_fail(const char* format, ...) {
  char buf[MALLOC_PRINTF_BUFSIZE];

  va_list ap;
  va_start(ap, format);
  malloc_vsnprintf(buf, MALLOC_PRINTF_BUFSIZE, format, ap);
  va_end(ap);

  if (safety_check_abort == NULL) {
    malloc_write(buf);
    abort();
  } else {
    safety_check_abort(buf);
  }
}
#define JEMALLOC_STATS_C_

#include "jemalloc/internal/ctl.h"
#include "jemalloc/internal/emitter.h"

#include "jemalloc/internal/mutex_prof.h"

const char* global_mutex_names[mutex_prof_num_global_mutexes] = {
#define OP(mtx) #mtx,
  MUTEX_PROF_GLOBAL_MUTEXES
#undef OP
};

const char* arena_mutex_names[mutex_prof_num_arena_mutexes] = {
#define OP(mtx) #mtx,
  MUTEX_PROF_ARENA_MUTEXES
#undef OP
};

#define CTL_GET(n, v, t)                                                   \
  do {                                                                     \
    size_t sz = sizeof(t);                                                 \
    xmallctl(n, (void*)v, &sz, NULL, 0);                                   \
  } while (0)

#define CTL_M2_GET(n, i, v, t)                                             \
  do {                                                                     \
    size_t mib[CTL_MAX_DEPTH];                                             \
    size_t miblen = sizeof(mib) / sizeof(size_t);                          \
    size_t sz = sizeof(t);                                                 \
    xmallctlnametomib(n, mib, &miblen);                                    \
    mib[2] = (i);                                                          \
    xmallctlbymib(mib, miblen, (void*)v, &sz, NULL, 0);                    \
  } while (0)

#define CTL_M2_M4_GET(n, i, j, v, t)                                       \
  do {                                                                     \
    size_t mib[CTL_MAX_DEPTH];                                             \
    size_t miblen = sizeof(mib) / sizeof(size_t);                          \
    size_t sz = sizeof(t);                                                 \
    xmallctlnametomib(n, mib, &miblen);                                    \
    mib[2] = (i);                                                          \
    mib[4] = (j);                                                          \
    xmallctlbymib(mib, miblen, (void*)v, &sz, NULL, 0);                    \
  } while (0)

/******************************************************************************/
/* Data. */

bool opt_stats_print = false;
char opt_stats_print_opts[stats_print_tot_num_options + 1] = "";

/******************************************************************************/

static uint64_t rate_per_second(uint64_t value, uint64_t uptime_ns) {
  uint64_t billion = 1000000000;
  if (uptime_ns == 0 || value == 0) { return 0; }
  if (uptime_ns < billion) {
    return value;
  } else {
    uint64_t uptime_s = uptime_ns / billion;
    return value / uptime_s;
  }
}

/* Calculate x.yyy and output a string (takes a fixed sized char array). */
static bool get_rate_str(uint64_t dividend, uint64_t divisor, char str[6]) {
  if (divisor == 0 || dividend > divisor) {
    /* The rate is not supposed to be greater than 1. */
    return true;
  }
  if (dividend > 0) { assert(UINT64_MAX / dividend >= 1000); }

  unsigned n = (unsigned)((dividend * 1000) / divisor);
  if (n < 10) {
    malloc_snprintf(str, 6, "0.00%u", n);
  } else if (n < 100) {
    malloc_snprintf(str, 6, "0.0%u", n);
  } else if (n < 1000) {
    malloc_snprintf(str, 6, "0.%u", n);
  } else {
    malloc_snprintf(str, 6, "1");
  }

  return false;
}

#define MUTEX_CTL_STR_MAX_LENGTH 128
static void gen_mutex_ctl_str(char* str, size_t buf_len, const char* prefix,
  const char* mutex, const char* counter) {
  malloc_snprintf(str, buf_len, "stats.%s.%s.%s", prefix, mutex, counter);
}

static void mutex_stats_init_cols(emitter_row_t* row,
  const char* table_name, emitter_col_t* name,
  emitter_col_t col_uint64_t[mutex_prof_num_uint64_t_counters],
  emitter_col_t col_uint32_t[mutex_prof_num_uint32_t_counters]) {
  mutex_prof_uint64_t_counter_ind_t k_uint64_t = 0;
  mutex_prof_uint32_t_counter_ind_t k_uint32_t = 0;

  emitter_col_t* col;

  if (name != NULL) {
    emitter_col_init(name, row);
    name->justify = emitter_justify_left;
    name->width = 21;
    name->type = emitter_type_title;
    name->str_val = table_name;
  }

#define WIDTH_uint32_t 12
#define WIDTH_uint64_t 16
#define OP(counter, counter_type, human, derived, base_counter)            \
  col = &col_##counter_type[k_##counter_type];                             \
  ++k_##counter_type;                                                      \
  emitter_col_init(col, row);                                              \
  col->justify = emitter_justify_right;                                    \
  col->width = derived ? 8 : WIDTH_##counter_type;                         \
  col->type = emitter_type_title;                                          \
  col->str_val = human;
  MUTEX_PROF_COUNTERS
#undef OP
#undef WIDTH_uint32_t
#undef WIDTH_uint64_t
  col_uint64_t[mutex_counter_total_wait_time_ps].width = 10;
}

static void mutex_stats_read_global(const char* name,
  emitter_col_t* col_name,
  emitter_col_t col_uint64_t[mutex_prof_num_uint64_t_counters],
  emitter_col_t col_uint32_t[mutex_prof_num_uint32_t_counters],
  uint64_t uptime) {
  char cmd[MUTEX_CTL_STR_MAX_LENGTH];

  col_name->str_val = name;

  emitter_col_t* dst;
#define EMITTER_TYPE_uint32_t emitter_type_uint32
#define EMITTER_TYPE_uint64_t emitter_type_uint64
#define OP(counter, counter_type, human, derived, base_counter)            \
  dst = &col_##counter_type[mutex_counter_##counter];                      \
  dst->type = EMITTER_TYPE_##counter_type;                                 \
  if (!derived) {                                                          \
    gen_mutex_ctl_str(                                                     \
      cmd, MUTEX_CTL_STR_MAX_LENGTH, "mutexes", name, #counter);           \
    CTL_GET(cmd, (counter_type*)&dst->bool_val, counter_type);             \
  } else {                                                                 \
    emitter_col_t* base                                                    \
      = &col_##counter_type[mutex_counter_##base_counter];                 \
    dst->counter_type##_val                                                \
      = rate_per_second(base->counter_type##_val, uptime);                 \
  }
  MUTEX_PROF_COUNTERS
#undef OP
#undef EMITTER_TYPE_uint32_t
#undef EMITTER_TYPE_uint64_t
}

static void mutex_stats_read_arena(unsigned arena_ind,
  mutex_prof_arena_ind_t mutex_ind, const char* name,
  emitter_col_t* col_name,
  emitter_col_t col_uint64_t[mutex_prof_num_uint64_t_counters],
  emitter_col_t col_uint32_t[mutex_prof_num_uint32_t_counters],
  uint64_t uptime) {
  char cmd[MUTEX_CTL_STR_MAX_LENGTH];

  col_name->str_val = name;

  emitter_col_t* dst;
#define EMITTER_TYPE_uint32_t emitter_type_uint32
#define EMITTER_TYPE_uint64_t emitter_type_uint64
#define OP(counter, counter_type, human, derived, base_counter)            \
  dst = &col_##counter_type[mutex_counter_##counter];                      \
  dst->type = EMITTER_TYPE_##counter_type;                                 \
  if (!derived) {                                                          \
    gen_mutex_ctl_str(cmd, MUTEX_CTL_STR_MAX_LENGTH, "arenas.0.mutexes",   \
      arena_mutex_names[mutex_ind], #counter);                             \
    CTL_M2_GET(                                                            \
      cmd, arena_ind, (counter_type*)&dst->bool_val, counter_type);        \
  } else {                                                                 \
    emitter_col_t* base                                                    \
      = &col_##counter_type[mutex_counter_##base_counter];                 \
    dst->counter_type##_val                                                \
      = rate_per_second(base->counter_type##_val, uptime);                 \
  }
  MUTEX_PROF_COUNTERS
#undef OP
#undef EMITTER_TYPE_uint32_t
#undef EMITTER_TYPE_uint64_t
}

static void mutex_stats_read_arena_bin(unsigned arena_ind, unsigned bin_ind,
  emitter_col_t col_uint64_t[mutex_prof_num_uint64_t_counters],
  emitter_col_t col_uint32_t[mutex_prof_num_uint32_t_counters],
  uint64_t uptime) {
  char cmd[MUTEX_CTL_STR_MAX_LENGTH];
  emitter_col_t* dst;

#define EMITTER_TYPE_uint32_t emitter_type_uint32
#define EMITTER_TYPE_uint64_t emitter_type_uint64
#define OP(counter, counter_type, human, derived, base_counter)            \
  dst = &col_##counter_type[mutex_counter_##counter];                      \
  dst->type = EMITTER_TYPE_##counter_type;                                 \
  if (!derived) {                                                          \
    gen_mutex_ctl_str(cmd, MUTEX_CTL_STR_MAX_LENGTH, "arenas.0.bins.0",    \
      "mutex", #counter);                                                  \
    CTL_M2_M4_GET(cmd, arena_ind, bin_ind, (counter_type*)&dst->bool_val,  \
      counter_type);                                                       \
  } else {                                                                 \
    emitter_col_t* base                                                    \
      = &col_##counter_type[mutex_counter_##base_counter];                 \
    dst->counter_type##_val                                                \
      = rate_per_second(base->counter_type##_val, uptime);                 \
  }
  MUTEX_PROF_COUNTERS
#undef OP
#undef EMITTER_TYPE_uint32_t
#undef EMITTER_TYPE_uint64_t
}

/* "row" can be NULL to avoid emitting in table mode. */
static void mutex_stats_emit(emitter_t* emitter, emitter_row_t* row,
  emitter_col_t col_uint64_t[mutex_prof_num_uint64_t_counters],
  emitter_col_t col_uint32_t[mutex_prof_num_uint32_t_counters]) {
  if (row != NULL) { emitter_table_row(emitter, row); }

  mutex_prof_uint64_t_counter_ind_t k_uint64_t = 0;
  mutex_prof_uint32_t_counter_ind_t k_uint32_t = 0;

  emitter_col_t* col;

#define EMITTER_TYPE_uint32_t emitter_type_uint32
#define EMITTER_TYPE_uint64_t emitter_type_uint64
#define OP(counter, type, human, derived, base_counter)                    \
  if (!derived) {                                                          \
    col = &col_##type[k_##type];                                           \
    ++k_##type;                                                            \
    emitter_json_kv(emitter, #counter, EMITTER_TYPE_##type,                \
      (const void*)&col->bool_val);                                        \
  }
  MUTEX_PROF_COUNTERS;
#undef OP
#undef EMITTER_TYPE_uint32_t
#undef EMITTER_TYPE_uint64_t
}

#define COL(row_name, column_name, left_or_right, col_width, etype)        \
  emitter_col_t col_##column_name;                                         \
  emitter_col_init(&col_##column_name, &row_name);                         \
  col_##column_name.justify = emitter_justify_##left_or_right;             \
  col_##column_name.width = col_width;                                     \
  col_##column_name.type = emitter_type_##etype;

#define COL_HDR(                                                           \
  row_name, column_name, human, left_or_right, col_width, etype)           \
  COL(row_name, column_name, left_or_right, col_width, etype)              \
  emitter_col_t header_##column_name;                                      \
  emitter_col_init(&header_##column_name, &header_##row_name);             \
  header_##column_name.justify = emitter_justify_##left_or_right;          \
  header_##column_name.width = col_width;                                  \
  header_##column_name.type = emitter_type_title;                          \
  header_##column_name.str_val = human ? human : #column_name;

static void stats_arena_bins_print(
  emitter_t* emitter, bool mutex, unsigned i, uint64_t uptime) {
  size_t page;
  bool in_gap, in_gap_prev;
  unsigned nbins, j;

  CTL_GET("arenas.page", &page, size_t);

  CTL_GET("arenas.nbins", &nbins, unsigned);

  emitter_row_t header_row;
  emitter_row_init(&header_row);

  emitter_row_t row;
  emitter_row_init(&row);

  COL_HDR(row, size, NULL, right, 20, size)
  COL_HDR(row, ind, NULL, right, 4, unsigned)
  COL_HDR(row, allocated, NULL, right, 13, uint64)
  COL_HDR(row, nmalloc, NULL, right, 13, uint64)
  COL_HDR(row, nmalloc_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, ndalloc, NULL, right, 13, uint64)
  COL_HDR(row, ndalloc_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, nrequests, NULL, right, 13, uint64)
  COL_HDR(row, nrequests_ps, "(#/sec)", right, 10, uint64)
  COL_HDR(row, nshards, NULL, right, 9, unsigned)
  COL_HDR(row, curregs, NULL, right, 13, size)
  COL_HDR(row, curslabs, NULL, right, 13, size)
  COL_HDR(row, nonfull_slabs, NULL, right, 15, size)
  COL_HDR(row, regs, NULL, right, 5, unsigned)
  COL_HDR(row, pgs, NULL, right, 4, size)
  /* To buffer a right- and left-justified column. */
  COL_HDR(row, justify_spacer, NULL, right, 1, title)
  COL_HDR(row, util, NULL, right, 6, title)
  COL_HDR(row, nfills, NULL, right, 13, uint64)
  COL_HDR(row, nfills_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, nflushes, NULL, right, 13, uint64)
  COL_HDR(row, nflushes_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, nslabs, NULL, right, 13, uint64)
  COL_HDR(row, nreslabs, NULL, right, 13, uint64)
  COL_HDR(row, nreslabs_ps, "(#/sec)", right, 8, uint64)

  /* Don't want to actually print the name. */
  header_justify_spacer.str_val = " ";
  col_justify_spacer.str_val = " ";

  emitter_col_t col_mutex64[mutex_prof_num_uint64_t_counters];
  emitter_col_t col_mutex32[mutex_prof_num_uint32_t_counters];

  emitter_col_t header_mutex64[mutex_prof_num_uint64_t_counters];
  emitter_col_t header_mutex32[mutex_prof_num_uint32_t_counters];

  if (mutex) {
    mutex_stats_init_cols(&row, NULL, NULL, col_mutex64, col_mutex32);
    mutex_stats_init_cols(
      &header_row, NULL, NULL, header_mutex64, header_mutex32);
  }

  /*
   * We print a "bins:" header as part of the table row; we need to adjust
   * the header size column to compensate.
   */
  header_size.width -= 5;
  emitter_table_printf(emitter, "bins:");
  emitter_table_row(emitter, &header_row);
  emitter_json_array_kv_begin(emitter, "bins");

  for (j = 0, in_gap = false; j < nbins; j++) {
    uint64_t nslabs;
    size_t reg_size, slab_size, curregs;
    size_t curslabs;
    size_t nonfull_slabs;
    uint32_t nregs, nshards;
    uint64_t nmalloc, ndalloc, nrequests, nfills, nflushes;
    uint64_t nreslabs;

    CTL_M2_M4_GET("stats.arenas.0.bins.0.nslabs", i, j, &nslabs, uint64_t);
    in_gap_prev = in_gap;
    in_gap = (nslabs == 0);

    if (in_gap_prev && !in_gap) {
      emitter_table_printf(emitter, "                     ---\n");
    }

    CTL_M2_GET("arenas.bin.0.size", j, &reg_size, size_t);
    CTL_M2_GET("arenas.bin.0.nregs", j, &nregs, uint32_t);
    CTL_M2_GET("arenas.bin.0.slab_size", j, &slab_size, size_t);
    CTL_M2_GET("arenas.bin.0.nshards", j, &nshards, uint32_t);

    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.nmalloc", i, j, &nmalloc, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.ndalloc", i, j, &ndalloc, uint64_t);
    CTL_M2_M4_GET("stats.arenas.0.bins.0.curregs", i, j, &curregs, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.nrequests", i, j, &nrequests, uint64_t);
    CTL_M2_M4_GET("stats.arenas.0.bins.0.nfills", i, j, &nfills, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.nflushes", i, j, &nflushes, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.nreslabs", i, j, &nreslabs, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.curslabs", i, j, &curslabs, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.bins.0.nonfull_slabs", i, j, &nonfull_slabs, size_t);

    if (mutex) {
      mutex_stats_read_arena_bin(i, j, col_mutex64, col_mutex32, uptime);
    }

    emitter_json_object_begin(emitter);
    emitter_json_kv(emitter, "nmalloc", emitter_type_uint64, &nmalloc);
    emitter_json_kv(emitter, "ndalloc", emitter_type_uint64, &ndalloc);
    emitter_json_kv(emitter, "curregs", emitter_type_size, &curregs);
    emitter_json_kv(emitter, "nrequests", emitter_type_uint64, &nrequests);
    emitter_json_kv(emitter, "nfills", emitter_type_uint64, &nfills);
    emitter_json_kv(emitter, "nflushes", emitter_type_uint64, &nflushes);
    emitter_json_kv(emitter, "nreslabs", emitter_type_uint64, &nreslabs);
    emitter_json_kv(emitter, "curslabs", emitter_type_size, &curslabs);
    emitter_json_kv(
      emitter, "nonfull_slabs", emitter_type_size, &nonfull_slabs);
    if (mutex) {
      emitter_json_object_kv_begin(emitter, "mutex");
      mutex_stats_emit(emitter, NULL, col_mutex64, col_mutex32);
      emitter_json_object_end(emitter);
    }
    emitter_json_object_end(emitter);

    size_t availregs = nregs * curslabs;
    char util[6];
    if (get_rate_str((uint64_t)curregs, (uint64_t)availregs, util)) {
      if (availregs == 0) {
        malloc_snprintf(util, sizeof(util), "1");
      } else if (curregs > availregs) {
        /*
         * Race detected: the counters were read in
         * separate mallctl calls and concurrent
         * operations happened in between.  In this case
         * no meaningful utilization can be computed.
         */
        malloc_snprintf(util, sizeof(util), " race");
      } else {
        not_reached();
      }
    }

    col_size.size_val = reg_size;
    col_ind.unsigned_val = j;
    col_allocated.size_val = curregs * reg_size;
    col_nmalloc.uint64_val = nmalloc;
    col_nmalloc_ps.uint64_val = rate_per_second(nmalloc, uptime);
    col_ndalloc.uint64_val = ndalloc;
    col_ndalloc_ps.uint64_val = rate_per_second(ndalloc, uptime);
    col_nrequests.uint64_val = nrequests;
    col_nrequests_ps.uint64_val = rate_per_second(nrequests, uptime);
    col_nshards.unsigned_val = nshards;
    col_curregs.size_val = curregs;
    col_curslabs.size_val = curslabs;
    col_nonfull_slabs.size_val = nonfull_slabs;
    col_regs.unsigned_val = nregs;
    col_pgs.size_val = slab_size / page;
    col_util.str_val = util;
    col_nfills.uint64_val = nfills;
    col_nfills_ps.uint64_val = rate_per_second(nfills, uptime);
    col_nflushes.uint64_val = nflushes;
    col_nflushes_ps.uint64_val = rate_per_second(nflushes, uptime);
    col_nslabs.uint64_val = nslabs;
    col_nreslabs.uint64_val = nreslabs;
    col_nreslabs_ps.uint64_val = rate_per_second(nreslabs, uptime);

    /*
     * Note that mutex columns were initialized above, if mutex ==
     * true.
     */

    emitter_table_row(emitter, &row);
  }
  emitter_json_array_end(emitter); /* Close "bins". */

  if (in_gap) {
    emitter_table_printf(emitter, "                     ---\n");
  }
}

static void stats_arena_lextents_print(
  emitter_t* emitter, unsigned i, uint64_t uptime) {
  unsigned nbins, nlextents, j;
  bool in_gap, in_gap_prev;

  CTL_GET("arenas.nbins", &nbins, unsigned);
  CTL_GET("arenas.nlextents", &nlextents, unsigned);

  emitter_row_t header_row;
  emitter_row_init(&header_row);
  emitter_row_t row;
  emitter_row_init(&row);

  COL_HDR(row, size, NULL, right, 20, size)
  COL_HDR(row, ind, NULL, right, 4, unsigned)
  COL_HDR(row, allocated, NULL, right, 13, size)
  COL_HDR(row, nmalloc, NULL, right, 13, uint64)
  COL_HDR(row, nmalloc_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, ndalloc, NULL, right, 13, uint64)
  COL_HDR(row, ndalloc_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, nrequests, NULL, right, 13, uint64)
  COL_HDR(row, nrequests_ps, "(#/sec)", right, 8, uint64)
  COL_HDR(row, curlextents, NULL, right, 13, size)

  /* As with bins, we label the large extents table. */
  header_size.width -= 6;
  emitter_table_printf(emitter, "large:");
  emitter_table_row(emitter, &header_row);
  emitter_json_array_kv_begin(emitter, "lextents");

  for (j = 0, in_gap = false; j < nlextents; j++) {
    uint64_t nmalloc, ndalloc, nrequests;
    size_t lextent_size, curlextents;

    CTL_M2_M4_GET(
      "stats.arenas.0.lextents.0.nmalloc", i, j, &nmalloc, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.lextents.0.ndalloc", i, j, &ndalloc, uint64_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.lextents.0.nrequests", i, j, &nrequests, uint64_t);
    in_gap_prev = in_gap;
    in_gap = (nrequests == 0);

    if (in_gap_prev && !in_gap) {
      emitter_table_printf(emitter, "                     ---\n");
    }

    CTL_M2_GET("arenas.lextent.0.size", j, &lextent_size, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.lextents.0.curlextents", i, j, &curlextents, size_t);

    emitter_json_object_begin(emitter);
    emitter_json_kv(
      emitter, "curlextents", emitter_type_size, &curlextents);
    emitter_json_object_end(emitter);

    col_size.size_val = lextent_size;
    col_ind.unsigned_val = nbins + j;
    col_allocated.size_val = curlextents * lextent_size;
    col_nmalloc.uint64_val = nmalloc;
    col_nmalloc_ps.uint64_val = rate_per_second(nmalloc, uptime);
    col_ndalloc.uint64_val = ndalloc;
    col_ndalloc_ps.uint64_val = rate_per_second(ndalloc, uptime);
    col_nrequests.uint64_val = nrequests;
    col_nrequests_ps.uint64_val = rate_per_second(nrequests, uptime);
    col_curlextents.size_val = curlextents;

    if (!in_gap) { emitter_table_row(emitter, &row); }
  }
  emitter_json_array_end(emitter); /* Close "lextents". */
  if (in_gap) {
    emitter_table_printf(emitter, "                     ---\n");
  }
}

static void stats_arena_extents_print(emitter_t* emitter, unsigned i) {
  unsigned j;
  bool in_gap, in_gap_prev;
  emitter_row_t header_row;
  emitter_row_init(&header_row);
  emitter_row_t row;
  emitter_row_init(&row);

  COL_HDR(row, size, NULL, right, 20, size)
  COL_HDR(row, ind, NULL, right, 4, unsigned)
  COL_HDR(row, ndirty, NULL, right, 13, size)
  COL_HDR(row, dirty, NULL, right, 13, size)
  COL_HDR(row, nmuzzy, NULL, right, 13, size)
  COL_HDR(row, muzzy, NULL, right, 13, size)
  COL_HDR(row, nretained, NULL, right, 13, size)
  COL_HDR(row, retained, NULL, right, 13, size)
  COL_HDR(row, ntotal, NULL, right, 13, size)
  COL_HDR(row, total, NULL, right, 13, size)

  /* Label this section. */
  header_size.width -= 8;
  emitter_table_printf(emitter, "extents:");
  emitter_table_row(emitter, &header_row);
  emitter_json_array_kv_begin(emitter, "extents");

  in_gap = false;
  for (j = 0; j < SC_NPSIZES; j++) {
    size_t ndirty, nmuzzy, nretained, total, dirty_bytes, muzzy_bytes,
      retained_bytes, total_bytes;
    CTL_M2_M4_GET("stats.arenas.0.extents.0.ndirty", i, j, &ndirty, size_t);
    CTL_M2_M4_GET("stats.arenas.0.extents.0.nmuzzy", i, j, &nmuzzy, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.extents.0.nretained", i, j, &nretained, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.extents.0.dirty_bytes", i, j, &dirty_bytes, size_t);
    CTL_M2_M4_GET(
      "stats.arenas.0.extents.0.muzzy_bytes", i, j, &muzzy_bytes, size_t);
    CTL_M2_M4_GET("stats.arenas.0.extents.0.retained_bytes", i, j,
      &retained_bytes, size_t);
    total = ndirty + nmuzzy + nretained;
    total_bytes = dirty_bytes + muzzy_bytes + retained_bytes;

    in_gap_prev = in_gap;
    in_gap = (total == 0);

    if (in_gap_prev && !in_gap) {
      emitter_table_printf(emitter, "                     ---\n");
    }

    emitter_json_object_begin(emitter);
    emitter_json_kv(emitter, "ndirty", emitter_type_size, &ndirty);
    emitter_json_kv(emitter, "nmuzzy", emitter_type_size, &nmuzzy);
    emitter_json_kv(emitter, "nretained", emitter_type_size, &nretained);

    emitter_json_kv(
      emitter, "dirty_bytes", emitter_type_size, &dirty_bytes);
    emitter_json_kv(
      emitter, "muzzy_bytes", emitter_type_size, &muzzy_bytes);
    emitter_json_kv(
      emitter, "retained_bytes", emitter_type_size, &retained_bytes);
    emitter_json_object_end(emitter);

    col_size.size_val = sz_pind2sz(j);
    col_ind.size_val = j;
    col_ndirty.size_val = ndirty;
    col_dirty.size_val = dirty_bytes;
    col_nmuzzy.size_val = nmuzzy;
    col_muzzy.size_val = muzzy_bytes;
    col_nretained.size_val = nretained;
    col_retained.size_val = retained_bytes;
    col_ntotal.size_val = total;
    col_total.size_val = total_bytes;

    if (!in_gap) { emitter_table_row(emitter, &row); }
  }
  emitter_json_array_end(emitter); /* Close "extents". */
  if (in_gap) {
    emitter_table_printf(emitter, "                     ---\n");
  }
}

static void stats_arena_mutexes_print(
  emitter_t* emitter, unsigned arena_ind, uint64_t uptime) {
  emitter_row_t row;
  emitter_col_t col_name;
  emitter_col_t col64[mutex_prof_num_uint64_t_counters];
  emitter_col_t col32[mutex_prof_num_uint32_t_counters];

  emitter_row_init(&row);
  mutex_stats_init_cols(&row, "", &col_name, col64, col32);

  emitter_json_object_kv_begin(emitter, "mutexes");
  emitter_table_row(emitter, &row);

  for (mutex_prof_arena_ind_t i = 0; i < mutex_prof_num_arena_mutexes;
       i++) {
    const char* name = arena_mutex_names[i];
    emitter_json_object_kv_begin(emitter, name);
    mutex_stats_read_arena(
      arena_ind, i, name, &col_name, col64, col32, uptime);
    mutex_stats_emit(emitter, &row, col64, col32);
    emitter_json_object_end(emitter); /* Close the mutex dict. */
  }
  emitter_json_object_end(emitter); /* End "mutexes". */
}

static void stats_arena_print(emitter_t* emitter, unsigned i, bool bins,
  bool large, bool mutex, bool extents) {
  unsigned nthreads;
  const char* dss;
  ssize_t dirty_decay_ms, muzzy_decay_ms;
  size_t page, pactive, pdirty, pmuzzy, mapped, retained;
  size_t base, internal, resident, metadata_thp, extent_avail;
  uint64_t dirty_npurge, dirty_nmadvise, dirty_purged;
  uint64_t muzzy_npurge, muzzy_nmadvise, muzzy_purged;
  size_t small_allocated;
  uint64_t small_nmalloc, small_ndalloc, small_nrequests, small_nfills,
    small_nflushes;
  size_t large_allocated;
  uint64_t large_nmalloc, large_ndalloc, large_nrequests, large_nfills,
    large_nflushes;
  size_t tcache_bytes, abandoned_vm;
  uint64_t uptime;

  CTL_GET("arenas.page", &page, size_t);

  CTL_M2_GET("stats.arenas.0.nthreads", i, &nthreads, unsigned);
  emitter_kv(emitter, "nthreads", "assigned threads", emitter_type_unsigned,
    &nthreads);

  CTL_M2_GET("stats.arenas.0.uptime", i, &uptime, uint64_t);
  emitter_kv(emitter, "uptime_ns", "uptime", emitter_type_uint64, &uptime);

  CTL_M2_GET("stats.arenas.0.dss", i, &dss, const char*);
  emitter_kv(
    emitter, "dss", "dss allocation precedence", emitter_type_string, &dss);

  CTL_M2_GET("stats.arenas.0.dirty_decay_ms", i, &dirty_decay_ms, ssize_t);
  CTL_M2_GET("stats.arenas.0.muzzy_decay_ms", i, &muzzy_decay_ms, ssize_t);
  CTL_M2_GET("stats.arenas.0.pactive", i, &pactive, size_t);
  CTL_M2_GET("stats.arenas.0.pdirty", i, &pdirty, size_t);
  CTL_M2_GET("stats.arenas.0.pmuzzy", i, &pmuzzy, size_t);
  CTL_M2_GET("stats.arenas.0.dirty_npurge", i, &dirty_npurge, uint64_t);
  CTL_M2_GET("stats.arenas.0.dirty_nmadvise", i, &dirty_nmadvise, uint64_t);
  CTL_M2_GET("stats.arenas.0.dirty_purged", i, &dirty_purged, uint64_t);
  CTL_M2_GET("stats.arenas.0.muzzy_npurge", i, &muzzy_npurge, uint64_t);
  CTL_M2_GET("stats.arenas.0.muzzy_nmadvise", i, &muzzy_nmadvise, uint64_t);
  CTL_M2_GET("stats.arenas.0.muzzy_purged", i, &muzzy_purged, uint64_t);

  emitter_row_t decay_row;
  emitter_row_init(&decay_row);

  /* JSON-style emission. */
  emitter_json_kv(
    emitter, "dirty_decay_ms", emitter_type_ssize, &dirty_decay_ms);
  emitter_json_kv(
    emitter, "muzzy_decay_ms", emitter_type_ssize, &muzzy_decay_ms);

  emitter_json_kv(emitter, "pactive", emitter_type_size, &pactive);
  emitter_json_kv(emitter, "pdirty", emitter_type_size, &pdirty);
  emitter_json_kv(emitter, "pmuzzy", emitter_type_size, &pmuzzy);

  emitter_json_kv(
    emitter, "dirty_npurge", emitter_type_uint64, &dirty_npurge);
  emitter_json_kv(
    emitter, "dirty_nmadvise", emitter_type_uint64, &dirty_nmadvise);
  emitter_json_kv(
    emitter, "dirty_purged", emitter_type_uint64, &dirty_purged);

  emitter_json_kv(
    emitter, "muzzy_npurge", emitter_type_uint64, &muzzy_npurge);
  emitter_json_kv(
    emitter, "muzzy_nmadvise", emitter_type_uint64, &muzzy_nmadvise);
  emitter_json_kv(
    emitter, "muzzy_purged", emitter_type_uint64, &muzzy_purged);

  /* Table-style emission. */
  COL(decay_row, decay_type, right, 9, title);
  col_decay_type.str_val = "decaying:";

  COL(decay_row, decay_time, right, 6, title);
  col_decay_time.str_val = "time";

  COL(decay_row, decay_npages, right, 13, title);
  col_decay_npages.str_val = "npages";

  COL(decay_row, decay_sweeps, right, 13, title);
  col_decay_sweeps.str_val = "sweeps";

  COL(decay_row, decay_madvises, right, 13, title);
  col_decay_madvises.str_val = "madvises";

  COL(decay_row, decay_purged, right, 13, title);
  col_decay_purged.str_val = "purged";

  /* Title row. */
  emitter_table_row(emitter, &decay_row);

  /* Dirty row. */
  col_decay_type.str_val = "dirty:";

  if (dirty_decay_ms >= 0) {
    col_decay_time.type = emitter_type_ssize;
    col_decay_time.ssize_val = dirty_decay_ms;
  } else {
    col_decay_time.type = emitter_type_title;
    col_decay_time.str_val = "N/A";
  }

  col_decay_npages.type = emitter_type_size;
  col_decay_npages.size_val = pdirty;

  col_decay_sweeps.type = emitter_type_uint64;
  col_decay_sweeps.uint64_val = dirty_npurge;

  col_decay_madvises.type = emitter_type_uint64;
  col_decay_madvises.uint64_val = dirty_nmadvise;

  col_decay_purged.type = emitter_type_uint64;
  col_decay_purged.uint64_val = dirty_purged;

  emitter_table_row(emitter, &decay_row);

  /* Muzzy row. */
  col_decay_type.str_val = "muzzy:";

  if (muzzy_decay_ms >= 0) {
    col_decay_time.type = emitter_type_ssize;
    col_decay_time.ssize_val = muzzy_decay_ms;
  } else {
    col_decay_time.type = emitter_type_title;
    col_decay_time.str_val = "N/A";
  }

  col_decay_npages.type = emitter_type_size;
  col_decay_npages.size_val = pmuzzy;

  col_decay_sweeps.type = emitter_type_uint64;
  col_decay_sweeps.uint64_val = muzzy_npurge;

  col_decay_madvises.type = emitter_type_uint64;
  col_decay_madvises.uint64_val = muzzy_nmadvise;

  col_decay_purged.type = emitter_type_uint64;
  col_decay_purged.uint64_val = muzzy_purged;

  emitter_table_row(emitter, &decay_row);

  /* Small / large / total allocation counts. */
  emitter_row_t alloc_count_row;
  emitter_row_init(&alloc_count_row);

  COL(alloc_count_row, count_title, left, 21, title);
  col_count_title.str_val = "";

  COL(alloc_count_row, count_allocated, right, 16, title);
  col_count_allocated.str_val = "allocated";

  COL(alloc_count_row, count_nmalloc, right, 16, title);
  col_count_nmalloc.str_val = "nmalloc";
  COL(alloc_count_row, count_nmalloc_ps, right, 8, title);
  col_count_nmalloc_ps.str_val = "(#/sec)";

  COL(alloc_count_row, count_ndalloc, right, 16, title);
  col_count_ndalloc.str_val = "ndalloc";
  COL(alloc_count_row, count_ndalloc_ps, right, 8, title);
  col_count_ndalloc_ps.str_val = "(#/sec)";

  COL(alloc_count_row, count_nrequests, right, 16, title);
  col_count_nrequests.str_val = "nrequests";
  COL(alloc_count_row, count_nrequests_ps, right, 10, title);
  col_count_nrequests_ps.str_val = "(#/sec)";

  COL(alloc_count_row, count_nfills, right, 16, title);
  col_count_nfills.str_val = "nfill";
  COL(alloc_count_row, count_nfills_ps, right, 10, title);
  col_count_nfills_ps.str_val = "(#/sec)";

  COL(alloc_count_row, count_nflushes, right, 16, title);
  col_count_nflushes.str_val = "nflush";
  COL(alloc_count_row, count_nflushes_ps, right, 10, title);
  col_count_nflushes_ps.str_val = "(#/sec)";

  emitter_table_row(emitter, &alloc_count_row);

  col_count_nmalloc_ps.type = emitter_type_uint64;
  col_count_ndalloc_ps.type = emitter_type_uint64;
  col_count_nrequests_ps.type = emitter_type_uint64;
  col_count_nfills_ps.type = emitter_type_uint64;
  col_count_nflushes_ps.type = emitter_type_uint64;

#define GET_AND_EMIT_ALLOC_STAT(small_or_large, name, valtype)             \
  CTL_M2_GET("stats.arenas.0." #small_or_large "." #name, i,               \
    &small_or_large##_##name, valtype##_t);                                \
  emitter_json_kv(                                                         \
    emitter, #name, emitter_type_##valtype, &small_or_large##_##name);     \
  col_count_##name.type = emitter_type_##valtype;                          \
  col_count_##name.valtype##_val = small_or_large##_##name;

  emitter_json_object_kv_begin(emitter, "small");
  col_count_title.str_val = "small:";

  GET_AND_EMIT_ALLOC_STAT(small, allocated, size)
  GET_AND_EMIT_ALLOC_STAT(small, nmalloc, uint64)
  col_count_nmalloc_ps.uint64_val
    = rate_per_second(col_count_nmalloc.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(small, ndalloc, uint64)
  col_count_ndalloc_ps.uint64_val
    = rate_per_second(col_count_ndalloc.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(small, nrequests, uint64)
  col_count_nrequests_ps.uint64_val
    = rate_per_second(col_count_nrequests.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(small, nfills, uint64)
  col_count_nfills_ps.uint64_val
    = rate_per_second(col_count_nfills.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(small, nflushes, uint64)
  col_count_nflushes_ps.uint64_val
    = rate_per_second(col_count_nflushes.uint64_val, uptime);

  emitter_table_row(emitter, &alloc_count_row);
  emitter_json_object_end(emitter); /* Close "small". */

  emitter_json_object_kv_begin(emitter, "large");
  col_count_title.str_val = "large:";

  GET_AND_EMIT_ALLOC_STAT(large, allocated, size)
  GET_AND_EMIT_ALLOC_STAT(large, nmalloc, uint64)
  col_count_nmalloc_ps.uint64_val
    = rate_per_second(col_count_nmalloc.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(large, ndalloc, uint64)
  col_count_ndalloc_ps.uint64_val
    = rate_per_second(col_count_ndalloc.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(large, nrequests, uint64)
  col_count_nrequests_ps.uint64_val
    = rate_per_second(col_count_nrequests.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(large, nfills, uint64)
  col_count_nfills_ps.uint64_val
    = rate_per_second(col_count_nfills.uint64_val, uptime);
  GET_AND_EMIT_ALLOC_STAT(large, nflushes, uint64)
  col_count_nflushes_ps.uint64_val
    = rate_per_second(col_count_nflushes.uint64_val, uptime);

  emitter_table_row(emitter, &alloc_count_row);
  emitter_json_object_end(emitter); /* Close "large". */

#undef GET_AND_EMIT_ALLOC_STAT

  /* Aggregated small + large stats are emitter only in table mode. */
  col_count_title.str_val = "total:";
  col_count_allocated.size_val = small_allocated + large_allocated;
  col_count_nmalloc.uint64_val = small_nmalloc + large_nmalloc;
  col_count_ndalloc.uint64_val = small_ndalloc + large_ndalloc;
  col_count_nrequests.uint64_val = small_nrequests + large_nrequests;
  col_count_nfills.uint64_val = small_nfills + large_nfills;
  col_count_nflushes.uint64_val = small_nflushes + large_nflushes;
  col_count_nmalloc_ps.uint64_val
    = rate_per_second(col_count_nmalloc.uint64_val, uptime);
  col_count_ndalloc_ps.uint64_val
    = rate_per_second(col_count_ndalloc.uint64_val, uptime);
  col_count_nrequests_ps.uint64_val
    = rate_per_second(col_count_nrequests.uint64_val, uptime);
  col_count_nfills_ps.uint64_val
    = rate_per_second(col_count_nfills.uint64_val, uptime);
  col_count_nflushes_ps.uint64_val
    = rate_per_second(col_count_nflushes.uint64_val, uptime);
  emitter_table_row(emitter, &alloc_count_row);

  emitter_row_t mem_count_row;
  emitter_row_init(&mem_count_row);

  emitter_col_t mem_count_title;
  emitter_col_init(&mem_count_title, &mem_count_row);
  mem_count_title.justify = emitter_justify_left;
  mem_count_title.width = 21;
  mem_count_title.type = emitter_type_title;
  mem_count_title.str_val = "";

  emitter_col_t mem_count_val;
  emitter_col_init(&mem_count_val, &mem_count_row);
  mem_count_val.justify = emitter_justify_right;
  mem_count_val.width = 16;
  mem_count_val.type = emitter_type_title;
  mem_count_val.str_val = "";

  emitter_table_row(emitter, &mem_count_row);
  mem_count_val.type = emitter_type_size;

  /* Active count in bytes is emitted only in table mode. */
  mem_count_title.str_val = "active:";
  mem_count_val.size_val = pactive * page;
  emitter_table_row(emitter, &mem_count_row);

#define GET_AND_EMIT_MEM_STAT(stat)                                        \
  CTL_M2_GET("stats.arenas.0." #stat, i, &stat, size_t);                   \
  emitter_json_kv(emitter, #stat, emitter_type_size, &stat);               \
  mem_count_title.str_val = #stat ":";                                     \
  mem_count_val.size_val = stat;                                           \
  emitter_table_row(emitter, &mem_count_row);

  GET_AND_EMIT_MEM_STAT(mapped)
  GET_AND_EMIT_MEM_STAT(retained)
  GET_AND_EMIT_MEM_STAT(base)
  GET_AND_EMIT_MEM_STAT(internal)
  GET_AND_EMIT_MEM_STAT(metadata_thp)
  GET_AND_EMIT_MEM_STAT(tcache_bytes)
  GET_AND_EMIT_MEM_STAT(resident)
  GET_AND_EMIT_MEM_STAT(abandoned_vm)
  GET_AND_EMIT_MEM_STAT(extent_avail)
#undef GET_AND_EMIT_MEM_STAT

  if (mutex) { stats_arena_mutexes_print(emitter, i, uptime); }
  if (bins) { stats_arena_bins_print(emitter, mutex, i, uptime); }
  if (large) { stats_arena_lextents_print(emitter, i, uptime); }
  if (extents) { stats_arena_extents_print(emitter, i); }
}

static void stats_general_print(emitter_t* emitter) {
  const char* cpv;
  bool bv, bv2;
  unsigned uv;
  uint32_t u32v;
  uint64_t u64v;
  ssize_t ssv, ssv2;
  size_t sv, bsz, usz, ssz, sssz, cpsz;

  bsz = sizeof(bool);
  usz = sizeof(unsigned);
  ssz = sizeof(size_t);
  sssz = sizeof(ssize_t);
  cpsz = sizeof(const char*);

  CTL_GET("version", &cpv, const char*);
  emitter_kv(emitter, "version", "Version", emitter_type_string, &cpv);

  /* config. */
  emitter_dict_begin(emitter, "config", "Build-time option settings");
#define CONFIG_WRITE_BOOL(name)                                            \
  do {                                                                     \
    CTL_GET("config." #name, &bv, bool);                                   \
    emitter_kv(emitter, #name, "config." #name, emitter_type_bool, &bv);   \
  } while (0)

  CONFIG_WRITE_BOOL(cache_oblivious);
  CONFIG_WRITE_BOOL(debug);
  CONFIG_WRITE_BOOL(fill);
  CONFIG_WRITE_BOOL(lazy_lock);
  emitter_kv(emitter, "malloc_conf", "config.malloc_conf",
    emitter_type_string, &config_malloc_conf);

  CONFIG_WRITE_BOOL(opt_safety_checks);
  CONFIG_WRITE_BOOL(prof);
  CONFIG_WRITE_BOOL(prof_libgcc);
  CONFIG_WRITE_BOOL(prof_libunwind);
  CONFIG_WRITE_BOOL(stats);
  CONFIG_WRITE_BOOL(utrace);
  CONFIG_WRITE_BOOL(xmalloc);
#undef CONFIG_WRITE_BOOL
  emitter_dict_end(emitter); /* Close "config" dict. */

  /* opt. */
#define OPT_WRITE(name, var, size, emitter_type)                           \
  if (je_mallctl("opt." name, (void*)&var, &size, NULL, 0) == 0) {         \
    emitter_kv(emitter, name, "opt." name, emitter_type, &var);            \
  }

#define OPT_WRITE_MUTABLE(name, var1, var2, size, emitter_type, altname)   \
  if (je_mallctl("opt." name, (void*)&var1, &size, NULL, 0) == 0           \
    && je_mallctl(altname, (void*)&var2, &size, NULL, 0) == 0) {           \
    emitter_kv_note(emitter, name, "opt." name, emitter_type, &var1,       \
      altname, emitter_type, &var2);                                       \
  }

#define OPT_WRITE_BOOL(name) OPT_WRITE(name, bv, bsz, emitter_type_bool)
#define OPT_WRITE_BOOL_MUTABLE(name, altname)                              \
  OPT_WRITE_MUTABLE(name, bv, bv2, bsz, emitter_type_bool, altname)

#define OPT_WRITE_UNSIGNED(name)                                           \
  OPT_WRITE(name, uv, usz, emitter_type_unsigned)

#define OPT_WRITE_SIZE_T(name) OPT_WRITE(name, sv, ssz, emitter_type_size)
#define OPT_WRITE_SSIZE_T(name)                                            \
  OPT_WRITE(name, ssv, sssz, emitter_type_ssize)
#define OPT_WRITE_SSIZE_T_MUTABLE(name, altname)                           \
  OPT_WRITE_MUTABLE(name, ssv, ssv2, sssz, emitter_type_ssize, altname)

#define OPT_WRITE_CHAR_P(name)                                             \
  OPT_WRITE(name, cpv, cpsz, emitter_type_string)

  emitter_dict_begin(emitter, "opt", "Run-time option settings");

  OPT_WRITE_BOOL("abort")
  OPT_WRITE_BOOL("abort_conf")
  OPT_WRITE_BOOL("confirm_conf")
  OPT_WRITE_BOOL("retain")
  OPT_WRITE_CHAR_P("dss")
  OPT_WRITE_UNSIGNED("narenas")
  OPT_WRITE_CHAR_P("percpu_arena")
  OPT_WRITE_SIZE_T("oversize_threshold")
  OPT_WRITE_CHAR_P("metadata_thp")
  OPT_WRITE_BOOL_MUTABLE("background_thread", "background_thread")
  OPT_WRITE_SSIZE_T_MUTABLE("dirty_decay_ms", "arenas.dirty_decay_ms")
  OPT_WRITE_SSIZE_T_MUTABLE("muzzy_decay_ms", "arenas.muzzy_decay_ms")
  OPT_WRITE_SIZE_T("lg_extent_max_active_fit")
  OPT_WRITE_CHAR_P("junk")
  OPT_WRITE_BOOL("zero")
  OPT_WRITE_BOOL("utrace")
  OPT_WRITE_BOOL("xmalloc")
  OPT_WRITE_BOOL("tcache")
  OPT_WRITE_SSIZE_T("lg_tcache_max")
  OPT_WRITE_CHAR_P("thp")
  OPT_WRITE_BOOL("prof")
  OPT_WRITE_CHAR_P("prof_prefix")
  OPT_WRITE_BOOL_MUTABLE("prof_active", "prof.active")
  OPT_WRITE_BOOL_MUTABLE(
    "prof_thread_active_init", "prof.thread_active_init")
  OPT_WRITE_SSIZE_T_MUTABLE("lg_prof_sample", "prof.lg_sample")
  OPT_WRITE_BOOL("prof_accum")
  OPT_WRITE_SSIZE_T("lg_prof_interval")
  OPT_WRITE_BOOL("prof_gdump")
  OPT_WRITE_BOOL("prof_final")
  OPT_WRITE_BOOL("prof_leak")
  OPT_WRITE_BOOL("stats_print")
  OPT_WRITE_CHAR_P("stats_print_opts")

  emitter_dict_end(emitter);

#undef OPT_WRITE
#undef OPT_WRITE_MUTABLE
#undef OPT_WRITE_BOOL
#undef OPT_WRITE_BOOL_MUTABLE
#undef OPT_WRITE_UNSIGNED
#undef OPT_WRITE_SSIZE_T
#undef OPT_WRITE_SSIZE_T_MUTABLE
#undef OPT_WRITE_CHAR_P

  /* prof. */
  if (config_prof) {
    emitter_dict_begin(emitter, "prof", "Profiling settings");

    CTL_GET("prof.thread_active_init", &bv, bool);
    emitter_kv(emitter, "thread_active_init", "prof.thread_active_init",
      emitter_type_bool, &bv);

    CTL_GET("prof.active", &bv, bool);
    emitter_kv(emitter, "active", "prof.active", emitter_type_bool, &bv);

    CTL_GET("prof.gdump", &bv, bool);
    emitter_kv(emitter, "gdump", "prof.gdump", emitter_type_bool, &bv);

    CTL_GET("prof.interval", &u64v, uint64_t);
    emitter_kv(
      emitter, "interval", "prof.interval", emitter_type_uint64, &u64v);

    CTL_GET("prof.lg_sample", &ssv, ssize_t);
    emitter_kv(
      emitter, "lg_sample", "prof.lg_sample", emitter_type_ssize, &ssv);

    emitter_dict_end(emitter); /* Close "prof". */
  }

  /* arenas. */
  /*
   * The json output sticks arena info into an "arenas" dict; the table
   * output puts them at the top-level.
   */
  emitter_json_object_kv_begin(emitter, "arenas");

  CTL_GET("arenas.narenas", &uv, unsigned);
  emitter_kv(emitter, "narenas", "Arenas", emitter_type_unsigned, &uv);

  /*
   * Decay settings are emitted only in json mode; in table mode, they're
   * emitted as notes with the opt output, above.
   */
  CTL_GET("arenas.dirty_decay_ms", &ssv, ssize_t);
  emitter_json_kv(emitter, "dirty_decay_ms", emitter_type_ssize, &ssv);

  CTL_GET("arenas.muzzy_decay_ms", &ssv, ssize_t);
  emitter_json_kv(emitter, "muzzy_decay_ms", emitter_type_ssize, &ssv);

  CTL_GET("arenas.quantum", &sv, size_t);
  emitter_kv(emitter, "quantum", "Quantum size", emitter_type_size, &sv);

  CTL_GET("arenas.page", &sv, size_t);
  emitter_kv(emitter, "page", "Page size", emitter_type_size, &sv);

  if (je_mallctl("arenas.tcache_max", (void*)&sv, &ssz, NULL, 0) == 0) {
    emitter_kv(emitter, "tcache_max", "Maximum thread-cached size class",
      emitter_type_size, &sv);
  }

  unsigned nbins;
  CTL_GET("arenas.nbins", &nbins, unsigned);
  emitter_kv(emitter, "nbins", "Number of bin size classes",
    emitter_type_unsigned, &nbins);

  unsigned nhbins;
  CTL_GET("arenas.nhbins", &nhbins, unsigned);
  emitter_kv(emitter, "nhbins", "Number of thread-cache bin size classes",
    emitter_type_unsigned, &nhbins);

  /*
   * We do enough mallctls in a loop that we actually want to omit them
   * (not just omit the printing).
   */
  if (emitter->output == emitter_output_json) {
    emitter_json_array_kv_begin(emitter, "bin");
    for (unsigned i = 0; i < nbins; i++) {
      emitter_json_object_begin(emitter);

      CTL_M2_GET("arenas.bin.0.size", i, &sv, size_t);
      emitter_json_kv(emitter, "size", emitter_type_size, &sv);

      CTL_M2_GET("arenas.bin.0.nregs", i, &u32v, uint32_t);
      emitter_json_kv(emitter, "nregs", emitter_type_uint32, &u32v);

      CTL_M2_GET("arenas.bin.0.slab_size", i, &sv, size_t);
      emitter_json_kv(emitter, "slab_size", emitter_type_size, &sv);

      CTL_M2_GET("arenas.bin.0.nshards", i, &u32v, uint32_t);
      emitter_json_kv(emitter, "nshards", emitter_type_uint32, &u32v);

      emitter_json_object_end(emitter);
    }
    emitter_json_array_end(emitter); /* Close "bin". */
  }

  unsigned nlextents;
  CTL_GET("arenas.nlextents", &nlextents, unsigned);
  emitter_kv(emitter, "nlextents", "Number of large size classes",
    emitter_type_unsigned, &nlextents);

  if (emitter->output == emitter_output_json) {
    emitter_json_array_kv_begin(emitter, "lextent");
    for (unsigned i = 0; i < nlextents; i++) {
      emitter_json_object_begin(emitter);

      CTL_M2_GET("arenas.lextent.0.size", i, &sv, size_t);
      emitter_json_kv(emitter, "size", emitter_type_size, &sv);

      emitter_json_object_end(emitter);
    }
    emitter_json_array_end(emitter); /* Close "lextent". */
  }

  emitter_json_object_end(emitter); /* Close "arenas" */
}

static void stats_print_helper(emitter_t* emitter, bool merged,
  bool destroyed, bool unmerged, bool bins, bool large, bool mutex,
  bool extents) {
  /*
   * These should be deleted.  We keep them around for a while, to aid in
   * the transition to the emitter code.
   */
  size_t allocated, active, metadata, metadata_thp, resident, mapped,
    retained;
  size_t num_background_threads;
  uint64_t background_thread_num_runs, background_thread_run_interval;

  CTL_GET("stats.allocated", &allocated, size_t);
  CTL_GET("stats.active", &active, size_t);
  CTL_GET("stats.metadata", &metadata, size_t);
  CTL_GET("stats.metadata_thp", &metadata_thp, size_t);
  CTL_GET("stats.resident", &resident, size_t);
  CTL_GET("stats.mapped", &mapped, size_t);
  CTL_GET("stats.retained", &retained, size_t);

  if (have_background_thread) {
    CTL_GET("stats.background_thread.num_threads", &num_background_threads,
      size_t);
    CTL_GET("stats.background_thread.num_runs", &background_thread_num_runs,
      uint64_t);
    CTL_GET("stats.background_thread.run_interval",
      &background_thread_run_interval, uint64_t);
  } else {
    num_background_threads = 0;
    background_thread_num_runs = 0;
    background_thread_run_interval = 0;
  }

  /* Generic global stats. */
  emitter_json_object_kv_begin(emitter, "stats");
  emitter_json_kv(emitter, "allocated", emitter_type_size, &allocated);
  emitter_json_kv(emitter, "active", emitter_type_size, &active);
  emitter_json_kv(emitter, "metadata", emitter_type_size, &metadata);
  emitter_json_kv(
    emitter, "metadata_thp", emitter_type_size, &metadata_thp);
  emitter_json_kv(emitter, "resident", emitter_type_size, &resident);
  emitter_json_kv(emitter, "mapped", emitter_type_size, &mapped);
  emitter_json_kv(emitter, "retained", emitter_type_size, &retained);

  emitter_table_printf(emitter,
    "Allocated: %zu, active: %zu, "
    "metadata: %zu (n_thp %zu), resident: %zu, mapped: %zu, "
    "retained: %zu\n",
    allocated, active, metadata, metadata_thp, resident, mapped, retained);

  /* Background thread stats. */
  emitter_json_object_kv_begin(emitter, "background_thread");
  emitter_json_kv(
    emitter, "num_threads", emitter_type_size, &num_background_threads);
  emitter_json_kv(
    emitter, "num_runs", emitter_type_uint64, &background_thread_num_runs);
  emitter_json_kv(emitter, "run_interval", emitter_type_uint64,
    &background_thread_run_interval);
  emitter_json_object_end(emitter); /* Close "background_thread". */

  emitter_table_printf(emitter,
    "Background threads: %zu, "
    "num_runs: %" FMTu64 ", run_interval: %" FMTu64 " ns\n",
    num_background_threads, background_thread_num_runs,
    background_thread_run_interval);

  if (mutex) {
    emitter_row_t row;
    emitter_col_t name;
    emitter_col_t col64[mutex_prof_num_uint64_t_counters];
    emitter_col_t col32[mutex_prof_num_uint32_t_counters];
    uint64_t uptime;

    emitter_row_init(&row);
    mutex_stats_init_cols(&row, "", &name, col64, col32);

    emitter_table_row(emitter, &row);
    emitter_json_object_kv_begin(emitter, "mutexes");

    CTL_M2_GET("stats.arenas.0.uptime", 0, &uptime, uint64_t);

    for (int i = 0; i < mutex_prof_num_global_mutexes; i++) {
      mutex_stats_read_global(
        global_mutex_names[i], &name, col64, col32, uptime);
      emitter_json_object_kv_begin(emitter, global_mutex_names[i]);
      mutex_stats_emit(emitter, &row, col64, col32);
      emitter_json_object_end(emitter);
    }

    emitter_json_object_end(emitter); /* Close "mutexes". */
  }

  emitter_json_object_end(emitter); /* Close "stats". */

  if (merged || destroyed || unmerged) {
    unsigned narenas;

    emitter_json_object_kv_begin(emitter, "stats.arenas");

    CTL_GET("arenas.narenas", &narenas, unsigned);
    size_t mib[3];
    size_t miblen = sizeof(mib) / sizeof(size_t);
    size_t sz;
    VARIABLE_ARRAY(bool, initialized, narenas);
    bool destroyed_initialized;
    unsigned i, j, ninitialized;

    xmallctlnametomib("arena.0.initialized", mib, &miblen);
    for (i = ninitialized = 0; i < narenas; i++) {
      mib[1] = i;
      sz = sizeof(bool);
      xmallctlbymib(mib, miblen, &initialized[i], &sz, NULL, 0);
      if (initialized[i]) { ninitialized++; }
    }
    mib[1] = MALLCTL_ARENAS_DESTROYED;
    sz = sizeof(bool);
    xmallctlbymib(mib, miblen, &destroyed_initialized, &sz, NULL, 0);

    /* Merged stats. */
    if (merged && (ninitialized > 1 || !unmerged)) {
      /* Print merged arena stats. */
      emitter_table_printf(emitter, "Merged arenas stats:\n");
      emitter_json_object_kv_begin(emitter, "merged");
      stats_arena_print(
        emitter, MALLCTL_ARENAS_ALL, bins, large, mutex, extents);
      emitter_json_object_end(emitter); /* Close "merged". */
    }

    /* Destroyed stats. */
    if (destroyed_initialized && destroyed) {
      /* Print destroyed arena stats. */
      emitter_table_printf(emitter, "Destroyed arenas stats:\n");
      emitter_json_object_kv_begin(emitter, "destroyed");
      stats_arena_print(
        emitter, MALLCTL_ARENAS_DESTROYED, bins, large, mutex, extents);
      emitter_json_object_end(emitter); /* Close "destroyed". */
    }

    /* Unmerged stats. */
    if (unmerged) {
      for (i = j = 0; i < narenas; i++) {
        if (initialized[i]) {
          char arena_ind_str[20];
          malloc_snprintf(arena_ind_str, sizeof(arena_ind_str), "%u", i);
          emitter_json_object_kv_begin(emitter, arena_ind_str);
          emitter_table_printf(emitter, "arenas[%s]:\n", arena_ind_str);
          stats_arena_print(emitter, i, bins, large, mutex, extents);
          /* Close "<arena-ind>". */
          emitter_json_object_end(emitter);
        }
      }
    }
    emitter_json_object_end(emitter); /* Close "stats.arenas". */
  }
}

void stats_print(
  void (*write_cb)(void*, const char*), void* cbopaque, const char* opts) {
  int err;
  uint64_t epoch;
  size_t u64sz;
#define OPTION(o, v, d, s) bool v = d;
  STATS_PRINT_OPTIONS
#undef OPTION

  /*
   * Refresh stats, in case mallctl() was called by the application.
   *
   * Check for OOM here, since refreshing the ctl cache can trigger
   * allocation.  In practice, none of the subsequent mallctl()-related
   * calls in this function will cause OOM if this one succeeds.
   * */
  epoch = 1;
  u64sz = sizeof(uint64_t);
  err = je_mallctl(
    "epoch", (void*)&epoch, &u64sz, (void*)&epoch, sizeof(uint64_t));
  if (err != 0) {
    if (err == EAGAIN) {
      malloc_write("<jemalloc>: Memory allocation failure in "
                   "mallctl(\"epoch\", ...)\n");
      return;
    }
    malloc_write("<jemalloc>: Failure in mallctl(\"epoch\", "
                 "...)\n");
    abort();
  }

  if (opts != NULL) {
    for (unsigned i = 0; opts[i] != '\0'; i++) {
      switch (opts[i]) {
#define OPTION(o, v, d, s)                                                 \
  case o: v = s; break;
        STATS_PRINT_OPTIONS
#undef OPTION
      default:;
      }
    }
  }

  emitter_t emitter;
  emitter_init(&emitter, json ? emitter_output_json : emitter_output_table,
    write_cb, cbopaque);
  emitter_begin(&emitter);
  emitter_table_printf(&emitter, "___ Begin jemalloc statistics ___\n");
  emitter_json_object_kv_begin(&emitter, "jemalloc");

  if (general) { stats_general_print(&emitter); }
  if (config_stats) {
    stats_print_helper(
      &emitter, merged, destroyed, unmerged, bins, large, mutex, extents);
  }

  emitter_json_object_end(&emitter); /* Closes the "jemalloc" dict. */
  emitter_table_printf(&emitter, "--- End jemalloc statistics ---\n");
  emitter_end(&emitter);
}

#include "jemalloc/internal/bit_util.h"
#include "jemalloc/internal/bitmap.h"
#include "jemalloc/internal/pages.h"

/*
 * This module computes the size classes used to satisfy allocations.  The
 * logic here was ported more or less line-by-line from a shell script, and
 * because of that is not the most idiomatic C.  Eventually we should fix
 * this, but for now at least the damage is compartmentalized to this file.
 */

sc_data_t sc_data_global;

static size_t reg_size_compute(int lg_base, int lg_delta, int ndelta) {
  return (ZU(1) << lg_base) + (ZU(ndelta) << lg_delta);
}

/* Returns the number of pages in the slab. */
static int slab_size(int lg_page, int lg_base, int lg_delta, int ndelta) {
  size_t page = (ZU(1) << lg_page);
  size_t reg_size = reg_size_compute(lg_base, lg_delta, ndelta);

  size_t try_slab_size = page;
  size_t try_nregs = try_slab_size / reg_size;
  size_t perfect_slab_size = 0;
  bool perfect = false;
  /*
   * This loop continues until we find the least common multiple of the
   * page size and size class size.  Size classes are all of the form
   * base + ndelta * delta == (ndelta + base/ndelta) * delta, which is
   * (ndelta + ngroup) * delta.  The way we choose slabbing strategies
   * means that delta is at most the page size and ndelta < ngroup.  So
   * the loop executes for at most 2 * ngroup - 1 iterations, which is
   * also the bound on the number of pages in a slab chosen by default.
   * With the current default settings, this is at most 7.
   */
  while (!perfect) {
    perfect_slab_size = try_slab_size;
    size_t perfect_nregs = try_nregs;
    try_slab_size += page;
    try_nregs = try_slab_size / reg_size;
    if (perfect_slab_size == perfect_nregs * reg_size) { perfect = true; }
  }
  return (int)(perfect_slab_size / page);
}

static void size_class(
  /* Output. */
  sc_t* sc,
  /* Configuration decisions. */
  int lg_max_lookup, int lg_page, int lg_ngroup,
  /* Inputs specific to the size class. */
  int index, int lg_base, int lg_delta, int ndelta) {
  sc->index = index;
  sc->lg_base = lg_base;
  sc->lg_delta = lg_delta;
  sc->ndelta = ndelta;
  sc->psz
    = (reg_size_compute(lg_base, lg_delta, ndelta) % (ZU(1) << lg_page)
      == 0);
  size_t size = (ZU(1) << lg_base) + (ZU(ndelta) << lg_delta);
  if (index == 0) { assert(!sc->psz); }
  if (size < (ZU(1) << (lg_page + lg_ngroup))) {
    sc->bin = true;
    sc->pgs = slab_size(lg_page, lg_base, lg_delta, ndelta);
  } else {
    sc->bin = false;
    sc->pgs = 0;
  }
  if (size <= (ZU(1) << lg_max_lookup)) {
    sc->lg_delta_lookup = lg_delta;
  } else {
    sc->lg_delta_lookup = 0;
  }
}

static void size_classes(
  /* Output. */
  sc_data_t* sc_data,
  /* Determined by the system. */
  size_t lg_ptr_size, int lg_quantum,
  /* Configuration decisions. */
  int lg_tiny_min, int lg_max_lookup, int lg_page, int lg_ngroup) {
  int ptr_bits = (1 << lg_ptr_size) * 8;
  int ngroup = (1 << lg_ngroup);
  int ntiny = 0;
  int nlbins = 0;
  int lg_tiny_maxclass = (unsigned)-1;
  int nbins = 0;
  int npsizes = 0;

  int index = 0;

  int ndelta = 0;
  int lg_base = lg_tiny_min;
  int lg_delta = lg_base;

  /* Outputs that we update as we go. */
  size_t lookup_maxclass = 0;
  size_t small_maxclass = 0;
  int lg_large_minclass = 0;
  size_t large_maxclass = 0;

  /* Tiny size classes. */
  while (lg_base < lg_quantum) {
    sc_t* sc = &sc_data->sc[index];
    size_class(sc, lg_max_lookup, lg_page, lg_ngroup, index, lg_base,
      lg_delta, ndelta);
    if (sc->lg_delta_lookup != 0) { nlbins = index + 1; }
    if (sc->psz) { npsizes++; }
    if (sc->bin) { nbins++; }
    ntiny++;
    /* Final written value is correct. */
    lg_tiny_maxclass = lg_base;
    index++;
    lg_delta = lg_base;
    lg_base++;
  }

  /* First non-tiny (pseudo) group. */
  if (ntiny != 0) {
    sc_t* sc = &sc_data->sc[index];
    /*
     * See the note in sc.h; the first non-tiny size class has an
     * unusual encoding.
     */
    lg_base--;
    ndelta = 1;
    size_class(sc, lg_max_lookup, lg_page, lg_ngroup, index, lg_base,
      lg_delta, ndelta);
    index++;
    lg_base++;
    lg_delta++;
    if (sc->psz) { npsizes++; }
    if (sc->bin) { nbins++; }
  }
  while (ndelta < ngroup) {
    sc_t* sc = &sc_data->sc[index];
    size_class(sc, lg_max_lookup, lg_page, lg_ngroup, index, lg_base,
      lg_delta, ndelta);
    index++;
    ndelta++;
    if (sc->psz) { npsizes++; }
    if (sc->bin) { nbins++; }
  }

  /* All remaining groups. */
  lg_base = lg_base + lg_ngroup;
  while (lg_base < ptr_bits - 1) {
    ndelta = 1;
    int ndelta_limit;
    if (lg_base == ptr_bits - 2) {
      ndelta_limit = ngroup - 1;
    } else {
      ndelta_limit = ngroup;
    }
    while (ndelta <= ndelta_limit) {
      sc_t* sc = &sc_data->sc[index];
      size_class(sc, lg_max_lookup, lg_page, lg_ngroup, index, lg_base,
        lg_delta, ndelta);
      if (sc->lg_delta_lookup != 0) {
        nlbins = index + 1;
        /* Final written value is correct. */
        lookup_maxclass = (ZU(1) << lg_base) + (ZU(ndelta) << lg_delta);
      }
      if (sc->psz) { npsizes++; }
      if (sc->bin) {
        nbins++;
        /* Final written value is correct. */
        small_maxclass = (ZU(1) << lg_base) + (ZU(ndelta) << lg_delta);
        if (lg_ngroup > 0) {
          lg_large_minclass = lg_base + 1;
        } else {
          lg_large_minclass = lg_base + 2;
        }
      }
      large_maxclass = (ZU(1) << lg_base) + (ZU(ndelta) << lg_delta);
      index++;
      ndelta++;
    }
    lg_base++;
    lg_delta++;
  }
  /* Additional outputs. */
  int nsizes = index;
  unsigned lg_ceil_nsizes = lg_ceil(nsizes);

  /* Fill in the output data. */
  sc_data->ntiny = ntiny;
  sc_data->nlbins = nlbins;
  sc_data->nbins = nbins;
  sc_data->nsizes = nsizes;
  sc_data->lg_ceil_nsizes = lg_ceil_nsizes;
  sc_data->npsizes = npsizes;
  sc_data->lg_tiny_maxclass = lg_tiny_maxclass;
  sc_data->lookup_maxclass = lookup_maxclass;
  sc_data->small_maxclass = small_maxclass;
  sc_data->lg_large_minclass = lg_large_minclass;
  sc_data->large_minclass = (ZU(1) << lg_large_minclass);
  sc_data->large_maxclass = large_maxclass;

  /*
   * We compute these values in two ways:
   *   - Incrementally, as above.
   *   - In macros, in sc.h.
   * The computation is easier when done incrementally, but putting it in
   * a constant makes it available to the fast paths without having to
   * touch the extra global cacheline.  We assert, however, that the two
   * computations are equivalent.
   */
  assert(sc_data->npsizes == SC_NPSIZES);
  assert(sc_data->lg_tiny_maxclass == SC_LG_TINY_MAXCLASS);
  assert(sc_data->small_maxclass == SC_SMALL_MAXCLASS);
  assert(sc_data->large_minclass == SC_LARGE_MINCLASS);
  assert(sc_data->lg_large_minclass == SC_LG_LARGE_MINCLASS);
  assert(sc_data->large_maxclass == SC_LARGE_MAXCLASS);

  /*
   * In the allocation fastpath, we want to assume that we can
   * unconditionally subtract the requested allocation size from
   * a ssize_t, and detect passing through 0 correctly.  This
   * results in optimal generated code.  For this to work, the
   * maximum allocation size must be less than SSIZE_MAX.
   */
  assert(SC_LARGE_MAXCLASS < SSIZE_MAX);
}

void sc_data_init(sc_data_t* sc_data) {
  assert(!sc_data->initialized);

  int lg_max_lookup = 12;

  size_classes(sc_data, LG_SIZEOF_PTR, LG_QUANTUM, SC_LG_TINY_MIN,
    lg_max_lookup, LG_PAGE, 2);

  sc_data->initialized = true;
}

static void sc_data_update_sc_slab_size(
  sc_t* sc, size_t reg_size, size_t pgs_guess) {
  size_t min_pgs = reg_size / PAGE;
  if (reg_size % PAGE != 0) { min_pgs++; }
  /*
   * BITMAP_MAXBITS is actually determined by putting the smallest
   * possible size-class on one page, so this can never be 0.
   */
  size_t max_pgs = BITMAP_MAXBITS * reg_size / PAGE;

  assert(min_pgs <= max_pgs);
  assert(min_pgs > 0);
  assert(max_pgs >= 1);
  if (pgs_guess < min_pgs) {
    sc->pgs = (int)min_pgs;
  } else if (pgs_guess > max_pgs) {
    sc->pgs = (int)max_pgs;
  } else {
    sc->pgs = (int)pgs_guess;
  }
}

void sc_data_update_slab_size(
  sc_data_t* data, size_t begin, size_t end, int pgs) {
  assert(data->initialized);
  for (int i = 0; i < data->nsizes; i++) {
    sc_t* sc = &data->sc[i];
    if (!sc->bin) { break; }
    size_t reg_size
      = reg_size_compute(sc->lg_base, sc->lg_delta, sc->ndelta);
    if (begin <= reg_size && reg_size <= end) {
      sc_data_update_sc_slab_size(sc, reg_size, pgs);
    }
  }
}

void sc_boot(sc_data_t* data) { sc_data_init(data); }

JEMALLOC_ALIGNED(CACHELINE)
size_t sz_pind2sz_tab[SC_NPSIZES + 1];

static void sz_boot_pind2sz_tab(const sc_data_t* sc_data) {
  int pind = 0;
  for (unsigned i = 0; i < SC_NSIZES; i++) {
    const sc_t* sc = &sc_data->sc[i];
    if (sc->psz) {
      sz_pind2sz_tab[pind]
        = (ZU(1) << sc->lg_base) + (ZU(sc->ndelta) << sc->lg_delta);
      pind++;
    }
  }
  for (int i = pind; i <= (int)SC_NPSIZES; i++) {
    sz_pind2sz_tab[pind] = sc_data->large_maxclass + PAGE;
  }
}

JEMALLOC_ALIGNED(CACHELINE)
size_t sz_index2size_tab[SC_NSIZES];

static void sz_boot_index2size_tab(const sc_data_t* sc_data) {
  for (unsigned i = 0; i < SC_NSIZES; i++) {
    const sc_t* sc = &sc_data->sc[i];
    sz_index2size_tab[i]
      = (ZU(1) << sc->lg_base) + (ZU(sc->ndelta) << (sc->lg_delta));
  }
}

/*
 * To keep this table small, we divide sizes by the tiny min size, which
 * gives the smallest interval for which the result can change.
 */
JEMALLOC_ALIGNED(CACHELINE)
uint8_t sz_size2index_tab[(SC_LOOKUP_MAXCLASS >> SC_LG_TINY_MIN) + 1];

static void sz_boot_size2index_tab(const sc_data_t* sc_data) {
  size_t dst_max = (SC_LOOKUP_MAXCLASS >> SC_LG_TINY_MIN) + 1;
  size_t dst_ind = 0;
  for (unsigned sc_ind = 0; sc_ind < SC_NSIZES && dst_ind < dst_max;
       sc_ind++) {
    const sc_t* sc = &sc_data->sc[sc_ind];
    size_t sz = (ZU(1) << sc->lg_base) + (ZU(sc->ndelta) << sc->lg_delta);
    size_t max_ind
      = ((sz + (ZU(1) << SC_LG_TINY_MIN) - 1) >> SC_LG_TINY_MIN);
    for (; dst_ind <= max_ind && dst_ind < dst_max; dst_ind++) {
      sz_size2index_tab[dst_ind] = sc_ind;
    }
  }
}

void sz_boot(const sc_data_t* sc_data) {
  sz_boot_pind2sz_tab(sc_data);
  sz_boot_index2size_tab(sc_data);
  sz_boot_size2index_tab(sc_data);
}
#define JEMALLOC_TCACHE_C_

/******************************************************************************/
/* Data. */

bool opt_tcache = true;
ssize_t opt_lg_tcache_max = LG_TCACHE_MAXCLASS_DEFAULT;

cache_bin_info_t* tcache_bin_info;
static unsigned stack_nelms; /* Total stack elms per tcache. */

unsigned nhbins;
size_t tcache_maxclass;

tcaches_t* tcaches;

/* Index of first element within tcaches that has never been used. */
static unsigned tcaches_past;

/* Head of singly linked list tracking available tcaches elements. */
static tcaches_t* tcaches_avail;

/* Protects tcaches{,_past,_avail}. */
static malloc_mutex_t tcaches_mtx;

/******************************************************************************/

size_t tcache_salloc(tsdn_t* tsdn, const void* ptr) {
  return arena_salloc(tsdn, ptr);
}

void tcache_event_hard(tsd_t* tsd, tcache_t* tcache) {
  szind_t binind = tcache->next_gc_bin;

  cache_bin_t* tbin;
  if (binind < SC_NBINS) {
    tbin = tcache_small_bin_get(tcache, binind);
  } else {
    tbin = tcache_large_bin_get(tcache, binind);
  }
  if (tbin->low_water > 0) {
    /*
     * Flush (ceiling) 3/4 of the objects below the low water mark.
     */
    if (binind < SC_NBINS) {
      tcache_bin_flush_small(tsd, tcache, tbin, binind,
        tbin->ncached - tbin->low_water + (tbin->low_water >> 2));
      /*
       * Reduce fill count by 2X.  Limit lg_fill_div such that
       * the fill count is always at least 1.
       */
      cache_bin_info_t* tbin_info = &tcache_bin_info[binind];
      if ((tbin_info->ncached_max >> (tcache->lg_fill_div[binind] + 1))
        >= 1) {
        tcache->lg_fill_div[binind]++;
      }
    } else {
      tcache_bin_flush_large(tsd, tbin, binind,
        tbin->ncached - tbin->low_water + (tbin->low_water >> 2), tcache);
    }
  } else if (tbin->low_water < 0) {
    /*
     * Increase fill count by 2X for small bins.  Make sure
     * lg_fill_div stays greater than 0.
     */
    if (binind < SC_NBINS && tcache->lg_fill_div[binind] > 1) {
      tcache->lg_fill_div[binind]--;
    }
  }
  tbin->low_water = tbin->ncached;

  tcache->next_gc_bin++;
  if (tcache->next_gc_bin == nhbins) { tcache->next_gc_bin = 0; }
}

void* tcache_alloc_small_hard(tsdn_t* tsdn, arena_t* arena,
  tcache_t* tcache, cache_bin_t* tbin, szind_t binind,
  bool* tcache_success) {
  void* ret;

  assert(tcache->arena != NULL);
  arena_tcache_fill_small(tsdn, arena, tcache, tbin, binind,
    config_prof ? tcache->prof_accumbytes : 0);
  if (config_prof) { tcache->prof_accumbytes = 0; }
  ret = cache_bin_alloc_easy(tbin, tcache_success);

  return ret;
}

/* Enabled with --enable-extra-size-check. */
static void tbin_extents_lookup_size_check(tsdn_t* tsdn, cache_bin_t* tbin,
  szind_t binind, size_t nflush, extent_t** extents) {
  rtree_ctx_t rtree_ctx_fallback;
  rtree_ctx_t* rtree_ctx = tsdn_rtree_ctx(tsdn, &rtree_ctx_fallback);

  /*
   * Verify that the items in the tcache all have the correct size; this
   * is useful for catching sized deallocation bugs, also to fail early
   * instead of corrupting metadata.  Since this can be turned on for opt
   * builds, avoid the branch in the loop.
   */
  szind_t szind;
  size_t sz_sum = binind * nflush;
  for (unsigned i = 0; i < nflush; i++) {
    rtree_extent_szind_read(tsdn, &extents_rtree, rtree_ctx,
      (uintptr_t) * (tbin->avail - 1 - i), true, &extents[i], &szind);
    sz_sum -= szind;
  }
  if (sz_sum != 0) {
    safety_check_fail(
      "<jemalloc>: size mismatch in thread cache "
      "detected, likely caused by sized deallocation bugs by "
      "application. Abort.\n");
    abort();
  }
}

void tcache_bin_flush_small(tsd_t* tsd, tcache_t* tcache, cache_bin_t* tbin,
  szind_t binind, unsigned rem) {
  bool merged_stats = false;

  assert(binind < SC_NBINS);
  assert((cache_bin_sz_t)rem <= tbin->ncached);

  arena_t* arena = tcache->arena;
  assert(arena != NULL);
  unsigned nflush = tbin->ncached - rem;
  VARIABLE_ARRAY(extent_t*, item_extent, nflush);

  /* Look up extent once per item. */
  if (config_opt_safety_checks) {
    tbin_extents_lookup_size_check(
      tsd_tsdn(tsd), tbin, binind, nflush, item_extent);
  } else {
    for (unsigned i = 0; i < nflush; i++) {
      item_extent[i] = iealloc(tsd_tsdn(tsd), *(tbin->avail - 1 - i));
    }
  }
  while (nflush > 0) {
    /* Lock the arena bin associated with the first object. */
    extent_t* extent = item_extent[0];
    unsigned bin_arena_ind = extent_arena_ind_get(extent);
    arena_t* bin_arena = arena_get(tsd_tsdn(tsd), bin_arena_ind, false);
    unsigned binshard = extent_binshard_get(extent);
    assert(binshard < bin_infos[binind].n_shards);
    bin_t* bin = &bin_arena->bins[binind].bin_shards[binshard];

    if (config_prof && bin_arena == arena) {
      if (arena_prof_accum(tsd_tsdn(tsd), arena, tcache->prof_accumbytes)) {
        prof_idump(tsd_tsdn(tsd));
      }
      tcache->prof_accumbytes = 0;
    }

    malloc_mutex_lock(tsd_tsdn(tsd), &bin->lock);
    if (config_stats && bin_arena == arena && !merged_stats) {
      merged_stats = true;
      bin->stats.nflushes++;
      bin->stats.nrequests += tbin->tstats.nrequests;
      tbin->tstats.nrequests = 0;
    }
    unsigned ndeferred = 0;
    for (unsigned i = 0; i < nflush; i++) {
      void* ptr = *(tbin->avail - 1 - i);
      extent = item_extent[i];
      assert(ptr != NULL && extent != NULL);

      if (extent_arena_ind_get(extent) == bin_arena_ind
        && extent_binshard_get(extent) == binshard) {
        arena_dalloc_bin_junked_locked(
          tsd_tsdn(tsd), bin_arena, bin, binind, extent, ptr);
      } else {
        /*
         * This object was allocated via a different
         * arena bin than the one that is currently
         * locked.  Stash the object, so that it can be
         * handled in a future pass.
         */
        *(tbin->avail - 1 - ndeferred) = ptr;
        item_extent[ndeferred] = extent;
        ndeferred++;
      }
    }
    malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
    arena_decay_ticks(tsd_tsdn(tsd), bin_arena, nflush - ndeferred);
    nflush = ndeferred;
  }
  if (config_stats && !merged_stats) {
    /*
     * The flush loop didn't happen to flush to this thread's
     * arena, so the stats didn't get merged.  Manually do so now.
     */
    unsigned binshard;
    bin_t* bin
      = arena_bin_choose_lock(tsd_tsdn(tsd), arena, binind, &binshard);
    bin->stats.nflushes++;
    bin->stats.nrequests += tbin->tstats.nrequests;
    tbin->tstats.nrequests = 0;
    malloc_mutex_unlock(tsd_tsdn(tsd), &bin->lock);
  }

  memmove(
    tbin->avail - rem, tbin->avail - tbin->ncached, rem * sizeof(void*));
  tbin->ncached = rem;
  if (tbin->ncached < tbin->low_water) { tbin->low_water = tbin->ncached; }
}

void tcache_bin_flush_large(tsd_t* tsd, cache_bin_t* tbin, szind_t binind,
  unsigned rem, tcache_t* tcache) {
  bool merged_stats = false;

  assert(binind < nhbins);
  assert((cache_bin_sz_t)rem <= tbin->ncached);

  arena_t* tcache_arena = tcache->arena;
  assert(tcache_arena != NULL);
  unsigned nflush = tbin->ncached - rem;
  VARIABLE_ARRAY(extent_t*, item_extent, nflush);

#ifndef JEMALLOC_EXTRA_SIZE_CHECK
  /* Look up extent once per item. */
  for (unsigned i = 0; i < nflush; i++) {
    item_extent[i] = iealloc(tsd_tsdn(tsd), *(tbin->avail - 1 - i));
  }
#else
  tbin_extents_lookup_size_check(
    tsd_tsdn(tsd), tbin, binind, nflush, item_extent);
#endif
  while (nflush > 0) {
    /* Lock the arena associated with the first object. */
    extent_t* extent = item_extent[0];
    unsigned locked_arena_ind = extent_arena_ind_get(extent);
    arena_t* locked_arena
      = arena_get(tsd_tsdn(tsd), locked_arena_ind, false);
    bool idump;

    if (config_prof) { idump = false; }

    bool lock_large = !arena_is_auto(locked_arena);
    if (lock_large) {
      malloc_mutex_lock(tsd_tsdn(tsd), &locked_arena->large_mtx);
    }
    for (unsigned i = 0; i < nflush; i++) {
      void* ptr = *(tbin->avail - 1 - i);
      assert(ptr != NULL);
      extent = item_extent[i];
      if (extent_arena_ind_get(extent) == locked_arena_ind) {
        large_dalloc_prep_junked_locked(tsd_tsdn(tsd), extent);
      }
    }
    if ((config_prof || config_stats) && (locked_arena == tcache_arena)) {
      if (config_prof) {
        idump = arena_prof_accum(
          tsd_tsdn(tsd), tcache_arena, tcache->prof_accumbytes);
        tcache->prof_accumbytes = 0;
      }
      if (config_stats) {
        merged_stats = true;
        arena_stats_large_flush_nrequests_add(tsd_tsdn(tsd),
          &tcache_arena->stats, binind, tbin->tstats.nrequests);
        tbin->tstats.nrequests = 0;
      }
    }
    if (lock_large) {
      malloc_mutex_unlock(tsd_tsdn(tsd), &locked_arena->large_mtx);
    }

    unsigned ndeferred = 0;
    for (unsigned i = 0; i < nflush; i++) {
      void* ptr = *(tbin->avail - 1 - i);
      extent = item_extent[i];
      assert(ptr != NULL && extent != NULL);

      if (extent_arena_ind_get(extent) == locked_arena_ind) {
        large_dalloc_finish(tsd_tsdn(tsd), extent);
      } else {
        /*
         * This object was allocated via a different
         * arena than the one that is currently locked.
         * Stash the object, so that it can be handled
         * in a future pass.
         */
        *(tbin->avail - 1 - ndeferred) = ptr;
        item_extent[ndeferred] = extent;
        ndeferred++;
      }
    }
    if (config_prof && idump) { prof_idump(tsd_tsdn(tsd)); }
    arena_decay_ticks(tsd_tsdn(tsd), locked_arena, nflush - ndeferred);
    nflush = ndeferred;
  }
  if (config_stats && !merged_stats) {
    /*
     * The flush loop didn't happen to flush to this thread's
     * arena, so the stats didn't get merged.  Manually do so now.
     */
    arena_stats_large_flush_nrequests_add(
      tsd_tsdn(tsd), &tcache_arena->stats, binind, tbin->tstats.nrequests);
    tbin->tstats.nrequests = 0;
  }

  memmove(
    tbin->avail - rem, tbin->avail - tbin->ncached, rem * sizeof(void*));
  tbin->ncached = rem;
  if (tbin->ncached < tbin->low_water) { tbin->low_water = tbin->ncached; }
}

void tcache_arena_associate(
  tsdn_t* tsdn, tcache_t* tcache, arena_t* arena) {
  assert(tcache->arena == NULL);
  tcache->arena = arena;

  if (config_stats) {
    /* Link into list of extant tcaches. */
    malloc_mutex_lock(tsdn, &arena->tcache_ql_mtx);

    ql_elm_new(tcache, link);
    ql_tail_insert(&arena->tcache_ql, tcache, link);
    cache_bin_array_descriptor_init(&tcache->cache_bin_array_descriptor,
      tcache->bins_small, tcache->bins_large);
    ql_tail_insert(&arena->cache_bin_array_descriptor_ql,
      &tcache->cache_bin_array_descriptor, link);

    malloc_mutex_unlock(tsdn, &arena->tcache_ql_mtx);
  }
}

static void tcache_arena_dissociate(tsdn_t* tsdn, tcache_t* tcache) {
  arena_t* arena = tcache->arena;
  assert(arena != NULL);
  if (config_stats) {
    /* Unlink from list of extant tcaches. */
    malloc_mutex_lock(tsdn, &arena->tcache_ql_mtx);
    if (config_debug) {
      bool in_ql = false;
      tcache_t* iter;
      ql_foreach(iter, &arena->tcache_ql, link) {
        if (iter == tcache) {
          in_ql = true;
          break;
        }
      }
      assert(in_ql);
    }
    ql_remove(&arena->tcache_ql, tcache, link);
    ql_remove(&arena->cache_bin_array_descriptor_ql,
      &tcache->cache_bin_array_descriptor, link);
    tcache_stats_merge(tsdn, tcache, arena);
    malloc_mutex_unlock(tsdn, &arena->tcache_ql_mtx);
  }
  tcache->arena = NULL;
}

void tcache_arena_reassociate(
  tsdn_t* tsdn, tcache_t* tcache, arena_t* arena) {
  tcache_arena_dissociate(tsdn, tcache);
  tcache_arena_associate(tsdn, tcache, arena);
}

bool tsd_tcache_enabled_data_init(tsd_t* tsd) {
  /* Called upon tsd initialization. */
  tsd_tcache_enabled_set(tsd, opt_tcache);
  tsd_slow_update(tsd);

  if (opt_tcache) {
    /* Trigger tcache init. */
    tsd_tcache_data_init(tsd);
  }

  return false;
}

/* Initialize auto tcache (embedded in TSD). */
static void tcache_init(tsd_t* tsd, tcache_t* tcache, void* avail_stack) {
  memset(&tcache->link, 0, sizeof(ql_elm(tcache_t)));
  tcache->prof_accumbytes = 0;
  tcache->next_gc_bin = 0;
  tcache->arena = NULL;

  ticker_init(&tcache->gc_ticker, TCACHE_GC_INCR);

  size_t stack_offset = 0;
  assert((TCACHE_NSLOTS_SMALL_MAX & 1U) == 0);
  memset(tcache->bins_small, 0, sizeof(cache_bin_t) * SC_NBINS);
  memset(tcache->bins_large, 0, sizeof(cache_bin_t) * (nhbins - SC_NBINS));
  unsigned i = 0;
  for (; i < SC_NBINS; i++) {
    tcache->lg_fill_div[i] = 1;
    stack_offset += tcache_bin_info[i].ncached_max * sizeof(void*);
    /*
     * avail points past the available space.  Allocations will
     * access the slots toward higher addresses (for the benefit of
     * prefetch).
     */
    tcache_small_bin_get(tcache, i)->avail
      = (void**)((uintptr_t)avail_stack + (uintptr_t)stack_offset);
  }
  for (; i < nhbins; i++) {
    stack_offset += tcache_bin_info[i].ncached_max * sizeof(void*);
    tcache_large_bin_get(tcache, i)->avail
      = (void**)((uintptr_t)avail_stack + (uintptr_t)stack_offset);
  }
  assert(stack_offset == stack_nelms * sizeof(void*));
}

/* Initialize auto tcache (embedded in TSD). */
bool tsd_tcache_data_init(tsd_t* tsd) {
  tcache_t* tcache = tsd_tcachep_get_unsafe(tsd);
  assert(tcache_small_bin_get(tcache, 0)->avail == NULL);
  size_t size = stack_nelms * sizeof(void*);
  /* Avoid false cacheline sharing. */
  size = sz_sa2u(size, CACHELINE);

  void* avail_array = ipallocztm(tsd_tsdn(tsd), size, CACHELINE, true, NULL,
    true, arena_get(TSDN_NULL, 0, true));
  if (avail_array == NULL) { return true; }

  tcache_init(tsd, tcache, avail_array);
  /*
   * Initialization is a bit tricky here.  After malloc init is done, all
   * threads can rely on arena_choose and associate tcache accordingly.
   * However, the thread that does actual malloc bootstrapping relies on
   * functional tsd, and it can only rely on a0.  In that case, we
   * associate its tcache to a0 temporarily, and later on
   * arena_choose_hard() will re-associate properly.
   */
  tcache->arena = NULL;
  arena_t* arena;
  if (!malloc_initialized()) {
    /* If in initialization, assign to a0. */
    arena = arena_get(tsd_tsdn(tsd), 0, false);
    tcache_arena_associate(tsd_tsdn(tsd), tcache, arena);
  } else {
    arena = arena_choose(tsd, NULL);
    /* This may happen if thread.tcache.enabled is used. */
    if (tcache->arena == NULL) {
      tcache_arena_associate(tsd_tsdn(tsd), tcache, arena);
    }
  }
  assert(arena == tcache->arena);

  return false;
}

/* Created manual tcache for tcache.create mallctl. */
tcache_t* tcache_create_explicit(tsd_t* tsd) {
  tcache_t* tcache;
  size_t size, stack_offset;

  size = sizeof(tcache_t);
  /* Naturally align the pointer stacks. */
  size = PTR_CEILING(size);
  stack_offset = size;
  size += stack_nelms * sizeof(void*);
  /* Avoid false cacheline sharing. */
  size = sz_sa2u(size, CACHELINE);

  tcache = ipallocztm(tsd_tsdn(tsd), size, CACHELINE, true, NULL, true,
    arena_get(TSDN_NULL, 0, true));
  if (tcache == NULL) { return NULL; }

  tcache_init(
    tsd, tcache, (void*)((uintptr_t)tcache + (uintptr_t)stack_offset));
  tcache_arena_associate(tsd_tsdn(tsd), tcache, arena_ichoose(tsd, NULL));

  return tcache;
}

static void tcache_flush_cache(tsd_t* tsd, tcache_t* tcache) {
  assert(tcache->arena != NULL);

  for (unsigned i = 0; i < SC_NBINS; i++) {
    cache_bin_t* tbin = tcache_small_bin_get(tcache, i);
    tcache_bin_flush_small(tsd, tcache, tbin, i, 0);

    if (config_stats) { assert(tbin->tstats.nrequests == 0); }
  }
  for (unsigned i = SC_NBINS; i < nhbins; i++) {
    cache_bin_t* tbin = tcache_large_bin_get(tcache, i);
    tcache_bin_flush_large(tsd, tbin, i, 0, tcache);

    if (config_stats) { assert(tbin->tstats.nrequests == 0); }
  }

  if (config_prof && tcache->prof_accumbytes > 0
    && arena_prof_accum(
      tsd_tsdn(tsd), tcache->arena, tcache->prof_accumbytes)) {
    prof_idump(tsd_tsdn(tsd));
  }
}

void tcache_flush(tsd_t* tsd) {
  assert(tcache_available(tsd));
  tcache_flush_cache(tsd, tsd_tcachep_get(tsd));
}

static void tcache_destroy(tsd_t* tsd, tcache_t* tcache, bool tsd_tcache) {
  tcache_flush_cache(tsd, tcache);
  arena_t* arena = tcache->arena;
  tcache_arena_dissociate(tsd_tsdn(tsd), tcache);

  if (tsd_tcache) {
    /* Release the avail array for the TSD embedded auto tcache. */
    void* avail_array
      = (void*)((uintptr_t)tcache_small_bin_get(tcache, 0)->avail
        - (uintptr_t)tcache_bin_info[0].ncached_max * sizeof(void*));
    idalloctm(tsd_tsdn(tsd), avail_array, NULL, NULL, true, true);
  } else {
    /* Release both the tcache struct and avail array. */
    idalloctm(tsd_tsdn(tsd), tcache, NULL, NULL, true, true);
  }

  /*
   * The deallocation and tcache flush above may not trigger decay since
   * we are on the tcache shutdown path (potentially with non-nominal
   * tsd).  Manually trigger decay to avoid pathological cases.  Also
   * include arena 0 because the tcache array is allocated from it.
   */
  arena_decay(
    tsd_tsdn(tsd), arena_get(tsd_tsdn(tsd), 0, false), false, false);

  if (arena_nthreads_get(arena, false) == 0
    && !background_thread_enabled()) {
    /* Force purging when no threads assigned to the arena anymore. */
    arena_decay(tsd_tsdn(tsd), arena, false, true);
  } else {
    arena_decay(tsd_tsdn(tsd), arena, false, false);
  }
}

/* For auto tcache (embedded in TSD) only. */
void tcache_cleanup(tsd_t* tsd) {
  tcache_t* tcache = tsd_tcachep_get(tsd);
  if (!tcache_available(tsd)) {
    assert(tsd_tcache_enabled_get(tsd) == false);
    if (config_debug) {
      assert(tcache_small_bin_get(tcache, 0)->avail == NULL);
    }
    return;
  }
  assert(tsd_tcache_enabled_get(tsd));
  assert(tcache_small_bin_get(tcache, 0)->avail != NULL);

  tcache_destroy(tsd, tcache, true);
  if (config_debug) { tcache_small_bin_get(tcache, 0)->avail = NULL; }
}

void tcache_stats_merge(tsdn_t* tsdn, tcache_t* tcache, arena_t* arena) {
  unsigned i;

  cassert(config_stats);

  /* Merge and reset tcache stats. */
  for (i = 0; i < SC_NBINS; i++) {
    cache_bin_t* tbin = tcache_small_bin_get(tcache, i);
    unsigned binshard;
    bin_t* bin = arena_bin_choose_lock(tsdn, arena, i, &binshard);
    bin->stats.nrequests += tbin->tstats.nrequests;
    malloc_mutex_unlock(tsdn, &bin->lock);
    tbin->tstats.nrequests = 0;
  }

  for (; i < nhbins; i++) {
    cache_bin_t* tbin = tcache_large_bin_get(tcache, i);
    arena_stats_large_flush_nrequests_add(
      tsdn, &arena->stats, i, tbin->tstats.nrequests);
    tbin->tstats.nrequests = 0;
  }
}

static bool tcaches_create_prep(tsd_t* tsd) {
  bool err;

  malloc_mutex_lock(tsd_tsdn(tsd), &tcaches_mtx);

  if (tcaches == NULL) {
    tcaches = base_alloc(tsd_tsdn(tsd), b0get(),
      sizeof(tcache_t*) * (MALLOCX_TCACHE_MAX + 1), CACHELINE);
    if (tcaches == NULL) {
      err = true;
      goto label_return;
    }
  }

  if (tcaches_avail == NULL && tcaches_past > MALLOCX_TCACHE_MAX) {
    err = true;
    goto label_return;
  }

  err = false;
label_return:
  malloc_mutex_unlock(tsd_tsdn(tsd), &tcaches_mtx);
  return err;
}

bool tcaches_create(tsd_t* tsd, unsigned* r_ind) {
  witness_assert_depth(tsdn_witness_tsdp_get(tsd_tsdn(tsd)), 0);

  bool err;

  if (tcaches_create_prep(tsd)) {
    err = true;
    goto label_return;
  }

  tcache_t* tcache = tcache_create_explicit(tsd);
  if (tcache == NULL) {
    err = true;
    goto label_return;
  }

  tcaches_t* elm;
  malloc_mutex_lock(tsd_tsdn(tsd), &tcaches_mtx);
  if (tcaches_avail != NULL) {
    elm = tcaches_avail;
    tcaches_avail = tcaches_avail->next;
    elm->tcache = tcache;
    *r_ind = (unsigned)(elm - tcaches);
  } else {
    elm = &tcaches[tcaches_past];
    elm->tcache = tcache;
    *r_ind = tcaches_past;
    tcaches_past++;
  }
  malloc_mutex_unlock(tsd_tsdn(tsd), &tcaches_mtx);

  err = false;
label_return:
  witness_assert_depth(tsdn_witness_tsdp_get(tsd_tsdn(tsd)), 0);
  return err;
}

static tcache_t* tcaches_elm_remove(
  tsd_t* tsd, tcaches_t* elm, bool allow_reinit) {
  malloc_mutex_assert_owner(tsd_tsdn(tsd), &tcaches_mtx);

  if (elm->tcache == NULL) { return NULL; }
  tcache_t* tcache = elm->tcache;
  if (allow_reinit) {
    elm->tcache = TCACHES_ELM_NEED_REINIT;
  } else {
    elm->tcache = NULL;
  }

  if (tcache == TCACHES_ELM_NEED_REINIT) { return NULL; }
  return tcache;
}

void tcaches_flush(tsd_t* tsd, unsigned ind) {
  malloc_mutex_lock(tsd_tsdn(tsd), &tcaches_mtx);
  tcache_t* tcache = tcaches_elm_remove(tsd, &tcaches[ind], true);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tcaches_mtx);
  if (tcache != NULL) {
    /* Destroy the tcache; recreate in tcaches_get() if needed. */
    tcache_destroy(tsd, tcache, false);
  }
}

void tcaches_destroy(tsd_t* tsd, unsigned ind) {
  malloc_mutex_lock(tsd_tsdn(tsd), &tcaches_mtx);
  tcaches_t* elm = &tcaches[ind];
  tcache_t* tcache = tcaches_elm_remove(tsd, elm, false);
  elm->next = tcaches_avail;
  tcaches_avail = elm;
  malloc_mutex_unlock(tsd_tsdn(tsd), &tcaches_mtx);
  if (tcache != NULL) { tcache_destroy(tsd, tcache, false); }
}

bool tcache_boot(tsdn_t* tsdn) {
  /* If necessary, clamp opt_lg_tcache_max. */
  if (opt_lg_tcache_max < 0
    || (ZU(1) << opt_lg_tcache_max) < SC_SMALL_MAXCLASS) {
    tcache_maxclass = SC_SMALL_MAXCLASS;
  } else {
    tcache_maxclass = (ZU(1) << opt_lg_tcache_max);
  }

  if (malloc_mutex_init(&tcaches_mtx, "tcaches", WITNESS_RANK_TCACHES,
        malloc_mutex_rank_exclusive)) {
    return true;
  }

  nhbins = sz_size2index(tcache_maxclass) + 1;

  /* Initialize tcache_bin_info. */
  tcache_bin_info = (cache_bin_info_t*)base_alloc(
    tsdn, b0get(), nhbins * sizeof(cache_bin_info_t), CACHELINE);
  if (tcache_bin_info == NULL) { return true; }
  stack_nelms = 0;
  unsigned i;
  for (i = 0; i < SC_NBINS; i++) {
    if ((bin_infos[i].nregs << 1) <= TCACHE_NSLOTS_SMALL_MIN) {
      tcache_bin_info[i].ncached_max = TCACHE_NSLOTS_SMALL_MIN;
    } else if ((bin_infos[i].nregs << 1) <= TCACHE_NSLOTS_SMALL_MAX) {
      tcache_bin_info[i].ncached_max = (bin_infos[i].nregs << 1);
    } else {
      tcache_bin_info[i].ncached_max = TCACHE_NSLOTS_SMALL_MAX;
    }
    stack_nelms += tcache_bin_info[i].ncached_max;
  }
  for (; i < nhbins; i++) {
    tcache_bin_info[i].ncached_max = TCACHE_NSLOTS_LARGE;
    stack_nelms += tcache_bin_info[i].ncached_max;
  }

  return false;
}

void tcache_prefork(tsdn_t* tsdn) {
  if (!config_prof && opt_tcache) {
    malloc_mutex_prefork(tsdn, &tcaches_mtx);
  }
}

void tcache_postfork_parent(tsdn_t* tsdn) {
  if (!config_prof && opt_tcache) {
    malloc_mutex_postfork_parent(tsdn, &tcaches_mtx);
  }
}

void tcache_postfork_child(tsdn_t* tsdn) {
  if (!config_prof && opt_tcache) {
    malloc_mutex_postfork_child(tsdn, &tcaches_mtx);
  }
}

/*
 * The hooks are a little bit screwy -- they're not genuinely exported in
 * the sense that we want them available to end-users, but we do want them
 * visible from outside the generated library, so that we can use them in
 * test code.
 */
JEMALLOC_EXPORT
void (*test_hooks_arena_new_hook)() = NULL;

JEMALLOC_EXPORT
void (*test_hooks_libc_hook)() = NULL;
#define JEMALLOC_TICKER_C_
#define JEMALLOC_TSD_C_

/******************************************************************************/
/* Data. */

static unsigned ncleanups;
static malloc_tsd_cleanup_t cleanups[MALLOC_TSD_CLEANUPS_MAX];

/* TSD_INITIALIZER triggers "-Wmissing-field-initializer" */
JEMALLOC_DIAGNOSTIC_PUSH
JEMALLOC_DIAGNOSTIC_IGNORE_MISSING_STRUCT_FIELD_INITIALIZERS

#ifdef JEMALLOC_MALLOC_THREAD_CLEANUP
JEMALLOC_TSD_TYPE_ATTR(tsd_t) tsd_tls = TSD_INITIALIZER;
JEMALLOC_TSD_TYPE_ATTR(bool) JEMALLOC_TLS_MODEL tsd_initialized = false;
bool tsd_booted = false;
#elif (defined(JEMALLOC_TLS))
JEMALLOC_TSD_TYPE_ATTR(tsd_t) tsd_tls = TSD_INITIALIZER;
pthread_key_t tsd_tsd;
bool tsd_booted = false;
#elif (defined(_WIN32))
DWORD tsd_tsd;
tsd_wrapper_t tsd_boot_wrapper = { false, TSD_INITIALIZER };
bool tsd_booted = false;
#else

/*
 * This contains a mutex, but it's pretty convenient to allow the mutex code
 * to have a dependency on tsd.  So we define the struct here, and only
 * refer to it by pointer in the header.
 */
struct tsd_init_head_s {
  ql_head(tsd_init_block_t) blocks;
  malloc_mutex_t lock;
};

pthread_key_t tsd_tsd;
tsd_init_head_t tsd_init_head
  = { ql_head_initializer(blocks), MALLOC_MUTEX_INITIALIZER };

tsd_wrapper_t tsd_boot_wrapper = { false, TSD_INITIALIZER };
bool tsd_booted = false;
#endif

JEMALLOC_DIAGNOSTIC_POP

/******************************************************************************/

/* A list of all the tsds in the nominal state. */
typedef ql_head(tsd_t) tsd_list_t;
static tsd_list_t tsd_nominal_tsds = ql_head_initializer(tsd_nominal_tsds);
static malloc_mutex_t tsd_nominal_tsds_lock;

/* How many slow-path-enabling features are turned on. */
static atomic_u32_t tsd_global_slow_count = ATOMIC_INIT(0);

static bool tsd_in_nominal_list(tsd_t* tsd) {
  tsd_t* tsd_list;
  bool found = false;
  /*
   * We don't know that tsd is nominal; it might not be safe to get data
   * out of it here.
   */
  malloc_mutex_lock(TSDN_NULL, &tsd_nominal_tsds_lock);
  ql_foreach(tsd_list, &tsd_nominal_tsds, TSD_MANGLE(tcache).tsd_link) {
    if (tsd == tsd_list) {
      found = true;
      break;
    }
  }
  malloc_mutex_unlock(TSDN_NULL, &tsd_nominal_tsds_lock);
  return found;
}

static void tsd_add_nominal(tsd_t* tsd) {
  assert(!tsd_in_nominal_list(tsd));
  assert(tsd_state_get(tsd) <= tsd_state_nominal_max);
  ql_elm_new(tsd, TSD_MANGLE(tcache).tsd_link);
  malloc_mutex_lock(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
  ql_tail_insert(&tsd_nominal_tsds, tsd, TSD_MANGLE(tcache).tsd_link);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
}

static void tsd_remove_nominal(tsd_t* tsd) {
  assert(tsd_in_nominal_list(tsd));
  assert(tsd_state_get(tsd) <= tsd_state_nominal_max);
  malloc_mutex_lock(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
  ql_remove(&tsd_nominal_tsds, tsd, TSD_MANGLE(tcache).tsd_link);
  malloc_mutex_unlock(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
}

static void tsd_force_recompute(tsdn_t* tsdn) {
  /*
   * The stores to tsd->state here need to synchronize with the exchange
   * in tsd_slow_update.
   */
  atomic_fence(ATOMIC_RELEASE);
  malloc_mutex_lock(tsdn, &tsd_nominal_tsds_lock);
  tsd_t* remote_tsd;
  ql_foreach(remote_tsd, &tsd_nominal_tsds, TSD_MANGLE(tcache).tsd_link) {
    assert(tsd_atomic_load(&remote_tsd->state, ATOMIC_RELAXED)
      <= tsd_state_nominal_max);
    tsd_atomic_store(
      &remote_tsd->state, tsd_state_nominal_recompute, ATOMIC_RELAXED);
  }
  malloc_mutex_unlock(tsdn, &tsd_nominal_tsds_lock);
}

void tsd_global_slow_inc(tsdn_t* tsdn) {
  atomic_fetch_add_u32(&tsd_global_slow_count, 1, ATOMIC_RELAXED);
  /*
   * We unconditionally force a recompute, even if the global slow count
   * was already positive.  If we didn't, then it would be possible for us
   * to return to the user, have the user synchronize externally with some
   * other thread, and then have that other thread not have picked up the
   * update yet (since the original incrementing thread might still be
   * making its way through the tsd list).
   */
  tsd_force_recompute(tsdn);
}

void tsd_global_slow_dec(tsdn_t* tsdn) {
  atomic_fetch_sub_u32(&tsd_global_slow_count, 1, ATOMIC_RELAXED);
  /* See the note in ..._inc(). */
  tsd_force_recompute(tsdn);
}

static bool tsd_local_slow(tsd_t* tsd) {
  return !tsd_tcache_enabled_get(tsd) || tsd_reentrancy_level_get(tsd) > 0;
}

bool tsd_global_slow() {
  return atomic_load_u32(&tsd_global_slow_count, ATOMIC_RELAXED) > 0;
}

/******************************************************************************/

static uint8_t tsd_state_compute(tsd_t* tsd) {
  if (!tsd_nominal(tsd)) { return tsd_state_get(tsd); }
  /* We're in *a* nominal state; but which one? */
  if (malloc_slow || tsd_local_slow(tsd) || tsd_global_slow()) {
    return tsd_state_nominal_slow;
  } else {
    return tsd_state_nominal;
  }
}

void tsd_slow_update(tsd_t* tsd) {
  uint8_t old_state;
  do {
    uint8_t new_state = tsd_state_compute(tsd);
    old_state = tsd_atomic_exchange(&tsd->state, new_state, ATOMIC_ACQUIRE);
  } while (old_state == tsd_state_nominal_recompute);
}

void tsd_state_set(tsd_t* tsd, uint8_t new_state) {
  /* Only the tsd module can change the state *to* recompute. */
  assert(new_state != tsd_state_nominal_recompute);
  uint8_t old_state = tsd_atomic_load(&tsd->state, ATOMIC_RELAXED);
  if (old_state > tsd_state_nominal_max) {
    /*
     * Not currently in the nominal list, but it might need to be
     * inserted there.
     */
    assert(!tsd_in_nominal_list(tsd));
    tsd_atomic_store(&tsd->state, new_state, ATOMIC_RELAXED);
    if (new_state <= tsd_state_nominal_max) { tsd_add_nominal(tsd); }
  } else {
    /*
     * We're currently nominal.  If the new state is non-nominal,
     * great; we take ourselves off the list and just enter the new
     * state.
     */
    assert(tsd_in_nominal_list(tsd));
    if (new_state > tsd_state_nominal_max) {
      tsd_remove_nominal(tsd);
      tsd_atomic_store(&tsd->state, new_state, ATOMIC_RELAXED);
    } else {
      /*
       * This is the tricky case.  We're transitioning from
       * one nominal state to another.  The caller can't know
       * about any races that are occuring at the same time,
       * so we always have to recompute no matter what.
       */
      tsd_slow_update(tsd);
    }
  }
}

static bool tsd_data_init(tsd_t* tsd) {
  /*
   * We initialize the rtree context first (before the tcache), since the
   * tcache initialization depends on it.
   */
  rtree_ctx_data_init(tsd_rtree_ctxp_get_unsafe(tsd));

  /*
   * A nondeterministic seed based on the address of tsd reduces
   * the likelihood of lockstep non-uniform cache index
   * utilization among identical concurrent processes, but at the
   * cost of test repeatability.  For debug builds, instead use a
   * deterministic seed.
   */
  *tsd_offset_statep_get(tsd) = config_debug ? 0 : (uint64_t)(uintptr_t)tsd;

  return tsd_tcache_enabled_data_init(tsd);
}

static void assert_tsd_data_cleanup_done(tsd_t* tsd) {
  assert(!tsd_nominal(tsd));
  assert(!tsd_in_nominal_list(tsd));
  assert(*tsd_arenap_get_unsafe(tsd) == NULL);
  assert(*tsd_iarenap_get_unsafe(tsd) == NULL);
  assert(*tsd_arenas_tdata_bypassp_get_unsafe(tsd) == true);
  assert(*tsd_arenas_tdatap_get_unsafe(tsd) == NULL);
  assert(*tsd_tcache_enabledp_get_unsafe(tsd) == false);
  assert(*tsd_prof_tdatap_get_unsafe(tsd) == NULL);
}

static bool tsd_data_init_nocleanup(tsd_t* tsd) {
  assert(tsd_state_get(tsd) == tsd_state_reincarnated
    || tsd_state_get(tsd) == tsd_state_minimal_initialized);
  /*
   * During reincarnation, there is no guarantee that the cleanup function
   * will be called (deallocation may happen after all tsd destructors).
   * We set up tsd in a way that no cleanup is needed.
   */
  rtree_ctx_data_init(tsd_rtree_ctxp_get_unsafe(tsd));
  *tsd_arenas_tdata_bypassp_get(tsd) = true;
  *tsd_tcache_enabledp_get_unsafe(tsd) = false;
  *tsd_reentrancy_levelp_get(tsd) = 1;
  assert_tsd_data_cleanup_done(tsd);

  return false;
}

tsd_t* tsd_fetch_slow(tsd_t* tsd, bool minimal) {
  assert(!tsd_fast(tsd));

  if (tsd_state_get(tsd) == tsd_state_nominal_slow) {
    /*
     * On slow path but no work needed.  Note that we can't
     * necessarily *assert* that we're slow, because we might be
     * slow because of an asynchronous modification to global state,
     * which might be asynchronously modified *back*.
     */
  } else if (tsd_state_get(tsd) == tsd_state_nominal_recompute) {
    tsd_slow_update(tsd);
  } else if (tsd_state_get(tsd) == tsd_state_uninitialized) {
    if (!minimal) {
      if (tsd_booted) {
        tsd_state_set(tsd, tsd_state_nominal);
        tsd_slow_update(tsd);
        /* Trigger cleanup handler registration. */
        tsd_set(tsd);
        tsd_data_init(tsd);
      }
    } else {
      tsd_state_set(tsd, tsd_state_minimal_initialized);
      tsd_set(tsd);
      tsd_data_init_nocleanup(tsd);
    }
  } else if (tsd_state_get(tsd) == tsd_state_minimal_initialized) {
    if (!minimal) {
      /* Switch to fully initialized. */
      tsd_state_set(tsd, tsd_state_nominal);
      assert(*tsd_reentrancy_levelp_get(tsd) >= 1);
      (*tsd_reentrancy_levelp_get(tsd))--;
      tsd_slow_update(tsd);
      tsd_data_init(tsd);
    } else {
      assert_tsd_data_cleanup_done(tsd);
    }
  } else if (tsd_state_get(tsd) == tsd_state_purgatory) {
    tsd_state_set(tsd, tsd_state_reincarnated);
    tsd_set(tsd);
    tsd_data_init_nocleanup(tsd);
  } else {
    assert(tsd_state_get(tsd) == tsd_state_reincarnated);
  }

  return tsd;
}

void* malloc_tsd_malloc(size_t size) {
  return a0malloc(CACHELINE_CEILING(size));
}

void malloc_tsd_dalloc(void* wrapper) { a0dalloc(wrapper); }

#if defined(JEMALLOC_MALLOC_THREAD_CLEANUP) || defined(_WIN32)
#ifndef _WIN32
JEMALLOC_EXPORT
#endif
void _malloc_thread_cleanup(void) {
  bool pending[MALLOC_TSD_CLEANUPS_MAX], again;
  unsigned i;

  for (i = 0; i < ncleanups; i++) { pending[i] = true; }

  do {
    again = false;
    for (i = 0; i < ncleanups; i++) {
      if (pending[i]) {
        pending[i] = cleanups[i]();
        if (pending[i]) { again = true; }
      }
    }
  } while (again);
}
#endif

void malloc_tsd_cleanup_register(bool (*f)(void)) {
  assert(ncleanups < MALLOC_TSD_CLEANUPS_MAX);
  cleanups[ncleanups] = f;
  ncleanups++;
}

static void tsd_do_data_cleanup(tsd_t* tsd) {
  prof_tdata_cleanup(tsd);
  iarena_cleanup(tsd);
  arena_cleanup(tsd);
  arenas_tdata_cleanup(tsd);
  tcache_cleanup(tsd);
  witnesses_cleanup(tsd_witness_tsdp_get_unsafe(tsd));
}

void tsd_cleanup(void* arg) {
  tsd_t* tsd = (tsd_t*)arg;

  switch (tsd_state_get(tsd)) {
  case tsd_state_uninitialized:
    /* Do nothing. */
    break;
  case tsd_state_minimal_initialized:
    /* This implies the thread only did free() in its life time. */
    /* Fall through. */
  case tsd_state_reincarnated:
    /*
     * Reincarnated means another destructor deallocated memory
     * after the destructor was called.  Cleanup isn't required but
     * is still called for testing and completeness.
     */
    assert_tsd_data_cleanup_done(tsd);
    /* Fall through. */
  case tsd_state_nominal:
  case tsd_state_nominal_slow:
    tsd_do_data_cleanup(tsd);
    tsd_state_set(tsd, tsd_state_purgatory);
    tsd_set(tsd);
    break;
  case tsd_state_purgatory:
    /*
     * The previous time this destructor was called, we set the
     * state to tsd_state_purgatory so that other destructors
     * wouldn't cause re-creation of the tsd.  This time, do
     * nothing, and do not request another callback.
     */
    break;
  default: not_reached();
  }
#ifdef JEMALLOC_JET
  test_callback_t test_callback = *tsd_test_callbackp_get_unsafe(tsd);
  int* data = tsd_test_datap_get_unsafe(tsd);
  if (test_callback != NULL) { test_callback(data); }
#endif
}

tsd_t* malloc_tsd_boot0(void) {
  tsd_t* tsd;

  ncleanups = 0;
  if (malloc_mutex_init(&tsd_nominal_tsds_lock, "tsd_nominal_tsds_lock",
        WITNESS_RANK_OMIT, malloc_mutex_rank_exclusive)) {
    return NULL;
  }
  if (tsd_boot0()) { return NULL; }
  tsd = tsd_fetch();
  *tsd_arenas_tdata_bypassp_get(tsd) = true;
  return tsd;
}

void malloc_tsd_boot1(void) {
  tsd_boot1();
  tsd_t* tsd = tsd_fetch();
  /* malloc_slow has been set properly.  Update tsd_slow. */
  tsd_slow_update(tsd);
  *tsd_arenas_tdata_bypassp_get(tsd) = false;
}

#ifdef _WIN32
static BOOL WINAPI _tls_callback(
  HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) {
  switch (fdwReason) {
#ifdef JEMALLOC_LAZY_LOCK
  case DLL_THREAD_ATTACH: isthreaded = true; break;
#endif
  case DLL_THREAD_DETACH: _malloc_thread_cleanup(); break;
  default: break;
  }
  return true;
}

/*
 * We need to be able to say "read" here (in the "pragma section"), but have
 * hooked "read". We won't read for the rest of the file, so we can get away
 * with unhooking.
 */
#ifdef read
#undef read
#endif

#ifdef _MSC_VER
#ifdef _M_IX86
#pragma comment(linker, "/INCLUDE:__tls_used")
#pragma comment(linker, "/INCLUDE:_tls_callback")
#else
#pragma comment(linker, "/INCLUDE:_tls_used")
#pragma comment(linker, "/INCLUDE:" STRINGIFY(tls_callback))
#endif
#pragma section(".CRT$XLY", long, read)
#endif
JEMALLOC_SECTION(".CRT$XLY")
JEMALLOC_ATTR(used)
BOOL(WINAPI* const tls_callback)
(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) = _tls_callback;
#endif

#if (!defined(JEMALLOC_MALLOC_THREAD_CLEANUP) && !defined(JEMALLOC_TLS)    \
  && !defined(_WIN32))
void* tsd_init_check_recursion(
  tsd_init_head_t* head, tsd_init_block_t* block) {
  pthread_t self = pthread_self();
  tsd_init_block_t* iter;

  /* Check whether this thread has already inserted into the list. */
  malloc_mutex_lock(TSDN_NULL, &head->lock);
  ql_foreach(iter, &head->blocks, link) {
    if (iter->thread == self) {
      malloc_mutex_unlock(TSDN_NULL, &head->lock);
      return iter->data;
    }
  }
  /* Insert block into list. */
  ql_elm_new(block, link);
  block->thread = self;
  ql_tail_insert(&head->blocks, block, link);
  malloc_mutex_unlock(TSDN_NULL, &head->lock);
  return NULL;
}

void tsd_init_finish(tsd_init_head_t* head, tsd_init_block_t* block) {
  malloc_mutex_lock(TSDN_NULL, &head->lock);
  ql_remove(&head->blocks, block, link);
  malloc_mutex_unlock(TSDN_NULL, &head->lock);
}
#endif

void tsd_prefork(tsd_t* tsd) {
  malloc_mutex_prefork(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
}

void tsd_postfork_parent(tsd_t* tsd) {
  malloc_mutex_postfork_parent(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
}

void tsd_postfork_child(tsd_t* tsd) {
  malloc_mutex_postfork_child(tsd_tsdn(tsd), &tsd_nominal_tsds_lock);
  ql_new(&tsd_nominal_tsds);

  if (tsd_state_get(tsd) <= tsd_state_nominal_max) { tsd_add_nominal(tsd); }
}
#define JEMALLOC_WITNESS_C_

#include "jemalloc/internal/malloc_io.h"

void witness_init(witness_t* witness, const char* name, witness_rank_t rank,
  witness_comp_t* comp, void* opaque) {
  witness->name = name;
  witness->rank = rank;
  witness->comp = comp;
  witness->opaque = opaque;
}

static void witness_lock_error_impl(
  const witness_list_t* witnesses, const witness_t* witness) {
  witness_t* w;

  malloc_printf("<jemalloc>: Lock rank order reversal:");
  ql_foreach(w, witnesses, link) {
    malloc_printf(" %s(%u)", w->name, w->rank);
  }
  malloc_printf(" %s(%u)\n", witness->name, witness->rank);
  abort();
}
witness_lock_error_t* JET_MUTABLE witness_lock_error
  = witness_lock_error_impl;

static void witness_owner_error_impl(const witness_t* witness) {
  malloc_printf(
    "<jemalloc>: Should own %s(%u)\n", witness->name, witness->rank);
  abort();
}
witness_owner_error_t* JET_MUTABLE witness_owner_error
  = witness_owner_error_impl;

static void witness_not_owner_error_impl(const witness_t* witness) {
  malloc_printf(
    "<jemalloc>: Should not own %s(%u)\n", witness->name, witness->rank);
  abort();
}
witness_not_owner_error_t* JET_MUTABLE witness_not_owner_error
  = witness_not_owner_error_impl;

static void witness_depth_error_impl(const witness_list_t* witnesses,
  witness_rank_t rank_inclusive, unsigned depth) {
  witness_t* w;

  malloc_printf("<jemalloc>: Should own %u lock%s of rank >= %u:", depth,
    (depth != 1) ? "s" : "", rank_inclusive);
  ql_foreach(w, witnesses, link) {
    malloc_printf(" %s(%u)", w->name, w->rank);
  }
  malloc_printf("\n");
  abort();
}
witness_depth_error_t* JET_MUTABLE witness_depth_error
  = witness_depth_error_impl;

void witnesses_cleanup(witness_tsd_t* witness_tsd) {
  witness_assert_lockless(witness_tsd_tsdn(witness_tsd));

  /* Do nothing. */
}

void witness_prefork(witness_tsd_t* witness_tsd) {
  if (!config_debug) { return; }
  witness_tsd->forking = true;
}

void witness_postfork_parent(witness_tsd_t* witness_tsd) {
  if (!config_debug) { return; }
  witness_tsd->forking = false;
}

void witness_postfork_child(witness_tsd_t* witness_tsd) {
  if (!config_debug) { return; }
#ifndef JEMALLOC_MUTEX_INIT_CB
  witness_list_t* witnesses;

  witnesses = &witness_tsd->witnesses;
  ql_new(witnesses);
#endif
  witness_tsd->forking = false;
}
