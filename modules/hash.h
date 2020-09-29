/* The MIT License

   Copyright (c) 2008, 2009, 2011 by Attractive Chaos <attractor@live.co.uk>
   (c) 2019, 2020 The Jet Language Team <sushpa@jetpilots.dev>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

#ifndef HAVE_DICT
#define HAVE_DICT

#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define jet_Dict__empty(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 2)
#define jet_Dict__deleted(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 1)
#define jet_Dict__emptyOrDel(flag, i) ((flag[i >> 4] >> ((i & 0xfU) << 1)) & 3)

#define jet_Dict__setNotDeleted(flag, i)                                       \
    (flag[i >> 4] &= ~(1ul << ((i & 0xfU) << 1)))
#define jet_Dict__setDeleted(flag, i) (flag[i >> 4] |= 1ul << ((i & 0xfU) << 1))

#define jet_Dict__setNotEmpty(flag, i)                                         \
    (flag[i >> 4] &= ~(2ul << ((i & 0xfU) << 1)))
#define jet_Dict__clearFlags(flag, i)                                          \
    (flag[i >> 4] &= ~(3ul << ((i & 0xfU) << 1)))

#define jet_Dict__flagsSize(m) ((m) < 16 ? 1 : (m) >> 4)

#define roundUp32(x)                                                           \
    (--(x), (x) |= (x) >> 1, (x) |= (x) >> 2, (x) |= (x) >> 4,                 \
        (x) |= (x) >> 8, (x) |= (x) >> 16, ++(x))

static const double jet_Dict_HASH_UPPER = 0.77;

// Sets do not have any values, but the type of the value ptr is char*.

// Generally it is not recommended to use jet_Dict_has because you call it and
// then you call jet_Dict_get again. So just call jet_Dict_get and do whatever.
// If you don't really want the index, then its fine to call jet_Dict_has.
#define jet_Dict(K, V) jet_Dict_##K##_##V
#define jet_Dict_init(K, V) jet_Dict_init_##K##_##V
#define jet_Dict_free(K, V) jet_Dict_free_##K##_##V
#define jet_Dict_freedata(K, V) jet_Dict_freedata_##K##_##V
#define jet_Dict_clear(K, V) jet_Dict_clear_##K##_##V
#define jet_Dict_resize(K, V) jet_Dict_resize_##K##_##V
#define jet_Dict_put(K, V) jet_Dict_put_##K##_##V
#define jet_Dict_get(K, V) jet_Dict_get_##K##_##V
#define jet_Dict_has(K, V) jet_Dict_has_##K##_##V
#define jet_Dict_delete(K, V) jet_Dict_del_##K##_##V
#define jet_Dict_deleteByKey(K, V) jet_Dict_delk_##K##_##V

// TODO: why not void instead of char?
#define jet_Set(K) jet_Dict(K, char)
#define jet_Set_init(K) jet_Dict_init(K, char)
#define jet_Set_free(K) jet_Dict_free(K, char)
#define jet_Set_freedata(K) jet_Dict_freedata(K, char)
#define jet_Set_clear(K) jet_Dict_clear(K, char)
#define jet_Set_resize(K) jet_Dict_resize(K, char)
#define jet_Set_put(K) jet_Dict_put(K, char)
#define jet_Set_get(K) jet_Dict_get(K, char)
#define jet_Set_has(K) jet_Dict_has(K, char)
#define jet_Set_del(K) jet_Dict_delete(K, char)
#define jet_Set_delk(K) jet_Dict_deleteByKey(K, char)

#define __DICT_TYPE(K, V)                                                      \
    typedef struct jet_Dict(K, V)                                              \
    {                                                                          \
        UInt32 nBuckets, size, nOccupied, upperBound;                          \
        UInt32* flags;                                                         \
        K* keys;                                                               \
        V* vals;                                                               \
    }                                                                          \
    jet_Dict(K, V);

// #define __DICT_PROTOTYPES(K, V)                                            \
//     jet_Dict(K, V) * jet_Dict_init(K, V)();                                        \
//     void jet_Dict_free(K, V)(jet_Dict(K, V) * h);                                  \
//     void jet_Dict_freedata(K, V)(jet_Dict(K, V) * h);                              \
//     void jet_Dict_clear(K, V)(jet_Dict(K, V) * h);                                 \
//     UInt32 jet_Dict_get(K, V)(const jet_Dict(K, V) * const h, K key);                    \
//     bool jet_Dict_has(K, V)(const jet_Dict(K, V) * const h, K key);                      \
//     int jet_Dict_resize(K, V)(jet_Dict(K, V) * h, UInt32 nnBuckets);               \
//     UInt32 jet_Dict_put(K, V)(jet_Dict(K, V) * h, K key, int* ret);                \
//     void jet_Dict_delete(K, V)(jet_Dict(K, V) * h, UInt32 x);

// TODO: move the implementation into jet_runtime.h
#define __DICT_IMPL(Scope, K, V, IsMap, hash, equal)                           \
    Scope jet_Dict(K, V) * jet_Dict_init(K, V)()                               \
    {                                                                          \
        return calloc(1, sizeof(jet_Dict(K, V)));                              \
    }                                                                          \
    Scope void jet_Dict_freedata(K, V)(jet_Dict(K, V) * h)                     \
    {                                                                          \
        if (h) {                                                               \
            free(h->keys);                                                     \
            free(h->flags);                                                    \
            if (IsMap) free(h->vals);                                          \
        }                                                                      \
    }                                                                          \
    Scope void jet_Dict_free(K, V)(jet_Dict(K, V) * h) { free(h); }            \
    Scope void jet_Dict_clear(K, V)(jet_Dict(K, V) * h)                        \
    {                                                                          \
        if (h and h->flags) {                                                  \
            memset(h->flags, 0xAA,                                             \
                jet_Dict__flagsSize(h->nBuckets) * sizeof(UInt32));            \
            h->size = h->nOccupied = 0;                                        \
        }                                                                      \
    }                                                                          \
    Scope UInt32 jet_Dict_get(K, V)(const jet_Dict(K, V)* const h, K key)      \
    {                                                                          \
        if (h->nBuckets) {                                                     \
            UInt32 k, i, last, mask, step = 0;                                 \
            mask = h->nBuckets - 1;                                            \
            k = hash(key);                                                     \
            i = k & mask;                                                      \
            last = i;                                                          \
            while (not jet_Dict__empty(h->flags, i)                            \
                and (jet_Dict__deleted(h->flags, i)                            \
                    or not equal(h->keys[i], key))) {                          \
                i = (i + (++step)) & mask;                                     \
                if (i == last) return h->nBuckets;                             \
            }                                                                  \
            return jet_Dict__emptyOrDel(h->flags, i) ? h->nBuckets : i;        \
        } else                                                                 \
            return 0;                                                          \
    }                                                                          \
    Scope bool jet_Dict_has(K, V)(const jet_Dict(K, V)* const h, K key)        \
    {                                                                          \
        UInt32 x = jet_Dict_get(K, V)(h, key);                                 \
        return x < h->nBuckets && jet_Dict_exist(h, x);                        \
    }                                                                          \
    Scope int jet_Dict_resize(K, V)(jet_Dict(K, V) * h, UInt32 nnBuckets)      \
    { /* This function uses 0.25*nBuckets bytes of working space instead       \
         of [sizeof(key_t+val_t)+.25]*nBuckets. */                             \
        UInt32* nFlags = 0;                                                    \
        UInt32 j = 1;                                                          \
        {                                                                      \
            roundUp32(nnBuckets);                                              \
            if (nnBuckets < 4) nnBuckets = 4;                                  \
            if (h->size >= (UInt32)(nnBuckets * jet_Dict_HASH_UPPER + 0.5))    \
                j = 0; /* requested size is too small */                       \
            else { /* size to be changed (shrink or expand); rehash */         \
                nFlags                                                         \
                    = malloc(jet_Dict__flagsSize(nnBuckets) * sizeof(UInt32)); \
                if (not nFlags) return -1;                                     \
                memset(nFlags, 0xAA,                                           \
                    jet_Dict__flagsSize(nnBuckets) * sizeof(UInt32));          \
                if (h->nBuckets < nnBuckets) { /* expand */                    \
                    K* nKeys = realloc(h->keys, nnBuckets * sizeof(K));        \
                    if (not nKeys) {                                           \
                        free(nFlags);                                          \
                        return -1;                                             \
                    }                                                          \
                    h->keys = nKeys;                                           \
                    if (IsMap) {                                               \
                        V* nVals = realloc(h->vals, nnBuckets * sizeof(V));    \
                        if (not nVals) {                                       \
                            free(nFlags);                                      \
                            return -1;                                         \
                        }                                                      \
                        h->vals = nVals;                                       \
                    }                                                          \
                } /* otherwise shrink */                                       \
            }                                                                  \
        }                                                                      \
        if (j) { /* rehashing is needed */                                     \
            for (j = 0; j != h->nBuckets; ++j) {                               \
                if (jet_Dict__emptyOrDel(h->flags, j) == 0) {                  \
                    K key = h->keys[j];                                        \
                    V val;                                                     \
                    UInt32 new_mask;                                           \
                    new_mask = nnBuckets - 1;                                  \
                    if (IsMap) val = h->vals[j];                               \
                    jet_Dict__setDeleted(h->flags, j);                         \
                    while (1) { /* kick-out process; sort of like in           \
                                   Cuckoo hashing */                           \
                        UInt32 k, i, step = 0;                                 \
                        k = hash(key);                                         \
                        i = k & new_mask;                                      \
                        while (not jet_Dict__empty(nFlags, i))                 \
                            i = (i + (++step)) & new_mask;                     \
                        jet_Dict__setNotEmpty(nFlags, i);                      \
                        if (i < h->nBuckets                                    \
                            and jet_Dict__emptyOrDel(h->flags, i) == 0) {      \
                            /* kick out the existing element */                \
                            {                                                  \
                                K tmp = h->keys[i];                            \
                                h->keys[i] = key;                              \
                                key = tmp;                                     \
                            }                                                  \
                            if (IsMap) {                                       \
                                V tmp = h->vals[i];                            \
                                h->vals[i] = val;                              \
                                val = tmp;                                     \
                            }                                                  \
                            jet_Dict__setDeleted(h->flags, i);                 \
                            /* mark it jet_Dict__deleted in the old table */   \
                        } else {                                               \
                            /* write the element and break the loop */         \
                            h->keys[i] = key;                                  \
                            if (IsMap) h->vals[i] = val;                       \
                            break;                                             \
                        }                                                      \
                    }                                                          \
                }                                                              \
            }                                                                  \
            if (h->nBuckets > nnBuckets) { /* shrink the hash table */         \
                h->keys = realloc(h->keys, nnBuckets * sizeof(K));             \
                if (IsMap) h->vals = realloc(h->vals, nnBuckets * sizeof(V));  \
            }                                                                  \
            free(h->flags); /* free the working space */                       \
            h->flags = nFlags;                                                 \
            h->nBuckets = nnBuckets;                                           \
            h->nOccupied = h->size;                                            \
            h->upperBound = (UInt32)(h->nBuckets * jet_Dict_HASH_UPPER + 0.5); \
        }                                                                      \
        return 0;                                                              \
    }                                                                          \
    Scope UInt32 jet_Dict_put(K, V)(jet_Dict(K, V) * h, K key, int* ret)       \
    {                                                                          \
        UInt32 x;                                                              \
        if (h->nOccupied >= h->upperBound) { /* update the hash table */       \
            if (h->nBuckets > (h->size << 1)) {                                \
                if (jet_Dict_resize(K, V)(h, h->nBuckets - 1) < 0) {           \
                    /* clear "jet_Dict__deleted" elements */                   \
                    *ret = -1;                                                 \
                    return h->nBuckets;                                        \
                }                                                              \
            } else if (jet_Dict_resize(K, V)(h, h->nBuckets + 1) < 0) {        \
                /* expand the hash table */                                    \
                *ret = -1;                                                     \
                return h->nBuckets;                                            \
            }                                                                  \
        } /* TODO: to implement automatically shrinking; resize() already      \
             support shrinking */                                              \
        {                                                                      \
            UInt32 k, i, site, last, mask = h->nBuckets - 1, step = 0;         \
            x = site = h->nBuckets;                                            \
            k = hash(key);                                                     \
            i = k & mask;                                                      \
            if (jet_Dict__empty(h->flags, i))                                  \
                x = i; /* for speed up */                                      \
            else {                                                             \
                last = i;                                                      \
                while (not jet_Dict__empty(h->flags, i)                        \
                    and (jet_Dict__deleted(h->flags, i)                        \
                        or not equal(h->keys[i], key))) {                      \
                    if (jet_Dict__deleted(h->flags, i)) site = i;              \
                    i = (i + (++step)) & mask;                                 \
                    if (i == last) {                                           \
                        x = site;                                              \
                        break;                                                 \
                    }                                                          \
                }                                                              \
                if (x == h->nBuckets) {                                        \
                    if (jet_Dict__empty(h->flags, i) and site != h->nBuckets)  \
                        x = site;                                              \
                    else                                                       \
                        x = i;                                                 \
                }                                                              \
            }                                                                  \
        }                                                                      \
        if (jet_Dict__empty(h->flags, x)) { /* not present at all */           \
            h->keys[x] = key;                                                  \
            jet_Dict__clearFlags(h->flags, x);                                 \
            ++h->size;                                                         \
            ++h->nOccupied;                                                    \
            *ret = 1;                                                          \
        } else if (jet_Dict__deleted(h->flags, x)) { /* jet_Dict__deleted */   \
            h->keys[x] = key;                                                  \
            jet_Dict__clearFlags(h->flags, x);                                 \
            ++h->size;                                                         \
            *ret = 2;                                                          \
        } else                                                                 \
            *ret = 0;                                                          \
        /* Don't touch h->keys[x] if present and not jet_Dict__deleted */      \
        return x;                                                              \
    }                                                                          \
    Scope void jet_Dict_delete(K, V)(jet_Dict(K, V) * h, UInt32 x)             \
    {                                                                          \
        if (x != h->nBuckets and not jet_Dict__emptyOrDel(h->flags, x)) {      \
            jet_Dict__setDeleted(h->flags, x);                                 \
            --h->size;                                                         \
        }                                                                      \
    }                                                                          \
    Scope void jet_Dict_deleteByKey(K, V)(jet_Dict(K, V) * h, K key)           \
    {                                                                          \
        jet_Dict_delete(K, V)(h, jet_Dict_get(K, V)(h, key));                  \
    }

#define DICT_DECLARE(K, V)                                                     \
    __DICT_TYPE(K, V)                                                          \
    __DICT_PROTOTYPES(K, V)

#define DICT_INIT2(Scope, K, V, IsMap, hash, equal)                            \
    __DICT_TYPE(K, V)                                                          \
    __DICT_IMPL(Scope, K, V, IsMap, hash, equal)

#define DICT_INIT(K, V, IsMap, hash, equal)                                    \
    DICT_INIT2(static, K, V, IsMap, hash, equal)

/* --- BEGIN OF HASH FUNCTIONS --- */

#define UInt32_hash(key) (UInt32)(key)

#define UInt32_equal(a, b) ((a) == (b))

#define UInt64_hash(key) (UInt32)((key) >> 33 ^ (key) ^ (key) << 11)

#define UInt64_equal(a, b) ((a) == (b))

// careful, this is really just pointer equality/hash,
// not underlying object equality/hash
// TODO: handle 32/64 bit
#define Ptr_hash(key) UInt64_hash((UInt64)key)
#define Ptr_equal(a, b) ((a) == (b))

#define Int64_equal(a, b) ((a) == (b))

#define Int64_hash(key) UInt64_hash((UInt64)key)

// #define Real64_hash(key) UInt64_hash(*(UInt64*)&key)

static UInt32 Real64_hash(double key)
{
    UInt64* ptr = (UInt64*)&key;
    return UInt64_hash(*ptr);
}

#define Real64_equal(a, b) ((a) == (b))

// ---
#define _rotl_KAZE(x, n) (((x) << (n)) | ((x) >> (32 - (n))))
#define _PADr_KAZE(x, n) (((x) << (n)) >> (n))
#define ROLInBits 27
// 5 in r.1; Caramba: it should be ROR by 5 not ROL, from the very
// beginning the idea was to mix two bytes by shifting/masking the first
// 5 'noisy' bits (ASCII 0-31 symbols).
// CAUTION: Add 8 more bytes to the buffer being hashed, usually malloc(...+8) -
// to prevent out of boundary reads!
static uint32_t FNV1A_Hash_Yorikke_v3(const char* str, uint32_t wrdlen)
{
    const uint32_t PRIME = 591798841;
    uint32_t hash32 = 2166136261;
    uint64_t PADDEDby8;
    const char* p = str;
    for (; wrdlen > 2 * sizeof(uint32_t);
         wrdlen -= 2 * sizeof(uint32_t), p += 2 * sizeof(uint32_t)) {
        hash32
            = (_rotl_KAZE(hash32, ROLInBits) ^ (*(uint32_t*)(p + 0))) * PRIME;
        hash32
            = (_rotl_KAZE(hash32, ROLInBits) ^ (*(uint32_t*)(p + 4))) * PRIME;
    }
    // Here 'wrdlen' is 1..8
    PADDEDby8 = _PADr_KAZE(*(uint64_t*)(p + 0),
        (8 - wrdlen) << 3); // when (8-8) the QWORD remains intact
    hash32
        = (_rotl_KAZE(hash32, ROLInBits) ^ *(uint32_t*)((char*)&PADDEDby8 + 0))
        * PRIME;
    hash32
        = (_rotl_KAZE(hash32, ROLInBits) ^ *(uint32_t*)((char*)&PADDEDby8 + 4))
        * PRIME;
    return hash32 ^ (hash32 >> 16);
}
// Last touch: 2019-Oct-03, Kaze
// ---

static inline UInt32 __ac_X31_hash_string(const char* s)
{
    UInt32 i = (UInt32)*s;
    if (i)
        for (++s; *s; ++s) i = (i << 5) - i + (UInt32)*s;
    return i;
}

#define CString_hash(key) __ac_X31_hash_string(key)

#define CString_equal(a, b) (strcmp(a, b) == 0)
// for CStrings, assuming they MUST end in \0, you can check for
// pointer equality to mean string equality. For Strings with lengths,
// this does not hold since the same buffer can hold a string and any of
// its prefixes, especially in Jet where not all strings end with '\0'.

static inline UInt32 __ac_Wang_hash(UInt32 key)
{
    key += ~(key << 15);
    key ^= (key >> 10);
    key += (key << 3);
    key ^= (key >> 6);
    key += ~(key << 11);
    key ^= (key >> 16);
    return key;
}
#define jet_Dict_int_hash_func2(key) __ac_Wang_hash((UInt32)key)

#define jet_Dict_exist(h, x) (not jet_Dict__emptyOrDel((h)->flags, (x)))

#define jet_Dict_key(h, x) ((h)->keys[x])

#define jet_Dict_val(h, x) ((h)->vals[x])

#define jet_Dict_begin(h) (UInt32)(0)

#define jet_Dict_end(h) ((h)->nBuckets)

#define jet_Dict_size(h) ((h)->size)

#define jet_Dict_nBuckets(h) ((h)->nBuckets)

#define jet_Dict_foreach(h, kvar, vvar, code)                                  \
    {                                                                          \
        for (UInt32 _i_ = jet_Dict_begin(h); _i_ != jet_Dict_end(h); ++_i_) {  \
            if (not jet_Dict_exist(h, _i_)) continue;                          \
            kvar = jet_Dict_key(h, _i_);                                       \
            vvar = jet_Dict_val(h, _i_);                                       \
            code;                                                              \
        }                                                                      \
    }

#define jet_Dict_foreach_value(h, vvar, code)                                  \
    {                                                                          \
        for (UInt32 _i_ = jet_Dict_begin(h); _i_ != jet_Dict_end(h); ++_i_) {  \
            if (not jet_Dict_exist(h, _i_)) continue;                          \
            vvar = jet_Dict_val(h, _i_);                                       \
            code;                                                              \
        }                                                                      \
    }

#define jet_Set_foreach(h, kvar, code) jet_Dict_foreach_key(h, kvar, code)
#define jet_Dict_foreach_key(h, kvar, code)                                    \
    {                                                                          \
        for (UInt32 _i_ = jet_Dict_begin(h); _i_ != jet_Dict_end(h); ++_i_) {  \
            if (not jet_Dict_exist(h, _i_)) continue;                          \
            kvar = jet_Dict_key(h, _i_);                                       \
            code;                                                              \
        }                                                                      \
    }

#define MAKE_SET(T) DICT_INIT(T, char, false, T##_hash, T##_equal)
#define MAKE_DICT(K, V) DICT_INIT(K, V, true, K##_hash, K##_equal)

// No predefined instantiations. MAKE_DICT calls will be inserted by the
// Jet compiler in every compilation unit depending on what types are found
// to have been used in dicts.
// Better yet, get rid of the macro templating and define ONLY a single
// Dict type void* -> void*; means you cannot embed structs but this is
// not too bad. void* to/from any type* needs no explicit cast in C.
// Besides you can still embed things up to 8B...

// MAKE_SET(UInt32)
// // MAKE_SET(Real64)
// MAKE_SET(CString)
// // MAKE_SET(Ptr)

// MAKE_DICT(UInt32, UInt32)
// MAKE_DICT(CString, UInt32)

// MAKE_DICT(UInt32, Ptr)
// MAKE_DICT(CString, Ptr)
// MAKE_DICT(UInt32, CString)
// MAKE_DICT(CString, CString)
// MAKE_DICT(UInt64, Ptr)
// MAKE_DICT(Ptr, UInt64)
// MAKE_DICT(Ptr, Ptr)

#endif /* HAVE_DICT */
