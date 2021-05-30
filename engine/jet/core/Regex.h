
#include <regex.h>

#define REX_MAX_SUBMATCH 5
typedef struct _RegexProg {
  regex_t prog;
} _RegexProg;

typedef struct {
  regmatch_t sub[REX_MAX_SUBMATCH + 1]; // 96B. You can have 5 submatches.
} RegexMatch;

typedef char* Regex; // fixme
typedef const char* const_Regex;

// _RegexProg is not really exposed in F+. why keep it public? Just hide it
// since regex literals and strings passed in to regex args will be
// transparently wrapped into a RegexProg_new call. take care to "optimise"
// literals, replace char classes e.g. \d \D etc.
_RegexProg rex__compile(const char* str, int matchCase, int justMatch) {
  regex_t reg;
  matchCase = (!matchCase) * REG_ICASE;
  justMatch = (!!justMatch) * REG_NOSUB;
  int err;
  if ((err = regcomp(&reg, str, REG_EXTENDED | matchCase | justMatch)))
    ; // handle error
  return (_RegexProg) { .prog = reg };
}
static const long szs = sizeof(regex_t);
static const long sza = sizeof(RegexMatch);
static const long szv = sizeof(_RegexProg);
static const long sze = sizeof(regmatch_t);

RegexMatch rex_match(_RegexProg prog, char* source) {
  RegexMatch match = {};
  int err;
  if ((err = regexec(&prog.prog, source, REX_MAX_SUBMATCH, match.sub, 0)))
    ; // handle error
  return match;
}

int rex_contains(_RegexProg prog, char* source) // yes or no
{
  RegexMatch match = {};
  int err;
  if ((err = regexec(&prog.prog, source, REX_MAX_SUBMATCH, match.sub, 0)))
    ; // handle error
  return !err;
}

#define CString_matches_re rex_matches

int rex_matches(char* source, char* pattern) {
  return rex_contains(rex__compile(pattern, 1, 1), source);
}
/* substitute into one string using the matches from the last regexec() */
// this needs work, it just works locally on a new buffer, not the original
// source string
static size_t _regsub(
    char* replacement, char* buf, int dlen, regmatch_t* match, int nmatch) {
  char* origSource = replacement;
  char* origDest = buf;
  char *start, *end;
  int i;

  end = buf + dlen - 1;
  while (*replacement != '\0') {
    if (*replacement == '\\') {
      switch (*++replacement) {
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
        i = *replacement - '0';
        if (match != NULL && nmatch > i)
          for (start = origSource + match[i].rm_so;
               start < origSource + match[i].rm_eo; start++)
            if (buf < end) *buf++ = *start;
        break;
      case '\\':
        if (buf < end) *buf++ = '\\';
        break;
      case '\0': replacement--; break;
      default:
        if (buf < end) *buf++ = *replacement;
        break;
      }
    } else if (*replacement == '&') {
      if (match != NULL && nmatch > 0)
        for (start = origSource + match[0].rm_so;
             start < origSource + match[0].rm_eo; start++)
          if (buf < end) *buf++ = *start;
    } else {
      if (buf < end) *buf++ = *replacement;
    }
    replacement++;
  }
  *buf = '\0';

  return buf - origDest;
}

#define Text char*
// be careful regsub does not allocate or check buffer size
Text rex_replace(RegexMatch match, char* source, char* replacement) {
  Text str = malloc(1 /* FIXME */);
  size_t written = _regsub(source, str, 1, match.sub, REX_MAX_SUBMATCH);

  return "";
}

int maisan() { return 0; }