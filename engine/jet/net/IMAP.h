#include "jet/base.h"
#include "MIME.h"

#ifndef HAVE_CURL
#include <curl/curl.h>
#endif

// REMOVE STRING.H DEP USE CSTRING
// #include <string.h>
// #include <stdlib.h>

typedef struct {
    bool imap4, imap4rev1, authPlain, authXOAuth2, saslIR, uidPlus, id,
        unselect, children, idle, namespace, literalPlus;
} imap_capabilities_t;

typedef struct {
    int count;
    char* first;
} StringString;

typedef struct {
    char *name, *email;
} mime_user_t;

typedef struct {
    // make all these char* instead of stringstring
    // StringString
    char* flags;
    char* subject;
    // StringString
    char* unsubscribe;
    int id, uid;
    struct tm date, internalDate;
    int size, rfc822Size;
    mime_user_t *from, *to;
} mime_header_t;
typedef struct {
    char* name;
    char *flags, *permanentFlags;
    int uidValidity;
    int uidNext;
    int messageCount; // total on the server
    int headersCount; // num of headers downloaded
    int recentCount;
    list_t(mime_header_t) * headers;
    void (*on_newHeaderReceived)(mime_header_t*);
} imap_folderinfo_t;

mime_header_t* IMAP_parseMIMEHeader(char* pos, int len);
char* trampleTheNext(char* charset, char* start) {
    char* eo = strpbrk(start + 1, charset);
    if (eo) {
        *eo++ = 0;
        return eo;
    }
}

mime_user_t* newMIMEUser(char* line) {
    mime_user_t* user = NEW(mime_user_t);
    user->name = line;
    user->email = strpbrk(line, "<");
    if (user->email) {
        *user->email++ = 0;
        char* pos = strpbrk(user->email, ">");
        if (pos) *pos = 0;
    }
    return user;
}

void parseImapFetchHeadersLB(char* pos, int len, imap_folderinfo_t* fi) {
    static mime_header_t* currMsg = NULL;
    // imap_folderinfo_t* fi = NEW(imap_folderinfo_t);
    // fi->on_newHeaderReceived = on_newHeaderReceived;
    list_t(mime_header_t)** hdrsTop = &(fi->headers);

    char* end = pos + len;
    // printf("#### %.*s\n", len, pos);
    // while (pos < end) {
    switch (*pos) {
    // case ' ':
    //     break;
    case '*': {
        char* w1 = pos + 2;
        pos = strpbrk(w1, " \r\n"); // trampleTheNext(" \r\n", w1);

        char* w2 = pos + 1;
        pos = strpbrk(w2, " \r\n"); // trampleTheNext(" \r\n", w2);
        // printf("[%.*s] [%.*s]\n", w2 - w1, w1, pos - w2, w2);
        // printf("[%s] [%s]\n", w1, w2);

        if (!strncmp(w2, "EXISTS", 6)) {
            // printf("%d exists <-- \n", atoi(w1));
            fi->messageCount = atoi(w1);
        } else if (!strncmp(w2, "RECENT", 6)) {
            // printf("%d recent <-- \n", atoi(w1));
            fi->recentCount = atoi(w1);
        } else if (!strncmp(w2, "[UIDVALIDITY", 12)) {
            // printf("%d uidval <-- \n", atoi(w2 + 12));
            fi->uidValidity = atoi(w2 + 12);
        } else if (!strncmp(w2, "[UIDNEXT", 8)) {
            // printf("%d uidnext <-- \n", atoi(w2 + 8));
            fi->uidNext = atoi(w2 + 8);
        } else if (!strncmp(w2, "FETCH", 5)) {
            // printf("*** begin new message #%d\n", atoi(w1));
            if (currMsg && fi->on_newHeaderReceived)
                fi->on_newHeaderReceived(currMsg);
            fi->headersCount++;
            currMsg = NEW(mime_header_t);
            hdrsTop = PtrList_append(hdrsTop, currMsg);

            currMsg->id = atoi(w1);

            pos++;
            assert(*pos++ == '(');
            while (1) {
                w1 = pos;
                pos = trampleTheNext(" \r\n", w1);
                // eo = strpbrk(w1, " \r\n");
                // if (eo) {
                //     *eo++ = 0;
                //     pos = eo;
                // }
                if (!strcmp("UID", w1)) {
                    w2 = pos;
                    pos = trampleTheNext(" \r\n", w2);
                    // eo = strpbrk(w2, " \r\n");
                    // if (eo) {
                    //     *eo++ = 0;
                    //     pos = eo;
                    // }
                    int uid = atoi(w2);
                    // printf("uid: %d\n", uid);
                    currMsg->uid = uid;

                } else if (!strcmp("RFC822.SIZE", w1)) {
                    w2 = pos;
                    pos = trampleTheNext(" \r\n", w2);
                    // eo = strpbrk(w2, " \r\n");
                    // if (eo) {
                    //     *eo++ = 0;
                    //     pos = eo;
                    // }
                    int size = atoi(w2);
                    // printf("size: %d\n", size);
                    currMsg->rfc822Size = size;
                } else if (!strcmp("INTERNALDATE", w1)) {
                    assert(*pos++ == '"');
                    w2 = pos;
                    struct tm dttm = {};
                    // printf("%.*s ====\n", 60, w2);
                    strptime(w2, "%d-%b-%Y %H:%M:%S", &dttm);
                    char redate[128];
                    strftime(redate, 128, "%a %d-%m-%Y %H:%M:%S ", &dttm);
                    // printf("  intrdate --> %s\n", redate);
                    currMsg->internalDate = dttm;
                } else if (!strcmp("FLAGS", w1)) {
                    assert(*pos++ == '(');
                    w2 = pos;
                    char* eo = strpbrk(w2, ")");
                    if (eo) {
                        *eo++ = ' '; // convert the ) to a space so you can do
                                     // simple substring search to check a
                                     // flag's presence and it will work for the
                                     // last as well.
                        assert(*eo == ' ');
                        *eo++ = 0; // trample the space after it
                        pos = eo;
                    }
                    char* flags = w2;
                    // printf("flags: %s\n", flags);
                    currMsg->flags = pstrndup(flags, pos - flags);
                } else {
                    // printf("*** breakout at %s\n", w1);
                    break; // out of while
                }
            }

        } else {
            printf("!!!!! unnkown item! [%s] [%s]\n", w1, w2);
        }

    } break;
    case ' ':
        // continuation of a header line probably
        break;
    default:
        // mostly a header line

        if (!strncmp(pos, "Date: ", 6)) {
            struct tm dttm = {};
            strptime(pos + 6, "%a, %d %b %Y %H:%M:%S", &dttm);
            char redate[128];
            strftime(redate, 128, "%a %d-%m-%Y %H:%M:%S ", &dttm);
            // printf("  date --> %s\n", redate);
            currMsg->date = dttm;
        } else if (!strncmp(pos, "From: ", 6)) {
            // printf("  from --> %.*s", len - 6, pos + 6);
            currMsg->from = newMIMEUser(pstrndup(pos + 6, len - 6));
        } else if (!strncmp(pos, "Subject: ", 9)) {
            // printf("  subj --> %.*s", len - 9, pos + 9);
            currMsg->subject = pstrndup(pos + 9, len - 9);
            ewdecode(currMsg->subject, len - 9);
        } else if (!strncmp(pos, "list_t-Unsubscribe: ", 18)) {
            // printf("  unsub --> %.*s", len - 18, pos + 18);
            currMsg->unsubscribe = pstrndup(pos + 18, len - 18);
        } else {
            char* w2 = strpbrk(pos, " ");
            if (w2 && !strncmp(w2, " OK FETCH completed.", 20)) {
                if (currMsg && fi->on_newHeaderReceived)
                    fi->on_newHeaderReceived(currMsg);
                fi->headersCount++;
            }
        }
    }
    pos++;
    // }
}

void parseImapFetch(char* pos, int len) {
    char* end = pos + len;
    while (pos < end) {
        switch (*pos) {
        case ' ':
        case '\r':
        case '\n': break;
        case '*': {
            // this with w1 and eo seems to be a candidate for macro or func
            char* w1 = pos + 2;
            pos = trampleTheNext(" \r\n", w1);
            if (!strcmp(w1, "CAPABILITY")) {
            } else {
                char* w2 = pos;
                pos = trampleTheNext(" \r\n", w2);
                if (!strcmp(w2, "EXISTS")) {
                    int exi = atoi(w1);
                    printf("exi --> %d\n", exi);
                } else if (!strcmp(w2, "RECENT")) {
                    int recent = atoi(w1);
                    printf("recent --> %d\n", recent);
                } else if (!strcmp(w2, "[UNSEEN")) {
                    int unseen = atoi(pos);
                    printf("unseen --> %d\n", unseen);
                } else if (!strcmp(w2, "[UIDVALIDITY")) {
                    int uidvali = atoi(pos);
                    printf("uidvali --> %d\n", uidvali);
                } else if (!strcmp(w2, "[UIDNEXT")) {
                    int uidnext = atoi(pos);
                    printf("uidnext --> %d\n", uidnext);
                } else if (!strcmp(w2, "FETCH")) {
                    int idx = atoi(w1);
                    printf("\nfetch idx --> %d\n", idx);
                    assert(*pos++ == '(');
                    while (1) {
                        w1 = pos;
                        pos = trampleTheNext(" \r\n", w1);
                        // eo = strpbrk(w1, " \r\n");
                        // if (eo) {
                        //     *eo++ = 0;
                        //     pos = eo;
                        // }
                        if (!strcmp("UID", w1)) {
                            w2 = pos;
                            pos = trampleTheNext(" \r\n", w2);
                            // eo = strpbrk(w2, " \r\n");
                            // if (eo) {
                            //     *eo++ = 0;
                            //     pos = eo;
                            // }
                            int uid = atoi(w2);
                            printf("uid: %d\n", uid);

                        } else if (!strcmp("RFC822.SIZE", w1)) {
                            w2 = pos;
                            pos = trampleTheNext(" \r\n", w2);
                            // eo = strpbrk(w2, " \r\n");
                            // if (eo) {
                            //     *eo++ = 0;
                            //     pos = eo;
                            // }
                            int size = atoi(w2);
                            printf("size: %d\n", size);
                        } else if (!strcmp("INTERNALDATE", w1)) {
                        } else if (!strcmp("FLAGS", w1)) {
                            assert(*pos++ == '(');
                            w2 = pos;
                            char* eo = strpbrk(w2, ")");
                            if (eo) {
                                *eo++ = 0;
                                if (*eo == ' ') *eo++ = 0;
                                pos = eo;
                            }
                            char* flags = w2;
                            printf("flags: %s\n", flags);
                        } else {
                            // printf("*** breakout at %s\n", w1);
                            break; // out of while
                        }
                    }
                }
            }
        }
            continue;
        case '{': {
            char* num = ++pos;
            while (isdigit(*pos)) pos++;
            if (*pos != '}') perror("expected '}', not found");
            *pos++ = 0;
            int sz = atoi(num);
            if (!sz) return;
            printf("parse %d B --- \n", sz);
            mime_header_t* header = IMAP_parseMIMEHeader(pos, sz);
            pos += sz;
        }
        case '(':
        case '[':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':;
        }
        pos++;
    }
}

static int curldbgcbfn(
    CURL* handle, curl_infotype type, char* data, size_t size, void* userptr) {
    if (type == CURLINFO_HEADER_IN) {
        // String* uptr = userptr;
        // String_appendChars(uptr, data, size);
        // char* origp = uptr->ref + uptr->len - size;
        // puts("---------------");
        // printf("%8zu ------\n%.*s", size, (int)size, data);
        // puts("===============");

        parseImapFetchHeadersLB(data, size, userptr);
        // parseImapFetch(origp, size);
        // dont write into data it will be lost!
    }
    return 0;
}

static size_t curlcbfn(void* data, size_t size, size_t nmemb, void* tgt) {
    // String_appendCString(tgt, data, nmemb * size);
    return nmemb * size;
}

const char imaptst[]
    = "* 4379 EXISTS\r\n"
      "* 0 RECENT\r\n"
      "* FLAGS (\\Seen \\Answered \\Flagged \\Deleted \\Draft $MDNSent)\r\n"
      "* OK [PERMANENTFLAGS (\\Seen \\Answered \\Flagged \\Deleted \\Draft "
      "$MDNSent)]Permanent flags\r\n"
      "* OK [UNSEEN 4379] Is the first unseen message\r\n"
      "* OK [UIDVALIDITY 14] UIDVALIDITY value\r\n"
      "* OK [UIDNEXT 24868] The next unique identifier value\r\n"
      "A003 OK [READ-WRITE] SELECT completed.\r\n"
      "* 119 FETCH (UID 7508 RFC822.SIZE 102161 FLAGS (\\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject list_t-Unsubscribe)] {451}\r\n"
      "Subject: "
      "=?utf-8?Q?Fantastic=20Weekend=20Deals=20at=20Domino=27s=20Pizza=C2="
      "A0="
      "F0=9F=91=8C?=\r\n"
      "From: =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.ch>,\r\n"
      " =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.co.uk>\r\n"
      "Date: Sat, 3 Jun 2017 09:16:01 +0000\r\n"
      "list_t-Unsubscribe: "
      "<http://dominos.us12.list-manage1.com/"
      "unsubscribe?u=0a3531a6a054617a5478d1324&id=8dcc978ca9&e=895bfd4899&"
      "c="
      "016c232dbc>, "
      "<mailto:unsubscribe-mc.us12_0a3531a6a054617a5478d1324.016c232dbc-"
      "895bfd4899@mailin1.us2.mcsv.net?subject=unsubscribe>\r\n"
      "\r\n"
      ")\r\n"
      "* 120 FETCH (UID 7513 RFC822.SIZE 48069 FLAGS (\\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject list_t-Unsubscribe)] {127}\r\n"
      "From: <Notification@Jio.com>\r\n"
      "Subject: Data quota exhausted for Jio Number 8169236325\r\n"
      "Date: Sat, 3 Jun 2017 09:05:52 -0700\r\n"
      "\r\n"
      ")\r\n"
      "* 121 FETCH (UID 7535 RFC822.SIZE 64661 FLAGS (\\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject list_t-Unsubscribe)] {184}\r\n"
      "From: LC <mail@executive-learning.co.in>\r\n"
      "Subject: Letter of Credit Transactions International Trade UCP 600, "
      "ISBP 745 INCOTERMS  2010 & URBPO\r\n"
      "Date: Mon, 5 Jun 2017 06:08:55 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 122 FETCH (UID 7536 RFC822.SIZE 84459 FLAGS (\\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject list_t-Unsubscribe)] {143}\r\n"
      "From: Princeton Academy <mail@executive-learning.co.in>\r\n"
      "Subject: Workshop on Key Account Management\r\n"
      "Date: Mon, 05 Jun 2017 06:12:57 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 123 FETCH (UID 7547 RFC822.SIZE 50492 FLAGS (\\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject list_t-Unsubscribe)] {144}\r\n"
      "From: Jio Notifications <Notification@Jio.com>\r\n"
      "Subject: Recharge successful for Jio Number 8169236325\r\n"
      "Date: Tue, 6 Jun 2017 01:56:30 -0700\r\n"
      "\r\n"
      ")";

mime_header_t* IMAP_parseMIMEHeader(char* pos, int len) {
    char* end = pos + len;
    mime_header_t* head = calloc(1, sizeof(mime_header_t));
    while (pos < end) {
        while (*pos == ' ' || *pos == '\r' || *pos == '\n') pos++;
        char* col = strpbrk(pos, ":\r\n");
        if (col && col[0] == ':' && col[1] == ' ') {
            // line is a (new) header field
            char* fld = pos;
            *col = 0;
            char* val = col + 2;
            col = strpbrk(val, "\r\n"); // yes, '\r' or '\n'
            while (col && col < end - 2 && col[2] == ' ')
                // start with space means continuation
                col = strpbrk(col + 2, "\r\n");
            if (!col)
                col = end;
            else
                *col = 0;

            if (!strcmp(fld, "Date")) {
                struct tm dttm = {};
                strptime(val, "%a, %d %b %Y %H:%M:%S", &dttm);
                char redate[128];
                strftime(redate, 128, "%a %d-%m-%Y %H:%M:%S ", &dttm);
                printf("  date --> %s\n", redate);
            } else if (!strcmp(fld, "From")) {
            } else if (!strcmp(fld, "Subject")) {
            } else if (!strcmp(fld, "list_t-Unsubscribe")) {
            }

            // TODO: now val may be multiline (softwrap) in which case
            // unwrap it

            // TODO: ewdecode should also handle multiline or does it?
            if (*val == '=') ewdecode(val, col - val);

            printf("field: [%s = %s]\n", fld, val);

            pos = col + 1;

        } else {
            // continuation of an existing header, most likely
        }
    }
}

// String*
void imaplist(char* url, char* user, char* pass, char* cmd,
    imap_folderinfo_t* fi) { //}, String* ret) {
    CURL* curl_handle = curl_easy_init(); // find a way to reuse the handle

    // imap_folderinfo_t* fi = NEW(imap_folderinfo_t);
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);
    // curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);

    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, curlcbfn);
    // curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, ret);

    curl_easy_setopt(curl_handle, CURLOPT_DEBUGFUNCTION, curldbgcbfn);
    curl_easy_setopt(curl_handle, CURLOPT_DEBUGDATA, fi);

    curl_easy_setopt(curl_handle, CURLOPT_USERNAME, user);
    curl_easy_setopt(curl_handle, CURLOPT_PASSWORD, pass);
    curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, cmd);

    // curl_easy_setopt(curl_handle, CURLOPT_HEADER, !!header);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERFUNCTION, curlcbhead);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERDATA, ret);

    // if (header) String_resize(ret, 512);
    curl_easy_perform(curl_handle);
    curl_easy_cleanup(curl_handle);
    // return fi;
}
