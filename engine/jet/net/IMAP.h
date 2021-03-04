
typedef struct {
    boolean_t imap4, imap4rev1, authPlain, authXOAuth2, saslIR, uidPlus, id,
        unselect, children, idle, namespace, literalPlus;
} IMAPServerCapabilities;

typedef struct {
    int count;
    char* first;
} StringString;

typedef struct {
    char* name;
    StringString flags, permanentFlags;
    int uidValidity;
    int uidNext;
    int messageCount;
    int recentCount;
} IMAPFolderInfo;

typedef struct {
    char *name, *email;
} IMAPUserAddress;

typedef struct {
    // make all these char* instead of stringstring
    StringString flags;
    char* subject;
    StringString unsubscribe;
    int id, uid;
    // DateTime date, internalDate;
    int size, rfc822Size;
    IMAPUserAddress from, to;
} IMAPMessageHeader;

IMAPMessageHeader* parseImapHeader(char* pos, int len);

void parseImapFetch(char* pos, int len) {
    char* end = pos + len;
    while (pos < end) {
        switch (*pos) {
        case ' ':
        case '\r':
        case '\n':
            break;
        case '*': {
            char* w1 = pos + 2;
            char* eo = strpbrk(w1, " \r\n");
            if (eo) {
                *eo++ = 0;
                pos = eo;
            }
            if (!strcmp(w1, "CAPABILITY")) {
            } else {
                char* w2 = pos;
                eo = strpbrk(w2, " \r\n");
                if (eo) {
                    *eo++ = 0;
                    pos = eo;
                }
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
                    printf("fetch idx --> %d\n", idx);
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
            printf("parse %d B --- \n", sz);
            IMAPMessageHeader* header = parseImapHeader(pos, sz);
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
      "* 119 FETCH (UID 7508 RFC822.SIZE 102161 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {451}\r\n"
      "Subject: "
      "=?utf-8?Q?Fantastic=20Weekend=20Deals=20at=20Domino=27s=20Pizza=C2=A0="
      "F0=9F=91=8C?=\r\n"
      "From: =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.ch>, "
      " =?utf-8?Q?Domino=27s=20Pizza?= <no-reply@dominos.co.uk>\r\n"
      "Date: Sat, 3 Jun 2017 09:16:01 +0000\r\n"
      "List-Unsubscribe: "
      "<http://dominos.us12.list-manage1.com/"
      "unsubscribe?u=0a3531a6a054617a5478d1324&id=8dcc978ca9&e=895bfd4899&c="
      "016c232dbc>, "
      "<mailto:unsubscribe-mc.us12_0a3531a6a054617a5478d1324.016c232dbc-"
      "895bfd4899@mailin1.us2.mcsv.net?subject=unsubscribe>\r\n"
      "\r\n"
      ")\r\n"
      "* 120 FETCH (UID 7513 RFC822.SIZE 48069 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {127}\r\n"
      "From: <Notification@Jio.com>\r\n"
      "Subject: Data quota exhausted for Jio Number 8169236325\r\n"
      "Date: Sat, 3 Jun 2017 09:05:52 -0700\r\n"
      "\r\n"
      ")\r\n"
      "* 121 FETCH (UID 7535 RFC822.SIZE 64661 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {184}\r\n"
      "From: LC <mail@executive-learning.co.in>\r\n"
      "Subject: Letter of Credit Transactions International Trade UCP 600, "
      "ISBP 745 INCOTERMS  2010 & URBPO\r\n"
      "Date: Mon, 5 Jun 2017 06:08:55 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 122 FETCH (UID 7536 RFC822.SIZE 84459 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {143}\r\n"
      "From: Princeton Academy <mail@executive-learning.co.in>\r\n"
      "Subject: Workshop on Key Account Management\r\n"
      "Date: Mon, 05 Jun 2017 06:12:57 +0530\r\n"
      "\r\n"
      ")\r\n"
      "* 123 FETCH (UID 7547 RFC822.SIZE 50492 FLAGS (\Seen) "
      "BODY[HEADER.FIELDS (From Date Subject List-Unsubscribe)] {144}\r\n"
      "From: Jio Notifications <Notification@Jio.com>\r\n"
      "Subject: Recharge successful for Jio Number 8169236325\r\n"
      "Date: Tue, 6 Jun 2017 01:56:30 -0700\r\n"
      "\r\n"
      ")";

#include "bqdecode.c"

IMAPMessageHeader* parseImapHeader(char* pos, int len) {
    char* end = pos + len;
    IMAPMessageHeader* head = calloc(1, sizeof(IMAPMessageHeader));
    while (pos < end) {
        while (*pos == ' ' || *pos == '\r' || *pos == '\n') pos++;
        char* col = strpbrk(pos, ":\r\n");
        if (col && col[0] == ':' && col[1] == ' ') {
            // line is a (new) header field
            char* fld = pos;
            *col = 0;
            char* val = col + 2;
            col = strpbrk(val, "\r\n");
            while (col && col < end - 2
                && col[2] == ' ') // start with space means continuation
                col = strpbrk(col, "\r\n");
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
            } else if (!strcmp(fld, "List-Unsubscribe")) {
            }

            // TODO: now val may be multiline (softwrap) in which case unwrap it

            // TODO: ewdecode should also handle multiline or does it?
            if (*val == '=') ewdecode(val, col - val);

            printf("field: [%s = %s]\n", fld, val);

            pos = col + 1;

        } else {
            // continuation of an existing header, most likely
        }
    }
}

String* imaplist(char* url, char* user, char* pass, char* cmd, String* ret) {
    CURL* curl_handle = curl_easy_init(); // find a way to reuse the handle
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);
    // curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);

    // curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, curlcbfn);
    // curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, ret);

    curl_easy_setopt(curl_handle, CURLOPT_DEBUGFUNCTION, curldbgcbfn);
    curl_easy_setopt(curl_handle, CURLOPT_DEBUGDATA, ret);

    curl_easy_setopt(curl_handle, CURLOPT_USERNAME, user);
    curl_easy_setopt(curl_handle, CURLOPT_PASSWORD, pass);
    curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, cmd);

    // curl_easy_setopt(curl_handle, CURLOPT_HEADER, !!header);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERFUNCTION, curlcbhead);
    // curl_easy_setopt(curl_handle, CURLOPT_HEADERDATA, ret);

    // if (header) String_resize(ret, 512);
    curl_easy_perform(curl_handle);
    curl_easy_cleanup(curl_handle);
    return ret;
}
