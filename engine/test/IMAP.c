#include "jet/net/IMAP.h"

void newHeader(MIMEHeader* header) {
    printf("---\nuid: %d\nrfcsize: %d\nsubject: %s\nfrom: %s [%s]\n",
        header->uid, header->rfc822Size, header->subject, header->from->name,
        header->from->email);
}
int main(int argc, char* argv[]) {
    char* cmd = "fetch 1:10 (flags uid rfc822.size internaldate "
                "body.peek[header.fields "
                "(subject from list-unsubscribe)])";
    // String* dat = imaplist(argv[1], argv[2], argv[3], cmd, (String[1]) {});
    IMAPFolderInfo* fi = NEW(IMAPFolderInfo);
    fi->on_newHeaderReceived = newHeader;

    imaplist(argv[1], argv[2], argv[3], cmd, fi);
    // String_print(dat);
    // parseImapFetch(strdup(imaptst), ctstrlen(imaptst));
    // parseImapFetch(dat->ref, dat->len);
    return 0;
}