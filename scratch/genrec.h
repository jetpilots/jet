int THE_FUNCNAME(const char* const str) {
    const char* pos = str - 1;
    switch (*++pos) {
    case 'f':
        switch (*++pos) {
        case 'u':
            switch (*++pos) {
            case 'n':
                switch (*++pos) {
                case 'c':
                    switch (*++pos) {
                    case 't':
                        switch (*++pos) {
                        case 'i':
                            switch (*++pos) {
                            case 'o':
                                switch (*++pos) {
                                case 'n':
                                    switch (*++pos) {
                                    case '\0':
                                        THE_CODE(function);
                                    }
                                    break;
                                }
                                break;
                            }
                            break;
                        }
                        break;
                    }
                    break;
                }
                break;
            }
            break;
        }
        break;
    }
    THE_ERRHANDLER(str);
}
