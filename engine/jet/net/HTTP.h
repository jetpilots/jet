
typedef enum {
    HTTPMethods_get,
    HTTPMethods_post,
    HTTPMethods_put,
    HTTPMethods_delete
} HTTPMethods;

typedef enum {
    MIMETypes_plain,
    MIMETypes_json,
    MIMETypes_xml,
    MIMETypes_yaml
} MIMETypes;

typedef struct {
    char* url;
    HTTPMethods method;
    TextEncoding dataEncoding;
    MIMETypes dataType;
    char* data;
} Request;

typedef struct {
    const char** headers;
    String body;
    uint16_t time, code;
    TextEncoding encoding : 8;
} Response;

httpdo(Request* req, Response* resp) { }
static int curldbgcbfn(
    CURL* handle, curl_infotype type, char* data, size_t size, void* userptr) {
    if (type == CURLINFO_HEADER_IN) String_appendCString(userptr, data, size);
    return 0;
}

static size_t curlcbfn(void* data, size_t size, size_t nmemb, void* tgt) {
    String_appendCString(tgt, data, nmemb * size);
    return nmemb * size;
}

static size_t curlcbhead(void* data, size_t size, size_t nmemb, void* target) {
    if (!strncmp("Content-Length: ", data, 16)) {
        // printf("*** found len: (%d) %s\n", atoi(data + 16), data + 16);
        String_growTo(target, atoi(data + 16));
        // this way you can get it in 1 alloc, well mostly. If you have headers
        // on they take some more space, so in practice you will need 1 more
        // allocation.

        // You could also do more fancy stuff like reading content-type and
        // doing something based on that. But let's leave that for the more
        // capable methods which take Request and return Response instead of the
        // simple async_httpget which just takes a url and returns a string.
    }
    return nmemb * size;
}

// I THINK ALL FUNCTIONS SHOULD BE PROMOTED TO TOPLEVEL EXPRS
// AND ALL RETURN TYPES SHOULD BE VOID
String* httpget(char* url, int header, int debug, String* ret) {
    CURL* curl_handle = curl_easy_init(); // find a way to reuse the handle
    curl_easy_setopt(curl_handle, CURLOPT_URL, url);
    curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);
    // curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, curlcbfn);
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, ret);
    curl_easy_setopt(curl_handle, CURLOPT_HEADER, !!header);
    curl_easy_setopt(curl_handle, CURLOPT_HEADERFUNCTION, curlcbhead);
    curl_easy_setopt(curl_handle, CURLOPT_HEADERDATA, ret);

    if (header) String_resize(ret, 512);
    curl_easy_perform(curl_handle);
    curl_easy_cleanup(curl_handle);
    return ret;
}
