typedef struct Window Window;
char* Window_get_title(Window* win) {
#ifdef JET_MAC
#endif
#ifdef JET_GTK
#endif
#ifdef JET_WIN
#endif
}