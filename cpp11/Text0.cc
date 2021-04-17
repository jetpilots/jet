struct Text0 {
    struct Line {
        int len, alloc;
        char str[];
    };
    int count, alloc;
    int len; // sum of (lines[i].len+1) over i=0:count
    Line* lines; //[count]
    char* buf; // [len]
};

// a continuous string made up of embeded -terminated strings (EOL is 0)

// socket class based on curl with linerecvd & debuglinerecvd events