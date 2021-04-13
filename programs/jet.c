typedef enum CompilerModes {
    CompilerMode_lint,
    CompilerMode_build,
    CompilerMode_run,
    CompilerMode_test
} CompilerModes;

typedef struct Compiler {
    Module* module;
    Parser* parser;
    Diagnostics* diag;
    char* filename;
    CompilerModes mode;
} Compiler;

typedef struct {
    char *file, *data;
    long size;
    PtrArray lines;
} Parser;

int main(int argc, char* argv[]) {
    Compiler* com = Compiler_new();
    Compiler_parseOpts(com, argc, argv);
    Compiler_loadModule(com);
    if (com->mode >= CompilerMode_lint)
        if (com->module)
            Module_write(com->module);
        else if (com->parser)
            Parser_format(com->parser);
    if (com->mode >= CompilerMode_build) Compiler_build();
    if (com->mode >= CompilerMode_run) Compiler_run();
    if (com->mode >= CompilerMode_test) Compiler_test();
}

void Parser_format(Parser* parser) {
    // this walks the tokens and reformats them. used when linter cannot be used
    // due to parse errors. also used as a standalone jetfmt tool (or not?).
    // walk over lines[], not data
    for_to(i, parser->lines.used) {
        int cols = parser->lines.ref[i + 1] - parser->lines.ref[i];
        // ^ FIXME
        while (!ISIN(2, parser->token, tkNullChar)) {
            // TODO: insert newlines where possible if cols > 80
            Token_format(parser->token);
        }
    }
}

void Token_format() { } // pretty print token and advance()

void Compiler_loadModule(Compiler* com) {
    // See if there is an up-to-date x-file for the module and load it.
    if ((com->module = Module_load(com->filename))) return;

    // Parse the module from the source file.
    com->parser = Parser_new(com->filename);

    // Parser_parse will return a module if it has no PARSE errors. It may later
    // be found to have other kinds of errors (e.g. during analysis). You can
    // lint a module with those other errors (but not build it). If there are
    // parse errors, you won't get a module (returns NULL). Name resolution
    // errors within parse() are tolerated for returning the module.

    if ((com->module = Parser_parse(com->parser))) {
        // When building the file, you need only analyse what is reachable from
        // within start(). Everything else is dead, unused code.
        if (com->mode >= CompilerMode_build) {
            Func* func = com->module->func("main()");
            if (func) {
                Func_analyse(func, com->module);
            } else
                com->diag->errorMissingMain();
        } else {
            // For linting, testing etc. you should analyse the whole module,
            // since you want to report as many errors as possible, even inside
            // unused code.
            Module_analyse(com->module);
        }
        // Save the module as an x-file for next time.
        if (!com->module->errors) Module_save(com->module);
    }
    // com->modules has a list of all modules including deps loaded during
    // parsing. They are in the right dependent order so you can traverse the
    // list and emit them one by one. Those that were parsed will not have
    // ->analysed flag set so analyse them now and save them as an x-file. Else
    // if they were loaded from x-file they have ->analysed set and won't be
    // rechecked.

    // if you are linting, only analyse the full root module. If you are
    // building, start analysis from start() and it will go wherever it needs.
}

Module_load() { }
// for file.jet see if file.x exists and is newer. then load and return it.