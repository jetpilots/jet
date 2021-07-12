thread_local size_t _scDepth_ = 0;
thread_local size_t _scPrintAbove_ = 0;
thread_local char* _stack_boundary;
thread_local const char* _err_ = NULL;

thread_local uint64_t __randstate;

bool __jet_dbglog = 0;
