#ifndef HAVE_JET_BASE_H
#include "jet/base.h"
#endif
// #include "jet/core/Dict.h"
#include "jet/os/Ticks.h"
#include "jet/os/sys.h"

#include "_rt/errhandler.h"
#include "_rt/entry.h"
#include "_rt/check.h"
#include "_rt/stack.h"

#ifdef GUI_COCOA
// figure out if this really needs to be read by all mods, or only
// modules in which any UI code is present
#include <Cocoa/Cocoa.h>
#define NSStr(s) [NSString stringWithUTF8String:s]
@class Window_;
typedef Window_* Window;
static Window Window_new_(IFDEBUG(char* cs_)) { return nil; }
@class Button_;
typedef Button_* Button;
static Button Button_new_(IFDEBUG(char* cs_)) { return nil; }
#define Button_addrof_view(b) &(b)

@class View_;
typedef View_* View;
static View View_new_(IFDEBUG(char* cs_)) { return nil; }
@class Label_;
typedef Label_* Label;
static Label Label_new_(IFDEBUG(char* cs_)) { return nil; }
@class TextField_;
typedef TextField_* TextField;
static TextField TextField_new_(IFDEBUG(char* cs_)) { return nil; }
@class VibrantView_;
typedef VibrantView_* VibrantView;
#endif

// *** This should go in the generated .c file of each module!
#define DECL_COV_PROF(nlines)                                              \
  monostatic UInt64 _cov_[nlines];                                         \
  monostatic Ticks _lprof_last_, _lprof_tmp_, _lprof_[nlines];
