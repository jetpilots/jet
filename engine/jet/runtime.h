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
@interface Window_ : NSWindow
@end
typedef Window_* Window;
Window Window_new_(IFDEBUG(char* cs_)) { return nil; }
@interface Button_ : NSButton
@end
typedef Button_* Button;
Button Button_new_(IFDEBUG(char* cs_)) { return nil; }
@interface View_ : NSView
@end
typedef View_* View;
View View_new_(IFDEBUG(char* cs_)) { return nil; }
@interface Label_ : NSTextField
@end
typedef Label_* Label;
Label Label_new_(IFDEBUG(char* cs_)) { return nil; }
@interface TextField_ : NSTextField
@end
typedef TextField_* TextField;
TextField TextField_new_(IFDEBUG(char* cs_)) { return nil; }
@interface VibrantView_ : NSVisualEffectView
@end
typedef VibrantView_* VibrantView;
#endif

// *** This should go in the generated .c file of each module!
#define DECL_COV_PROF(nlines)                                              \
  monostatic UInt64 _cov_[nlines];                                         \
  monostatic Ticks _lprof_last_, _lprof_tmp_, _lprof_[nlines];
