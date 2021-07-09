
#include <objc/objc.h>

Class NSWindow, NSButton, NSView;

Class View, Window, Button;

#define _o_c(c) c = objc_getClass(#c)
#define _o_sc(c, s) c = obj_allocateClassPair(s, #c, 0)
#define _o_rc(c) objc_registerClassPair(c)
#define _o_s sel_registerName
#define _o_im(c, s, f, t) class_addMethod(c, _o_s(s), (IMP)f, t)
#define _o_iv class_addIvar

bool IMP_View_isFlipped(id self, SEL sel) {
  bool* ret;
  object_getInstanceVariable(self, "flipped", &ret);
  return *ret;
}

void _oc_init() {
  _o_c(NSWindow);
  _o_c(NSButton);
  _o_c(NSView);

  // Make View: NSView
  _o_sc(View, NSView);
  _o_iv(View, "flipped", 1, 0, "B");
  _o_im(View, "isFlipped", IMP_View_isFlipped, "B@:");
  _o_rc(View);

  _o_sc(Button, NSButton);
  _o_rc(Button);

  _o_sc(Window, NSWindow);
  _o_rc(Window);
}
