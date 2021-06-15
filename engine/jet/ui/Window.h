#include "Cocoa/Cocoa.h"

typedef struct Window {
  NSWindow* win;
  void (*onload)(struct Window* self);
} * Window;

void Window_setTitle(Window w, char* s) {
  [w->win setTitle:[NSString stringWithCString:s encoding:NSUTF8StringEncoding]];
}

Window Window_new() {
  Window w = malloc(sizeof(struct Window));
  // you ll need to subc NSWindow & that will be Window. not C struct like above.
  w->win = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 500, 400)
                                       styleMask:NSWindowStyleMaskTitled
                                       | NSWindowStyleMaskResizable
                                       | NSWindowStyleMaskMiniaturizable
                                       | NSWindowStyleMaskClosable
                                         backing:NSBackingStoreBuffered
                                           defer:NO] ;//autorelease];
  [w->win setReleasedWhenClosed:YES];
  [w->win setTitle:@"Untitled"];

  [w->win cascadeTopLeftFromPoint:NSMakePoint(20, 20)];

  w->onload = NULL;
  // puts("new Win");
  return w;
}

void Window_show(Window w) {
   [w->win makeKeyAndOrderFront:nil];
 }