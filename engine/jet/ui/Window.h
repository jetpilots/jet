#include "Cocoa/Cocoa.h"

typedef struct Window {
  NSWindow* win;
  void (*onload)(struct Window* self);
} * Window;

void Window_setTitle(Window w, char* s) {
  [w->win setTitle:[NSString stringWithCString:s encoding:NSUTF8StringEncoding]];
}

void Window_resize(Window w, int width, int height){
  NSRect f = [w->win frame];
  if (width) {
    f.size.width=width;
    }
  if(height){
    f.origin.y += f.size.height-height;
    f.size.height=height;
    };
  [w->win setFrame:f display:NO animate:YES];
}

void Window_reposition(Window w, int left, int top){
  // NSRect f = [w->win frame];

  //   f.origin.x=left;

  //   f.origin.y +=    f.size.height-height;



  // [w->win setFrame:f display:NO animate:YES];
}

void Window_setEffect(Window w) {
  NSVisualEffectView* v = [[NSVisualEffectView alloc] initWithFrame:[[w->win contentView] frame]];
  [v setAutoresizesSubviews:YES];
  [v setAutoresizingMask:NSViewHeightSizable|NSViewWidthSizable];
  [v addSubview:[w->win contentView]];
  [w->win setContentView:v];
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
[w->win makeKeyAndOrderFront:nil];

  w->onload = NULL;
  // puts("new Win");
  return w;
}

// theres strange behaviour if you dont call this. so let's just add it in Window_new
// void Window_show(Window w) {
//    [w->win makeKeyAndOrderFront:nil];
//  }