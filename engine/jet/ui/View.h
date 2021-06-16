#include "Cocoa/Cocoa.h"

typedef struct View {
  NSWindow* v;
  void (*ondraw)(struct View* self);
} * View;

void View_setDrawFunc(View v, void (*ondraw)( View* )) {

}

void View_resize(View w, int width, int height) {
  NSRect f = [w->win frame];
  if (width) { f.size.width = width; }
  if (height) {
    f.origin.y += f.size.height - height;
    f.size.height = height;
  };
  [w->win setFrame:f display:NO animate:YES];
}

void View_reposition(View w, int left, int top) {
  // NSRect f = [w->win frame];

  //   f.origin.x=left;

  //   f.origin.y +=    f.size.height-height;

  // [w->win setFrame:f display:NO animate:YES];
}

// void View_setEffect(View w) {
//   NSVisualEffectView* v = [[NSVisualEffectView alloc]
//       initWithFrame:[[w->win contentView] frame]];
//   [v setAutoresizesSubviews:YES];
//   [v setAutoresizingMask:NSViewHeightSizable | NSViewWidthSizable];
//   [v addSubview:[w->win contentView]];
//   [w->win setContentView:v];
}

View View_new() {
  View w = malloc(sizeof(struct View));
  // you ll need to subc NSWindow & that will be View. not C struct like
  // above.
  w->win = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 500, 400)
                                       styleMask:NSWindowStyleMaskTitled
                                       | NSWindowStyleMaskResizable
                                       | NSWindowStyleMaskMiniaturizable
                                       | NSWindowStyleMaskClosable
                                         backing:NSBackingStoreBuffered
                                           defer:NO]; // autorelease];
  // [w->win setReleasedWhenClosed:YES];
  // [w->win setTitle:@"Untitled"];

  // [w->win cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
  // [w->win makeKeyAndOrderFront:nil];

  w->onload = NULL;
  // puts("new Win");
  return w;
}

// theres strange behaviour if you dont call this. so let's just add it in
// View_new void View_show(View w) {
//    [w->win makeKeyAndOrderFront:nil];
//  }