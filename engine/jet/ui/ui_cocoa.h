#include "Cocoa/Cocoa.h"

extern void start(IFDEBUG(const char* callsite_));

/*** APP DELEGATE -------------------------------------------------------*/

@interface App : NSResponder <NSApplicationDelegate> {
  BOOL autoQuit;
  @private
  NSMenu* menu;
}
@end

@implementation App
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:
    (NSApplication*)sender {
  return autoQuit;
}
- (void)applicationDidFinishLaunching:(NSNotification*)notification {
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
  [NSApp activateIgnoringOtherApps:YES];
  autoQuit = YES;
  start(IFDEBUG("start\n"));
}
@end
void App_start() {
  @autoreleasepool {
    [NSApplication sharedApplication];
    [NSApp setDelegate:[[App alloc] init]];
    [NSApp run];
  }
}

/*** VIEW ---------------------------------------------------------------*/

@interface View : NSView {
@private
    void (*ondrawrect)(View* v );
    void (*ondraw)();
    void (*ondrop)();
    void (*onload)();
    void (*onunload)();
    void (*onfocus)();
    void (*onblur)();
    void (*onmousedown)(NSEvent* event);
    void (*onmouseup)(NSEvent* event);
    void (*onkeydown)(NSEvent* event);
    void (*onkeyup)(NSEvent* event);
    void (*onmousemove)(NSEvent* event);
    void (*onmousedrag)(NSEvent* event);
    void (*onmouseover)(NSEvent* event);
    void (*onmouseout)(NSEvent* event);
    NSTrackingArea* trackingArea;
}
@end
@implementation View
- (BOOL)isFlipped {return YES;}
- (void)setDrawFunc:(void(*)(View* )) func {
    ondrawrect=func;
}
- (id)initWithTrackingInFrame:(NSRect)frame {
    self = [self initWithFrame:frame];
    if (self) {
        trackingArea = [[NSTrackingArea alloc]
            initWithRect:frame
                 options: NSTrackingMouseEnteredAndExited
                        | NSTrackingMouseMoved
                        | NSTrackingActiveInKeyWindow
                   owner:self
                userInfo:nil];
        [self addTrackingArea:trackingArea];
    }
    return self;
}
- (void)drawRect:(NSRect)dirtyRect {
    if (ondrawrect) ondrawrect(self );
}
- (void)mouseDown:(NSEvent*)event {
    if (onmousedown) onmousedown(event);
}
- (void)mouseUp:(NSEvent*)event {
    if (onmouseup) onmouseup(event);
}
- (void)mouseMoved:(NSEvent*)event {
    if (onmousemove) onmousemove(event);
}
- (void)mouseEntered:(NSEvent*)event {
    if (onmouseover) onmouseover(event);
}
- (void)mouseExited:(NSEvent*)event {
    if (onmouseout) onmouseout(event);
}
@end

void View_setDrawFunc(View* v, void (*ondraw)(View* )) {
[v setDrawFunc:ondraw];
}

void View_resize(View* v, int width, int height) {
  NSRect f = [v frame];
  if (width) { f.size.width = width; }
  if (height) { f.size.height = height; };
  [v setFrameSize:f.size];
}

void View_grow(View* v, int dw, int dh) {
  NSRect f = [v frame];
  f.size.width += dw;
  f.size.height += dh;
  [v setFrameSize:f.size];
}

void View_move(View* v, int dx, int dy) {
  NSRect f = [v frame];
  f.origin.x += dx;
  f.origin.y += dy;
  [v setFrameOrigin:f.origin];
}

void View_rotate(View* v, int deg) {
  double d = [v frameRotation];
  [v setFrameRotation:d+deg];
}

void View_reposition(View* v, int left, int top) {
  // NSRect f = [w->win frame];

  //   f.origin.x=left;

  //   f.origin.y +=    f.size.height-height;

  // [w->win setFrame:f display:NO animate:YES];
}

void View_wrapInEffect(View* v) {
  NSVisualEffectView* vs = [[NSVisualEffectView alloc]
      initWithFrame:[v frame]];
  [vs setAutoresizesSubviews:YES];
  [vs setAutoresizingMask:NSViewHeightSizable | NSViewWidthSizable];
  [[v superview] addSubview:vs positioned:NSWindowBelow relativeTo:v];
  [vs addSubview:v];
}

double View_height(View* v){
  NSRect r = [v frame];
  return r.size.height;
}
double View_width(View* v){
  NSRect r = [v frame];
  return r.size.width;
}

View* View_new() {
  View* v = [[View alloc] initWithFrame:NSMakeRect(0, 0, 500, 400) ];
  return v;
}

/*** WINDOW -------------------------------------------------------------*/

@interface Window : NSWindow {} @end
@implementation Window
@end

void Window_setDrawFunc(Window* w, void (*ondraw)( View* )) {
  [[w contentView] setDrawFunc:ondraw];
}

void Window_setTitle(Window* w, char* s) {
  [w setTitle:[NSString stringWithCString:s encoding:NSUTF8StringEncoding]];
}

void Window_resize(Window* w, int width, int height) {
  NSRect f = [w frame];
  if (width) { f.size.width = width; }
  if (height) {
    f.origin.y += f.size.height - height;
    f.size.height = height;
  };
  [w setFrame:f display:NO animate:YES];
}

void Window_reposition(Window* w, int left, int top) {
  // NSRect f = [w frame];

  //   f.origin.x=left;

  //   f.origin.y +=    f.size.height-height;

  // [w setFrame:f display:NO animate:YES];
}

void Window_setEffect(Window* w) {
  NSVisualEffectView* v =
      [[NSVisualEffectView alloc] initWithFrame:[[w contentView] frame]];
  [v setAutoresizesSubviews:YES];
  [v setAutoresizingMask:NSViewHeightSizable | NSViewWidthSizable];
  [v addSubview:[w contentView]];
  [w setContentView:v];
}

void Window_addSubview(Window* w, View* v){
  [[w contentView] addSubview:v];
}

Window* Window_new() {
  Window* w = [[Window alloc]
    initWithContentRect: NSMakeRect(0, 0, 500, 400)
              styleMask: NSWindowStyleMaskTitled
                       | NSWindowStyleMaskResizable
                       | NSWindowStyleMaskMiniaturizable
                       | NSWindowStyleMaskClosable
                backing: NSBackingStoreBuffered
                  defer: YES]; // autorelease];
  [w setReleasedWhenClosed:YES];
  [w setTitle:@"Untitled"];

  [w cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
  [w makeKeyAndOrderFront:nil];

// replace the contentView (NSView) with a View
  [w setContentView:[[View alloc] initWithFrame:[[w contentView] frame]]];

  // w.onload = NULL;
  // puts("new Win");
  return w;
}

// theres strange behaviour if you dont call this. so let's just add it in
// Window_new void Window_show(Window* w) {
//    [w makeKeyAndOrderFront:nil];
//  }


/*** GLOBALS ------------------------------------------------------------*/

void MsgBox(const char* text, const char* subtext) {
    NSAlert* alert = [[NSAlert alloc] init];
    alert.messageText = [NSString stringWithUTF8String:text];
    alert.alertStyle = NSAlertStyleInformational;
    alert.informativeText = [NSString stringWithUTF8String:subtext];
    [alert runModal];
}

/*** DRAWING ------------------------------------------------------------*/

// TODO: Shape : NSBezierPath
// void fillShape(Shape* s) {
//   [s fill];
// }

//TODO: all funcs must take a dummy Graphics arg
void fillOval(double x, double y, double w, double h) {
    [[NSBezierPath
        bezierPathWithOvalInRect:NSMakeRect(x - w / 2, y - h / 2, w, h)]
        fill];
}
void fillRect( double x, double y, double w, double h) {
    [NSBezierPath fillRect:NSMakeRect(x,y,w,h)];
}
void drawRect( double x, double y, double w, double h) {
    [NSBezierPath strokeRect:NSMakeRect(x,y,w,h)];
}
void drawOval(double x, double y, double r1, double r2) {
    [[NSBezierPath
        bezierPathWithOvalInRect:NSMakeRect(x - r1 / 2, y - r2 / 2, r1, r2)]
        stroke];
}
void drawCircle(double x, double y, double r) { drawOval(x, y, r, r); }
