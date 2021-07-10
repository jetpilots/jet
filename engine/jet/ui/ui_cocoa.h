#include "Cocoa/Cocoa.h"

extern void start(IFDEBUG(const char* callsite_));
#define NSStr(s) [NSString stringWithUTF8String:s]
/*** APP DELEGATE -------------------------------------------------------*/

@interface App_ : NSResponder <NSApplicationDelegate> {
  BOOL autoQuit;
  @private
  NSMenu* menu;
}
@end

@implementation App_
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:
  (NSApplication*)sender {
  return autoQuit;
}
- (void)applicationDidFinishLaunching:(NSNotification*)notification {
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
  [NSApp activateIgnoringOtherApps:YES];
  autoQuit = YES;
  _jet_entry_run_(IFDEBUG("start\n"));
}
@end
void App_start() {
  @autoreleasepool {
    [NSApplication sharedApplication];
    [NSApp setDelegate:[[App_ alloc] init]];
    [NSApp run];
  }
}

/*** VIBRANT VIEW -------------------------------------------------------*/

@interface VibrantView_ : NSVisualEffectView
@end
@implementation VibrantView_
- (BOOL)isFlipped {
  return YES;
}
@end

/*** VIEW ---------------------------------------------------------------*/

@interface View_ : NSView {
  @private
  void (*ondrawrect)(View_* v);
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
@implementation View_
- (BOOL)isFlipped {
  return YES;
}
- (void)setDrawFunc:(void (*)(View_*))func {
  ondrawrect = func;
}
- (id)initWithTrackingInFrame:(NSRect)frame {
  self = [self initWithFrame:frame];
  if (self) {
    trackingArea = [[NSTrackingArea alloc]
      initWithRect:frame
           options:NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved
           | NSTrackingActiveInKeyWindow
             owner:self
          userInfo:nil];
    [self addTrackingArea:trackingArea];
  }
  return self;
}
- (void)drawRect:(NSRect)dirtyRect {
  if (ondrawrect) ondrawrect(self);
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

void View_setDrawFunc(View_* v, void (*ondraw)(View_*)) {
  // if (v->ondrawrect) return; // raise error
  [v setDrawFunc:ondraw];
}

// typedef void id
void View_resize(id v, int width, int height) {
  NSRect f = [v frame];
  if (width) { f.size.width = width; }
  if (height) { f.size.height = height; };
  [v setFrameSize:f.size];
}

void View_grow(id v, int dw, int dh) {
  NSRect f = [v frame];
  f.size.width += dw;
  f.size.height += dh;
  [v setFrameSize:f.size];
}

void View_move(id v, int dx, int dy) {
  NSRect f = [v frame];
  f.origin.x += dx;
  f.origin.y += dy;
  [v setFrameOrigin:f.origin];
}

void View_rotate(id v, int deg) {
  double d = [v frameRotation];
  [v setFrameRotation:d + deg];
}

void View_reposition(id v, int left, int top) {
  NSRect ref;
  if ([v superview])
    ref = [v superview].frame;
  else
    ref = [v window].contentView.frame;

  NSRect f = [v frame];
  f.origin.x = left + ref.origin.x;
  f.origin.y = ref.size.height - f.size.height - top;

  [v setFrame:f display:NO animate:YES];
}

void View_wrapInEffect(View_* v) {
  VibrantView_* vs = [VibrantView_.alloc initWithFrame:[v frame]];
  [vs setAutoresizesSubviews:YES];
  // [vs setAutoresizingMask:NSViewHeightSizable | NSViewWidthSizable];
  [vs setAutoresizingMask:[v autoresizingMask]];
  id sup = [v superview];
  if (!sup) sup = [v window].contentView;
  [sup addSubview:vs positioned:NSWindowBelow relativeTo:v];
  [vs addSubview:v];
}

double View_height(id v) { return [v frame].size.height; }
double View_width(id v) { return [v frame].size.width; }
View_* View_parent(id v) { return (View_*)[v superview]; }

// Returns Window_* */
id View_window(id v) { return [v window]; }

View_* View_new() {
  return [View_.alloc initWithFrame:NSMakeRect(0, 0, 500, 400)];
}

/*** WINDOW -------------------------------------------------------------*/

@interface Window_ : NSWindow {
}
@end
@implementation Window_
@end

void Window_setDrawFunc(Window_* w, void (*ondraw)(View_*)) {
  [w.contentView setDrawFunc:ondraw];
}

void Window_setTitle(Window_* w, char* s) { [w setTitle:NSStr(s)]; }

void Window_resize(Window_* w, int width, int height) {
  NSRect f = w.frame;
  if (width) { f.size.width = width; }
  if (height) {
    f.origin.y += f.size.height - height;
    f.size.height = height;
  };
  [w setFrame:f display:NO animate:YES];
}

void Window_reposition(Window_* w, int left, int top) {
  NSRect f = w.frame;
  NSRect sf = w.screen.frame;
  f.origin.x = left + sf.origin.x;
  // NOPE: you need to know the screen height here.
  f.origin.y = sf.size.height - f.size.height - top;
  [w setFrame:f display:NO animate:YES];
}

void Window_setEffect(Window_* w) {
  VibrantView_* v = [VibrantView_.alloc initWithFrame:w.contentView.frame];
  [v setAutoresizesSubviews:YES];
  [v setAutoresizingMask:NSViewHeightSizable | NSViewWidthSizable];
  [v addSubview:w.contentView];
  [w setContentView:v];
}

void Window_addSubview(Window_* w, id v) { [w.contentView addSubview:v]; }

Window_* Window_new() {
  Window_* w = [[Window_ alloc]
    initWithContentRect:NSMakeRect(0, 0, 500, 400)
              styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskResizable
              | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskClosable
                backing:NSBackingStoreBuffered
                  defer:YES]; // autorelease];
  [w setReleasedWhenClosed:YES];
  [w setTitle:@"Untitled"];

  [w cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
  [w makeKeyAndOrderFront:nil];

  // replace the contentView (NSView) with a View_
  [w setContentView:[View_.alloc initWithFrame:w.contentView.frame]];

  // w.onload = NULL;
  // puts("new Win");
  return w;
}

// theres strange behaviour if you dont call this. so let's just add it in
// Window_new void Window_show(Window_* w) {
//    [w makeKeyAndOrderFront:nil];
//  }

/*** BUTTON -------------------------------------------------------------*/

@interface Button_ : NSButton {
  @public
  void (*onclick)(Button_*);
}
@end
@implementation Button_
- (void)clicked {
  if (onclick) onclick(self);
}
@end

Button_* Button_new(char* s) {
  Button_* ret = [Button_ buttonWithTitle:NSStr(s)
                                   target:nil
                                   action:@selector(clicked)];
  ret.target = ret;
  return ret;
}

void Button_setOnClick(Button_* b, void (*fn)(Button_*)) {
  b->onclick = fn;
}

// TODO: this should be Control_setTitle & others just call that. Many funcs
// are like this & would be a pain to repeat wrappers. In Jet you have a
// parallel hierarchy & Label_ is a Control (as is Button_, Checkbox, etc) &
// setTitle(c Control) is defined & derived types just use that (via
// dispatcher -- but that will optimise to the right call). setTitle(c
// Control) or title!(c Control) or c.title = "..."?
void Button_setTitle(Button_* b, char* s) {
  [b setTitle:NSStr(s)];
  // [b sizeToFit];
}

/*** SLIDER -------------------------------------------------------------*/

@interface Slider_ : NSSlider {
  @public
  void (*onchange)(Slider_*);
}
@end
@implementation Slider_
- (void)changed {
  if (onchange) onchange(self);
}
@end

Slider_* Slider_new(double min, double max, double val) {
  Slider_* ret = [Slider_ sliderWithValue:val
                                 minValue:min
                                 maxValue:max
                                   target:nil
                                   action:@selector(changed)];
  ret.target = ret;
  return ret;
}

double Slider_value(Slider_* s) { return s.doubleValue; }
void Slider_setValue(Slider_* s, double v) { s.doubleValue = v; }

/*** TEXTFIELD ----------------------------------------------------------*/

@interface TextField_ : NSTextField {
  @public
  void (*onchange)(TextField_*);
}
@end
@implementation TextField_
- (void)changed {
  if (onchange) onchange(self);
}
@end

TextField_* TextField_new(char* s) {
  TextField_* ret = [TextField_ textFieldWithString:NSStr(s)];
  return ret;
}

void TextField_setOnChange(TextField_* b, void (*fn)(TextField_*)) {
  b->onchange = fn;
}

void TextField_setText(TextField_* f, char* s) {
  f.stringValue = NSStr(s);
  // [f sizeToFit];
}

/*** LABEL --------------------------------------------------------------*/

@interface Label_ : NSTextField
@end
@implementation Label_
@end

Label_* Label_new(char* s) {
  Label_* ret = [Label_ labelWithString:NSStr(s)];
  //  ret.target=ret;
  return ret;
}

// void Button_setOnClick(Button_* b, void (*fn)(Button_*)){b->onclick=fn;}

void Label_setTitle(Label_* l, char* s) {
  l.stringValue = NSStr(s);
  // [l sizeToFit];
}

/*** GLOBALS ------------------------------------------------------------*/

void msgBox(const char* text, const char* subtext) {
  NSAlert* alert = [[NSAlert alloc] init];
  alert.messageText = NSStr(text);
  alert.alertStyle = NSAlertStyleInformational;
  alert.informativeText = NSStr(subtext);
  [alert runModal];
}

/*** DRAWING ------------------------------------------------------------*/

// TODO: Shape : NSBezierPath
// void fillShape(Shape* s) {
//   [s fill];
// }

// TODO: all funcs must take a dummy Graphics arg
void fillOval(double x, double y, double w, double h) {
  [[NSBezierPath
    bezierPathWithOvalInRect:NSMakeRect(x - w / 2, y - h / 2, w, h)] fill];
}
void fillRect(double x, double y, double w, double h) {
  [NSBezierPath fillRect:NSMakeRect(x, y, w, h)];
}
void drawRect(double x, double y, double w, double h) {
  [NSBezierPath strokeRect:NSMakeRect(x, y, w, h)];
}
void drawOval(double x, double y, double r1, double r2) {
  [[NSBezierPath bezierPathWithOvalInRect:NSMakeRect(x - r1 / 2, y - r2 / 2,
                                            r1, r2)] stroke];
}
void drawCircle(double x, double y, double r) { drawOval(x, y, r, r); }
