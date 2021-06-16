#include "Cocoa/Cocoa.h"

@interface View : NSView {
@private
    void (*ondrawrect)(View* v, NSRect dirtyRect);
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

- (void)setDrawFunc:(void(*)(View*, NSRect)) func {
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
    if (ondrawrect) ondrawrect(self, dirtyRect);
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