
@interface JetUIView : NSView {
    void (*ondrawrect)(NSRect dirtyRect);
    void (*ondraw)();
    void (*ondrop)();
    void (*onload)();
    void (*onunload)();
    void (*onfocus)();
    void (*onblur)();
    void (*onmousedown)(JetUIEvent* event);
    void (*onmouseup)(JetUIEvent* event);
    void (*onkeydown)(JetUIEvent* event);
    void (*onkeyup)(JetUIEvent* event);
    void (*onmousemove)(JetUIEvent* event);
    void (*onmousedrag)(JetUIEvent* event);
    void (*onmouseover)(JetUIEvent* event);
    void (*onmouseout)(JetUIEvent* event);
@private
    NSTrackingArea* trackingArea;
}
@end
@implementation JetUIView

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
    if (ondrawrect) ondrawrect(dirtyRect);
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