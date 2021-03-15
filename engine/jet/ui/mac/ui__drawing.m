
void fillOval(jet_Point p, jet_Size s) {
    // NSBezierPath* thePath = ;
    NSRect ovalRect = NSMakeRect(p.x - s.w / 2, p.y - s.h / 2, s.w, s.h);
    [[NSBezierPath bezierPathWithOvalInRect:ovalRect] fill];
}
void fillRect(jet_Rect r) {
    // jet_Rect should have same layout as NSRect
    [[NSBezierPath rectangle:r] fill];
}
// void drawCircle(jet_Point p, double r) { drawOval(p.x, p.y, r, r); }


void drawOval(double x, double y, double r1, double r2) {
    // NSBezierPath* thePath = ;
    NSRect ovalRect = NSMakeRect(x - r1 / 2, y - r2 / 2, r1, r2);
    [[NSBezierPath bezierPathWithOvalInRect:ovalRect] fill];
}

void drawCircle(double x, double y, double r) { drawOval(x, y, r, r); }
