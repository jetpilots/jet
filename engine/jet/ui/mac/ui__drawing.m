
void fillOval( double x, double y, double w, double h) {
    [[NSBezierPath
        bezierPathWithOvalInRect:NSMakeRect(x - w / 2, y - h / 2, w, h)]
        fill];
}
void fillRect( double x, double y, double w, double h) {
    [[NSBezierPath rectangle:NSMakeRect(x,y,w,h)] fill];
}
void drawOval(double x, double y, double r1, double r2) {
    [[NSBezierPath
        bezierPathWithOvalInRect:NSMakeRect(x - r1 / 2, y - r2 / 2, r1, r2)]
        fill];
}
void drawCircle(double x, double y, double r) { drawOval(x, y, r, r); }
