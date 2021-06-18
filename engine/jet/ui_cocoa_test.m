#define IFDEBUG(s)


#if 0
#include "rt0.m"
// ^ rt0.m used ideally, but now issues due to not finding clock_Time & what not. sort out the C/H separation in engine core files
#else
#include "ui/ui_cocoa.h"
int main() { App_start(); }
#endif

void drawFunc(View* v) {
  drawRect(0,0,View_width(v),View_height(v));
  fillOval(100,100,80,100);
}

void bclick(Button*b){
    MsgBox("Click!","Bhabru");
}

void start(IFDEBUG(const char* callsite_)) {
  Window* w = Window_new();
  Window_setTitle(w,"My App RBCK");
  Window_setEffect(w);
  // Window_show(w);
  Window_resize(w,800,600);

  View* v = View_new();
  // View_resize(v,400,300);
  // View_move(v,100,100);
  View_setDrawFunc(v, drawFunc);
  Window_addSubview(w,v);
  // View_rotate(v,-15);

  MsgBox("wahoo!","Yonza");

  Button* b = Button_new("Kecks");
  Button_setTitle(b,"Jackdaws");
  Button_setOnClick(b, bclick);
  Window_addSubview(w, b);
  View_move(b, 200,150);

  Label*l=Label_new("Dingbats:");
  Window_addSubview(w, l);

  TextField*t=TextField_new("Wonk");
  Window_addSubview(w, t);
  View_move(t, 72,0);

}
