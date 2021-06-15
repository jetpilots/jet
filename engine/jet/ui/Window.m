
#include "App.h"
#include "Window.h"

int main() {
  App_start(); // compiler should auto insert this. or there should be alt main for GUI
}

void start(){
  Window w = Window_new();
  Window_setTitle(w,"My App RBCK");
  Window_show(w);
  }