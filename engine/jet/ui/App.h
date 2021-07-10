#include "Cocoa/Cocoa.h"

// typedef struct {
//   char autoQuit;
// } _App;
// _App _app = {};

extern void start();

// @interface JetUIApp {
//   @public
//   BOOL autoQuit;
// }
// @end
@interface JetUIAppController : NSResponder <NSApplicationDelegate> {
  BOOL autoQuit;
  @private
  NSMenu* menu;
  NSWindow* window;
}
@end

@implementation JetUIAppController
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:
  (NSApplication*)sender {
  return autoQuit;
}
- (void)applicationDidFinishLaunching:(NSNotification*)notification {
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
  autoQuit = YES;
  // Menu
  // menu = loadMainMenu();
  // [NSApp setMainMenu:menu]; // makeMainMenu()];

  // Window
  // if (!window) {
  //   window =
  //       [[[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 500, 400)
  //                                    styleMask:NSWindowStyleMaskTitled
  //                                    | NSWindowStyleMaskResizable
  //                                    | NSWindowStyleMaskMiniaturizable
  //                                    | NSWindowStyleMaskClosable
  //                                      backing:NSBackingStoreBuffered
  //                                        defer:NO] autorelease];
  //   [window setReleasedWhenClosed:NO];
  //   [window setTitle:@"My App RBCK"];
  // }
  // [window cascadeTopLeftFromPoint:NSMakePoint(20, 20)];
  // [window makeKeyAndOrderFront:nil];

  // // Toolbar
  // ExampleToolbarController1* tbCon = [ExampleToolbarController1 new];
  // window.toolbar = tbCon.toolbar;

  // // Content view
  // // id nsv = [[KaleidoView alloc]
  // // initWithTrackingInFrame:NSMakeRect(0,0,200,200)];
  // id nsv =
  //     [[ScatterPlotView alloc] initWithFrame:NSMakeRect(0, 0, 200, 200)];
  // // [nsv setAutoresizingMask: NSViewHeightSizable|NSViewWidthSizable];

  // // [nsv setData];
  // [nsv setRandomData:3000];
  // // [[window contentView] addSubview: nsv];
  // window.contentView = nsv; //<< you can do this too

  // [window.contentView setAutoresizesSubviews:YES];
  [NSApp activateIgnoringOtherApps:YES];
  // puts("startUI:");
  start();
  // puts("/startUI");
}
@end

void App_start(void) {
  @autoreleasepool {
    [NSApplication sharedApplication];
    [NSApp setDelegate:[[JetUIAppController alloc] init]];
    [NSApp run];
  }
}