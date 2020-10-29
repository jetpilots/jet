// NSImageNameActionTemplate
// NSImageNameAddTemplate
// NSImageNameAdvanced
// NSImageNameApplicationIcon
// NSImageNameBluetoothTemplate
// NSImageNameBonjour
// NSImageNameBookmarksTemplate
// NSImageNameCaution
// NSImageNameColorPanel
// NSImageNameColumnViewTemplate
// NSImageNameComputer
// NSImageNameDotMac
// NSImageNameEnterFullScreenTemplate
// NSImageNameEveryone
// NSImageNameExitFullScreenTemplate
// NSImageNameFlowViewTemplate
// NSImageNameFolder
// NSImageNameFolderBurnable
// NSImageNameFolderSmart
// NSImageNameFollowLinkFreestandingTemplate
// NSImageNameFontPanel
// NSImageNameGoBackTemplate
// NSImageNameGoForwardTemplate
// NSImageNameGoLeftTemplate
// NSImageNameGoRightTemplate
// NSImageNameHomeTemplate
// NSImageNameIChatTheaterTemplate
// NSImageNameIconViewTemplate
// NSImageNameInfo
// NSImageNameInvalidDataFreestandingTemplate
// NSImageNameLeftFacingTriangleTemplate
// NSImageNameListViewTemplate
// NSImageNameLockLockedTemplate
// NSImageNameLockUnlockedTemplate
// NSImageNameMenuMixedStateTemplate
// NSImageNameMenuOnStateTemplate
// NSImageNameMobileMe
// NSImageNameMultipleDocuments
// NSImageNameNetwork
// NSImageNamePathTemplate
// NSImageNamePreferencesGeneral
// NSImageNameQuickLookTemplate
// NSImageNameRefreshFreestandingTemplate
// NSImageNameRefreshTemplate
// NSImageNameRemoveTemplate
// NSImageNameRevealFreestandingTemplate
// NSImageNameRightFacingTriangleTemplate
// NSImageNameShareTemplate
// NSImageNameSlideshowTemplate
// NSImageNameSmartBadgeTemplate
// NSImageNameStatusAvailable
// NSImageNameStatusNone
// NSImageNameStatusPartiallyAvailable
// NSImageNameStatusUnavailable
// NSImageNameStopProgressFreestandingTemplate
// NSImageNameStopProgressTemplate
// NSImageNameTouchBarAddDetailTemplate
// NSImageNameTouchBarAddTemplate
// NSImageNameTouchBarAlarmTemplate
// NSImageNameTouchBarAudioInputMuteTemplate
// NSImageNameTouchBarAudioInputTemplate
// NSImageNameTouchBarAudioOutputMuteTemplate
// NSImageNameTouchBarAudioOutputVolumeHighTemplate
// NSImageNameTouchBarAudioOutputVolumeLowTemplate
// NSImageNameTouchBarAudioOutputVolumeMediumTemplate
// NSImageNameTouchBarAudioOutputVolumeOffTemplate
// NSImageNameTouchBarBookmarksTemplate
// NSImageNameTouchBarColorPickerFill
// NSImageNameTouchBarColorPickerFont
// NSImageNameTouchBarColorPickerStroke
// NSImageNameTouchBarCommunicationAudioTemplate
// NSImageNameTouchBarCommunicationVideoTemplate
// NSImageNameTouchBarComposeTemplate
// NSImageNameTouchBarDeleteTemplate
// NSImageNameTouchBarDownloadTemplate
// NSImageNameTouchBarEnterFullScreenTemplate
// NSImageNameTouchBarExitFullScreenTemplate
// NSImageNameTouchBarFastForwardTemplate
// NSImageNameTouchBarFolderCopyToTemplate
// NSImageNameTouchBarFolderMoveToTemplate
// NSImageNameTouchBarFolderTemplate
// NSImageNameTouchBarGetInfoTemplate
// NSImageNameTouchBarGoBackTemplate
// NSImageNameTouchBarGoDownTemplate
// NSImageNameTouchBarGoForwardTemplate
// NSImageNameTouchBarGoUpTemplate
// NSImageNameTouchBarHistoryTemplate
// NSImageNameTouchBarIconViewTemplate
// NSImageNameTouchBarListViewTemplate
// NSImageNameTouchBarMailTemplate
// NSImageNameTouchBarNewFolderTemplate
// NSImageNameTouchBarNewMessageTemplate
// NSImageNameTouchBarOpenInBrowserTemplate
// NSImageNameTouchBarPauseTemplate
// NSImageNameTouchBarPlayheadTemplate
// NSImageNameTouchBarPlayPauseTemplate
// NSImageNameTouchBarPlayTemplate
// NSImageNameTouchBarQuickLookTemplate
// NSImageNameTouchBarRecordStartTemplate
// NSImageNameTouchBarRecordStopTemplate
// NSImageNameTouchBarRefreshTemplate
// NSImageNameTouchBarRemoveTemplate
// NSImageNameTouchBarRewindTemplate
// NSImageNameTouchBarRotateLeftTemplate
// NSImageNameTouchBarRotateRightTemplate
// NSImageNameTouchBarSearchTemplate
// NSImageNameTouchBarShareTemplate
// NSImageNameTouchBarSidebarTemplate
// NSImageNameTouchBarSkipAhead15SecondsTemplate
// NSImageNameTouchBarSkipAhead30SecondsTemplate
// NSImageNameTouchBarSkipAheadTemplate
// NSImageNameTouchBarSkipBack15SecondsTemplate
// NSImageNameTouchBarSkipBack30SecondsTemplate
// NSImageNameTouchBarSkipBackTemplate
// NSImageNameTouchBarSkipToEndTemplate
// NSImageNameTouchBarSkipToStartTemplate
// NSImageNameTouchBarSlideshowTemplate
// NSImageNameTouchBarTagIconTemplate
// NSImageNameTouchBarTextBoldTemplate
// NSImageNameTouchBarTextBoxTemplate
// NSImageNameTouchBarTextCenterAlignTemplate
// NSImageNameTouchBarTextItalicTemplate
// NSImageNameTouchBarTextJustifiedAlignTemplate
// NSImageNameTouchBarTextLeftAlignTemplate
// NSImageNameTouchBarTextListTemplate
// NSImageNameTouchBarTextRightAlignTemplate
// NSImageNameTouchBarTextStrikethroughTemplate
// NSImageNameTouchBarTextUnderlineTemplate
// NSImageNameTouchBarUserAddTemplate
// NSImageNameTouchBarUserGroupTemplate
// NSImageNameTouchBarUserTemplate
// NSImageNameTouchBarVolumeDownTemplate
// NSImageNameTouchBarVolumeUpTemplate
// NSImageNameTrashEmpty
// NSImageNameTrashFull
// NSImageNameUser
// NSImageNameUserAccounts
// NSImageNameUserGroup
// NSImageNameUserGuest

#include "Cocoa/Cocoa.h"

void MsgBox (NSString *text, NSString* subtext) {	NSAlert* alert = [[NSAlert alloc] init];
    alert.messageText=text;
    alert.alertStyle = NSAlertStyleInformational;
    alert.informativeText=subtext;
    [alert runModal];
}

NSString*  		EXAMPLE_TOOLBAR_ID		=	@"EXAMPLE/TOOLBAR/MAIN";
NSString*  		EXAMPLE_TOOL_TEST_ID	=	@"EXAMPLE/TOOL/TEST";

@interface	ExampleToolbarController1 : NSObject <NSToolbarDelegate>
@property	(readonly,nonatomic,strong)		NSToolbar*		toolbar;
@end
@implementation ExampleToolbarController1
- (id)init
{
	self	=	[super init];
	if (self)
	{
		_toolbar	=	[[NSToolbar alloc] initWithIdentifier:EXAMPLE_TOOLBAR_ID];
		[_toolbar setDelegate:self];
	}
	return	self;
}
- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar
{
	return	@[EXAMPLE_TOOL_TEST_ID];
}
- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar
{
	return	@[EXAMPLE_TOOL_TEST_ID];
}
- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar itemForItemIdentifier:(NSString *)itemIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
	if ([itemIdentifier isEqualToString:EXAMPLE_TOOL_TEST_ID])
	{
		NSToolbarItem*	item1	=	[[NSToolbarItem alloc] initWithItemIdentifier:EXAMPLE_TOOL_TEST_ID];
		item1.label				=	@"Test!";
		item1.target			=	self;
        item1.image = [NSImage imageNamed:NSImageNameActionTemplate];
		item1.action			=	@selector(userClickTest:);
		return	item1;
	}

	abort();
}
- (void)userClickTest:(id)sender
{
    MsgBox(@"Toolbar hit", @"The toolbar button was clicked.");
}
@end


// @interface MainWindowDelegate: NSObject<NSWindowDelegate> {}
// @end
// @implementation MainWindowDelegate
// -(void)windowDidMiniaturize: (NSNotification*)notification {
//     // print("Window minimized")
// }
// -(void)windowWillClose: (NSNotification*)notification {
//     // print("Window closing")
// }
// @end

// @interface MainWindowController: NSWindowController {} @end
// @implementation MainWindowController
// -(void)windowDidLoad {
//     [super windowDidLoad];
//     // [[self window] setDelegate:self];
//     printf("windowDidLoad\n");
//     // Implement this method to handle any initialization after your window
//     // controller's window has been loaded from its nib file.
// }
// @end

// @interface AppController : NSObject<NSAnimationDelegate, NSToolbarDelegate, NSApplicationDelegate> {
//     @private
//     MainWindowController* mainWinC;
//     MainWindowDelegate* mainWinD;
//     NSView* mainView;
// }
// @end
// @implementation AppController
// -(void)setupMainWindow {
//     mainWinC = [[MainWindowController alloc] initWithWindow:
//                 [[NSWindow alloc]   initWithContentRect:NSMakeRect(100,100,500,500)
//                                     styleMask: NSWindowStyleMaskTitled
//                                                 |NSWindowStyleMaskResizable
//                                                 |NSWindowStyleMaskMiniaturizable
//                                                 |NSWindowStyleMaskClosable
//                                     backing: NSBackingStoreBuffered
//                                     defer: NO]];
//     mainWinD = [[MainWindowDelegate alloc] init];
//     [[mainWinC window] setTitle:@"MyTitle"];
// }
// -(void)showMainWindow:(id)sender {
//     [[mainWinC window] makeKeyAndOrderFront: nil];
// }
// -(void)applicationWillFinishLaunching: (NSNotification*)inNotification {
//     printf("setup: %p\n",mainWinC);
//     [self setupMainWindow];
// }
// -(void)applicationDidFinishLaunching: (NSNotification*)inNotification {
//     printf("launch: %p\n",mainWinC);
//     [[mainWinC window] setDelegate:mainWinD];
//     [mainWinC showWindow:self];
//     [mainWinC windowDidLoad];
// }
// -(BOOL)applicationShouldTerminateAfterLastWindowClosed: (NSApplication*)inApp {return YES;}
// @end

void drawOval(double x,double y,double r1, double r2)
{
    // NSBezierPath* thePath = ;
    NSRect ovalRect = NSMakeRect(x-r1/2,y-r2/2,r1,r2);
    [[NSBezierPath bezierPathWithOvalInRect:ovalRect] fill];
}

void drawCircle(double x,double y,double r)
{drawOval(  x,  y,  r,   r);
}

 static   double sx[] = {100,200,300,400,450}, sy[] = {40,314,357,127,244},sr[]={10,10,20,10,10};
static   NSPoint spoints[] = {{0,65},{100,313},{200,166},{300,122},{400,255},{500,285}};

@interface LinePlotView: NSView {
    @private
      NSPoint* points;
    int count;
struct {double xmin,xmax,ymin,ymax, xspan,yspan;} bounds;
}
@end
@implementation LinePlotView
- (id)initWithFrame:(NSRect)frame {
    count=0;points= NULL;
     return [super initWithFrame:frame] ;
}
- (void)setData {
    points=spoints; count=6;
    [self computeBounds];
}
- (void)drawRect:(NSRect)dirtyRect {
        [[NSColor whiteColor] set];   // set the drawing color to white
    NSRectFill([self bounds]);    // fill the view with white

    // the following statements trace two polygons with n sides
    // and connect all of the vertices with lines

    [[NSColor blackColor] set];   // set the drawing color to black

    // NSAffineTransform* xform = [NSAffineTransform transform];
    // [xform translateXBy:-bounds.xmin yBy:-bounds.ymin];
    // [xform scaleXBy:  yBy: ];
    // [xform concat];
    // Draw content...

NSBezierPath* path =  [NSBezierPath bezierPath];
[path appendBezierPathWithPoints:points count:count];
[path stroke];

        // for (int i=0;i<count;i++) {
        //     // NSPoint p;
        //     // p.x = x[i];//(x[i]-bounds.xmin)/bounds.xspan;
        //     // p.y = y[i];//(y[i]-bounds.ymin)/bounds.yspan;
        //     drawOvalInRect(NSMakeRect(points[i].x,points[i].y);
        // }
}
- (void) computeBounds {
     bounds.xmin=bounds.ymin=1e300;
    bounds.xmax=bounds.ymax=-1e300;
    for (int i=0;i<count;i++) {
        double x = points[i].x, y = points[i].y;
        if (x <bounds.xmin) bounds.xmin=x ;
        if (x >bounds.xmax) bounds.xmax=x ;
        if (y <bounds.ymin) bounds.ymin=y ;
        if (y >bounds.ymax) bounds.ymax=y ;
        // if (r[i]<bounds.rmin) bounds.rmin=r[i];
        // if (r[i]>bounds.rmax) bounds.rmax=r[i];
    }
    bounds.xspan=bounds.xmax-bounds.xmin;
    bounds.yspan=bounds.ymax-bounds.ymin;
    // bounds.rspan=bounds.rmax-bounds.rmin;
    // TODO: apply some scaling to r[i]s so the circle size distribution can be easy on the eyes
}
@end


@interface ScatterPlotView: NSView {
    @private
      double *x,*y,*r;
 int count;
 struct {double xmin,xmax,ymin,ymax,rmin,rmax,rspan,xspan,yspan;} bounds;
 }
@end
@implementation ScatterPlotView
- (id)initWithFrame:(NSRect)frame {
    count=0;x=y=r=NULL;
     return [super initWithFrame:frame] ;
}
- (void)setData {
    x=sx;y=sy;r=sr;count=5;
   [self computeBounds];
}
// - (BOOL)isOpaque {return YES;}
- (void)drawRect:(NSRect)dirtyRect {
        [[NSColor textBackgroundColor] set];   // set the drawing color to white
    NSRectFill([self bounds]);    // fill the view with white

    // the following statements trace two polygons with n sides
    // and connect all of the vertices with lines

    const int marginleft=80,marginbottom=80;
    const int marginright=80,margintop=80;
    // double scale =  dirtyRect.size.width /bounds.xspan;
    double scalex = (dirtyRect.size.width-marginleft-marginright)/bounds.xspan;
    double scaley = (dirtyRect.size.height-margintop-marginbottom)/bounds.yspan;
    NSAffineTransform* xform = [NSAffineTransform transform];
    [xform scaleXBy:scalex  yBy:scaley ];
    [xform translateXBy:-bounds.xmin+(marginleft)/scalex  yBy:-bounds.ymin+marginbottom/scaley ];
    // printf("%f %f %f\n" ,bounds.xspan,dirtyRect.size.width, scale);
    [xform concat];
    // Draw content...

    [[NSColor tertiaryLabelColor] set];   // set the drawing color to black

    const int ngridx=10,ngridy=10;
    for (int i=0;i<ngridx;i++)
    {
        NSPoint point1={bounds.xmin+i*(bounds.xspan)/ngridx,bounds.ymin}, point2={bounds.xmin+i*(bounds.xspan)/ngridx,bounds.ymax};
        [NSBezierPath strokeLineFromPoint: point1
                        toPoint: point2];
                        }
    for (int i=0;i<ngridy;i++)
    {
        NSPoint point1={bounds.xmin,bounds.ymin+i*(bounds.yspan)/ngridy}, point2={bounds.xmax,bounds.ymin+i*(bounds.yspan)/ngridy};
        [NSBezierPath strokeLineFromPoint: point1
                        toPoint: point2];
                        }

    [[NSColor textColor] set];   // set the drawing color to black
    [NSBezierPath strokeRect: NSMakeRect(bounds.xmin,bounds.ymin,bounds.xspan,bounds.yspan)];

    [[NSFont fontWithName:@"Helvetica" size:12.0] set];
    NSDictionary* fontInfo = @{
        NSForegroundColorAttributeName: [NSColor textColor]
    };

    for (int i=0;i<=ngridx;i++)
    {
        NSPoint point1={bounds.xmin+i*(bounds.xspan)/ngridx,bounds.ymin};
        NSString* label = [[NSString alloc] initWithFormat:@"%g" , point1.x];
        NSSize labelsize = [label sizeWithAttributes:nil ];
        [label drawAtPoint:NSMakePoint(point1.x-labelsize.width/2,point1.y-labelsize.height) withAttributes:fontInfo];
    }

    for (int i=0;i<=ngridy;i++)
    {
        NSPoint point2={bounds.xmin, bounds.ymin+i*(bounds.yspan)/ngridy};
        NSString* label = [[NSString alloc] initWithFormat:@"%g" , point2.y];
        NSSize labelsize = [label sizeWithAttributes:nil ];
        [label drawAtPoint:NSMakePoint(point2.x-labelsize.width,point2.y-labelsize.height/2)  withAttributes:fontInfo];
    }


    [[NSColor systemBlueColor] set];   // set the drawing color to black


    for (int i=0;i<count;i++) {
        // double rad = r[i]/scalex;//,hrad=rad/2;
        NSPoint p;
        p.x = x[i];//(x[i]-bounds.xmin)/bounds.xspan;
        p.y = y[i];//(y[i]-bounds.ymin)/bounds.yspan;
        drawOval(p.x, p.y, r[i]/scalex, r[i]/scaley);
    }

}
- (void) computeBounds {
    bounds.rmin=bounds.xmin=bounds.ymin=1e300;
    bounds.rmax=bounds.xmax=bounds.ymax=-1e300;
    for (int i=0;i<count;i++) {
        if (x[i]<bounds.xmin) bounds.xmin=x[i];
        if (x[i]>bounds.xmax) bounds.xmax=x[i];
        if (y[i]<bounds.ymin) bounds.ymin=y[i];
        if (y[i]>bounds.ymax) bounds.ymax=y[i];
        if (r[i]<bounds.rmin) bounds.rmin=r[i];
        if (r[i]>bounds.rmax) bounds.rmax=r[i];
    }
    bounds.xspan=bounds.xmax-bounds.xmin;
    bounds.yspan=bounds.ymax-bounds.ymin;
    bounds.rspan=bounds.rmax-bounds.rmin;
    // TODO: apply some scaling to r[i]s so the circle size distribution can be easy on the eyes
}
@end


#define X(t) (sin(t)+1) * width * 0.5     // macro for X(t)
#define Y(t) (cos(t)+1) * height * 0.5    // macro for Y(t)

@interface KaleidoView: NSView {
    @private
 NSTrackingArea* trackingArea;
 }
@end
@implementation KaleidoView
// actually this method should be in a basic View class that all will inherit
- (id)initWithTrackingInFrame:(NSRect)frame {
    self = [self initWithFrame:frame];
    if (self) {
        trackingArea = [[NSTrackingArea alloc] initWithRect:frame
            options: (NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingActiveInKeyWindow )
            owner:self userInfo:nil];
        [self addTrackingArea:trackingArea];
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect {
    double f,g;
    double   pi = 2 * acos(0.0);

    int n = 12;                 // number of sides of the polygon

    // get the size of the application's window and view objects
    float width  = [self bounds].size.width;
    float height = [self bounds].size.height;

    [[NSColor underPageBackgroundColor] set];   // set the drawing color to white
    NSRectFill([self bounds]);    // fill the view with white

    // the following statements trace two polygons with n sides
    // and connect all of the vertices with lines

    [[NSColor textColor] set];   // set the drawing color to black

    for (f=0; f<2*pi; f+=2*pi/n) {        // draw the fancy pattern
        for (g=0; g<2*pi; g+=2*pi/n) {
            NSPoint p1 = NSMakePoint(X(f),Y(f));
            NSPoint p2 = NSMakePoint(X(g),Y(g));
            [NSBezierPath strokeLineFromPoint:p1 toPoint:p2];
        }
    }
}
- (void)mouseDown:(NSEvent *)event {
    NSPoint loc = [event locationInWindow];
printf("down %f %f\n",loc.x, loc.y);
NSRect f = [self frame];
f.origin.x+=20;
[[self animator] setFrame:f ];
}
- (void)mouseUp:(NSEvent *)event {
    NSPoint loc = [event locationInWindow];
printf("up %f %f\n",loc.x, loc.y);
}

- (void)mouseMoved_disabled:(NSEvent *)event {
    NSPoint loc = [event locationInWindow];
printf("move %f %f\n",loc.x, loc.y);
}

- (void)mouseEntered:(NSEvent *)event {
    NSPoint loc = [event locationInWindow];
printf("in %f %f\n",loc.x, loc.y);
}

- (void)mouseExited:(NSEvent *)event {
    NSPoint loc = [event locationInWindow];
printf("out %f %f\n",loc.x, loc.y);
}

@end

NSMenu* makeMainMenu() {
    id mainMenu = [NSMenu new]; // `title` really doesn't matter.
    id mainAppMenuItem = [[NSMenuItem alloc] initWithTitle: @"Application"  action: nil  keyEquivalent: @""]; // `title` really doesn't matter.
    id mainFileMenuItem = [[NSMenuItem alloc] initWithTitle: @"File"  action: nil  keyEquivalent: @""];
    [mainMenu addItem: mainAppMenuItem];
    [mainMenu addItem: mainFileMenuItem];

    id appMenu = [NSMenu new]; // `title` really doesn't matter.
    [mainAppMenuItem setSubmenu: appMenu];

    id appServicesMenu = [NSMenu new];
    [NSApp setServicesMenu:appServicesMenu];

    id appName = [[NSProcessInfo processInfo] processName];

    [appMenu addItemWithTitle: [@"About " stringByAppendingString:appName]  action: nil  keyEquivalent: @""];

    [appMenu addItem: [NSMenuItem separatorItem]];
    [appMenu addItemWithTitle: @"Preferences..."  action: nil  keyEquivalent: @","];

    [appMenu addItem: [NSMenuItem separatorItem]];
    [appMenu addItemWithTitle: [@"Hide " stringByAppendingString:appName] action: @selector(hide:) keyEquivalent: @"h"];
    id mHideOthers = [[NSMenuItem alloc] initWithTitle: @"Hide Others" action: @selector(hideOtherApplications:)  keyEquivalent: @"h"];
    [mHideOthers setKeyEquivalentModifierMask:NSEventModifierFlagOption|NSEventModifierFlagCommand ];
    [appMenu addItem: mHideOthers];

    [appMenu addItemWithTitle: @"Show All" action: @selector(unhideAllApplications:) keyEquivalent: @""];

    [appMenu addItem: [NSMenuItem separatorItem]];
    [[appMenu addItemWithTitle: @"Services" action: nil keyEquivalent: @""] setSubmenu: appServicesMenu];
    [appMenu addItem: [NSMenuItem separatorItem]];
    [appMenu addItemWithTitle: [@"Quit " stringByAppendingString:appName] action: @selector(terminate:) keyEquivalent: @"q"];

    id fileMenu = [[NSMenu alloc] initWithTitle: @"File"];
    [mainFileMenuItem setSubmenu: fileMenu];
    [fileMenu addItemWithTitle: @"New..." action: @selector(NSDocumentController:newDocument:) keyEquivalent: @"n"];

    return mainMenu;
}

@interface ApplicationController: NSResponder <NSApplicationDelegate>
@end

@implementation ApplicationController
{
	NSWindow*	window;
}
- (void)applicationDidFinishLaunching:(NSNotification *)notification
{
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

    // Menu
    [NSApp setMainMenu: makeMainMenu()];

    // Window
    window = [[[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 500, 400)
                                    styleMask: NSWindowStyleMaskTitled
                                                |NSWindowStyleMaskResizable
                                                |NSWindowStyleMaskMiniaturizable
                                                |NSWindowStyleMaskClosable
                                    backing:NSBackingStoreBuffered
                                    defer:NO] autorelease];
    [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
    [window setTitle:@"My App RBCK"];
    [window makeKeyAndOrderFront:nil];

    // Toolbar
    ExampleToolbarController1* tbCon = [ExampleToolbarController1 new];
    window.toolbar = tbCon.toolbar;

    // Content view
    // id nsv = [[KaleidoView alloc] initWithTrackingInFrame:NSMakeRect(0,0,200,200)];
    id nsv = [[ScatterPlotView alloc] initWithFrame:NSMakeRect(0,0,200,200)];
    // [nsv setAutoresizingMask: NSViewHeightSizable|NSViewWidthSizable];
    [nsv setData];
    [nsv setNeedsDisplay:YES];
    // [[window contentView] addSubview: nsv];
    window.contentView = nsv; //<< you can do this too

    // [window.contentView setAutoresizesSubviews:YES];
    [NSApp activateIgnoringOtherApps:YES];
}
@end


int main(int argc, char *argv[]) {
// #ifdef CLI_MODE
//     @autoreleasepool {
//         MyCLIController* con = [[[MyCLIController alloc] initWithArgs:argv count:argc] autorelease];
//     }
//     return [con run];
// #else
	@autoreleasepool {
        [NSApplication sharedApplication];
		[NSApp setDelegate: [[ApplicationController alloc] init] ];
		[NSApp run];
	}
    return 0;


    // [NSAutoreleasePool new];
    // [NSApplication sharedApplication];
    // [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    // // id menubar = [[NSMenu new] autorelease];
    // // id appMenuItem = [[NSMenuItem new] autorelease];
    // // [menubar addItem::appMenuItem];
    // NSMenu*menubar=makeMainMenu();
    // [NSApp setMainMenu:menubar];

    // // id appMenu = [[NSMenu new] autorelease];
    // // id appName = [[NSProcessInfo processInfo] processName];
    // // id quitTitle = [@"Quit " stringByAppendingString:appName];
    // // id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
    // // action:@selector(terminate:) keyEquivalent:@"q"] autorelease];
    // // [appMenu addItem::quitMenuItem];
    // // [appMenuItem setSubmenu:appMenu];
    // NSWindow* window = [[[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 200, 200)
    //                                 styleMask: NSWindowStyleMaskTitled
    //                                             |NSWindowStyleMaskResizable
    //                                             |NSWindowStyleMaskMiniaturizable
    //                                             |NSWindowStyleMaskClosable
    //                                 backing:NSBackingStoreBuffered
    //                                 defer:NO] autorelease];
    // [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
    // [window setTitle:@"My App RBCK"];
    // [window makeKeyAndOrderFront:nil];
    // id nsv = [[KaleidoView alloc] initWithTrackingInFrame:NSMakeRect(0,0,200,200)];
    // [nsv setAutoresizingMask: NSViewHeightSizable|NSViewWidthSizable];
    // [[window contentView] addSubview: nsv];
    // ExampleToolbarController1* tbCon = [ExampleToolbarController1 new];
    // window.toolbar = tbCon.toolbar;
    // [NSApp activateIgnoringOtherApps:YES];
    // [NSApp run];
    // return 0;

    // NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // NSApplication * application = [NSApplication sharedApplication];
    // [application setActivationPolicy:NSApplicationActivationPolicyRegular];

    // AppController* appDelegate = [[[AppController alloc] init] autorelease];

    // [application setDelegate:appDelegate];
    // [application run];

    // [pool drain];

    // return EXIT_SUCCESS;
    // return NSApplicationMain(argc, (  char**)argv);
// #endif
}