void MsgBox(NSString* text, NSString* subtext) {
    NSAlert* alert = [[NSAlert alloc] init];
    alert.messageText = text;
    alert.alertStyle = NSAlertStyleInformational;
    alert.informativeText = subtext;
    [alert runModal];
}

void JetUI_run() {
     @autoreleasepool {
        [NSApplication sharedApplication];
        [NSApp setDelegate:[[ApplicationController alloc] init]];
        [NSApp run];
    }
}