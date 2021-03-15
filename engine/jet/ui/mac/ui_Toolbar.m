
// NSString* EXAMPLE_TOOLBAR_ID = @"EXAMPLE/TOOLBAR/MAIN";
// NSString* EXAMPLE_TOOL_TEST_ID = @"EXAMPLE/TOOL/TEST";

@interface JetUIToolbar : NSObject <NSToolbarDelegate>
@private
// todo: remove @property and see
@property (readonly, nonatomic, strong) NSToolbar* toolbar;
NSArray<NSString*> *allowed, *default;
NSDictionary<NSString* , NSToolbarItem*>* items;
- (id)initWithIdentifier:(NSString*)ident;
@end

@implementation JetUIToolbar // ExampleToolbarController1
- (id)initWithIdentifier:(NSString*)ident allowedItems:(NSArray*)allowed defaultItems:(NSArray*) default
{
    self = [super init];
    if (self) {
        _toolbar = [[NSToolbar alloc] initWithIdentifier:ident];
        [_toolbar setDelegate:self];
        self.allowed=allowed;
        self.default=default;
    }
    return self;
}
- (NSArray*)toolbarAllowedItemIdentifiers:(NSToolbar*)toolbar {
    return self.allowed;//@[ EXAMPLE_TOOL_TEST_ID ];
}
- (NSArray*)toolbarDefaultItemIdentifiers:(NSToolbar*)toolbar {
    return self.default;//@[ EXAMPLE_TOOL_TEST_ID ];
}
- (NSToolbarItem*)toolbar:(NSToolbar*)toolbar
        itemForItemIdentifier:(NSString*)itemIdentifier
    willBeInsertedIntoToolbar:(BOOL)flag {
        // create a Dictionary with whats what and use it on toolbar init
    if ([itemIdentifier isEqualToString:EXAMPLE_TOOL_TEST_ID]) {
        NSToolbarItem* item1 =
            [[NSToolbarItem alloc] initWithItemIdentifier:EXAMPLE_TOOL_TEST_ID];
        item1.label = @"Test!";
        item1.target = self;
        item1.image = [NSImage imageNamed:NSImageNameActionTemplate];
        item1.action = @selector(userClickTest:);
        return item1;
    }

    abort();
}
- (void)userClickTest:(id)sender {
    MsgBox(@"Toolbar hit", @"The toolbar button was clicked.");
}
@end