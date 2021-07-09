// Compile time detection macros

// Runtime detection macros

// Which OS?
#define JET_OS_MACOS
#define JET_OS_LINUX
#define JET_OS_WINDOWS
#define JET_OS_IS_32BIT

// Which CPU architecture?
#define JET_ARCH_X86
#define JET_ARCH_X86_64
#define JET_ARCH_AARCH64

// Which GUI backend?
#define JET_GUI_COCOA
#define JET_GUI_GTK3
#define JET_GUI_WIN32
#define JET_GUI_WINUI

// Being compiled as one unit (monolithic)?
#define JET_IS_MONOLITHIC

// Testing or running?
#define JET_TESTING // = name of test entry point