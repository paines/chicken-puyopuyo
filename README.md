chicken-puyopuyo
================

A puyopuyo clon written in Chicken Scheme



OSX Issues:
;;call this for osx. however window is painted behind all other windows and lacking inputs/events....
(declare (foreign-declare "#include <dlfcn.h>\n"))
(foreign-code
 "
    void* cocoa_lib;
    cocoa_lib = dlopen(\"/System/Library/Frameworks/Cocoa.framework/Cocoa\", RTLD_LAZY );
    void (*nsappload)(void);
    nsappload = (void(*)()) dlsym( cocoa_lib, \"NSApplicationLoad\");
    nsappload();
")
