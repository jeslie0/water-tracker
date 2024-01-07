#include "cppffi.h"
#include <iostream>

void cpp_print()
{
    std::cout << "HELLO WORLD" << "\n";
    return;
}

int baz(int a) {
    return 1 + a;
}
