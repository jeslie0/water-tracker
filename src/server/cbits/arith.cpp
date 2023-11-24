#include "arith.h"
#include <iostream>

arith::arith() { std::cout << "Constructed an arith instance!" << std::endl; }
arith::~arith() { std::cout << "Destructed an arith instance!" << std::endl; }

int arith::add(int x, int y) { return x + y; }
int arith::sub(int x, int y) { return x - y; }
int arith::mult(int x, int y) { return x * y; }
int arith::div(int x, int y) { return x / y; }

