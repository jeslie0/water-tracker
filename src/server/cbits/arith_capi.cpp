#include "arith_capi.h"
#include "arith.h"
#include <iostream>

MY_EXTERN arith *arith_new() { return new arith(); }

MY_EXTERN void arith_delete(arith *p) { std::cout << "DESTRUCTING" << std::endl; delete p; }

MY_EXTERN int arith_add(arith *p, int x, int y) { return p->add(x, y); }

MY_EXTERN int arith_sub(arith *p, int x, int y) { return p->sub(x, y); }

MY_EXTERN int arith_mult(arith *p, int x, int y) { return p->mult(x, y); }

MY_EXTERN int arith_div(arith *p, int x, int y) { return p->div(x, y); }
