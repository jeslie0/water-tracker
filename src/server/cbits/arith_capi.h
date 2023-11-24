#pragma once

#ifdef __cplusplus
#define MY_EXTERN extern "C"
#else
#define MY_EXTERN
#endif

// Define a opaque struct for our "arith" class
typedef struct arith arith;

// Define the constructor
MY_EXTERN arith *arith_new();

// Define the destructor
MY_EXTERN void arith_delete(arith* p);

MY_EXTERN int arith_add(arith *p, int x, int y);
MY_EXTERN int arith_sub(arith *p, int x, int y);
MY_EXTERN int arith_mult(arith *p, int x, int y);
MY_EXTERN int arith_div(arith *p, int x, int y);
