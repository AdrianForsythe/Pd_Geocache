//==============================================================================
//  Filename:	R250.h
//  Author:	Jon Cline
//  Created:	August 28, 1996
//
//	This file defines the interface for a pseudo-random number generator
//	class R250, which extends the base class RNG (g++-include).
//==============================================================================

/******************************************************/
/** These routines were adapted from an article in   **/
/**   Dr. Dobb's Journal: Maier, W. L. 1991.  A Fast **/
/**   Pseudo Random Number Generator.  Dr. Dobb's    **/
/**   Journal 16(5):152-xxx.                         **/
/******************************************************/

static unsigned long r250_buffer[250];
static int r250_index;

void r250_init(int seed);
void r250_init_t(void);
long int r250_init_rt(void);
unsigned long r250(void);
unsigned long safe_r250(void);
double safe_dr250(void);
double dr250(void);
float fr250(void);

int dump_r250(char *fn);
int get_r250(char *fn, int del);
