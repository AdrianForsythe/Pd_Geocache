//==============================================================================
/******************************************************/
/** These routines were adapted from an article in   **/
/**   Dr. Dobb's Journal: Maier, W. L. 1991.  A Fast **/
/**   Pseudo Random Number Generator.  Dr. Dobb's    **/
/**   Journal 16(5):152-xxx.                         **/
/******************************************************/
/*
	Big problem with original r250 code: Although one
	should never rely on randomness of low-order bits,
	this generator is particularly bad with rand() under
	gcc.  Under gcc, rand() gives alternating even and odd 
	numbers!  Thus the XOR'd pair (147 entries apart)
	are always even and odd, which always produces an odd
	number.  Thus the shuffle I added below (with no 
	thought of sufficiency or efficiency).

	Note the newer pair of functions at the bottom of this
	file (thanks to Ben)--they not only allow resumption
	of a run of a stochastic model, they also allow the
	generation of identical random number sequences on 
	different machines.

	-John S.,  20oct94
*/

#include <stdio.h>
#include <stdlib.h>
#include "r250.h"
#include "time.h"

void r250_init_t(void)
	{
	time_t tm;
	tm = time(&tm);
	r250_init((long)tm);	
	}

long int r250_init_rt(void)
	{
	time_t tm;
	tm = time(&tm);
	r250_init((long)tm);	
	return ((long)tm);
	}

void r250_init(int seed)
{
  int j,k;
  unsigned long mask;
  unsigned long msb;
  unsigned long token;
  srand(seed);
  r250_index=0;
  for (j=0;j<250;j++)
	{
	 r250_buffer[j]=rand();
	 token=rand();
	 token<<=16;
	 r250_buffer[j]^=token;
	}
  for (j=0;j<250;j++)
	 {
	 if (rand()>1073741824)
	   r250_buffer[j]|=0x80000000;
	  }
	msb=0x80000000;
  mask=0xffffffff;
  for (j=0;j<31;j++)
	{
	k=7*j+3;
	r250_buffer[k]&=mask;
	r250_buffer[k]|=msb;
	mask>>=1;
	msb>>=1;
	}
}

unsigned long safe_r250(void)
{
	return r250();
}

unsigned long r250(void)
{
  register int j;
  register unsigned long new_rand;
 
  if (r250_index>=147)
	j=r250_index-147;
  else
	j=r250_index+103;
 
 new_rand=r250_buffer[r250_index]^r250_buffer[j];
 r250_buffer[r250_index]=new_rand;
 if (r250_index>=249)
	r250_index=0;
 else
	r250_index++;
 
 return new_rand;
}

float fr250(void)
{
	return (float)dr250();
}

double safe_dr250(void)
{
	double tmp;
	do{tmp = dr250();}while(tmp==1.0);
	return(tmp);
}

double dr250(void)
{
  register int j;
  register unsigned long new_rand;
 
  if (r250_index>=147)
	j=r250_index-147;
  else
	j=r250_index+103;
 
 new_rand=r250_buffer[r250_index]^r250_buffer[j];
 r250_buffer[r250_index]=new_rand;
 if (r250_index>=249)
	r250_index=0;
 else
	r250_index++;
 
 return new_rand/(double)0xffffffff;
}	

/* Two routines to dump the state of the r250 random-number-generator
 *  to a system file and retrieve it, respectively.  Saves/retrieves the 250
 * longs and the int index to the table ...
 * 
 *  Both functions take the name of the system file as the first argument.
 *  The retrieval function takes a second argument, whether to delete the
 *    system file after retrieving the information.
 *
 *   Both functions return 0 if successful, -1 if file open fails.
 *
 *    Ben Bolker 17 Jan 1994
 */

int dump_r250(char * fn)
{
  FILE * dumpfile;
  int j;

  if ((dumpfile = fopen(fn,"w")) == NULL)
    return -1;
  else {
    (void)fprintf(dumpfile,"%d\n",r250_index);
    for (j=0; j<250;j++)
      (void)fprintf(dumpfile,"%ld\n",r250_buffer[j]);
    fclose(dumpfile);
    return 0;
  }
}	

/**************************************************************/
int get_r250(char * fn, int del)
{
  FILE * dumpfile;
  int j;

  if ((dumpfile = fopen(fn,"r")) == NULL)
    return -1;
  else {
    (void)fscanf(dumpfile,"%d\n",&r250_index);
    for (j=0; j<250; j++)
      (void)fscanf(dumpfile,"%ld\n",&(r250_buffer[j]));
    fclose(dumpfile);
    //if (del==1) unlink(fn);
    return 0;
  }
}	
