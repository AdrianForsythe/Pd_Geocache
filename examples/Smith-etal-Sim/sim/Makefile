# Makefile for myprog 
# CHANGE 'myprog' to your program name on TARGETS line below.
# Lines that start with '#' are comments in Makefile's.
# This Makefile ASSUMES that myprog.c (or myprog.o) already exists
#	and it links your program with the public copy of r250.o
# You must have in your C program:
#	#include <r250.h> #	and you should declare an integer seed and assign it some value.
# To use this, just copy it into the directory with myprog.c and enter 'make'

CC  = g++
r250 = r250

# the next line links with the math library; you can add more (see below)
LIBS =		-lm #-lqt
INCLUDE =	-I /home/adrianforsythe/anaconda3/include -I /home/adrianforsythe/anaconda3/include/qt -I ${QTDIR}
QT = -L ${QTLIB}
# If you want to compile for debugging, change "-O" to "-g"
CFLAGS =	-g -O ${INCLUDE} ${LIBS} 

#all: test 

#test: test.cc
#	${CC} ${CFLAGS} ${LDFLAGS} ${QT} -o test test.cc 

all: Simulate 

Simulate: Simulate.cc ${r250}.cc
	${CC} ${CFLAGS} ${LDFLAGS} -o Simulate Simulate.cc ${r250}.cc

${r250}.o: ${r250}.cc
	cd ${r250} random; make r250 


clean:
		rm -f core *.o

