
/*

  Filename:    HRTmonLoad.h
  Author:      Alain Malek
  Description: Loading routines used by HRTmon and HRTmonPrefs

*/

#include <dos/dos.h>
#include <exec/exec.h>
#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <graphics/displayinfo.h>
#include <graphics/gfxbase.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/intuition.h>
#include <proto/gadtools.h>
#include <proto/graphics.h>
#include <proto/utility.h>
#include <stdio.h>
#include <string.h>

#include "assembler.h"


#define SCREEN_PAL	 0
#define SCREEN_NTSC	 1
#define SCREEN_MULTISCAN 2
#define SCREEN_MAX	 3

#define TRUE 1
#define FALSE 0

#define CFGVER 0x224

// config file format

typedef struct {
	char id[4];			// HRT
	int  version;
	unsigned int address;
	unsigned short col0, col1;
	char abs;
	char autoa;
	char right;
	char key;
	char ide;
	char insert;
	char lview;
	char vbr;
	char hexmode;
	unsigned char keyboard;
	unsigned char delay;
	unsigned char screenmode;
	char path[512];
} CONFIG;

extern CONFIG config;
extern int execver;

void set_default();
void LoadInit();
int Load(char *path);
int LoadABS(int *memory,int *place);

int ShowReq(char *text,char*choice);


