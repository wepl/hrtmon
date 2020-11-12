
/*

  Filename:    HRTmonLoad.c
  Author:      Alain Malek
  Description: Loading routines used by HRTmon and HRTmonPrefs

*/

#include "HRTmonLoad.h"

#define nb_keyboard 4		// nb keyboards selectable in HRTmonPrefs

/*******************************************************************/

static char A1200;
static char CD32;
static char AGA;

int execver;

struct GfxBase *GfxBase;

// simple Loadseg for HRTmon file
// load from memory to ABS place


CONFIG config;

// HRTmon header

typedef struct {
	char jmps[20];
	unsigned int mon_size;
	unsigned short col0, col1;
	char right;
	char keyboard;
	char key;
	char ide;
	char a1200;
	char aga;
	char insert;
	char delay;
	char lview;
	char cd32;
	char screenmode;
	char vbr;
	char entered;
	char hexmode;
	unsigned short error_sr;
	unsigned int error_pc;
	unsigned short error_status;
	char newid[6];
	unsigned short	mon_version;
	unsigned short	mon_revision;
	unsigned int	whd_base;
	unsigned short	whd_version;
	unsigned short	whd_revision;
	unsigned int	max_chip;
} HRTCFG;


/*******************************************************************/

void set_default()
{
	strcpy(config.id,"HRT");
	config.version = CFGVER;
	config.address = 0x1c000;
	config.col0 = 0x000;
	config.col1 = 0xeee;
	config.abs = TRUE;
	config.autoa = TRUE;
	config.right = TRUE;
	config.key = TRUE;
	config.ide = FALSE;
	config.insert = FALSE;
	config.lview = TRUE;
	config.vbr = TRUE;
	config.hexmode = FALSE;
	config.keyboard = 0;
	config.delay = 15;
	config.screenmode = SCREEN_PAL;
}

/*******************************************************************/

void LoadInit()
{
	BPTR cf;
	int bread;

	GfxBase = (struct GfxBase *) OpenLibrary(GRAPHICSNAME,0);
	AGA = (GfxBase->ChipRevBits0) & 8;	/* Test LISA bit */

	A1200 = (FindName(&(SysBase->DeviceList), "carddisk.device") != 0);
	CD32 = (FindName(&(SysBase->DeviceList), "elsat.device") != 0);
	execver = SysBase->LibNode.lib_Version;

	set_default();

	if ( cf=Open("ENV:HRTmon.prefs",MODE_OLDFILE) ) {
	    bread = Read(cf, &config, sizeof(CONFIG));
	    if (bread != sizeof(CONFIG))
	        config.version = 0;
	    Close(cf);
	    if(strcmp(config.id, "HRT") || config.version != CFGVER)
	    	set_default();
	    if (config.screenmode >= SCREEN_MAX)
	    	config.screenmode = SCREEN_PAL;
	}

}


/*******************************************************************/

int Load(char *path)
{
	int *Buffer;
	BPTR filelock,filehandle;
	struct FileInfoBlock infoblock;
	int autoaddr;
	char hrtname[512+32];
	char tmptxt[256];

	strcpy(hrtname, path);
	strcat(hrtname, "HRTmon.data");

	if ( ! (filelock=Lock(hrtname,ACCESS_READ)) ) {		// Lock file
	    strcpy(hrtname, config.path);
	    strcat(hrtname, "/HRTmon.data");
	    if ( ! (filelock=Lock(hrtname,ACCESS_READ)) ) {
	        printf("Can't open file HRTmon.data\n");
	        return (TRUE);
	    }
	}

	if ( Examine(filelock,&infoblock) == FALSE ) {		// Examine file
	    printf("Can't open file HRTmon.data\n");
	    UnLock(filelock);
	    return (TRUE);
	}

	UnLock(filelock);					// UnLock file

	if ( (Buffer=AllocMem(infoblock.fib_Size,MEMF_ANY)) == NULL ) {
	    printf("Not enough memory to proceed.\n");
	    return (TRUE);
	}

	if ( filehandle=Open(hrtname,MODE_OLDFILE) ) {
	    Read(filehandle,Buffer,infoblock.fib_Size);		// Read file
	    Close(filehandle);
	}
	else {						// if Can't Open
	    printf("Can't open file HRTmon.data\n");
	    return (TRUE);
	}

	if (config.autoa)
		autoaddr = 0;
	else
		autoaddr = config.address;

	if ( LoadABS(Buffer,&autoaddr) == FALSE ) {
	    FreeMem(Buffer,infoblock.fib_Size);
	    printf("Installation of HRTmon failed.\n");
	    return (TRUE);
	}

	FreeMem(Buffer,infoblock.fib_Size);

	DoJSR((unsigned int)autoaddr+8);		// Init HRTmon

	sprintf(tmptxt,"HRTmon installed at $%x.\n",autoaddr);
	ShowReq(tmptxt,"OK");

	return (FALSE);
}

/*******************************************************************/
// Only for HRTmon.data file ! (single simple code Hunk)
// Load file from 'memory' to 'place' and AllocAbs place if 'abs'=TRUE.

int LoadABS(int *memory,int *place)
{
	int *p,len,i;
	int *base;
	char *base2;
	HRTCFG *hrtcfg;


	if ( (*(memory+6) != 0x3e9) || (*memory != 0x3f3) )
	    return (FALSE);

	len=*(memory+7);

	if (*place == 0) {
		if (execver >= 39)
			*place = (int)AllocMem(len*4, MEMF_PUBLIC | MEMF_REVERSE);
		else
			*place = (int)AllocMem(len*4, MEMF_PUBLIC);

		if (*place == 0) {
			printf("Not enough memory !\n");
			return(FALSE);
		}
	}

	if (config.abs && (!config.autoa)) {
	    if ( AllocAbs(len*4,(APTR)*place) == 0 ) {
		printf("Can't AllocAbs memory at address.\n");
		return (FALSE);
	    }
	}

	CopyMemQuick( (ULONG*)memory+8, (ULONG*)*place, len*4);

	if (config.abs || config.autoa)
	    ((ULONG*)*place)[5]=len*4;
	else
	    ((ULONG*)*place)[5]=0;


	hrtcfg = (HRTCFG*)(*place);

	hrtcfg->col0 = config.col0;
	hrtcfg->col1 = config.col1;
	hrtcfg->right = config.right;
	hrtcfg->keyboard = config.keyboard;
	hrtcfg->key = config.key;
	hrtcfg->ide = config.ide;
	hrtcfg->a1200 = A1200;
	hrtcfg->aga = AGA;
	hrtcfg->insert = config.insert;
	hrtcfg->delay = config.delay;
	hrtcfg->lview = config.lview;
	hrtcfg->cd32 = CD32;
	hrtcfg->screenmode = config.screenmode;
	hrtcfg->vbr = config.vbr;
	hrtcfg->hexmode = config.hexmode;

	p=memory+8+len+3;		//p points on reloc_32 hunk offsets
	len=*(p-2);

	base2=(char*)*place;

	for ( i=0; i<len ; i++ ) {

	    base=(int*)&base2[*p];
	    *base = *base + *place;
	    p++;
	}

	if (execver >= 37)
	    CacheClearU();

	return (TRUE);
}

/*******************************************************************/


