
/*********************************/
/* HRTmon preferences and loader */
/*    by Hornet of Alcatraz      */
/*        Alain Malek            */
/*********************************/
/*
  This was my first C program, that's why it is so badly coded ...
  Maybe the whole HRTmonPrefs program should be recoded from scratch
*/

// compiled with DICE
// dcc HRTmonPrefs.c HRTmonGUI.o assembler.o HRTboot.o HRTmonLoad.o reqtoolss.lib


#include "HRTmonLoad.h"
#include "HRTmonGUI2.h"
#include "assembler.h"

#define nb_sectors 300		// nb sectors to write for boot-disk

__far extern int BootBlock[];

#include <dos/dos.h>
#include <reqtools.h>
#include <proto/reqtools.h>

/*******************************************************************/

struct ReqToolsBase *ReqToolsBase;

int ShowReq(char *text,char*choice);	// Show EZRequest. from reqtools

/*******************************************************************/

void main()
{
	struct ExecBase *SysBase=(int*)*((int*)4);

	if (!(ReqToolsBase = (struct ReqToolsBase *)
		OpenLibrary (REQTOOLSNAME, REQTOOLSVERSION)))
	    exit (1);

	LoadInit();


	MainGTags[7]=(config.autoa & 1);	// disable address field
	MainGTags[10]=(config.abs & 1);
	MainGTags[14]=(config.right & 1);
	MainGTags[19]=config.keyboard;
	MainGTags[22]=config.ide & 1;
	MainGTags[25]=config.autoa & 1;
	MainGTags[29]=config.key & 1;
	MainGTags[32]=(config.insert & 1);
	MainGTags[39]=config.delay;
	MainGTags[52]=(config.lview & 1);
	MainGTags[57]=config.screenmode;
	MainGTags[60]=(config.vbr & 1);
	MainGTags[63]=(config.hexmode & 1);

	SetupScreen();
	OpenMainWindow();

	sprintf( ((struct StringInfo*) (MainGadgets[GD_address] -> SpecialInfo)) -> Buffer
	    ,"$%x",config.address);
	RefreshGList(MainGadgets[GD_address],MainWnd,NULL,1);

	if ( RemHRTmon() )
	    ShowReq("HRTmon located\n"
		    "and removed.","OK");

	do
	    WaitPort( MainWnd->UserPort );
	while ( HandleMainIDCMP() );

	CloseMainWindow();
	CloseDownScreen();

	CloseLibrary(ReqToolsBase);
	exit (0);

}

/*******************************************************************/

int ShowReq(char *text,char*choice)
{
	struct TagItem myTags[4]={
	    RT_Window,NULL,
	    RT_LockWindow,TRUE,			// show busy pointer on parent window
	    RTEZ_ReqTitle,"HRTmon requester",
	    TAG_DONE
	};

	myTags[0].ti_Data=MainWnd;	// init RT_Window field

	return ( rtEZRequest (text,choice,NULL,myTags) );

}

/*******************************************************************/

int MainCloseWindow ()
{
	return (FALSE);
}

/*******************************************************************/
// Menu save

int MainSave()
{
BPTR cf1,cf2;

	BPTR lock;

	memset(config.path, 0, 512);

	if ( !(cf1=Open("ENV:HRTmon.prefs",MODE_NEWFILE)) ) {
	    ShowReq("Couldn't save prefs.","OK");
	    return (TRUE);
	}

	if ( !(cf2=Open("ENVARC:HRTmon.prefs",MODE_NEWFILE)) ) {
	    ShowReq("Couldn't save prefs.","OK");
	    Close (cf1);
	    return (TRUE);
	}


	lock = GetProgramDir();
	if (lock)
		NameFromLock(lock, config.path, 510);

	Write(cf1, &config, sizeof(CONFIG));
	Write(cf2, &config, sizeof(CONFIG));

	Close(cf1);
	Close(cf2);

	return (TRUE);
}

/*******************************************************************/
// Menu quit

int MainQuit()
{
	return (FALSE);
}
/*******************************************************************/

int RightClicked()
{
	config.right = !config.right;
	return (TRUE);
}

/*******************************************************************/

int BootDiskClicked()
{
	char *Buffer;
	BPTR filelock,filehandle;
	struct FileInfoBlock infoblock;
	char EmptyBlock[512];
	int i;
	int autoaddr;

	if (config.autoa)
		autoaddr = 0;
	else
		autoaddr = config.address;


	if ( (filelock=Lock("HRTmon.data",ACCESS_READ)) == NULL ) {	// Lock file
	    ShowReq("Can't open file HRTmon.data","OK");
	    return (TRUE);
	}

	if ( Examine(filelock,&infoblock) == FALSE ) {		// Examine file
	    ShowReq("Can't open file HRTmon.data","OK");
	    UnLock(filelock);
	    return (TRUE);
	}

	UnLock(filelock);					// UnLock file

	if ( (Buffer=AllocMem(infoblock.fib_Size,MEMF_ANY)) == NULL ) {
	    ShowReq("Not enough memory to proceed.","OK");
	    return (TRUE);
	}

	if ( filehandle=Open("HRTmon.data",MODE_OLDFILE) ) {
	    Read(filehandle,Buffer,infoblock.fib_Size);		// Read file
	    Close(filehandle);
	}
	else {						// if Can't Open
	    ShowReq("Can't open file HRTmon.data","OK");
	    return (TRUE);
	}

	if ( LoadABS(Buffer,&autoaddr) == FALSE ) {
	    FreeMem(Buffer,infoblock.fib_Size);
	    return (TRUE);
	}

	FreeMem(Buffer,infoblock.fib_Size);

// **** End of loading of HRTmon to 'address' *************
// **** Now we will make the boot-disk ! ******************

	if (!Inhibit("DF0:",DOSTRUE)) {
	    ShowReq("Couldn't allocate DF0:","OK");
	    if (config.abs || config.autoa)
		FreeMem( (APTR)autoaddr, ((ULONG*)autoaddr)[5] );
	    return (TRUE);	//Cancel clicked
	}


	if ( !ShowReq("Insert a formatted floppy disk in df0:\n"
		"Warning ! The disk will be erased.","OK|Cancel") ) {
	    if (config.abs || config.autoa)
		FreeMem( (APTR)autoaddr, ((ULONG*)autoaddr)[5] );
	    Inhibit("DF0:",FALSE);
	    return (TRUE);	//Cancel clicked
	}

	if ( !WriteSec((char*)autoaddr,2,nb_sectors) ) {
	    if (config.abs || config.autoa)
		FreeMem( (APTR)autoaddr, ((ULONG*)autoaddr)[5] );
	    ShowReq("Couldn't write to disk !","OK");
	    Inhibit("DF0:",FALSE);
	    return (TRUE);
	}

	if (config.abs || config.autoa)
	    FreeMem( (APTR)autoaddr, ((ULONG*)autoaddr)[5] );

	BootBlock[2]=autoaddr;		// Load address for monitor
	BootBlock[1]=0;			// clear checksum
	BootBlock[1]=BootSum(BootBlock);

	WriteSec(BootBlock,0,2);

	for (i=0;i<512;i++)
	    EmptyBlock[i]=0;

	WriteSec(EmptyBlock,880,1);

	Inhibit("DF0:",FALSE);

	ShowReq ("HRTmon Boot-Disk\n"
		"succesfully installed.","OK");

	return (TRUE);
}

/*******************************************************************/

int InstallClicked()
{
	return Load("");
}

/*******************************************************************/

int WriteSec(char* Buffer,int Offset,int Len) {	// Offset and Len in sector

static struct IOStdReq diskio;
static struct MsgPort replyport;
char *clear;
int i,result;

	clear=&diskio;
	for (i=0;i< sizeof(struct IOStdReq);i++)
	 *clear=0;

	clear=&replyport;
	for (i=0;i< sizeof(struct MsgPort);i++)
	 *clear=0;

	replyport.mp_SigTask=FindTask( NULL );
	AddPort(&replyport);

	if (OpenDevice("trackdisk.device",0,&diskio,0)) {
	    ShowReq("Couldn't open trackdisk.device !","OK");
	    RemPort(&replyport);
	    return (FALSE);
	}

	diskio.io_Message.mn_ReplyPort=&replyport;

	diskio.io_Command=CMD_WRITE;
	diskio.io_Data=Buffer;
	diskio.io_Offset=Offset*512;
	diskio.io_Length=Len*512;

	if ( DoIO(&diskio) )
	    result=FALSE;
	else
	    result=TRUE;

	diskio.io_Command=CMD_UPDATE;
	diskio.io_Data=NULL;
	diskio.io_Offset=0;
	diskio.io_Length=0;
	DoIO(&diskio);

	diskio.io_Command=9;		// Switch motor OFF
	diskio.io_Data=NULL;
	diskio.io_Offset=0;
	diskio.io_Length=0;
	DoIO(&diskio);

	CloseDevice(&diskio);
	RemPort(&replyport);

	return (result);
}

/*******************************************************************/

int CancelClicked()
{
	return (FALSE);
}

/*******************************************************************/

int addressClicked()
{
	char *my_string,*p;
	int val=0;

	my_string=((struct StringInfo*) (MainGadgets[GD_address] -> SpecialInfo)) -> Buffer;

	if (*my_string != '$') {
	    sprintf(my_string,"$%x",config.address);
	    RefreshGList(MainGadgets[GD_address],MainWnd,NULL,1);
	    ShowReq("address must start with '$'","OK");
	    return (TRUE);
	}

/* convert string in hex and check if really in hex */

	for (p=my_string+1 ; *p != 0 ; p++) {
	    val=val*16;
	    if (*p>='0' && *p<='9')
		val=val+(*p-'0');
	    else
		if (*p>='a' && *p<='f')
		    val=val+(*p-'a'+10);
		else
		    if (*p>='A' && *p<='F')
			val=val+(*p-'A'+10);
		    else {
			sprintf(my_string,"$%x",config.address);
			RefreshGList(MainGadgets[GD_address],MainWnd,NULL,1);
			ShowReq("Not an hex number !","OK");
			return(TRUE);
		    }

	}

	if ( val & 0xf ) {
	    sprintf(my_string,"$%x",config.address);
	    RefreshGList(MainGadgets[GD_address],MainWnd,NULL,1);
	    ShowReq("Not a multiple of 16 !","OK");
	    return(TRUE);
	}

	config.address=val;

	return (TRUE);
}

/*******************************************************************/

int AllocAbsClicked()
{
	config.abs=!config.abs;
	return (TRUE);
}

/*******************************************************************/

int VBRClicked()
{
	config.vbr=!config.vbr;
	return (TRUE);
}

/*******************************************************************/

int hexmodeClicked()
{
	config.hexmode=!config.hexmode;
	return (TRUE);
}

/*******************************************************************/

int keyboardClicked()
{
	config.keyboard = MainMsg.Code;

	return (TRUE);
}

/*******************************************************************/

int screenmodeClicked()
{
	config.screenmode = MainMsg.Code;

	return (TRUE);
}

/*******************************************************************/

int SaveBClicked() {

	return (MainSave());

}

/*******************************************************************/

int IDEClicked()
{
	config.ide=!config.ide;
	return (TRUE);
}


/*******************************************************************/

int InsertClicked()
{
	config.insert=!config.insert;
	return (TRUE);
}

/*******************************************************************/

int AutoAllocClicked()
{
	config.autoa=!config.autoa;
	if (config.autoa) {
		(MainGadgets[GD_address] -> Flags) |= GFLG_DISABLED;
	} else {
		(MainGadgets[GD_address] -> Flags) &= (~GFLG_DISABLED);
	}

	RefreshGList(MainGadgets[GD_address],MainWnd,NULL,1);

	return (TRUE);
}

/*******************************************************************/

int KeyClicked()
{
	config.key=!config.key;
	return (TRUE);
}

/*******************************************************************/

int repClicked()
{

	config.delay = MainMsg.Code;

	return (TRUE);
}


/*******************************************************************/

int LViewClicked()
{
	config.lview = !config.lview;
	return (TRUE);
}

/*******************************************************************/

