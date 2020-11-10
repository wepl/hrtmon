/*
 *  Source machine generated by GadToolsBox V2.0b
 *  which is (c) Copyright 1991-1993 Jaba Development
 *
 *  GUI Designed by : -- Unnamed --
 */

#include <exec/types.h>
#include <intuition/intuition.h>
#include <intuition/classes.h>
#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>
#include <graphics/displayinfo.h>
#include <graphics/gfxbase.h>
#include <clib/exec_protos.h>
#include <clib/intuition_protos.h>
#include <clib/gadtools_protos.h>
#include <clib/graphics_protos.h>
#include <clib/utility_protos.h>
#include <string.h>

#include "HRTmonGUI2.h"

struct Screen         *Scr = NULL;
UBYTE                 *PubScreenName = "Workbench";
APTR                   VisualInfo = NULL;
struct Window         *MainWnd = NULL;
struct Gadget         *MainGList = NULL;
struct Menu           *MainMenus = NULL;
struct IntuiMessage    MainMsg;
struct Gadget         *MainGadgets[17];
UWORD                  MainLeft = 32;
UWORD                  MainTop = 13;
UWORD                  MainWidth = 492;
UWORD                  MainHeight = 213;
UBYTE                 *MainWdt = (UBYTE *)"HRTmonPrefs v2.2";

UBYTE *keyboard0Labels[] = {
	(UBYTE *)"USA",
	(UBYTE *)"CH",
	(UBYTE *)"D",
	(UBYTE *)"F",
	NULL };

UBYTE *screenmode0Labels[] = {
	(UBYTE *)"PAL",
	(UBYTE *)"NTSC",
	(UBYTE *)"Multiscan",
	NULL };

struct TextAttr topaz8 = {
	( STRPTR )"topaz.font", 8, 0x00, 0x00 };

struct NewMenu MainNewMenu[] = {
	NM_TITLE, (STRPTR)"Preferences", NULL, 0, NULL, NULL,
	NM_ITEM, (STRPTR)"Save", NULL, 0, 0L, (APTR)MainSave,
	NM_ITEM, (STRPTR)NM_BARLABEL, NULL, 0, 0L, NULL,
	NM_ITEM, (STRPTR)"Quit", NULL, 0, 0L, (APTR)MainQuit,
	NM_END, NULL, NULL, 0, 0L, NULL };

UWORD MainGTypes[] = {
	BUTTON_KIND,
	BUTTON_KIND,
	STRING_KIND,
	CHECKBOX_KIND,
	BUTTON_KIND,
	CHECKBOX_KIND,
	CYCLE_KIND,
	CHECKBOX_KIND,
	CHECKBOX_KIND,
	BUTTON_KIND,
	CHECKBOX_KIND,
	CHECKBOX_KIND,
	SLIDER_KIND,
	CHECKBOX_KIND,
	CYCLE_KIND,
	CHECKBOX_KIND,
	CHECKBOX_KIND
};

struct NewGadget MainNGad[] = {
	28, 175, 81, 13, (UBYTE *)"Save", NULL, GD_SaveB, PLACETEXT_IN, NULL, (APTR)SaveBClicked,
	205, 175, 81, 13, (UBYTE *)"Quit", NULL, GD_Cancel, PLACETEXT_IN, NULL, (APTR)CancelClicked,
	32, 3, 97, 13, (UBYTE *)"Load Address", NULL, GD_address, PLACETEXT_RIGHT, NULL, (APTR)addressClicked,
	32, 60, 26, 11, (UBYTE *)"Alloc Abs", NULL, GD_AllocAbs, PLACETEXT_RIGHT, NULL, (APTR)AllocAbsClicked,
	28, 195, 258, 13, (UBYTE *)"Make Boot-Disk...", NULL, GD_BootDisk, PLACETEXT_IN, NULL, (APTR)BootDiskClicked,
	32, 107, 26, 11, (UBYTE *)"Right-Mouse Enter", NULL, GD_Right, PLACETEXT_RIGHT, NULL, (APTR)RightClicked,
	280, 57, 97, 13, (UBYTE *)"Keyboard", NULL, GD_keyboard, PLACETEXT_RIGHT, NULL, (APTR)keyboardClicked,
	32, 140, 26, 11, (UBYTE *)"IDE harddisk", NULL, GD_IDE, PLACETEXT_RIGHT, NULL, (APTR)IDEClicked,
	32, 76, 26, 11, (UBYTE *)"Auto Alloc", NULL, GD_AutoAlloc, PLACETEXT_RIGHT, NULL, (APTR)AutoAllocClicked,
	116, 175, 81, 13, (UBYTE *)"Install", NULL, GD_Install, PLACETEXT_IN, NULL, (APTR)InstallClicked,
	32, 124, 26, 11, (UBYTE *)"Key Enter ('\\' on US kb)", NULL, GD_Key, PLACETEXT_RIGHT, NULL, (APTR)KeyClicked,
	32, 92, 26, 11, (UBYTE *)"Insert mode as default", NULL, GD_Insert, PLACETEXT_RIGHT, NULL, (APTR)InsertClicked,
	28, 25, 153, 13, (UBYTE *)"Repeat key delay", NULL, GD_rep, PLACETEXT_BELOW, NULL, (APTR)repClicked,
	32, 156, 26, 11, (UBYTE *)"LoadView task", NULL, GD_LView, PLACETEXT_RIGHT, NULL, (APTR)LViewClicked,
	280, 75, 97, 13, (UBYTE *)"Screen Mode", NULL, GD_screenmode, PLACETEXT_RIGHT, NULL, (APTR)screenmodeClicked,
	280, 92, 26, 11, (UBYTE *)"No VBR move", NULL, GD_VBR, PLACETEXT_RIGHT, NULL, (APTR)VBRClicked,
	280, 108, 26, 11, (UBYTE *)"Hex mode", NULL, GD_hexmode, PLACETEXT_RIGHT, NULL, (APTR)hexmodeClicked
};

ULONG MainGTags[] = {
	(TAG_DONE),
	(TAG_DONE),
	(GTST_String), (ULONG)"0", (GTST_MaxChars), 16, (GA_Disabled), TRUE, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTCY_Labels), (ULONG)&keyboard0Labels[ 0 ], (GTCY_Active), 1, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTSL_Min), 1, (GTSL_Max), 25, (GTSL_Level), 15, (GTSL_MaxLevelLen), 7, (GTSL_LevelFormat), (ULONG)"%d%02d", (GTSL_LevelPlace), (PLACETEXT_RIGHT), (PGA_Freedom), LORIENT_HORIZ, (GA_RelVerify), TRUE, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTCY_Labels), (ULONG)&screenmode0Labels[ 0 ], (GTCY_Active), 1, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE),
	(GTCB_Checked), TRUE, (TAG_DONE)
};

int SetupScreen( void )
{
	if ( ! ( Scr = LockPubScreen( PubScreenName )))
		return( 1L );

	if ( ! ( VisualInfo = GetVisualInfo( Scr, TAG_DONE )))
		return( 2L );

	return( 0L );
}

void CloseDownScreen( void )
{
	if ( VisualInfo ) {
		FreeVisualInfo( VisualInfo );
		VisualInfo = NULL;
	}

	if ( Scr        ) {
		UnlockPubScreen( NULL, Scr );
		Scr = NULL;
	}
}

int HandleMainIDCMP( void )
{
	struct IntuiMessage	*m;
	struct MenuItem		*n;
	int			(*func)();
	BOOL			running = TRUE;

	while( m = GT_GetIMsg( MainWnd->UserPort )) {

		CopyMem(( char * )m, ( char * )&MainMsg, (long)sizeof( struct IntuiMessage ));

		GT_ReplyIMsg( m );

		switch ( MainMsg.Class ) {

			case	IDCMP_REFRESHWINDOW:
				GT_BeginRefresh( MainWnd );
				GT_EndRefresh( MainWnd, TRUE );
				break;

			case	IDCMP_CLOSEWINDOW:
				running = MainCloseWindow();
				break;

			case	IDCMP_GADGETUP:
			case	IDCMP_GADGETDOWN:
				func = ( void * )(( struct Gadget * )MainMsg.IAddress )->UserData;
				running = func();
				break;

			case	IDCMP_MENUPICK:
				while( MainMsg.Code != MENUNULL ) {
					n = ItemAddress( MainMenus, MainMsg.Code );
					func = (void *)(GTMENUITEM_USERDATA( n ));
					running = func();
					MainMsg.Code = n->NextSelect;
				}
				break;
		}
	}
	return( running );
}

int OpenMainWindow( void )
{
	struct NewGadget	ng;
	struct Gadget	*g;
	UWORD		lc, tc;
	UWORD		offx = Scr->WBorLeft, offy = Scr->WBorTop + Scr->RastPort.TxHeight + 1;

	if ( ! ( g = CreateContext( &MainGList )))
		return( 1L );

	for( lc = 0, tc = 0; lc < Main_CNT; lc++ ) {

		CopyMem((char * )&MainNGad[ lc ], (char * )&ng, (long)sizeof( struct NewGadget ));

		ng.ng_VisualInfo = VisualInfo;
		ng.ng_TextAttr   = &topaz8;
		ng.ng_LeftEdge  += offx;
		ng.ng_TopEdge   += offy;

		MainGadgets[ lc ] = g = CreateGadgetA((ULONG)MainGTypes[ lc ], g, &ng, ( struct TagItem * )&MainGTags[ tc ] );

		while( MainGTags[ tc ] ) tc += 2;
		tc++;

		if ( NOT g )
			return( 2L );
	}

	if ( ! ( MainMenus = CreateMenus( MainNewMenu, GTMN_FrontPen, 0L, TAG_DONE )))
		return( 3L );

	LayoutMenus( MainMenus, VisualInfo, GTMN_TextAttr, &topaz8, TAG_DONE );

	if ( ! ( MainWnd = OpenWindowTags( NULL,
				WA_Left,	MainLeft,
				WA_Top,		MainTop,
				WA_Width,	MainWidth,
				WA_Height,	MainHeight + offy,
				WA_IDCMP,	BUTTONIDCMP|STRINGIDCMP|CHECKBOXIDCMP|CYCLEIDCMP|SLIDERIDCMP|IDCMP_MENUPICK|IDCMP_CLOSEWINDOW|IDCMP_REFRESHWINDOW,
				WA_Flags,	WFLG_DRAGBAR|WFLG_DEPTHGADGET|WFLG_CLOSEGADGET|WFLG_SMART_REFRESH|WFLG_ACTIVATE,
				WA_Gadgets,	MainGList,
				WA_Title,	MainWdt,
				TAG_DONE )))
	return( 4L );

	SetMenuStrip( MainWnd, MainMenus );
	GT_RefreshWindow( MainWnd, NULL );

	return( 0L );
}

void CloseMainWindow( void )
{
	if ( MainMenus      ) {
		ClearMenuStrip( MainWnd );
		FreeMenus( MainMenus );
		MainMenus = NULL;	}

	if ( MainWnd        ) {
		CloseWindow( MainWnd );
		MainWnd = NULL;
	}

	if ( MainGList      ) {
		FreeGadgets( MainGList );
		MainGList = NULL;
	}
}

