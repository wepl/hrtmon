/*
 *  Source machine generated by GadToolsBox V2.0b
 *  which is (c) Copyright 1991-1993 Jaba Development
 *
 *  GUI Designed by : -- Unnamed --
 */

#define GetString( g )      ((( struct StringInfo * )g->SpecialInfo )->Buffer  )
#define GetNumber( g )      ((( struct StringInfo * )g->SpecialInfo )->LongInt )

#define GD_SaveB                               0
#define GD_Cancel                              1
#define GD_address                             2
#define GD_AllocAbs                            3
#define GD_BootDisk                            4
#define GD_Right                               5
#define GD_keyboard                            6
#define GD_IDE                                 7
#define GD_AutoAlloc                           8
#define GD_Install                             9
#define GD_Key                                 10
#define GD_Insert                              11
#define GD_rep                                 12
#define GD_LView                               13
#define GD_screenmode                          14
#define GD_VBR                                 15
#define GD_hexmode                             16

#define GDX_SaveB                              0
#define GDX_Cancel                             1
#define GDX_address                            2
#define GDX_AllocAbs                           3
#define GDX_BootDisk                           4
#define GDX_Right                              5
#define GDX_keyboard                           6
#define GDX_IDE                                7
#define GDX_AutoAlloc                          8
#define GDX_Install                            9
#define GDX_Key                                10
#define GDX_Insert                             11
#define GDX_rep                                12
#define GDX_LView                              13
#define GDX_screenmode                         14
#define GDX_VBR                                15
#define GDX_hexmode                            16

#define Main_CNT 17

extern struct IntuitionBase *IntuitionBase;
extern struct Library       *GadToolsBase;

extern struct Screen        *Scr;
extern UBYTE                 *PubScreenName;
extern APTR                  VisualInfo;
extern struct Window        *MainWnd;
extern struct Gadget        *MainGList;
extern struct Menu          *MainMenus;
extern struct IntuiMessage   MainMsg;
extern struct Gadget        *MainGadgets[17];
extern UWORD                 MainLeft;
extern UWORD                 MainTop;
extern UWORD                 MainWidth;
extern UWORD                 MainHeight;
extern UBYTE                *MainWdt;
extern UBYTE                *keyboard0Labels[];
extern UBYTE                *screenmode0Labels[];
extern struct TextAttr       topaz8;
extern struct NewMenu        MainNewMenu[];
extern UWORD                 MainGTypes[];
extern struct NewGadget      MainNGad[];
extern ULONG                 MainGTags[];

extern int SaveBClicked( void );
extern int CancelClicked( void );
extern int addressClicked( void );
extern int AllocAbsClicked( void );
extern int BootDiskClicked( void );
extern int RightClicked( void );
extern int keyboardClicked( void );
extern int IDEClicked( void );
extern int AutoAllocClicked( void );
extern int InstallClicked( void );
extern int KeyClicked( void );
extern int InsertClicked( void );
extern int repClicked( void );
extern int LViewClicked( void );
extern int screenmodeClicked( void );
extern int VBRClicked( void );
extern int hexmodeClicked( void );
extern int MainSave( void );
extern int MainQuit( void );

extern int SetupScreen( void );
extern void CloseDownScreen( void );
extern int HandleMainIDCMP( void );
extern int MainCloseWindow();
extern int OpenMainWindow( void );
extern void CloseMainWindow( void );
