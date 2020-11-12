
/*

  Filename:    HRTmon.c
  Author:      Alain Malek
  Description: Load and install HRTmon quietly
               (use config saved by HRTmonPrefs)

  Usage:       HRTmon -r      (-r to only remove HRTmon)

*/


#include "HRTmonLoad.h"

int ShowReq(char *text,char*choice)
{
	printf("%s\n",text);
	return 0;
}

int main(int argc, char **argv)
{

	LoadInit();

	if ( RemHRTmon() )
	    printf("HRTmon located and removed\n");

	if (argc != 2 || strcmp(argv[1],"-r"))
	    Load(config.path);

	return 0;

}

