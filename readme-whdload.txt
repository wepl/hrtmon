
to use the whdload commands whdload version 8.1.1908 or better is required
moreover in the HrtmonPrefs the option "No VBR move" must be disabled

the following commands are currently available:

 WPR	start length - protect memory from reading
	see whdload.doc section resload_ProtectRead

 WPRW	start length - protect memory from reading and writing
	see whdload.doc section resload_ProtectReadWrite

 WPSMC	start length - snoop memory region for selfmodifying code
	see whdload.doc section resload_ProtectSMC

 WPW	start length - protect memory from writing
	see whdload.doc section resload_ProtectWrite

 WPD	start length - delete a previousy created memory protection

 WQ	- quit whdload
	returns to workbench

 WD	- quit whdload with debug
	write whdload's coredumps and returns to workbench
	(there is no stackframe cleanup!)

 WL	name start - load file into memory
	does the same as command L and has the same parameters
	the actual directory is that from the slave (or ws_CurrentDir if used)

 WS	name start end - save memory region to a file
	does the same as command S and has the same parameters
	the actual directory is that from the slave (or ws_CurrentDir if used)
	if you are using FFS it's recommend to save to RAM: to avoid non 
	validated disks if something goes wrong

the following keys are currently available:

 Alt + PrtSc	dumps the actual screen as "RAM:hrtmon-screen.txt"

Wepl 01.09.2001 (wepl@whdload.org)

