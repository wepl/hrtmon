
to use the WHDLoad commands WHDLoad version 8.1.1908 or better is required,
moreover in the HRTmonPrefs the option "No VBR move" must be checked

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

 WQ	- quit WHDLoad
	returns to workbench

 WD	- quit WHDLoad with debug
	write WHDLoad's coredumps and returns to workbench
	(there is no stackframe cleanup!)

 WI	- WHDLoad info
	shows the WHDLoad version and size of chip memory used
	if WHDLoad has version >= 16.6 it also shows start, end and size
	of expansion memory (only if expmem is used)
	if WHDLoad has version >= 16.9 it also shows start, end and size
	of slave memory

 WLM	name start - load file into memory
	does the same as command L and has the same parameters
	the actual directory(ies) is that from the Slave

 WSM	name start end - save memory region to a file
	does the same as command S and has the same parameters
	the file will be saved to the first data directory of the current
	WHDLoad run

the following keys are currently available:

 Alt + PrtSc	dumps the actual screen as "hrtmon-screen.txt" to the first
		data directory of the current WHDLoad run

