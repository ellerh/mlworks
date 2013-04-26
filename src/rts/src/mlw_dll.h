/*  === TOP LEVEL OF RUNTIME SYSTEM ===
 *
 *  Copyright (C) 1991 Harlequin Ltd.
 *
 *  Description
 *  -----------
 *  This file contains the main() function, which parses the command
 *  line, loads and invokes the modules, etc.
 *
 *  Revision Log
 *  ------------
 *  $Log: mlw_dll.h,v $
 *  Revision 1.2  1998/09/16 11:06:56  jont
 *  [Bug #30108]
 *  Add trampoline to declarations
 *
 * Revision 1.1  1997/05/22  11:19:53  andreww
 * new unit
 * [Bug #30045]
 * conversion of main.h to be part of dll.
 *
 * Revision 1.3  1995/09/26  10:40:15  jont
 * Add runtime to record the current executable
 * for the benefit of executable image saving
 *
 * Revision 1.2  1994/06/09  14:42:10  nickh
 * new file
 *
 * Revision 1.1  1994/06/09  11:10:01  nickh
 * new file
 *
 *  Revision 1.6  1993/09/07  11:02:33  daveb
 *  Added -mono option.
 *
 *  Revision 1.5  1992/10/02  09:27:17  richard
 *  Added missing consts.
 *
 *  Revision 1.4  1992/09/01  11:25:48  richard
 *  Implemented argument passing to the modules.
 *
 *  Revision 1.3  1992/08/26  15:46:19  richard
 *  The module table is now a pervasive value.
 *
 *  Revision 1.2  1992/08/19  07:06:10  richard
 *  Added modules as an export.
 *
 *  Revision 1.1  1992/07/23  11:48:35  richard
 *  Initial revision
 *
 */

#include "mlw_ci_os.h"

#ifndef mlw_dll_h
#define mlw_dll_h

extern mlval image_continuation;

extern int module_argc;        /* arguments to be passed to ML */
extern const char *const *module_argv;

extern int mono;		 /* running on a mono screen? */

extern mlw_ci_export int mlw_main(int argc, const char *const *argv);

mlw_ci_export void trampoline(mlval setup, void (*declare)(void));

extern const char *runtime;   /* The executable we started from */
#endif
