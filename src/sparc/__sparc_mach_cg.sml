(* __sparc_mach_cg.sml the structure *)
(*
$Log: __sparc_mach_cg.sml,v $
Revision 1.5  1998/01/30 09:48:38  johnh
[Bug #30326]
Merge in change from branch MLWorks_workspace_97

 * Revision 1.4.10.2  1997/11/20  17:09:10  daveb
 * [Bug #30326]
 *
 * Revision 1.4.10.1  1997/09/11  21:08:49  daveb
 * branched from trunk for label MLWorks_workspace_97
 *
 * Revision 1.4  1995/02/13  14:48:33  matthew
 * Adding Options structure
 *
Revision 1.3  1994/06/09  16:06:08  nickh
New runtime directory structure.

Revision 1.2  1994/03/08  18:15:15  jont
Remove module type to separate file

Revision 1.1  1993/11/18  12:34:49  jont
Initial revision

Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

require "../rts/gen/__tags";
require "../utils/__text";
require "../utils/__print";
require "../utils/__mlworks_timer" ;
require "../utils/__lists";
require "../utils/__crash";
require "../main/__info";
require "../main/__options";
require "../utils/__sexpr";
require "../main/__reals";
require "../main/__code_module";
require "../basics/__ident";
require "../mir/__mirtables";
require "../mir/__mirregisters";
require "__machspec";
require "__sparc_schedule";
require "../rts/gen/__implicit";
require "../utils/_diagnostic";
require "_mach_cg";


structure Sparc_Mach_Cg_ = Mach_Cg(
  structure Tags = Tags_
  structure Print = Print_
  structure Timer = Timer_
  structure Lists = Lists_
  structure Crash = Crash_
  structure Info = Info_
  structure Options = Options_
  structure Sexpr = Sexpr_
  structure Reals = Reals_
  structure Ident = Ident_
  structure MirTables = MirTables_
  structure MirRegisters = MirRegisters_
  structure MachSpec = MachSpec_
  structure Code_Module = Code_Module_
  structure Sparc_Schedule = Sparc_Schedule_
  structure Implicit_Vector = ImplicitVector_
  structure Diagnostic = Diagnostic(structure Text = Text_ )
)
