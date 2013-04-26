(* Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
Result: OK

 *
 * Test OS.FileSys.remove.  All tests should return true.
 * 
 * Revision Log
 * ------------
 *
 * $Log: os_file_sys_remove.sml,v $
 * Revision 1.4  1997/11/21 10:45:27  daveb
 * [Bug #30323]
 *
 *  Revision 1.3  1997/04/01  16:42:54  jont
 *  Modify to stop displaying syserror type
 *
 *  Revision 1.2  1996/05/22  10:19:10  daveb
 *  Shell.Module renamed to Shell.Build.
 *
 *  Revision 1.1  1996/05/17  11:49:37  stephenb
 *  new unit
 *
 *
 *)


(* Test if an exception is raised when attempting to remove a non-existent
 * file
 *)

val a = (OS.FileSys.remove "no such file"; false) handle OS.SysErr _ => true;


val b = (OS.FileSys.remove ""; false) handle OS.SysErr _ => true;


(* Should test that remove actually works, but to to that need to create a
 * file first which cannot be done portably until the IO library is sorted out.
 *)
