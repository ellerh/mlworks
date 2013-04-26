(* Object_Output the structure *)
(*
 * Functions to output code as genuine (.o) object files, either in assembler
 * format, or binary
 *
 * Copyright (c) 1998, Harlequin Group plc
 * All rights reserved
 *
 * $Log: __object_output.sml,v $
 * Revision 1.3  1999/02/02 16:01:37  mitchell
 * [Bug #190500]
 * Remove redundant require statements
 *
 * Revision 1.2  1998/08/25  12:09:15  jont
 * Automatic checkin:
 * changed attribute _comment to ' * '
 *
 *
 *)

require "__sparc_assembly";
require "../main/__code_module";
require "../main/__project";
require "../basics/__module_id";

structure Object_Output_ =
  struct
    datatype OUTPUT_TYPE = ASM | BINARY

    type Opcode = Sparc_Assembly_.opcode

    type Module = Code_Module_.Module

    type ModuleId = ModuleId_.ModuleId

    type Project = Project_.Project

    fun output_object_code _ _ = ()
      (* Not implemented yet *)

  end
