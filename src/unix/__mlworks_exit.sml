(* Copyright (C) 1999 Harlequin Ltd.  All rights reserved.
 *
 * Unix interface to process termination.
 * This provides a set of MLWorks-specific exit codes, and
 * OS-specific exit/terminate functions.
 * 
 * We don't use OS.Process directly because it can't provide
 * the application-specific status values.
 *
 * Any type/value that shares a name with one in OS.Process behaves 
 * according to the description of that type/value in OS.Process.
 *
 * For more information on the additional error codes see where they are
 * used (mainly main/_batch.sml) since their names and values were initially 
 * based on the context in which they were used and the integer values
 * used in that context.
 * 
 * Revision Log
 * ------------
 *
 * $Log: __mlworks_exit.sml,v $
 * Revision 1.3  1999/05/27 10:48:17  johnh
 * [Bug #190553]
 * Fix require statements to fix bootstrap compiler.
 *
 *  Revision 1.2  1999/05/14  17:42:58  daveb
 *  [Bug #190553]
 *  Fix typo.
 *
 *  Revision 1.1  1999/05/13  15:26:32  daveb
 *  new unit
 *  New unit.
 *
 *
 *)

require "../utils/mlworks_exit";
require "__unix";

structure MLWorksExit :> MLWORKS_EXIT =
  struct
    type status = Unix.exit_status

    val success = Unix.W_EXITED
    val failure = Unix.W_EXITSTATUS 0w1
    val uncaughtIOException = Unix.W_EXITSTATUS 0w2
    val badUsage = Unix.W_EXITSTATUS 0w3
    val stop = Unix.W_EXITSTATUS 0w4
    val save = Unix.W_EXITSTATUS 0w5
    val badInput = Unix.W_EXITSTATUS 0w6

    val exit = Unix.exit
  end
