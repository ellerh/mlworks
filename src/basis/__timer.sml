(*  Copyright (C) 1995 Harlequin Ltd.
 *
 *
 * Design decisions
 * ----------------
 *
 * When a CPU timer is started, what is returned is is the cpu, usr and gc
 * at that point.  When this is checked at a later point, all that is done
 * is to get times again and compute the difference.  The attraction of
 * this method is that it only requires one runtime routine (the one to
 * return the current user, system and gc time), all the rest can be done
 * in SML.  This is at the cost of doing some allocation i.e. generating
 * the times only to unpick them to compute their difference.  
 *
 * Unlike the previous interface, the CPU timer doensn't bundle in the
 * real timer.  I didn't do this because I could see no advantage to
 * doing so.
 *
 * Revision Log
 * ------------
 *  $Log: __timer.sml,v $
 *  Revision 1.5  1999/05/12 11:31:55  daveb
 *  [Bug #190554]
 *  Separated out GC timers.
 *
 * Revision 1.4  1997/11/18  17:17:54  jont
 * [Bug #30085]
 * Add totalCPUTimer and totalRealTimer
 *
 * Revision 1.3  1996/05/30  09:09:53  stephenb
 * Update so that it matches March 1996 basis definition.
 *
 * Revision 1.2  1996/05/01  15:26:08  stephenb
 * Update wrt change in Time implementation.
 *
 * Revision 1.1  1996/04/18  11:34:42  jont
 * new unit
 *
 *  Revision 1.1  1995/04/13  13:35:47  jont
 *  new unit
 *  No reason given
 *)

require "^.system.__time";
require "timer";


structure Timer : TIMER =
  struct

    val env = MLWorks.Internal.Runtime.environment

    datatype cpu_timer = 
      CPU_TIMER of 
       { usr: Time.time
       , sys: Time.time
       , gc:  Time.time
       }

    datatype real_timer = REAL_TIMER of Time.time | TOTAL


    val now : unit -> cpu_timer = env "Timer.now"


    val startCPUTimer : unit -> cpu_timer = now

    fun totalCPUTimer() =
      CPU_TIMER{usr=Time.zeroTime, sys=Time.zeroTime, gc=Time.zeroTime}

    fun checkCPUTimer (CPU_TIMER {usr, sys, ...}) =
      let
        val (CPU_TIMER {usr= usr', sys=sys', ...}) = now ()
        val usr'' =  Time.-(usr', usr)
        val sys'' =  Time.-(sys', sys)
      in
        {usr=usr'', sys=sys''}
      end

    fun checkGCTime (CPU_TIMER {gc, ...}) =
      let
        val (CPU_TIMER {gc=gc', ...}) = now ()
      in
	Time.-(gc', gc)
      end


    fun startRealTimer () = REAL_TIMER (Time.now ())

    fun totalRealTimer() = TOTAL

    val startTime = env "Time.start"

    fun checkRealTimer arg =
      let
        val t = case arg of
	  REAL_TIMER t => t
	| TOTAL => startTime ()
      in
        Time.-(Time.now (), t)
      end

  end
