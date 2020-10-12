with Real_Time_No_Elab;
--  pragma Elaborate_All (Real_Time_No_Elab);
pragma Elaborate (Real_Time_No_Elab);

with Real_Time_No_Elab.Timing_Events_No_Elab;
use Real_Time_No_Elab.Timing_Events_No_Elab;
pragma Elaborate (Real_Time_No_Elab.Timing_Events_No_Elab);

with System.Multiprocessors;
with System.BB.Threads;
with System.BB.Time;
with System;

package CPU_Budget_Monitor is
   pragma Preelaborate;

   type CPU_Budget_Exceeded is new Timing_Event with
        record
            Id : System.BB.Threads.Thread_Id; --  @todo it seems useless...
        end record;

   BE_Happened : array (System.Multiprocessors.CPU)
                            of CPU_Budget_Exceeded;
    --  A timing event for each CPU.
    --  BE means "Budget Exceeded".

   Last_Thread_On_CPU : array (1 .. 1) of Integer :=
        (others => -1);
    --  It holds the indentifier of the last thread which
    --  inform the inspector on the given CPU.

   type BE_Log is array (System.Priority) of Natural;

   BE_Detected : BE_Log := (others => 0);
    --  BE_Detected (Pri) is the number of time the thread with priority "Pri"
    --  has exceeded its budget. Therefore, the threads must have
    --  different priorities between them.

   --  CPU_Budget_Exceeded handler.
   procedure CPU_BE_Detected (E : in out Timing_Event);

   procedure Start_Monitor (For_Time : System.BB.Time.Time_Span);

   procedure Clear_Monitor (Cancelled : out Boolean);

end CPU_Budget_Monitor;
