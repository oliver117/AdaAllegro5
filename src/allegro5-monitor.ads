with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package Allegro5.Monitor is

   -- Describes a monitors size and position relative to other monitors. x1,
   --y1 will be 0, 0 on the primary display. Other monitors can have negative
   --values if they are to the left or above the primary display.
   type ALLEGRO_MONITOR_INFO is record
      x1 : aliased int;
      y1 : aliased int;
      x2 : aliased int;
      y2 : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MONITOR_INFO);

   -- Get the number of video "adapters" attached to the computer. Each video
   --card attached to the computer counts as one or more adapters. An adapter
   --is thus really a video port that can have a monitor connected to it.
   function al_get_num_video_adapters return int;
   pragma Import (C, al_get_num_video_adapters, "al_get_num_video_adapters");

   -- Get information about a monitor's position on the desktop. adapter is a
   --number from 0 to al_get_num_video_adapters()-1.
   --
   -- Returns true on success, false on failure.
   function al_get_monitor_info
     (adapter : int;
      info    : access ALLEGRO_MONITOR_INFO)
      return    Extensions.bool;
   pragma Import (C, al_get_monitor_info, "al_get_monitor_info");

end Allegro5.Monitor;
