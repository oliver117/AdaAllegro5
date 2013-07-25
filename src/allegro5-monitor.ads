with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package Allegro5.Monitor is

   type ALLEGRO_MONITOR_INFO is record
      x1 : aliased int;
      y1 : aliased int;
      x2 : aliased int;
      y2 : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MONITOR_INFO);

   function al_get_num_video_adapters return int;
   pragma Import (C, al_get_num_video_adapters, "al_get_num_video_adapters");

   function al_get_monitor_info (adapter : int; info : access ALLEGRO_MONITOR_INFO) return Extensions.bool;
   pragma Import (C, al_get_monitor_info, "al_get_monitor_info");

end Allegro5.Monitor;
