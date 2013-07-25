with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with stdint;
limited with Allegro5.Events;

package Allegro5.Timer is

   --  arg-macro: function ALLEGRO_USECS_TO_SECS (x)
   --    return (x) / 1000000.0;
   --  arg-macro: function ALLEGRO_MSECS_TO_SECS (x)
   --    return (x) / 1000.0;
   --  arg-macro: function ALLEGRO_BPS_TO_SECS (x)
   --    return 1.0 / (x);
   --  arg-macro: function ALLEGRO_BPM_TO_SECS (x)
   --    return 60.0 / (x);
   function al_create_timer (speed_secs : double) return System.Address;
   pragma Import (C, al_create_timer, "al_create_timer");

   procedure al_destroy_timer (timer : System.Address);
   pragma Import (C, al_destroy_timer, "al_destroy_timer");

   procedure al_start_timer (timer : System.Address);
   pragma Import (C, al_start_timer, "al_start_timer");

   procedure al_stop_timer (timer : System.Address);
   pragma Import (C, al_stop_timer, "al_stop_timer");

   function al_get_timer_started (timer : System.Address) return Extensions.bool;
   pragma Import (C, al_get_timer_started, "al_get_timer_started");

   function al_get_timer_speed (timer : System.Address) return double;
   pragma Import (C, al_get_timer_speed, "al_get_timer_speed");

   procedure al_set_timer_speed (timer : System.Address; speed_secs : double);
   pragma Import (C, al_set_timer_speed, "al_set_timer_speed");

   function al_get_timer_count (timer : System.Address) return stdint.int64_t;
   pragma Import (C, al_get_timer_count, "al_get_timer_count");

   procedure al_set_timer_count (timer : System.Address; count : stdint.int64_t);
   pragma Import (C, al_set_timer_count, "al_set_timer_count");

   procedure al_add_timer_count (timer : System.Address; diff : stdint.int64_t);
   pragma Import (C, al_add_timer_count, "al_add_timer_count");

   function al_get_timer_event_source (timer : System.Address) return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_timer_event_source, "al_get_timer_event_source");

end Allegro5.Timer;
