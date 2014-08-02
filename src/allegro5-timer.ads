with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

limited with Allegro5.Events;

package Allegro5.Timer is

   type ALLEGRO_TIMER is new System.Address;

   function ALLEGRO_USECS_TO_SECS (x : double) return double;

   function ALLEGRO_MSECS_TO_SECS (x : double) return double;

   function ALLEGRO_BPS_TO_SECS (x : double) return double;

   function ALLEGRO_BPM_TO_SECS (x : double) return double;

   function al_create_timer (speed_secs : double) return ALLEGRO_TIMER;
   pragma Import (C, al_create_timer, "al_create_timer");

   procedure al_destroy_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_destroy_timer, "al_destroy_timer");

   procedure al_start_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_start_timer, "al_start_timer");

   procedure al_stop_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_stop_timer, "al_stop_timer");

   function al_get_timer_started (timer : ALLEGRO_TIMER) return Extensions.bool;
   pragma Import (C, al_get_timer_started, "al_get_timer_started");

   function al_get_timer_speed (timer : ALLEGRO_TIMER) return double;
   pragma Import (C, al_get_timer_speed, "al_get_timer_speed");

   procedure al_set_timer_speed (timer : ALLEGRO_TIMER; speed_secs : double);
   pragma Import (C, al_set_timer_speed, "al_set_timer_speed");

   function al_get_timer_count (timer : ALLEGRO_TIMER) return Integer_64;
   pragma Import (C, al_get_timer_count, "al_get_timer_count");

   procedure al_set_timer_count (timer : ALLEGRO_TIMER; count : Integer_64);
   pragma Import (C, al_set_timer_count, "al_set_timer_count");

   procedure al_add_timer_count (timer : ALLEGRO_TIMER; diff : Integer_64);
   pragma Import (C, al_add_timer_count, "al_add_timer_count");

   function al_get_timer_event_source (timer : ALLEGRO_TIMER) return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_timer_event_source, "al_get_timer_event_source");

end Allegro5.Timer;
