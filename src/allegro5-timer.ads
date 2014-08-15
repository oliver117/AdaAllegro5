with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

limited with Allegro5.Events;

package Allegro5.Timer is

   -- This is an abstract data type representing a timer object.
   type ALLEGRO_TIMER is new System.Address;

   -- Convert microseconds to seconds.
   function ALLEGRO_USECS_TO_SECS (x : double) return double;

   -- Convert milliseconds to seconds.
   function ALLEGRO_MSECS_TO_SECS (x : double) return double;

   -- Convert beats per second to seconds.
   function ALLEGRO_BPS_TO_SECS (x : double) return double;

   -- Convert beats per minute to seconds.
   function ALLEGRO_BPM_TO_SECS (x : double) return double;

   -- Allocates and initializes a timer. If successful, a pointer to a new
   --timer object is returned, otherwise NULL is returned.
   --speed_secs is in seconds per "tick", and must be positive.
   --The new timer is initially stopped.

   -- Usage note: typical granularity is on the order of microseconds,
   --but with some drivers might only be milliseconds.
   function al_create_timer (speed_secs : double) return ALLEGRO_TIMER;
   pragma Import (C, al_create_timer, "al_create_timer");

   -- Uninstall the timer specified. If the timer is started, it will
   --automatically be stopped before uninstallation. It will also automatically
   --unregister the timer with any event queues.

   -- Does nothing if passed the NULL pointer.
   procedure al_destroy_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_destroy_timer, "al_destroy_timer");

   -- Start the timer specified. From then, the timer's counter will
   --increment at a constant rate, and it will begin generating events.
   --Starting a timer that is already started does nothing.
   procedure al_start_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_start_timer, "al_start_timer");

   -- Stop the timer specified. The timer's counter will stop
   --incrementing and it will stop generating events. Stopping a timer that
   --is already stopped does nothing.
   procedure al_stop_timer (timer : ALLEGRO_TIMER);
   pragma Import (C, al_stop_timer, "al_stop_timer");

   -- Return true if the timer specified is currently started.
   function al_get_timer_started
     (timer : ALLEGRO_TIMER) return Extensions.bool;
   pragma Import (C, al_get_timer_started, "al_get_timer_started");

   -- Return the timer's speed, in seconds. (The same value passed to
   --al_create_timer or al_set_timer_speed.)
   function al_get_timer_speed (timer : ALLEGRO_TIMER) return double;
   pragma Import (C, al_get_timer_speed, "al_get_timer_speed");

   -- Set the timer's speed, i.e. the rate at which its counter will be
   --incremented when it is started. This can be done when the timer is
   --started or stopped. If the timer is currently running, it is made to
   --look as though the speed change occurred precisely at the last tick.

   -- speed_secs has exactly the same meaning as with al_create_timer.
   procedure al_set_timer_speed (timer : ALLEGRO_TIMER; speed_secs : double);
   pragma Import (C, al_set_timer_speed, "al_set_timer_speed");

   -- Return the timer's counter value. The timer can be started or stopped.
   function al_get_timer_count (timer : ALLEGRO_TIMER) return Integer_64;
   pragma Import (C, al_get_timer_count, "al_get_timer_count");

   -- Set the timer's counter value. The timer can be started or stopped.
   --The count value may be positive or negative, but will always be
   --incremented by +1 at each tick.
   procedure al_set_timer_count (timer : ALLEGRO_TIMER; count : Integer_64);
   pragma Import (C, al_set_timer_count, "al_set_timer_count");

   -- Add diff to the timer's counter value. This is similar to writing:

   --    al_set_timer_count(timer, al_get_timer_count(timer) + diff);

   -- except that the addition is performed atomically,
   --so no ticks will be lost.
   procedure al_add_timer_count (timer : ALLEGRO_TIMER; diff : Integer_64);
   pragma Import (C, al_add_timer_count, "al_add_timer_count");

   -- Retrieve the associated event source.
   function al_get_timer_event_source
     (timer : ALLEGRO_TIMER)
      return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_timer_event_source, "al_get_timer_event_source");

end Allegro5.Timer;
