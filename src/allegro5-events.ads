with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with stdint;
with System;

limited with Allegro5.Altime;
with Allegro5.Base;

use Allegro5;


package Allegro5.Events is

   function ALLEGRO_EVENT_TYPE_IS_USER (t : unsigned) return Extensions.bool;

   function ALLEGRO_GET_EVENT_TYPE(a : int; b : int; c : int; d : int) return int renames Allegro5.Base.AL_ID;

   subtype ALLEGRO_EVENT_TYPE is unsigned;

   type ALLEGRO_EVENT_SOURCE_uu_pad_array is array (0 .. 31) of aliased int;
   type ALLEGRO_EVENT_SOURCE is record
      uu_pad : aliased ALLEGRO_EVENT_SOURCE_uu_pad_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_EVENT_SOURCE);

   type ALLEGRO_ANY_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : access ALLEGRO_EVENT_SOURCE;
      timestamp : aliased double;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_ANY_EVENT);

   type ALLEGRO_DISPLAY_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : System.Address;
      timestamp : aliased double;
      x : aliased int;
      y : aliased int;
      width : aliased int;
      height : aliased int;
      orientation : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_DISPLAY_EVENT);

   type ALLEGRO_DISPLAY is new System.Address;

   type ALLEGRO_JOYSTICK_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : System.Address;
      timestamp : aliased double;
      id : System.Address;
      stick : aliased int;
      axis : aliased int;
      pos : aliased float;
      button : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_JOYSTICK_EVENT);

   type ALLEGRO_KEYBOARD_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : System.Address;
      timestamp : aliased double;
      display : System.Address;
      keycode : aliased int;
      unichar : aliased int;
      modifiers : aliased unsigned;
      repeat : aliased Extensions.bool;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_KEYBOARD_EVENT);

   subtype ALLEGRO_KEYBOARD is Extensions.opaque_structure_def;

   type ALLEGRO_MOUSE_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : System.Address;
      timestamp : aliased double;
      display : System.Address;
      x : aliased int;
      y : aliased int;
      z : aliased int;
      w : aliased int;
      dx : aliased int;
      dy : aliased int;
      dz : aliased int;
      dw : aliased int;
      button : aliased unsigned;
      pressure : aliased float;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MOUSE_EVENT);

   subtype ALLEGRO_MOUSE is Extensions.opaque_structure_def;

   type ALLEGRO_TIMER_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : System.Address;
      timestamp : aliased double;
      count : aliased stdint.int64_t;
      error : aliased double;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TIMER_EVENT);

   subtype ALLEGRO_TIMER is Extensions.opaque_structure_def;

   type ALLEGRO_USER_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : access ALLEGRO_EVENT_SOURCE;
      timestamp : aliased double;
      uu_internal_u_descr : System.Address;
      data1 : aliased stdint.intptr_t;
      data2 : aliased stdint.intptr_t;
      data3 : aliased stdint.intptr_t;
      data4 : aliased stdint.intptr_t;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_USER_EVENT);

   subtype ALLEGRO_USER_EVENT_DESCRIPTOR is Extensions.opaque_structure_def;

   type ALLEGRO_EVENT (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            c_type : aliased ALLEGRO_EVENT_TYPE;
         when 1 =>
            any : aliased ALLEGRO_ANY_EVENT;
         when 2 =>
            display : aliased ALLEGRO_DISPLAY_EVENT;
         when 3 =>
            joystick : aliased ALLEGRO_JOYSTICK_EVENT;
         when 4 =>
            keyboard : aliased ALLEGRO_KEYBOARD_EVENT;
         when 5 =>
            mouse : aliased ALLEGRO_MOUSE_EVENT;
         when 6 =>
            timer : aliased ALLEGRO_TIMER_EVENT;
         when others =>
            user : aliased ALLEGRO_USER_EVENT;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_EVENT);
   pragma Unchecked_Union (ALLEGRO_EVENT);

   procedure al_init_user_event_source (arg1 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_init_user_event_source, "al_init_user_event_source");

   procedure al_destroy_user_event_source (arg1 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_destroy_user_event_source, "al_destroy_user_event_source");

   function al_emit_user_event
     (arg1 : access ALLEGRO_EVENT_SOURCE;
      arg2 : access ALLEGRO_EVENT;
      dtor : access procedure (arg1 : access ALLEGRO_USER_EVENT)) return Extensions.bool;
   pragma Import (C, al_emit_user_event, "al_emit_user_event");

   procedure al_unref_user_event (arg1 : access ALLEGRO_USER_EVENT);
   pragma Import (C, al_unref_user_event, "al_unref_user_event");

   procedure al_set_event_source_data (arg1 : access ALLEGRO_EVENT_SOURCE; data : stdint.intptr_t);
   pragma Import (C, al_set_event_source_data, "al_set_event_source_data");

   function al_get_event_source_data (arg1 : System.Address) return stdint.intptr_t;
   pragma Import (C, al_get_event_source_data, "al_get_event_source_data");

   subtype ALLEGRO_EVENT_QUEUE is Extensions.opaque_structure_def;

   function al_create_event_queue return System.Address;
   pragma Import (C, al_create_event_queue, "al_create_event_queue");

   procedure al_destroy_event_queue (arg1 : System.Address);
   pragma Import (C, al_destroy_event_queue, "al_destroy_event_queue");

   procedure al_register_event_source (arg1 : System.Address; arg2 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_register_event_source, "al_register_event_source");

   procedure al_unregister_event_source (arg1 : System.Address; arg2 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_unregister_event_source, "al_unregister_event_source");

   function al_is_event_queue_empty (arg1 : System.Address) return Extensions.bool;
   pragma Import (C, al_is_event_queue_empty, "al_is_event_queue_empty");

   function al_get_next_event (arg1 : System.Address; ret_event : access ALLEGRO_EVENT) return Extensions.bool;
   pragma Import (C, al_get_next_event, "al_get_next_event");

   function al_peek_next_event (arg1 : System.Address; ret_event : access ALLEGRO_EVENT) return Extensions.bool;
   pragma Import (C, al_peek_next_event, "al_peek_next_event");

   function al_drop_next_event (arg1 : System.Address) return Extensions.bool;
   pragma Import (C, al_drop_next_event, "al_drop_next_event");

   procedure al_flush_event_queue (arg1 : System.Address);
   pragma Import (C, al_flush_event_queue, "al_flush_event_queue");

   procedure al_wait_for_event (arg1 : System.Address; ret_event : access ALLEGRO_EVENT);
   pragma Import (C, al_wait_for_event, "al_wait_for_event");

   function al_wait_for_event_timed
     (arg1 : System.Address;
      ret_event : access ALLEGRO_EVENT;
      secs : float) return Extensions.bool;
   pragma Import (C, al_wait_for_event_timed, "al_wait_for_event_timed");

   function al_wait_for_event_until
     (queue : System.Address;
      ret_event : access ALLEGRO_EVENT;
      timeout : access Allegro5.Altime.ALLEGRO_TIMEOUT) return Extensions.bool;
   pragma Import (C, al_wait_for_event_until, "al_wait_for_event_until");

end Allegro5.Events;
