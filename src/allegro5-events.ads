with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with stdint;
with System;

with Allegro5.Altime;
with Allegro5.Base;
with Allegro5.Display;
with Allegro5.Joystick;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.Mouse;
with Allegro5.Timer;

package Allegro5.Events is

   type ALLEGRO_USER_EVENT_DESCRIPTOR is new System.Address;

   type ALLEGRO_EVENT_QUEUE is new System.Address;

   -- A macro which evaluates to true if the event type is not a builtin
   --event type, i.e. one of those described in ALLEGRO_EVENT_TYPE.
   function ALLEGRO_EVENT_TYPE_IS_USER (t : unsigned) return Extensions.bool;

   function ALLEGRO_GET_EVENT_TYPE(a : int; b : int; c : int; d : int) return int renames Allegro5.Base.AL_ID;

   subtype ALLEGRO_EVENT_TYPE is unsigned;
   ALLEGRO_EVENT_JOYSTICK_AXIS : constant ALLEGRO_EVENT_TYPE := 1;
   ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN : constant ALLEGRO_EVENT_TYPE := 2;
   ALLEGRO_EVENT_JOYSTICK_BUTTON_UP : constant ALLEGRO_EVENT_TYPE := 3;
   ALLEGRO_EVENT_JOYSTICK_CONFIGURATION : constant ALLEGRO_EVENT_TYPE := 4;
   ALLEGRO_EVENT_KEY_DOWN : constant ALLEGRO_EVENT_TYPE := 10;
   ALLEGRO_EVENT_KEY_CHAR : constant ALLEGRO_EVENT_TYPE := 11;
   ALLEGRO_EVENT_KEY_UP : constant ALLEGRO_EVENT_TYPE := 12;
   ALLEGRO_EVENT_MOUSE_AXES : constant ALLEGRO_EVENT_TYPE := 20;
   ALLEGRO_EVENT_MOUSE_BUTTON_DOWN : constant ALLEGRO_EVENT_TYPE := 21;
   ALLEGRO_EVENT_MOUSE_BUTTON_UP : constant ALLEGRO_EVENT_TYPE := 22;
   ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY : constant ALLEGRO_EVENT_TYPE := 23;
   ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY : constant ALLEGRO_EVENT_TYPE := 24;
   ALLEGRO_EVENT_MOUSE_WARPED : constant ALLEGRO_EVENT_TYPE := 25;
   ALLEGRO_EVENT_TIMER : constant ALLEGRO_EVENT_TYPE := 30;
   ALLEGRO_EVENT_DISPLAY_EXPOSE : constant ALLEGRO_EVENT_TYPE := 40;
   ALLEGRO_EVENT_DISPLAY_RESIZE : constant ALLEGRO_EVENT_TYPE := 41;
   ALLEGRO_EVENT_DISPLAY_CLOSE : constant ALLEGRO_EVENT_TYPE := 42;
   ALLEGRO_EVENT_DISPLAY_LOST : constant ALLEGRO_EVENT_TYPE := 43;
   ALLEGRO_EVENT_DISPLAY_FOUND : constant ALLEGRO_EVENT_TYPE := 44;
   ALLEGRO_EVENT_DISPLAY_SWITCH_IN : constant ALLEGRO_EVENT_TYPE := 45;
   ALLEGRO_EVENT_DISPLAY_SWITCH_OUT : constant ALLEGRO_EVENT_TYPE := 46;
   ALLEGRO_EVENT_DISPLAY_ORIENTATION : constant ALLEGRO_EVENT_TYPE := 47;

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
      source : Display.ALLEGRO_DISPLAY;
      timestamp : aliased double;
      x : aliased int;
      y : aliased int;
      width : aliased int;
      height : aliased int;
      orientation : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_DISPLAY_EVENT);

   type ALLEGRO_JOYSTICK_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : Joystick.ALLEGRO_JOYSTICK;
      timestamp : aliased double;
      id : Joystick.ALLEGRO_JOYSTICK;
      stick : aliased int;
      axis : aliased int;
      pos : aliased float;
      button : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_JOYSTICK_EVENT);

   type ALLEGRO_KEYBOARD_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : Keyboard.ALLEGRO_KEYBOARD;
      timestamp : aliased double;
      display : Allegro5.Display.ALLEGRO_DISPLAY;
      keycode : aliased Keycodes.ALLEGRO_KEYCODE;
      unichar : aliased int;
      modifiers : aliased Keycodes.ALLEGRO_KEYMOD;
      repeat : aliased Extensions.bool;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_KEYBOARD_EVENT);

   subtype ALLEGRO_KEYBOARD is Extensions.opaque_structure_def;

   type ALLEGRO_MOUSE_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : Mouse.ALLEGRO_MOUSE;
      timestamp : aliased double;
      display : Allegro5.Display.ALLEGRO_DISPLAY;
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
      source : Timer.ALLEGRO_TIMER;
      timestamp : aliased double;
      count : aliased Integer_64;
      error : aliased double;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TIMER_EVENT);

   type ALLEGRO_USER_EVENT is record
      c_type : aliased ALLEGRO_EVENT_TYPE;
      source : access ALLEGRO_EVENT_SOURCE;
      timestamp : aliased double;
      uu_internal_u_descr : ALLEGRO_USER_EVENT_DESCRIPTOR;
      data1 : aliased stdint.intptr_t;
      data2 : aliased stdint.intptr_t;
      data3 : aliased stdint.intptr_t;
      data4 : aliased stdint.intptr_t;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_USER_EVENT);

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

   function al_get_event_source_data (arg1 : ALLEGRO_EVENT_SOURCE) return stdint.intptr_t;
   pragma Import (C, al_get_event_source_data, "al_get_event_source_data");

   function al_create_event_queue return ALLEGRO_EVENT_QUEUE;
   pragma Import (C, al_create_event_queue, "al_create_event_queue");

   procedure al_destroy_event_queue (queue : ALLEGRO_EVENT_QUEUE);
   pragma Import (C, al_destroy_event_queue, "al_destroy_event_queue");

   procedure al_register_event_source (queue : ALLEGRO_EVENT_QUEUE; arg2 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_register_event_source, "al_register_event_source");

   procedure al_unregister_event_source (queue : ALLEGRO_EVENT_QUEUE; arg2 : access ALLEGRO_EVENT_SOURCE);
   pragma Import (C, al_unregister_event_source, "al_unregister_event_source");

   function al_is_event_queue_empty (queue : ALLEGRO_EVENT_QUEUE) return Extensions.bool;
   pragma Import (C, al_is_event_queue_empty, "al_is_event_queue_empty");

   function al_get_next_event (queue : ALLEGRO_EVENT_QUEUE; ret_event : access ALLEGRO_EVENT) return Extensions.bool;
   pragma Import (C, al_get_next_event, "al_get_next_event");

   function al_peek_next_event (queue : ALLEGRO_EVENT_QUEUE; ret_event : access ALLEGRO_EVENT) return Extensions.bool;
   pragma Import (C, al_peek_next_event, "al_peek_next_event");

   function al_drop_next_event (queue : ALLEGRO_EVENT_QUEUE) return Extensions.bool;
   pragma Import (C, al_drop_next_event, "al_drop_next_event");

   procedure al_flush_event_queue (queue : ALLEGRO_EVENT_QUEUE);
   pragma Import (C, al_flush_event_queue, "al_flush_event_queue");

   procedure al_wait_for_event (queue : ALLEGRO_EVENT_QUEUE; ret_event : access ALLEGRO_EVENT);
   pragma Import (C, al_wait_for_event, "al_wait_for_event");

   function al_wait_for_event_timed
     (queue : ALLEGRO_EVENT_QUEUE;
      ret_event : access ALLEGRO_EVENT;
      secs : float) return Extensions.bool;
   pragma Import (C, al_wait_for_event_timed, "al_wait_for_event_timed");

   function al_wait_for_event_until
     (queue : ALLEGRO_EVENT_QUEUE;
      ret_event : access ALLEGRO_EVENT;
      timeout : access Allegro5.Altime.ALLEGRO_TIMEOUT) return Extensions.bool;
   pragma Import (C, al_wait_for_event_until, "al_wait_for_event_until");

end Allegro5.Events;
