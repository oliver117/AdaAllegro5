with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;
with Interfaces.C.Strings;
limited with Allegro5.Events;

package Allegro5.Joystick is

   subtype ALLEGRO_JOYSTICK is Extensions.opaque_structure_def;

   type ALLEGRO_JOYSTICK_STATE_axis_array is array (0 .. 2) of aliased float;

   type anon_28 is record
      axis : aliased ALLEGRO_JOYSTICK_STATE_axis_array;
   end record;
   pragma Convention (C, anon_28);

   type ALLEGRO_JOYSTICK_STATE_stick_array is array (0 .. 7) of aliased anon_28;
   type ALLEGRO_JOYSTICK_STATE_button_array is array (0 .. 31) of aliased int;

   type ALLEGRO_JOYSTICK_STATE is record
      stick : aliased ALLEGRO_JOYSTICK_STATE_stick_array;
      button : aliased ALLEGRO_JOYSTICK_STATE_button_array;
   end record;
   pragma Convention (C, ALLEGRO_JOYSTICK_STATE);

   subtype ALLEGRO_JOYFLAGS is unsigned;
   ALLEGRO_JOYFLAG_DIGITAL : constant ALLEGRO_JOYFLAGS := 1;
   ALLEGRO_JOYFLAG_ANALOGUE : constant ALLEGRO_JOYFLAGS := 2;

   function al_install_joystick return Extensions.bool;
   pragma Import (C, al_install_joystick, "al_install_joystick");

   procedure al_uninstall_joystick;
   pragma Import (C, al_uninstall_joystick, "al_uninstall_joystick");

   function al_is_joystick_installed return Extensions.bool;
   pragma Import (C, al_is_joystick_installed, "al_is_joystick_installed");

   function al_reconfigure_joysticks return Extensions.bool;
   pragma Import (C, al_reconfigure_joysticks, "al_reconfigure_joysticks");

   function al_get_num_joysticks return int;
   pragma Import (C, al_get_num_joysticks, "al_get_num_joysticks");

   function al_get_joystick (joyn : int) return ALLEGRO_JOYSTICK;
   pragma Import (C, al_get_joystick, "al_get_joystick");

   procedure al_release_joystick (arg1 : ALLEGRO_JOYSTICK);
   pragma Import (C, al_release_joystick, "al_release_joystick");

   function al_get_joystick_active (arg1 : ALLEGRO_JOYSTICK) return Extensions.bool;
   pragma Import (C, al_get_joystick_active, "al_get_joystick_active");

   function al_get_joystick_name (arg1 : ALLEGRO_JOYSTICK) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_name, "al_get_joystick_name");

   function al_get_joystick_num_sticks (arg1 : System.Address) return int;
   pragma Import (C, al_get_joystick_num_sticks, "al_get_joystick_num_sticks");

   function al_get_joystick_stick_flags (arg1 : System.Address; stick : int) return int;
   pragma Import (C, al_get_joystick_stick_flags, "al_get_joystick_stick_flags");

   function al_get_joystick_stick_name (arg1 : System.Address; stick : int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_stick_name, "al_get_joystick_stick_name");

   function al_get_joystick_num_axes (arg1 : System.Address; stick : int) return int;
   pragma Import (C, al_get_joystick_num_axes, "al_get_joystick_num_axes");

   function al_get_joystick_axis_name
     (arg1 : System.Address;
      stick : int;
      axis : int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_axis_name, "al_get_joystick_axis_name");

   function al_get_joystick_num_buttons (arg1 : System.Address) return int;
   pragma Import (C, al_get_joystick_num_buttons, "al_get_joystick_num_buttons");

   function al_get_joystick_button_name (arg1 : System.Address; buttonn : int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_button_name, "al_get_joystick_button_name");

   procedure al_get_joystick_state (arg1 : ALLEGRO_JOYSTICK; ret_state : ALLEGRO_JOYSTICK_STATE);
   pragma Import (C, al_get_joystick_state, "al_get_joystick_state");

   function al_get_joystick_event_source return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_joystick_event_source, "al_get_joystick_event_source");

end Allegro5.Joystick;
