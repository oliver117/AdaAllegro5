with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.Display;
limited with Allegro5.Events;
with Allegro5.Keycodes;


package Allegro5.Keyboard is

   type ALLEGRO_KEYBOARD is new System.Address;

   type ALLEGRO_KEYBOARD_STATE_uu_key_down_u_internal_u_u_array is array (0 .. 7) of aliased unsigned;
   type ALLEGRO_KEYBOARD_STATE is record
      display : Allegro5.Display.ALLEGRO_DISPLAY;
      uu_key_down_u_internal_u_u : aliased ALLEGRO_KEYBOARD_STATE_uu_key_down_u_internal_u_u_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_KEYBOARD_STATE);

   function al_is_keyboard_installed return Extensions.bool;
   pragma Import (C, al_is_keyboard_installed, "al_is_keyboard_installed");

   function al_install_keyboard return Extensions.bool;
   pragma Import (C, al_install_keyboard, "al_install_keyboard");

   procedure al_uninstall_keyboard;
   pragma Import (C, al_uninstall_keyboard, "al_uninstall_keyboard");

   function al_set_keyboard_leds (leds : int) return Extensions.bool;
   pragma Import (C, al_set_keyboard_leds, "al_set_keyboard_leds");

   function al_keycode_to_name (keycode : Keycodes.ALLEGRO_KEYCODE) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_keycode_to_name, "al_keycode_to_name");

   procedure al_get_keyboard_state (ret_state : access ALLEGRO_KEYBOARD_STATE);
   pragma Import (C, al_get_keyboard_state, "al_get_keyboard_state");

   function al_key_down (arg1 : ALLEGRO_KEYBOARD_STATE; keycode : Keycodes.ALLEGRO_KEYCODE) return Extensions.bool;
   pragma Import (C, al_key_down, "al_key_down");

   function al_get_keyboard_event_source return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_keyboard_event_source, "al_get_keyboard_event_source");

end Allegro5.Keyboard;
