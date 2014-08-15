with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.Display;
limited with Allegro5.Events;
with Allegro5.Keycodes;

package Allegro5.Keyboard is

   -- TODO: remove this?
   type ALLEGRO_KEYBOARD is new Extensions.opaque_structure_def;

   type ALLEGRO_KEYBOARD_STATE_uu_key_down_u_internal_u_u_array is
     array (0 .. 7) of aliased unsigned;

   -- This is a structure that is used to hold a "snapshot" of a keyboard's
   --state at a particular instant. It contains the following
   --publically readable fields:

   -- * display - points to the display that had keyboard focus at the
   --time the state was saved. If no display was focused, this points to NULL.

   -- You cannot read the state of keys directly. Use the function al_key_down.
   type ALLEGRO_KEYBOARD_STATE is record
      display                    : Allegro5.Display.ALLEGRO_DISPLAY;
      uu_key_down_u_internal_u_u : aliased ALLEGRO_KEYBOARD_STATE_uu_key_down_u_internal_u_u_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_KEYBOARD_STATE);

   -- Returns true if al_install_keyboard was called successfully.
   function al_is_keyboard_installed return Extensions.bool;
   pragma Import (C, al_is_keyboard_installed, "al_is_keyboard_installed");

   -- Install a keyboard driver. Returns true if successful. If a driver was
   --already installed, nothing happens and true is returned.
   function al_install_keyboard return Extensions.bool;
   pragma Import (C, al_install_keyboard, "al_install_keyboard");

   -- Uninstalls the active keyboard driver, if any. This will automatically
   --unregister the keyboard event source with any event queues.

   -- This function is automatically called when Allegro is shut down.
   procedure al_uninstall_keyboard;
   pragma Import (C, al_uninstall_keyboard, "al_uninstall_keyboard");

   -- Overrides the state of the keyboard LED indicators. Set to -1 to return
   --to default behavior. False is returned if the current keyboard driver
   --cannot set LED indicators.
   function al_set_keyboard_leds (leds : int) return Extensions.bool;
   pragma Import (C, al_set_keyboard_leds, "al_set_keyboard_leds");

   -- Converts the given keycode to a description of the key.
   function al_keycode_to_name
     (keycode : Keycodes.ALLEGRO_KEYCODE)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_keycode_to_name, "al_keycode_to_name");

   -- Save the state of the keyboard specified at the time the function is
   --called into the structure pointed to by ret_state.
   procedure al_get_keyboard_state (ret_state : access ALLEGRO_KEYBOARD_STATE);
   pragma Import (C, al_get_keyboard_state, "al_get_keyboard_state");

   -- Return true if the key specified was held down in the state specified.
   function al_key_down
     (state   : ALLEGRO_KEYBOARD_STATE;
      keycode : Keycodes.ALLEGRO_KEYCODE) return Extensions.bool;
   pragma Import (C, al_key_down, "al_key_down");

   -- Retrieve the keyboard event source.

   -- Returns NULL if the keyboard subsystem was not installed.
   function al_get_keyboard_event_source
     return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import
     (C,
      al_get_keyboard_event_source,
      "al_get_keyboard_event_source");

end Allegro5.Keyboard;
