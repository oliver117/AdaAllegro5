with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Display;
limited with Allegro5.Events;

package Allegro5.Mouse is

   type ALLEGRO_MOUSE is private;

   ALLEGRO_MOUSE_MAX_EXTRA_AXES : constant := 4;

   type ALLEGRO_MOUSE_STATE_more_axes_array is array (0 .. 3) of aliased int;

   -- Public fields (read only):
   --
   -- x - mouse x position
   -- y - mouse y position
   -- w, z - mouse wheel position (2D 'ball')
   -- buttons - mouse buttons bitfield
   -- The zeroth bit is set if the primary mouse button is held down, the
   --first bit is set if the secondary mouse button is held down, and so on.
   type ALLEGRO_MOUSE_STATE is record
      x         : aliased int;
      y         : aliased int;
      z         : aliased int;
      w         : aliased int;
      more_axes : aliased ALLEGRO_MOUSE_STATE_more_axes_array;
      buttons   : aliased int;
      pressure  : aliased Float;
      display   : Allegro5.Display.ALLEGRO_DISPLAY;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MOUSE_STATE);

   -- Returns true if al_install_mouse was called successfully.
   function al_is_mouse_installed return  Extensions.bool;
   pragma Import (C, al_is_mouse_installed, "al_is_mouse_installed");

   -- Install a mouse driver.
   --
   -- Returns true if successful. If a driver was already installed, nothing
   --happens and true is returned.
   function al_install_mouse return  Extensions.bool;
   pragma Import (C, al_install_mouse, "al_install_mouse");

   -- Uninstalls the active mouse driver, if any. This will automatically
   --unregister the mouse event source with any event queues.
   --
   -- This function is automatically called when Allegro is shut down.
   procedure al_uninstall_mouse;
   pragma Import (C, al_uninstall_mouse, "al_uninstall_mouse");

   -- Return the number of buttons on the mouse. The first button is 1.
   function al_get_mouse_num_buttons return unsigned;
   pragma Import (C, al_get_mouse_num_buttons, "al_get_mouse_num_buttons");

   -- Return the number of buttons on the mouse. The first axis is 0.
   function al_get_mouse_num_axes return unsigned;
   pragma Import (C, al_get_mouse_num_axes, "al_get_mouse_num_axes");

   -- Try to position the mouse at the given coordinates on the given display.
   --The mouse movement resulting from a successful move will generate an
   --ALLEGRO_EVENT_MOUSE_WARPED event.
   --
   -- Returns true on success, false on failure.
   function al_set_mouse_xy
     (display : Allegro5.Display.ALLEGRO_DISPLAY;
      x       : int;
      y       : int)
      return    Extensions.bool;
   pragma Import (C, al_set_mouse_xy, "al_set_mouse_xy");

   -- Set the mouse wheel position to the given value.
   --
   -- Returns true on success, false on failure.
   function al_set_mouse_z (z : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_z, "al_set_mouse_z");

   -- Set the second mouse wheel position to the given value.
   --
   -- Returns true on success, false on failure.
   function al_set_mouse_w (w : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_w, "al_set_mouse_w");

   -- Set the given mouse axis to the given value.
   --
   -- The axis number must not be 0 or 1, which are the X and Y axes. Use
   --al_set_mouse_xy for that.
   --
   -- Returns true on success, false on failure.
   function al_set_mouse_axis
     (axis  : int;
      value : int)
      return  Extensions.bool;
   pragma Import (C, al_set_mouse_axis, "al_set_mouse_axis");

   -- Save the state of the mouse specified at the time the function is called
   --into the given structure.
   procedure al_get_mouse_state (ret_state : access ALLEGRO_MOUSE_STATE);
   pragma Import (C, al_get_mouse_state, "al_get_mouse_state");

   -- Return true if the mouse button specified was held down in the state
   --specified. Unlike most things, the first mouse button is numbered 1.
   function al_mouse_button_down
     (state  : ALLEGRO_MOUSE_STATE;
      button : int)
      return   Extensions.bool;
   pragma Import (C, al_mouse_button_down, "al_mouse_button_down");

   -- Extract the mouse axis value from the saved state. The axes are numbered
   --from 0, in this order: x-axis, y-axis, z-axis, w-axis.
   function al_get_mouse_state_axis
     (state : ALLEGRO_MOUSE_STATE;
      axis  : int)
      return  int;
   pragma Import (C, al_get_mouse_state_axis, "al_get_mouse_state_axis");

   -- On platforms where this information is available, this function returns
   --the global location of the mouse cursor, relative to the desktop. You
   --should not normally use this function, as the information is not useful
   --except for special scenarios as moving a window.
   --
   -- Returns true on success, false on failure.
   function al_get_mouse_cursor_position
     (ret_x : access int;
      ret_y : access int)
      return  Extensions.bool;
   pragma Import
     (C,
      al_get_mouse_cursor_position,
      "al_get_mouse_cursor_position");

   -- Confine the mouse cursor to the given display. The mouse cursor can only
   --be confined to one display at a time.
   --
   -- Returns true if successful, otherwise returns false. Do not assume that
   --the cursor will remain confined until you call al_ungrab_mouse. It may
   --lose the confined status at any time for other reasons.
   function al_grab_mouse
     (display : Allegro5.Display.ALLEGRO_DISPLAY)
      return    Extensions.bool;
   pragma Import (C, al_grab_mouse, "al_grab_mouse");

   -- Stop confining the mouse cursor to any display belonging to the program.
   function al_ungrab_mouse return  Extensions.bool;
   pragma Import (C, al_ungrab_mouse, "al_ungrab_mouse");

   -- Retrieve the mouse event source.
   --
   -- Returns NULL if the mouse subsystem was not installed.
   function al_get_mouse_event_source return access
     Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_mouse_event_source, "al_get_mouse_event_source");

private
   type ALLEGRO_MOUSE is new System.Address;

end Allegro5.Mouse;
