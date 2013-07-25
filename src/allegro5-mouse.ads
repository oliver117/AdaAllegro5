with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
limited with Allegro5.Events;

package Allegro5.Mouse is

   ALLEGRO_MOUSE_MAX_EXTRA_AXES : constant := 4;

   type ALLEGRO_MOUSE_STATE_more_axes_array is array (0 .. 3) of aliased int;
   type ALLEGRO_MOUSE_STATE is record
      x : aliased int;
      y : aliased int;
      z : aliased int;
      w : aliased int;
      more_axes : aliased ALLEGRO_MOUSE_STATE_more_axes_array;
      buttons : aliased int;
      pressure : aliased float;
      display : System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MOUSE_STATE);

   function al_is_mouse_installed return Extensions.bool;
   pragma Import (C, al_is_mouse_installed, "al_is_mouse_installed");

   function al_install_mouse return Extensions.bool;
   pragma Import (C, al_install_mouse, "al_install_mouse");

   procedure al_uninstall_mouse;
   pragma Import (C, al_uninstall_mouse, "al_uninstall_mouse");

   function al_get_mouse_num_buttons return unsigned;
   pragma Import (C, al_get_mouse_num_buttons, "al_get_mouse_num_buttons");

   function al_get_mouse_num_axes return unsigned;
   pragma Import (C, al_get_mouse_num_axes, "al_get_mouse_num_axes");

   function al_set_mouse_xy
     (display : System.Address;
      x : int;
      y : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_xy, "al_set_mouse_xy");

   function al_set_mouse_z (z : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_z, "al_set_mouse_z");

   function al_set_mouse_w (w : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_w, "al_set_mouse_w");

   function al_set_mouse_axis (axis : int; value : int) return Extensions.bool;
   pragma Import (C, al_set_mouse_axis, "al_set_mouse_axis");

   procedure al_get_mouse_state (ret_state : access ALLEGRO_MOUSE_STATE);
   pragma Import (C, al_get_mouse_state, "al_get_mouse_state");

   function al_mouse_button_down (state : System.Address; button : int) return Extensions.bool;
   pragma Import (C, al_mouse_button_down, "al_mouse_button_down");

   function al_get_mouse_state_axis (state : System.Address; axis : int) return int;
   pragma Import (C, al_get_mouse_state_axis, "al_get_mouse_state_axis");

   function al_get_mouse_cursor_position (ret_x : access int; ret_y : access int) return Extensions.bool;
   pragma Import (C, al_get_mouse_cursor_position, "al_get_mouse_cursor_position");

   function al_grab_mouse (display : System.Address) return Extensions.bool;
   pragma Import (C, al_grab_mouse, "al_grab_mouse");

   function al_ungrab_mouse return Extensions.bool;
   pragma Import (C, al_ungrab_mouse, "al_ungrab_mouse");

   function al_get_mouse_event_source return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_mouse_event_source, "al_get_mouse_event_source");

end Allegro5.Mouse;
