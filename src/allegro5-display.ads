with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
limited with Allegro5.Events;
with Interfaces.C.Strings;

package Allegro5.Display is

   subtype ALLEGRO_DISPLAY is System.Address;

   type ALLEGRO_DISPLAY_OPTIONS is
     (ALLEGRO_RED_SIZE,
      ALLEGRO_GREEN_SIZE,
      ALLEGRO_BLUE_SIZE,
      ALLEGRO_ALPHA_SIZE,
      ALLEGRO_RED_SHIFT,
      ALLEGRO_GREEN_SHIFT,
      ALLEGRO_BLUE_SHIFT,
      ALLEGRO_ALPHA_SHIFT,
      ALLEGRO_ACC_RED_SIZE,
      ALLEGRO_ACC_GREEN_SIZE,
      ALLEGRO_ACC_BLUE_SIZE,
      ALLEGRO_ACC_ALPHA_SIZE,
      ALLEGRO_STEREO,
      ALLEGRO_AUX_BUFFERS,
      ALLEGRO_COLOR_SIZE,
      ALLEGRO_DEPTH_SIZE,
      ALLEGRO_STENCIL_SIZE,
      ALLEGRO_SAMPLE_BUFFERS,
      ALLEGRO_SAMPLES,
      ALLEGRO_RENDER_METHOD,
      ALLEGRO_FLOAT_COLOR,
      ALLEGRO_FLOAT_DEPTH,
      ALLEGRO_SINGLE_BUFFER,
      ALLEGRO_SWAP_METHOD,
      ALLEGRO_COMPATIBLE_DISPLAY,
      ALLEGRO_UPDATE_DISPLAY_REGION,
      ALLEGRO_VSYNC,
      ALLEGRO_MAX_BITMAP_SIZE,
      ALLEGRO_SUPPORT_NPOT_BITMAP,
      ALLEGRO_CAN_DRAW_INTO_BITMAP,
      ALLEGRO_SUPPORT_SEPARATE_ALPHA,
      ALLEGRO_DISPLAY_OPTIONS_COUNT);
   pragma Convention (C, ALLEGRO_DISPLAY_OPTIONS);

   type ALLEGRO_DISPLAY_ORIENTATION is
     (ALLEGRO_DISPLAY_ORIENTATION_0_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_90_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_180_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_270_DEGREES,
      ALLEGRO_DISPLAY_ORIENTATION_FACE_UP,
      ALLEGRO_DISPLAY_ORIENTATION_FACE_DOWN);
   pragma Convention (C, ALLEGRO_DISPLAY_ORIENTATION);

   procedure al_set_new_display_refresh_rate (refresh_rate : int);
   pragma Import (C, al_set_new_display_refresh_rate, "al_set_new_display_refresh_rate");

   procedure al_set_new_display_flags (flags : int);
   pragma Import (C, al_set_new_display_flags, "al_set_new_display_flags");

   function al_get_new_display_refresh_rate return int;
   pragma Import (C, al_get_new_display_refresh_rate, "al_get_new_display_refresh_rate");

   function al_get_new_display_flags return int;
   pragma Import (C, al_get_new_display_flags, "al_get_new_display_flags");

   function al_get_display_width (display : System.Address) return int;
   pragma Import (C, al_get_display_width, "al_get_display_width");

   function al_get_display_height (display : System.Address) return int;
   pragma Import (C, al_get_display_height, "al_get_display_height");

   function al_get_display_format (display : System.Address) return int;
   pragma Import (C, al_get_display_format, "al_get_display_format");

   function al_get_display_refresh_rate (display : System.Address) return int;
   pragma Import (C, al_get_display_refresh_rate, "al_get_display_refresh_rate");

   function al_get_display_flags (display : System.Address) return int;
   pragma Import (C, al_get_display_flags, "al_get_display_flags");

   function al_set_display_flag
     (display : System.Address;
      flag : int;
      onoff : Extensions.bool) return Extensions.bool;
   pragma Import (C, al_set_display_flag, "al_set_display_flag");

   function al_toggle_display_flag
     (display : System.Address;
      flag : int;
      onoff : Extensions.bool) return Extensions.bool;
   pragma Import (C, al_toggle_display_flag, "al_toggle_display_flag");

   function al_create_display (w : int; h : int) return System.Address;
   pragma Import (C, al_create_display, "al_create_display");

   procedure al_destroy_display (display : System.Address);
   pragma Import (C, al_destroy_display, "al_destroy_display");

   function al_get_current_display return System.Address;
   pragma Import (C, al_get_current_display, "al_get_current_display");

   procedure al_set_target_bitmap (bitmap : System.Address);
   pragma Import (C, al_set_target_bitmap, "al_set_target_bitmap");

   procedure al_set_target_backbuffer (display : System.Address);
   pragma Import (C, al_set_target_backbuffer, "al_set_target_backbuffer");

   function al_get_backbuffer (display : System.Address) return System.Address;
   pragma Import (C, al_get_backbuffer, "al_get_backbuffer");

   function al_get_target_bitmap return System.Address;
   pragma Import (C, al_get_target_bitmap, "al_get_target_bitmap");

   function al_acknowledge_resize (display : System.Address) return Extensions.bool;
   pragma Import (C, al_acknowledge_resize, "al_acknowledge_resize");

   function al_resize_display
     (display : System.Address;
      width : int;
      height : int) return Extensions.bool;
   pragma Import (C, al_resize_display, "al_resize_display");

   procedure al_flip_display;
   pragma Import (C, al_flip_display, "al_flip_display");

   procedure al_update_display_region
     (x : int;
      y : int;
      width : int;
      height : int);
   pragma Import (C, al_update_display_region, "al_update_display_region");

   function al_is_compatible_bitmap (bitmap : System.Address) return Extensions.bool;
   pragma Import (C, al_is_compatible_bitmap, "al_is_compatible_bitmap");

   function al_wait_for_vsync return Extensions.bool;
   pragma Import (C, al_wait_for_vsync, "al_wait_for_vsync");

   function al_get_display_event_source (display : System.Address) return access Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import (C, al_get_display_event_source, "al_get_display_event_source");

   procedure al_set_display_icon (display : System.Address; icon : System.Address);
   pragma Import (C, al_set_display_icon, "al_set_display_icon");

   procedure al_set_display_icons
     (display : System.Address;
      num_icons : int;
      icons : System.Address);
   pragma Import (C, al_set_display_icons, "al_set_display_icons");

   function al_get_new_display_adapter return int;
   pragma Import (C, al_get_new_display_adapter, "al_get_new_display_adapter");

   procedure al_set_new_display_adapter (adapter : int);
   pragma Import (C, al_set_new_display_adapter, "al_set_new_display_adapter");

   procedure al_set_new_window_position (x : int; y : int);
   pragma Import (C, al_set_new_window_position, "al_set_new_window_position");

   procedure al_get_new_window_position (x : access int; y : access int);
   pragma Import (C, al_get_new_window_position, "al_get_new_window_position");

   procedure al_set_window_position
     (display : System.Address;
      x : int;
      y : int);
   pragma Import (C, al_set_window_position, "al_set_window_position");

   procedure al_get_window_position
     (display : System.Address;
      x : access int;
      y : access int);
   pragma Import (C, al_get_window_position, "al_get_window_position");

   procedure al_set_window_title (display : System.Address; title : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_window_title, "al_set_window_title");

   procedure al_set_new_display_option
     (option : ALLEGRO_DISPLAY_OPTIONS;
      value : int;
      importance : int);
   pragma Import (C, al_set_new_display_option, "al_set_new_display_option");

   function al_get_new_display_option (option : int; importance : access int) return int;
   pragma Import (C, al_get_new_display_option, "al_get_new_display_option");

   procedure al_reset_new_display_options;
   pragma Import (C, al_reset_new_display_options, "al_reset_new_display_options");

   function al_get_display_option (display : System.Address; option : ALLEGRO_DISPLAY_OPTIONS) return int;
   pragma Import (C, al_get_display_option, "al_get_display_option");

   procedure al_hold_bitmap_drawing (hold : Extensions.bool);
   pragma Import (C, al_hold_bitmap_drawing, "al_hold_bitmap_drawing");

   function al_is_bitmap_drawing_held return Extensions.bool;
   pragma Import (C, al_is_bitmap_drawing_held, "al_is_bitmap_drawing_held");

end Allegro5.Display;
