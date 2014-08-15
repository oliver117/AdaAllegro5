with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

with Allegro5.Bitmap;
limited with Allegro5.Events;

package Allegro5.Display is

   -- An opaque type representing an open display or window.
   type ALLEGRO_DISPLAY is new Extensions.opaque_structure_def;

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

   -- Sets the refresh rate to use when creating new displays on
   --the calling thread. If the refresh rate is not available,
   --al_create_display will fail. A list of modes with refresh rates can
   --be found with al_get_num_display_modes and al_get_display_mode.

   -- The default setting is zero (don't care).
   procedure al_set_new_display_refresh_rate (refresh_rate : int);
   pragma Import
     (C,
      al_set_new_display_refresh_rate,
      "al_set_new_display_refresh_rate");

   -- Sets various flags to be used when creating new displays
   --on the calling thread.
   procedure al_set_new_display_flags (flags : int);
   pragma Import (C, al_set_new_display_flags, "al_set_new_display_flags");

   -- Get the requested refresh rate to be used when creating new displays
   --on the calling thread.
   function al_get_new_display_refresh_rate return int;
   pragma Import
     (C,
      al_get_new_display_refresh_rate,
      "al_get_new_display_refresh_rate");

   -- Get the display flags to be used when creating
   --new displays on the calling thread.
   function al_get_new_display_flags return int;
   pragma Import (C, al_get_new_display_flags, "al_get_new_display_flags");

   -- Gets the width of the display. This is like SCREEN_W in Allegro 4.x.
   function al_get_display_width (display : ALLEGRO_DISPLAY) return int;
   pragma Import (C, al_get_display_width, "al_get_display_width");

   -- Gets the height of the display. This is like SCREEN_H in Allegro 4.x.
   function al_get_display_height (display : ALLEGRO_DISPLAY) return int;
   pragma Import (C, al_get_display_height, "al_get_display_height");

   -- Gets the pixel format of the display.
   function al_get_display_format (display : ALLEGRO_DISPLAY) return int;
   pragma Import (C, al_get_display_format, "al_get_display_format");

   -- Gets the refresh rate of the display.
   function al_get_display_refresh_rate (display : ALLEGRO_DISPLAY) return int;
   pragma Import
     (C,
      al_get_display_refresh_rate,
      "al_get_display_refresh_rate");

   -- Gets the flags of the display.

   -- In addition to the flags set for the display at creation time
   --with al_set_new_display_flags it can also have the ALLEGRO_MINIMIZED flag
   --set, indicating that the window is currently minimized. This flag is
   --very platform-dependent as even a minimized application may still render
   --a preview version so normally you should not care whether it
   --is minimized or not.
   function al_get_display_flags (display : ALLEGRO_DISPLAY) return int;
   pragma Import (C, al_get_display_flags, "al_get_display_flags");

   -- Enable or disable one of the display flags. The flags are the same as
   --for al_set_new_display_flags. The only flags that can be changed
   --after creation are:

   -- * ALLEGRO_FULLSCREEN_WINDOW
   -- * ALLEGRO_FRAMELESS

   -- Returns true if the driver supports toggling the specified
   --flag else false. You can use al_get_display_flags to query whether
   --the given display property actually changed.
   function al_set_display_flag
     (display : ALLEGRO_DISPLAY;
      flag    : int;
      onoff   : Extensions.bool) return Extensions.bool;
   pragma Import (C, al_set_display_flag, "al_set_display_flag");

   -- Deprecated synonym for al_set_display_flag.
   function al_toggle_display_flag
     (display : ALLEGRO_DISPLAY;
      flag    : int;
      onoff   : Extensions.bool) return Extensions.bool;
   pragma Import (C, al_toggle_display_flag, "al_toggle_display_flag");

   -- Create a display, or window, with the specified dimensions.
   --The parameters of the display are determined by the last calls
   --to al_set_new_display_*. Default parameters are used if none are
   --set explicitly. Creating a new display will automatically make it the
   --active one, with the backbuffer selected for drawing.

   -- Returns NULL on error.

   -- Each display has a distinct OpenGL rendering context associated with it.
   --See al_set_target_bitmap for the discussion about rendering contexts.
   function al_create_display (w : int; h : int) return ALLEGRO_DISPLAY;
   pragma Import (C, al_create_display, "al_create_display");

   -- Destroy a display.

   -- If the target bitmap of the calling thread is tied to the display,
   --then it implies a call to "al_set_target_bitmap(NULL);" before the
   --display is destroyed.

   -- That special case notwithstanding, you should make sure no threads
   --are currently targeting a bitmap which is tied to the display before
   --you destroy it.
   procedure al_destroy_display (display : ALLEGRO_DISPLAY);
   pragma Import (C, al_destroy_display, "al_destroy_display");

   function al_get_current_display return ALLEGRO_DISPLAY;
   pragma Import (C, al_get_current_display, "al_get_current_display");

   -- This function selects the bitmap to which all subsequent drawing
   --operations in the calling thread will draw to. To return to drawing
   --to a display, set the backbuffer of the display as the target bitmap,
   --using al_get_backbuffer. As a convenience, you may also
   --use al_set_target_backbuffer.
   procedure al_set_target_bitmap (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP);
   pragma Import (C, al_set_target_bitmap, "al_set_target_bitmap");

   -- Same as al_set_target_bitmap(al_get_backbuffer(display));
   procedure al_set_target_backbuffer (display : ALLEGRO_DISPLAY);
   pragma Import (C, al_set_target_backbuffer, "al_set_target_backbuffer");

   -- Return a special bitmap representing the back-buffer of the display.

   -- Care should be taken when using the backbuffer bitmap (and its
   --sub-bitmaps) as the source bitmap (e.g as the bitmap argument
--to al_draw_bitmap). Only untransformed operations are hardware accelerated.
   --This consists of al_draw_bitmap and al_draw_bitmap_region when the
   --current transformation is the identity. If the tranformation is
   --not the identity, or some other drawing operation is used, the call will
   --be routed through the memory bitmap routines, which are slow. If you need
   --those operations to be accelerated, then first copy a region of the
   --backbuffer into a temporary bitmap (via the al_draw_bitmap and
   --al_draw_bitmap_region), and then use that temporary bitmap as
   --the source bitmap.
   function al_get_backbuffer
     (display : ALLEGRO_DISPLAY) return Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_get_backbuffer, "al_get_backbuffer");

   function al_get_target_bitmap return Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_get_target_bitmap, "al_get_target_bitmap");

   -- When the user receives a resize event from a resizable display, if they
   --wish the display to be resized they must call this function to let the
   --graphics driver know that it can now resize the display.
   --Returns true on success.

   -- Adjusts the clipping rectangle to the full size of the backbuffer.

   -- Note that a resize event may be outdated by the time you acknowledge it;
   --there could be further resize events generated in the meantime.
   function al_acknowledge_resize
     (display : ALLEGRO_DISPLAY) return Extensions.bool;
   pragma Import (C, al_acknowledge_resize, "al_acknowledge_resize");

   -- Resize the display. Returns true on success, or false on error.
   --This works on both fullscreen and windowed displays, regardless of
   --the ALLEGRO_RESIZABLE flag.

   -- Adjusts the clipping rectangle to the full size of the backbuffer.
   function al_resize_display
     (display : ALLEGRO_DISPLAY;
      width   : int;
      height  : int) return Extensions.bool;
   pragma Import (C, al_resize_display, "al_resize_display");

   -- Copies or updates the front and back buffers so that what has been
   --drawn previously on the currently selected display becomes visible
   --on screen. Pointers to the special back buffer bitmap remain valid and
   --retain their semantics as the back buffer, although the contents may
   --have changed.
   procedure al_flip_display;
   pragma Import (C, al_flip_display, "al_flip_display");

   -- Does the same as al_flip_display, but tries to update only the
   --specified region. With many drivers this is not possible, but for
   --some it can improve performance.

   -- The ALLEGRO_UPDATE_DISPLAY_REGION option (see al_get_display_option)
   --will specify the behavior of this function in the display.
   procedure al_update_display_region
     (x      : int;
      y      : int;
      width  : int;
      height : int);
   pragma Import (C, al_update_display_region, "al_update_display_region");

   function al_is_compatible_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_is_compatible_bitmap, "al_is_compatible_bitmap");

   -- Wait for the beginning of a vertical retrace. Some driver/card/monitor
   --combinations may not be capable of this.

   -- Note how al_flip_display usually already waits for the vertical
   --retrace, so unless you are doing something special, there is no reason
   --to call this function.
   function al_wait_for_vsync return Extensions.bool;
   pragma Import (C, al_wait_for_vsync, "al_wait_for_vsync");

   -- Retrieve the associated event source.
   function al_get_display_event_source
     (display : ALLEGRO_DISPLAY) return access Events.ALLEGRO_EVENT_SOURCE;
   pragma Import
     (C,
      al_get_display_event_source,
      "al_get_display_event_source");

   -- Changes the icon associated with the display (window). Same as
   --al_set_display_icons with one icon.
   procedure al_set_display_icon
     (display : ALLEGRO_DISPLAY;
      icon    : Bitmap.ALLEGRO_BITMAP);
   pragma Import (C, al_set_display_icon, "al_set_display_icon");

   -- Changes the icons associated with the display (window). Multiple icons
   --can be provided for use in different contexts, e.g. window frame, taskbar,
   --alt-tab popup. The number of icons must be at least one.

   -- Note: If the underlying OS requires an icon of a size not provided then
   --one of the bitmaps will be scaled up or down to the required size.
   --The choice of bitmap is implementation dependent.
   procedure al_set_display_icons
     (display   : ALLEGRO_DISPLAY;
      num_icons : int;
      icons     : Bitmap.ALLEGRO_BITMAP);
   pragma Import (C, al_set_display_icons, "al_set_display_icons");

   -- Gets the video adapter index where new displays will be created by the
   --calling thread, if previously set with al_set_new_display_adapter.
   --Otherwise returns ALLEGRO_DEFAULT_DISPLAY_ADAPTER.
   function al_get_new_display_adapter return int;
   pragma Import (C, al_get_new_display_adapter, "al_get_new_display_adapter");

   -- Sets the adapter to use for new displays created by the calling thread.
   --The adapter has a monitor attached to it. Information about the monitor
   --can be gotten using al_get_num_video_adapters and al_get_monitor_info.

   -- To return to the default behaviour, pass ALLEGRO_DEFAULT_DISPLAY_ADAPTER.
   procedure al_set_new_display_adapter (adapter : int);
   pragma Import (C, al_set_new_display_adapter, "al_set_new_display_adapter");

   -- Sets where the top left pixel of the client area of newly created
   --windows (non-fullscreen) will be on screen, for displays created by the
   --calling thread. Negative values allowed on some multihead systems.
   procedure al_set_new_window_position (x : int; y : int);
   pragma Import (C, al_set_new_window_position, "al_set_new_window_position");

   -- Get the position where new non-fullscreen displays created by the
   --calling thread will be placed.
   procedure al_get_new_window_position (x : out int; y : out int);
   pragma Import (C, al_get_new_window_position, "al_get_new_window_position");

   -- Sets the position on screen of a non-fullscreen display.
   procedure al_set_window_position
     (display : ALLEGRO_DISPLAY;
      x       : int;
      y       : int);
   pragma Import (C, al_set_window_position, "al_set_window_position");

   -- Gets the position of a non-fullscreen display.
   procedure al_get_window_position
     (display : ALLEGRO_DISPLAY;
      x       : out int;
      y       : out int);
   pragma Import (C, al_get_window_position, "al_get_window_position");

   -- Set the title on a display.
   procedure al_set_window_title
     (display : ALLEGRO_DISPLAY;
      title   : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_window_title, "al_set_window_title");

   -- Set an extra display option, to be used when creating new displays on
   --the calling thread. Display options differ from display flags, and specify
   --some details of the context to be created within the window itself.
   --These mainly have no effect on Allegro itself, but you may want to specify
   --them, for example if you want to use multisampling.
   procedure al_set_new_display_option
     (option     : ALLEGRO_DISPLAY_OPTIONS;
      value      : int;
      importance : int);
   pragma Import (C, al_set_new_display_option, "al_set_new_display_option");

   -- Retrieve an extra display setting which was previously set
   --with al_set_new_display_option.
   function al_get_new_display_option
     (option     : int;
      importance : out int) return int;
   pragma Import (C, al_get_new_display_option, "al_get_new_display_option");

   -- This undoes any previous call to al_set_new_display_option
   --on the calling thread.
   procedure al_reset_new_display_options;
   pragma Import
     (C,
      al_reset_new_display_options,
      "al_reset_new_display_options");

   -- Return an extra display setting of the display.
   function al_get_display_option
     (display : ALLEGRO_DISPLAY;
      option  : ALLEGRO_DISPLAY_OPTIONS) return int;
   pragma Import (C, al_get_display_option, "al_get_display_option");

   -- Enables or disables deferred bitmap drawing. This allows for efficient
   --drawing of many bitmaps that share a parent bitmap, such as sub-bitmaps
   --from a tilesheet or simply identical bitmaps. Drawing bitmaps that do not
   --share a parent is less efficient, so it is advisable to stagger bitmap
   --drawing calls such that the parent bitmap is the same for large number of
   --those calls. While deferred bitmap drawing is enabled,
   --the only functions that can be used are the bitmap drawing functions
   --and font drawing functions. Changing the state such as the blending modes
   --will result in undefined behaviour. One exception to this rule are the
   --transformations. It is possible to set a new transformation while the
   --drawing is held.

   --No drawing is guaranteed to take place until you disable the hold.
   --Thus, the idiom of this function's usage is to enable the deferred
   --bitmap drawing, draw as many bitmaps as possible, taking care to stagger
   --bitmaps that share parent bitmaps, and then disable deferred drawing.
   --As mentioned above, this function also works with bitmap and truetype
   --fonts, so if multiple lines of text need to be drawn, this function can
   --speed things up.
   procedure al_hold_bitmap_drawing (hold : Extensions.bool);
   pragma Import (C, al_hold_bitmap_drawing, "al_hold_bitmap_drawing");

   -- Returns whether the deferred bitmap drawing mode is turned on or off.
   function al_is_bitmap_drawing_held return Extensions.bool;
   pragma Import (C, al_is_bitmap_drawing_held, "al_is_bitmap_drawing_held");

end Allegro5.Display;
