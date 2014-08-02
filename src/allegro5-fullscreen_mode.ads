with Interfaces.C; use Interfaces.C;

package Allegro5.Fullscreen_Mode is

   -- Used for fullscreen mode queries. Contains information about a
   --supported fullscreen modes. The refresh_rate may be zero if unknown.
   type ALLEGRO_DISPLAY_MODE is record
      width        : aliased int;
      height       : aliased int;
      format       : aliased int;
      refresh_rate : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_DISPLAY_MODE);

   -- Get the number of available fullscreen display modes for the current
   --set of display parameters. This will use the values set with
   --al_set_new_display_refresh_rate, and al_set_new_display_flags to find
   --the number of modes that match. Settings the new display parameters to
   --zero will give a list of all modes for the default driver.
   function al_get_num_display_modes return int;
   pragma Import (C, al_get_num_display_modes, "al_get_num_display_modes");

   -- Retrieves a fullscreen mode. Display parameters should not be changed
   --between a call of al_get_num_display_modes and al_get_display_mode.
   --Index must be between 0 and the number returned from
   --al_get_num_display_modes-1. mode must be an allocated
   --ALLEGRO_DISPLAY_MODE structure. This function will return NULL on failure,
   --and the mode parameter that was passed in on success.
   function al_get_display_mode
     (index : int;
      mode  : access ALLEGRO_DISPLAY_MODE) return access ALLEGRO_DISPLAY_MODE;
   pragma Import (C, al_get_display_mode, "al_get_display_mode");

end Allegro5.Fullscreen_Mode;
