with Interfaces.C; use Interfaces.C;

package Allegro5.Fullscreen_Mode is

   type ALLEGRO_DISPLAY_MODE is record
      width : aliased int;
      height : aliased int;
      format : aliased int;
      refresh_rate : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_DISPLAY_MODE);

   function al_get_num_display_modes return int;
   pragma Import (C, al_get_num_display_modes, "al_get_num_display_modes");

   function al_get_display_mode (index : int; mode : access ALLEGRO_DISPLAY_MODE) return access ALLEGRO_DISPLAY_MODE;
   pragma Import (C, al_get_display_mode, "al_get_display_mode");

end Allegro5.Fullscreen_Mode;
