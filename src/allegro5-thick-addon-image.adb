with Allegro5.Addon.Image;

with Interfaces.C;

package body Allegro5.Thick.Addon.Image is

   package A5AI renames Allegro5.Addon.Image;

   ----------------------
   -- Init_Image_Addon --
   ----------------------

   function Init_Image_Addon
      return Boolean
   is
      use type Interfaces.C.unsigned_char;
   begin
      return 0 /= A5AI.al_init_image_addon;
   end Init_Image_Addon;

   --------------------------
   -- Shutdown_Image_Addon --
   --------------------------

   procedure Shutdown_Image_Addon
   is
   begin
      A5AI.al_shutdown_image_addon;
   end Shutdown_Image_Addon;

   -------------------------------
   -- Get_Allegro_Image_Version --
   -------------------------------

   function Get_Allegro_Image_Version
      return Unsigned_32
   is
   begin
      return A5AI.al_get_allegro_image_version;
   end Get_Allegro_Image_Version;

end Allegro5.Thick.Addon.Image;
