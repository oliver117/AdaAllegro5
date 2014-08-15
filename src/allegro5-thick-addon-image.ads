with Interfaces; use Interfaces;

package Allegro5.Thick.Addon.Image is

   function Init_Image_Addon return Boolean with
      Inline => True;

   procedure Shutdown_Image_Addon with
      Inline => True;

   function Get_Allegro_Image_Version return Unsigned_32 with
      Inline => True;

end Allegro5.Thick.Addon.Image;
