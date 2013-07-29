with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with stdint;


package Allegro5.Allegro.Image is

   function al_init_image_addon return Extensions.bool;
   pragma Import (C, al_init_image_addon, "al_init_image_addon");

   procedure al_shutdown_image_addon;
   pragma Import (C, al_shutdown_image_addon, "al_shutdown_image_addon");

   function al_get_allegro_image_version return stdint.uint32_t;
   pragma Import (C, al_get_allegro_image_version, "al_get_allegro_image_version");

end Allegro5.Allegro.Image;
