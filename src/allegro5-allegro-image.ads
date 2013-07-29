with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with stdint;

package Allegro5.Allegro.Image is

   -- Initializes the image addon. This registers bitmap format handlers for
   --al_load_bitmap, al_load_bitmap_f, al_save_bitmap, al_save_bitmap_f.
   --
   -- The following types are built into the Allegro image addon and
   --guaranteed to be available: BMP, PCX, TGA. Every platform also supports
   --JPEG and PNG via external dependencies.
   --
   -- Other formats may be available depending on the operating system and
   --installed libraries, but are not guaranteed and should not be assumed to
   --be universally available.
   function al_init_image_addon return  Extensions.bool;
   pragma Import (C, al_init_image_addon, "al_init_image_addon");

   -- Shut down the image addon. This is done automatically at program exit,
   --but can be called any time the user wishes as well.
   procedure al_shutdown_image_addon;
   pragma Import (C, al_shutdown_image_addon, "al_shutdown_image_addon");

   -- Returns the (compiled) version of the addon, in the same format as
   --al_get_allegro_version.
   function al_get_allegro_image_version return  stdint.uint32_t;
   pragma Import
     (C,
      al_get_allegro_image_version,
      "al_get_allegro_image_version");

end Allegro5.Allegro.Image;
