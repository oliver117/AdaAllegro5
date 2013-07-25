with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.Bitmap;
with Allegro5.File;

use Allegro5;

package Allegro5.Bitmap_IO is

   type ALLEGRO_IIO_LOADER_FUNCTION is access function (arg1 : Strings.chars_ptr) return Bitmap.ALLEGRO_BITMAP;

   type ALLEGRO_IIO_FS_LOADER_FUNCTION is access function (arg1 : File.ALLEGRO_FILE) return Bitmap.ALLEGRO_BITMAP;

   type ALLEGRO_IIO_SAVER_FUNCTION is access function (arg1 : Strings.chars_ptr; arg2 : Bitmap.ALLEGRO_BITMAP) return Extensions.bool;

   type ALLEGRO_IIO_FS_SAVER_FUNCTION is access function (arg1 : File.ALLEGRO_FILE; arg2 : Bitmap.ALLEGRO_BITMAP) return Extensions.bool;

   function al_register_bitmap_loader (ext : Strings.chars_ptr; loader : access function (arg1 : Strings.chars_ptr) return Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_register_bitmap_loader, "al_register_bitmap_loader");

   function al_register_bitmap_saver (ext : Strings.chars_ptr; saver : access function (arg1 : Strings.chars_ptr; arg2 : Bitmap.ALLEGRO_BITMAP) return Extensions.bool) return Extensions.bool;
   pragma Import (C, al_register_bitmap_saver, "al_register_bitmap_saver");

   function al_register_bitmap_loader_f (ext : Strings.chars_ptr; fs_loader : access function (arg1 : File.ALLEGRO_FILE) return Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_register_bitmap_loader_f, "al_register_bitmap_loader_f");

   function al_register_bitmap_saver_f (ext : Strings.chars_ptr; fs_saver : access function (arg1 : File.ALLEGRO_FILE; arg2 :Bitmap.ALLEGRO_BITMAP) return Extensions.bool) return Extensions.bool;
   pragma Import (C, al_register_bitmap_saver_f, "al_register_bitmap_saver_f");

   function al_load_bitmap (filename : Strings.chars_ptr) return Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_load_bitmap, "al_load_bitmap");

   function al_load_bitmap_f (fp : File.ALLEGRO_FILE; ident : Strings.chars_ptr) return Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_load_bitmap_f, "al_load_bitmap_f");

   function al_save_bitmap (filename : Strings.chars_ptr; bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_save_bitmap, "al_save_bitmap");

   function al_save_bitmap_f
     (fp : File.ALLEGRO_FILE;
      ident : Strings.chars_ptr;
      bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_save_bitmap_f, "al_save_bitmap_f");

end Allegro5.Bitmap_IO;
