with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.Bitmap;
with Allegro5.File;

package Allegro5.Bitmap_IO is

   type ALLEGRO_IIO_LOADER_FUNCTION is access function
     (filename : Interfaces.C.Strings.chars_ptr)
      return Bitmap.ALLEGRO_BITMAP;

   type ALLEGRO_IIO_FS_LOADER_FUNCTION is access function
     (fp : File.ALLEGRO_FILE)
      return Bitmap.ALLEGRO_BITMAP;

   type ALLEGRO_IIO_SAVER_FUNCTION is access function
     (filename : Interfaces.C.Strings.chars_ptr;
      bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return Extensions.bool;

   type ALLEGRO_IIO_FS_SAVER_FUNCTION is access function
     (fp : File.ALLEGRO_FILE;
      bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return Extensions.bool;

   -- Register a handler for al_load_bitmap. The given function will be used
   --to handle the loading of bitmaps files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_bitmap_loader
     (ext    : Interfaces.C.Strings.chars_ptr;
      loader : access function
     (arg1 : Interfaces.C.Strings.chars_ptr)
      return Bitmap.ALLEGRO_BITMAP)
      return   Extensions.bool;
   pragma Import (C, al_register_bitmap_loader, "al_register_bitmap_loader");

   -- Register a handler for al_save_bitmap. The given function will be used
   --to handle the loading of bitmaps files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The saver argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_bitmap_saver
     (ext   : Interfaces.C.Strings.chars_ptr;
      saver : access function
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Bitmap.ALLEGRO_BITMAP)
      return Extensions.bool)
      return  Extensions.bool;
   pragma Import (C, al_register_bitmap_saver, "al_register_bitmap_saver");

   -- Register a handler for al_load_bitmap_f. The given function will be used
   --to handle the loading of bitmaps files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The fs_loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_bitmap_loader_f
     (ext       : Interfaces.C.Strings.chars_ptr;
      fs_loader : access function
     (arg1 : File.ALLEGRO_FILE)
      return Bitmap.ALLEGRO_BITMAP)
      return      Extensions.bool;
   pragma Import
     (C,
      al_register_bitmap_loader_f,
      "al_register_bitmap_loader_f");

   -- Register a handler for al_save_bitmap_f. The given function will be used
   --to handle the loading of bitmaps files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The saver_f argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_bitmap_saver_f
     (ext      : Interfaces.C.Strings.chars_ptr;
      fs_saver : access function
     (arg1 : File.ALLEGRO_FILE;
      arg2 : Bitmap.ALLEGRO_BITMAP)
      return Extensions.bool)
      return     Extensions.bool;
   pragma Import
     (C,
      al_register_bitmap_saver_f,
      "al_register_bitmap_saver_f");

   -- Loads an image file into an ALLEGRO_BITMAP. The file type is determined
   --by the extension.
   --
   -- Returns NULL on error.
   function al_load_bitmap
     (filename : Interfaces.C.Strings.chars_ptr)
      return     Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_load_bitmap, "al_load_bitmap");

   -- Loads an image from an ALLEGRO_FILE stream into an ALLEGRO_BITMAP. The
   --file type is determined by the passed 'ident' parameter, which is a file
   --name extension including the leading dot.
   --
   -- Returns NULL on error. The file remains open afterwards.
   function al_load_bitmap_f
     (fp    : File.ALLEGRO_FILE;
      ident : Interfaces.C.Strings.chars_ptr)
      return  Bitmap.ALLEGRO_BITMAP;
   pragma Import (C, al_load_bitmap_f, "al_load_bitmap_f");

   -- Saves an ALLEGRO_BITMAP to an image file. The file type is determined by
   --the extension.
   --
   -- Returns true on success, false on error.
   function al_save_bitmap
     (filename : Interfaces.C.Strings.chars_ptr;
      bitmap   : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return     Extensions.bool;
   pragma Import (C, al_save_bitmap, "al_save_bitmap");

   -- Saves an ALLEGRO_BITMAP to an ALLEGRO_FILE stream. The file type is
   --determined by the passed 'ident' parameter, which is a file name
   --extension including the leading dot.
   --
   -- Returns true on success, false on error. The file remains open
   --afterwards.
   function al_save_bitmap_f
     (fp     : File.ALLEGRO_FILE;
      ident  : Interfaces.C.Strings.chars_ptr;
      bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return   Extensions.bool;
   pragma Import (C, al_save_bitmap_f, "al_save_bitmap_f");

end Allegro5.Bitmap_IO;
