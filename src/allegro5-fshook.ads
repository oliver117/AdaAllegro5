with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with stdint;
with System;

with Allegro5.File;

use Allegro5;


package Allegro5.Fshook is

   EOF : constant := -1;

   type ALLEGRO_FS_INTERFACE;
   type ALLEGRO_FS_ENTRY is record
      vtable : access constant ALLEGRO_FS_INTERFACE;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FS_ENTRY);

   subtype ALLEGRO_FILE_MODE is unsigned;
   ALLEGRO_FILEMODE_READ : constant ALLEGRO_FILE_MODE := 1;
   ALLEGRO_FILEMODE_WRITE : constant ALLEGRO_FILE_MODE := 2;
   ALLEGRO_FILEMODE_EXECUTE : constant ALLEGRO_FILE_MODE := 4;
   ALLEGRO_FILEMODE_HIDDEN : constant ALLEGRO_FILE_MODE := 8;
   ALLEGRO_FILEMODE_ISFILE : constant ALLEGRO_FILE_MODE := 16;
   ALLEGRO_FILEMODE_ISDIR : constant ALLEGRO_FILE_MODE := 32;

   type ALLEGRO_FS_INTERFACE is record
      fs_create_entry : access function (path : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_FS_ENTRY;
      fs_destroy_entry : access procedure (e : access ALLEGRO_FS_ENTRY);
      fs_entry_name : access function (e : access ALLEGRO_FS_ENTRY) return Interfaces.C.Strings.chars_ptr;
      fs_update_entry : access function (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_entry_mode : access function (e : access ALLEGRO_FS_ENTRY) return stdint.uint32_t;
      fs_entry_atime : access function (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_mtime : access function (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_ctime : access function (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_size : access function (e : access ALLEGRO_FS_ENTRY) return stdint.off_t;
      fs_entry_exists : access function (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_remove_entry : access function (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_open_directory : access function (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_read_directory : access function (e : access ALLEGRO_FS_ENTRY) return access ALLEGRO_FS_ENTRY;
      fs_close_directory : access function (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_filename_exists : access function (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_remove_filename : access function (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_get_current_directory : access function return Interfaces.C.Strings.chars_ptr;
      fs_change_directory : access function (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_make_directory : access function (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_open_file : access function (e : access ALLEGRO_FS_ENTRY; mode : Interfaces.C.Strings.chars_ptr) return File.ALLEGRO_FILE;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FS_INTERFACE);

   function al_create_fs_entry (path : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_FS_ENTRY;
   pragma Import (C, al_create_fs_entry, "al_create_fs_entry");

   procedure al_destroy_fs_entry (e : access ALLEGRO_FS_ENTRY);
   pragma Import (C, al_destroy_fs_entry, "al_destroy_fs_entry");

   function al_get_fs_entry_name (e : access ALLEGRO_FS_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_fs_entry_name, "al_get_fs_entry_name");

   function al_update_fs_entry (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_update_fs_entry, "al_update_fs_entry");

   function al_get_fs_entry_mode (e : access ALLEGRO_FS_ENTRY) return stdint.uint32_t;
   pragma Import (C, al_get_fs_entry_mode, "al_get_fs_entry_mode");

   function al_get_fs_entry_atime (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_atime, "al_get_fs_entry_atime");

   function al_get_fs_entry_mtime (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_mtime, "al_get_fs_entry_mtime");

   function al_get_fs_entry_ctime (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_ctime, "al_get_fs_entry_ctime");

   function al_get_fs_entry_size (e : access ALLEGRO_FS_ENTRY) return stdint.off_t;
   pragma Import (C, al_get_fs_entry_size, "al_get_fs_entry_size");

   function al_fs_entry_exists (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_fs_entry_exists, "al_fs_entry_exists");

   function al_remove_fs_entry (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_remove_fs_entry, "al_remove_fs_entry");

   function al_open_directory (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_open_directory, "al_open_directory");

   function al_read_directory (e : access ALLEGRO_FS_ENTRY) return access ALLEGRO_FS_ENTRY;
   pragma Import (C, al_read_directory, "al_read_directory");

   function al_close_directory (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_close_directory, "al_close_directory");

   function al_filename_exists (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_filename_exists, "al_filename_exists");

   function al_remove_filename (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_remove_filename, "al_remove_filename");

   function al_get_current_directory return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_current_directory, "al_get_current_directory");

   function al_change_directory (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_change_directory, "al_change_directory");

   function al_make_directory (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_make_directory, "al_make_directory");

   function al_open_fs_entry (e : access ALLEGRO_FS_ENTRY; mode : Interfaces.C.Strings.chars_ptr) return File.ALLEGRO_FILE;
   pragma Import (C, al_open_fs_entry, "al_open_fs_entry");

   function al_get_fs_interface return ALLEGRO_FS_INTERFACE;
   pragma Import (C, al_get_fs_interface, "al_get_fs_interface");

   procedure al_set_fs_interface (vtable : ALLEGRO_FS_INTERFACE);
   pragma Import (C, al_set_fs_interface, "al_set_fs_interface");

   procedure al_set_standard_fs_interface;
   pragma Import (C, al_set_standard_fs_interface, "al_set_standard_fs_interface");

end Allegro5.Fshook;
