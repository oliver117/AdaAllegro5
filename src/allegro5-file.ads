with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;
with stdint;
with Allegro5.UTF8;
use Allegro5;

package Allegro5.File is

   subtype ALLEGRO_FILE is Extensions.opaque_structure_def;

   type ALLEGRO_FILE_INTERFACE is record
      fi_fopen : access function (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr) return ALLEGRO_FILE;
      fi_fclose : access procedure (arg1 : ALLEGRO_FILE);
      fi_fread : access function
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : stdint.size_t) return stdint.size_t;
      fi_fwrite : access function
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : stdint.size_t) return stdint.size_t;
      fi_fflush : access function (arg1 : System.Address) return Extensions.bool;
      fi_ftell : access function (arg1 : System.Address) return stdint.int64_t;
      fi_fseek : access function
           (arg1 : System.Address;
            arg2 : stdint.int64_t;
            arg3 : int) return Extensions.bool;
      fi_feof : access function (arg1 : System.Address) return Extensions.bool;
      fi_ferror : access function (arg1 : System.Address) return Extensions.bool;
      fi_fclearerr : access procedure (arg1 : System.Address);
      fi_fungetc : access function (arg1 : System.Address; arg2 : int) return int;
      fi_fsize : access function (arg1 : System.Address) return stdint.off_t;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FILE_INTERFACE);

   type ALLEGRO_SEEK is 
     (ALLEGRO_SEEK_SET,
      ALLEGRO_SEEK_CUR,
      ALLEGRO_SEEK_END);
   pragma Convention (C, ALLEGRO_SEEK);

   function al_fopen (path : Interfaces.C.Strings.chars_ptr; mode : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_fopen, "al_fopen");

   function al_fopen_interface
     (vt : System.Address;
      path : Interfaces.C.Strings.chars_ptr;
      mode : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_fopen_interface, "al_fopen_interface");

   function al_create_file_handle (vt : System.Address; userdata : System.Address) return System.Address;
   pragma Import (C, al_create_file_handle, "al_create_file_handle");

   procedure al_fclose (f : System.Address);
   pragma Import (C, al_fclose, "al_fclose");

   function al_fread
     (f : System.Address;
      ptr : System.Address;
      size : stdint.size_t) return stdint.size_t;
   pragma Import (C, al_fread, "al_fread");

   function al_fwrite
     (f : System.Address;
      ptr : System.Address;
      size : stdint.size_t) return stdint.size_t;
   pragma Import (C, al_fwrite, "al_fwrite");

   function al_fflush (f : System.Address) return Extensions.bool;
   pragma Import (C, al_fflush, "al_fflush");

   function al_ftell (f : System.Address) return stdint.int64_t;
   pragma Import (C, al_ftell, "al_ftell");

   function al_fseek
     (f : System.Address;
      offset : stdint.int64_t;
      whence : int) return Extensions.bool;
   pragma Import (C, al_fseek, "al_fseek");

   function al_feof (f : System.Address) return Extensions.bool;
   pragma Import (C, al_feof, "al_feof");

   function al_ferror (f : System.Address) return Extensions.bool;
   pragma Import (C, al_ferror, "al_ferror");

   procedure al_fclearerr (f : System.Address);
   pragma Import (C, al_fclearerr, "al_fclearerr");

   function al_fungetc (f : System.Address; c : int) return int;
   pragma Import (C, al_fungetc, "al_fungetc");

   function al_fsize (f : System.Address) return stdint.int64_t;
   pragma Import (C, al_fsize, "al_fsize");

   function al_fgetc (f : System.Address) return int;
   pragma Import (C, al_fgetc, "al_fgetc");

   function al_fputc (f : System.Address; c : int) return int;
   pragma Import (C, al_fputc, "al_fputc");

   function al_fread16le (f : System.Address) return stdint.int16_t;
   pragma Import (C, al_fread16le, "al_fread16le");

   function al_fread16be (f : System.Address) return stdint.int16_t;
   pragma Import (C, al_fread16be, "al_fread16be");

   function al_fwrite16le (f : System.Address; w : stdint.int16_t) return stdint.size_t;
   pragma Import (C, al_fwrite16le, "al_fwrite16le");

   function al_fwrite16be (f : System.Address; w : stdint.int16_t) return stdint.size_t;
   pragma Import (C, al_fwrite16be, "al_fwrite16be");

   function al_fread32le (f : System.Address) return stdint.int32_t;
   pragma Import (C, al_fread32le, "al_fread32le");

   function al_fread32be (f : System.Address) return stdint.int32_t;
   pragma Import (C, al_fread32be, "al_fread32be");

   function al_fwrite32le (f : System.Address; l : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_fwrite32le, "al_fwrite32le");

   function al_fwrite32be (f : System.Address; l : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_fwrite32be, "al_fwrite32be");

   function al_fgets
     (f : System.Address;
      p : Interfaces.C.Strings.chars_ptr;
      max : stdint.size_t) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_fgets, "al_fgets");

   function al_fget_ustr (f : System.Address) return access UTF8.ALLEGRO_USTR;
   pragma Import (C, al_fget_ustr, "al_fget_ustr");

   function al_fputs (f : System.Address; p : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_fputs, "al_fputs");

   function al_fopen_fd (fd : int; mode : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_fopen_fd, "al_fopen_fd");

   function al_make_temp_file (tmpl : Interfaces.C.Strings.chars_ptr; ret_path : System.Address) return System.Address;
   pragma Import (C, al_make_temp_file, "al_make_temp_file");

   function al_fopen_slice
     (fp : System.Address;
      initial_size : stdint.size_t;
      mode : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_fopen_slice, "al_fopen_slice");

   function al_get_new_file_interface return System.Address;
   pragma Import (C, al_get_new_file_interface, "al_get_new_file_interface");

   procedure al_set_new_file_interface (file_interface : System.Address);
   pragma Import (C, al_set_new_file_interface, "al_set_new_file_interface");

   procedure al_set_standard_file_interface;
   pragma Import (C, al_set_standard_file_interface, "al_set_standard_file_interface");

   function al_get_file_userdata (f : System.Address) return System.Address;
   pragma Import (C, al_get_file_userdata, "al_get_file_userdata");

end Allegro5.File;
