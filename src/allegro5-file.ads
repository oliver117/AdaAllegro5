with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with stdint;
with System;

with Allegro5.Path;
with Allegro5.UTF8;

package Allegro5.File is

   type ALLEGRO_FILE is new System.Address;

   type ALLEGRO_FILE_INTERFACE is record
      fi_fopen : access function (path : Interfaces.C.Strings.chars_ptr; mode : Interfaces.C.Strings.chars_ptr) return System.Address;
      fi_fclose : access procedure (f : ALLEGRO_FILE);
      fi_fread : access function
           (f : ALLEGRO_FILE;
            ptr : System.Address;
            size : size_t) return size_t;
      fi_fwrite : access function
           (f : ALLEGRO_FILE;
            ptr : System.Address;
            size : size_t) return size_t;
      fi_fflush : access function (f : ALLEGRO_FILE) return Extensions.bool;
      fi_ftell : access function (f : ALLEGRO_FILE) return Integer_64;
      fi_fseek : access function
           (f : ALLEGRO_FILE;
            offset : Integer_64;
            whence : int) return Extensions.bool;
      fi_feof : access function (f : ALLEGRO_FILE) return Extensions.bool;
      fi_ferror : access function (f : ALLEGRO_FILE) return Extensions.bool;
      fi_fclearerr : access procedure (f : ALLEGRO_FILE);
      fi_fungetc : access function (f : ALLEGRO_FILE; c : int) return int;
      fi_fsize : access function (f : ALLEGRO_FILE) return stdint.off_t;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FILE_INTERFACE);

   type ALLEGRO_SEEK is 
     (ALLEGRO_SEEK_SET,
      ALLEGRO_SEEK_CUR,
      ALLEGRO_SEEK_END);
   pragma Convention (C, ALLEGRO_SEEK);

   function al_fopen (path : Interfaces.C.Strings.chars_ptr; mode : Interfaces.C.Strings.chars_ptr) return ALLEGRO_FILE;
   pragma Import (C, al_fopen, "al_fopen");

   function al_fopen_interface
     (vt : ALLEGRO_FILE_INTERFACE;
      path : Interfaces.C.Strings.chars_ptr;
      mode : Interfaces.C.Strings.chars_ptr) return ALLEGRO_FILE;
   pragma Import (C, al_fopen_interface, "al_fopen_interface");

   function al_create_file_handle (vt : ALLEGRO_FILE_INTERFACE; userdata : System.Address) return ALLEGRO_FILE;
   pragma Import (C, al_create_file_handle, "al_create_file_handle");

   procedure al_fclose (f : ALLEGRO_FILE);
   pragma Import (C, al_fclose, "al_fclose");

   function al_fread
     (f : ALLEGRO_FILE;
      ptr : System.Address;
      size : size_t) return size_t;
   pragma Import (C, al_fread, "al_fread");

   function al_fwrite
     (f : ALLEGRO_FILE;
      ptr : System.Address;
      size : size_t) return size_t;
   pragma Import (C, al_fwrite, "al_fwrite");

   function al_fflush (f : ALLEGRO_FILE) return Extensions.bool;
   pragma Import (C, al_fflush, "al_fflush");

   function al_ftell (f : ALLEGRO_FILE) return Integer_64;
   pragma Import (C, al_ftell, "al_ftell");

   function al_fseek
     (f : ALLEGRO_FILE;
      offset : Integer_64;
      whence : int) return Extensions.bool;
   pragma Import (C, al_fseek, "al_fseek");

   function al_feof (f : ALLEGRO_FILE) return Extensions.bool;
   pragma Import (C, al_feof, "al_feof");

   function al_ferror (f : ALLEGRO_FILE) return Extensions.bool;
   pragma Import (C, al_ferror, "al_ferror");

   procedure al_fclearerr (f : ALLEGRO_FILE);
   pragma Import (C, al_fclearerr, "al_fclearerr");

   function al_fungetc (f : ALLEGRO_FILE; c : int) return int;
   pragma Import (C, al_fungetc, "al_fungetc");

   function al_fsize (f : ALLEGRO_FILE) return Integer_64;
   pragma Import (C, al_fsize, "al_fsize");

   function al_fgetc (f : ALLEGRO_FILE) return int;
   pragma Import (C, al_fgetc, "al_fgetc");

   function al_fputc (f : ALLEGRO_FILE; c : int) return int;
   pragma Import (C, al_fputc, "al_fputc");

   function al_fread16le (f : ALLEGRO_FILE) return Integer_16;
   pragma Import (C, al_fread16le, "al_fread16le");

   function al_fread16be (f : ALLEGRO_FILE) return Integer_16;
   pragma Import (C, al_fread16be, "al_fread16be");

   function al_fwrite16le (f : ALLEGRO_FILE; w : Integer_16) return size_t;
   pragma Import (C, al_fwrite16le, "al_fwrite16le");

   function al_fwrite16be (f : ALLEGRO_FILE; w : Integer_16) return size_t;
   pragma Import (C, al_fwrite16be, "al_fwrite16be");

   function al_fread32le (f : ALLEGRO_FILE) return Integer_32;
   pragma Import (C, al_fread32le, "al_fread32le");

   function al_fread32be (f : ALLEGRO_FILE) return Integer_32;
   pragma Import (C, al_fread32be, "al_fread32be");

   function al_fwrite32le (f : ALLEGRO_FILE; l : Integer_32) return size_t;
   pragma Import (C, al_fwrite32le, "al_fwrite32le");

   function al_fwrite32be (f : ALLEGRO_FILE; l : Integer_32) return size_t;
   pragma Import (C, al_fwrite32be, "al_fwrite32be");

   function al_fgets
     (f : ALLEGRO_FILE;
      p : Interfaces.C.Strings.chars_ptr;
      max : size_t) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_fgets, "al_fgets");

   function al_fget_ustr (f : ALLEGRO_FILE) return access UTF8.ALLEGRO_USTR;
   pragma Import (C, al_fget_ustr, "al_fget_ustr");

   function al_fputs (f : ALLEGRO_FILE; p : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_fputs, "al_fputs");

   function al_fopen_fd (fd : int; mode : Interfaces.C.Strings.chars_ptr) return ALLEGRO_FILE;
   pragma Import (C, al_fopen_fd, "al_fopen_fd");

   function al_make_temp_file (tmpl : Interfaces.C.Strings.chars_ptr; ret_path : Path.ALLEGRO_PATH) return ALLEGRO_FILE;
   pragma Import (C, al_make_temp_file, "al_make_temp_file");

   function al_fopen_slice
     (fp : ALLEGRO_FILE;
      initial_size : size_t;
      mode : Interfaces.C.Strings.chars_ptr) return ALLEGRO_FILE;
   pragma Import (C, al_fopen_slice, "al_fopen_slice");

   function al_get_new_file_interface return ALLEGRO_FILE_INTERFACE;
   pragma Import (C, al_get_new_file_interface, "al_get_new_file_interface");

   procedure al_set_new_file_interface (file_interface : ALLEGRO_FILE_INTERFACE);
   pragma Import (C, al_set_new_file_interface, "al_set_new_file_interface");

   procedure al_set_standard_file_interface;
   pragma Import (C, al_set_standard_file_interface, "al_set_standard_file_interface");

   function al_get_file_userdata (f : ALLEGRO_FILE) return System.Address;
   pragma Import (C, al_get_file_userdata, "al_get_file_userdata");

end Allegro5.File;
