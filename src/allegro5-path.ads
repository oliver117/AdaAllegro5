with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;

package Allegro5.Path is


   ALLEGRO_NATIVE_PATH_SEP : aliased constant Character := '/';
   ALLEGRO_NATIVE_DRIVE_SEP : aliased constant Character := Character'Val (0);

   type ALLEGRO_PATH is new Interfaces.C.Extensions.opaque_structure_def;

   function al_create_path (str : Interfaces.C.Strings.chars_ptr) return ALLEGRO_PATH;
   pragma Import (C, al_create_path, "al_create_path");

   function al_create_path_for_directory (str : Interfaces.C.Strings.chars_ptr) return ALLEGRO_PATH;
   pragma Import (C, al_create_path_for_directory, "al_create_path_for_directory");

   function al_clone_path (path : ALLEGRO_PATH) return ALLEGRO_PATH;
   pragma Import (C, al_clone_path, "al_clone_path");

   function al_get_path_num_components (path : ALLEGRO_PATH) return int;
   pragma Import (C, al_get_path_num_components, "al_get_path_num_components");

   function al_get_path_component (path : ALLEGRO_PATH; i : int) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_component, "al_get_path_component");

   procedure al_replace_path_component
     (path : ALLEGRO_PATH;
      i : int;
      s : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_replace_path_component, "al_replace_path_component");

   procedure al_remove_path_component (path : ALLEGRO_PATH; i : int);
   pragma Import (C, al_remove_path_component, "al_remove_path_component");

   procedure al_insert_path_component
     (path : ALLEGRO_PATH;
      i : int;
      s : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_insert_path_component, "al_insert_path_component");

   function al_get_path_tail (path : ALLEGRO_PATH) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_tail, "al_get_path_tail");

   procedure al_drop_path_tail (path : ALLEGRO_PATH);
   pragma Import (C, al_drop_path_tail, "al_drop_path_tail");

   procedure al_append_path_component (path : ALLEGRO_PATH; s : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_append_path_component, "al_append_path_component");

   function al_join_paths (path : ALLEGRO_PATH; tail : ALLEGRO_PATH) return Extensions.bool;
   pragma Import (C, al_join_paths, "al_join_paths");

   function al_rebase_path (head : ALLEGRO_PATH; tail : ALLEGRO_PATH) return Extensions.bool;
   pragma Import (C, al_rebase_path, "al_rebase_path");

   function al_path_cstr (path : ALLEGRO_PATH; delim : char) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_path_cstr, "al_path_cstr");

   procedure al_destroy_path (path : ALLEGRO_PATH);
   pragma Import (C, al_destroy_path, "al_destroy_path");

   procedure al_set_path_drive (path : ALLEGRO_PATH; drive : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_path_drive, "al_set_path_drive");

   function al_get_path_drive (path : ALLEGRO_PATH) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_drive, "al_get_path_drive");

   procedure al_set_path_filename (path : ALLEGRO_PATH; filename : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_path_filename, "al_set_path_filename");

   function al_get_path_filename (path : ALLEGRO_PATH) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_filename, "al_get_path_filename");

   function al_get_path_extension (path : ALLEGRO_PATH) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_extension, "al_get_path_extension");

   function al_set_path_extension (path : ALLEGRO_PATH; extension : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_set_path_extension, "al_set_path_extension");

   function al_get_path_basename (path : ALLEGRO_PATH) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_path_basename, "al_get_path_basename");

   function al_make_path_canonical (path : ALLEGRO_PATH) return Extensions.bool;
   pragma Import (C, al_make_path_canonical, "al_make_path_canonical");

end Allegro5.Path;
