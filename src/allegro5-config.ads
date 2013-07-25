with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package Allegro5.Config is

   subtype ALLEGRO_CONFIG is Extensions.opaque_structure_def;
   subtype ALLEGRO_CONFIG_SECTION is Extensions.opaque_structure_def;
   subtype ALLEGRO_CONFIG_ENTRY is Extensions.opaque_structure_def;

   function al_create_config return ALLEGRO_CONFIG;
   pragma Import (C, al_create_config, "al_create_config");

   procedure al_add_config_section (config : ALLEGRO_CONFIG; name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_add_config_section, "al_add_config_section");

   procedure al_set_config_value
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_config_value, "al_set_config_value");

   procedure al_add_config_comment
     (config : System.Address;
      section : Interfaces.C.Strings.chars_ptr;
      comment : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_add_config_comment, "al_add_config_comment");

   function al_get_config_value
     (config : System.Address;
      section : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_config_value, "al_get_config_value");

   function al_load_config_file (filename : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_load_config_file, "al_load_config_file");

   function al_load_config_file_f (filename : System.Address) return System.Address;
   pragma Import (C, al_load_config_file_f, "al_load_config_file_f");

   function al_save_config_file (filename : Interfaces.C.Strings.chars_ptr; config : System.Address) return Extensions.bool;
   pragma Import (C, al_save_config_file, "al_save_config_file");

   function al_save_config_file_f (file : System.Address; config : System.Address) return Extensions.bool;
   pragma Import (C, al_save_config_file_f, "al_save_config_file_f");

   procedure al_merge_config_into (master : System.Address; add : System.Address);
   pragma Import (C, al_merge_config_into, "al_merge_config_into");

   function al_merge_config (cfg1 : System.Address; cfg2 : System.Address) return System.Address;
   pragma Import (C, al_merge_config, "al_merge_config");

   procedure al_destroy_config (config : System.Address);
   pragma Import (C, al_destroy_config, "al_destroy_config");

   function al_get_first_config_section (config : System.Address; iterator : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_section, "al_get_first_config_section");

   function al_get_next_config_section (iterator : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_section, "al_get_next_config_section");

   function al_get_first_config_entry
     (config : System.Address;
      section : Interfaces.C.Strings.chars_ptr;
      iterator : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_entry, "al_get_first_config_entry");

   function al_get_next_config_entry (iterator : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_entry, "al_get_next_config_entry");

end Allegro5.Config;
