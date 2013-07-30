with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.File;

package Allegro5.Config is

   type ALLEGRO_CONFIG is new System.Address;
   type ALLEGRO_CONFIG_SECTION is new System.Address;
   type ALLEGRO_CONFIG_ENTRY is new System.Address;

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
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      comment : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_add_config_comment, "al_add_config_comment");

   function al_get_config_value
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_config_value, "al_get_config_value");

   function al_load_config_file (filename : Interfaces.C.Strings.chars_ptr) return ALLEGRO_CONFIG;
   pragma Import (C, al_load_config_file, "al_load_config_file");

   function al_load_config_file_f (filename : File.ALLEGRO_FILE) return ALLEGRO_CONFIG;
   pragma Import (C, al_load_config_file_f, "al_load_config_file_f");

   function al_save_config_file (filename : Interfaces.C.Strings.chars_ptr; config : ALLEGRO_CONFIG) return Extensions.bool;
   pragma Import (C, al_save_config_file, "al_save_config_file");

   function al_save_config_file_f (file : Allegro5.File.ALLEGRO_FILE; config : ALLEGRO_CONFIG) return Extensions.bool;
   pragma Import (C, al_save_config_file_f, "al_save_config_file_f");

   procedure al_merge_config_into (master : ALLEGRO_CONFIG; add : ALLEGRO_CONFIG);
   pragma Import (C, al_merge_config_into, "al_merge_config_into");

   function al_merge_config (cfg1 : ALLEGRO_CONFIG; cfg2 : ALLEGRO_CONFIG) return ALLEGRO_CONFIG;
   pragma Import (C, al_merge_config, "al_merge_config");

   procedure al_destroy_config (config : ALLEGRO_CONFIG);
   pragma Import (C, al_destroy_config, "al_destroy_config");

   function al_get_first_config_section (config : ALLEGRO_CONFIG; iterator : ALLEGRO_CONFIG_SECTION) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_section, "al_get_first_config_section");

   function al_get_next_config_section (iterator : ALLEGRO_CONFIG_SECTION) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_section, "al_get_next_config_section");

   function al_get_first_config_entry
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      iterator : ALLEGRO_CONFIG_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_entry, "al_get_first_config_entry");

   function al_get_next_config_entry (iterator : ALLEGRO_CONFIG_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_entry, "al_get_next_config_entry");

end Allegro5.Config;
