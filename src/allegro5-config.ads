with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.File;

package Allegro5.Config is

   -- An abstract configuration structure.
   type ALLEGRO_CONFIG is private;
   type ALLEGRO_CONFIG_SECTION is private;
   type ALLEGRO_CONFIG_ENTRY is private;

   -- Create an empty configuration structure.
   function al_create_config return ALLEGRO_CONFIG;
   pragma Import (C, al_create_config, "al_create_config");

   -- Add a section to a configuration structure with the given name. If the section already exists then nothing happens.
   procedure al_add_config_section (config : ALLEGRO_CONFIG; name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_add_config_section, "al_add_config_section");

   -- Set a value in a section of a configuration. If the section doesn't yet exist, it will be created. If a value already existed for the given key, it will be overwritten. The section can be NULL or "" for the global section.
   --
   -- For consistency with the on-disk format of config files, any leading and trailing whitespace will be stripped from the value. If you have significant whitespace you wish to preserve, you should add your own quote characters and remove them when reading the values back in.
   procedure al_set_config_value
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr;
      value : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_config_value, "al_set_config_value");

   -- Add a comment in a section of a configuration. If the section doesn't yet exist, it will be created. The section can be NULL or "" for the global section.
   --
   -- The comment may or may not begin with a hash character. Any newlines in the comment string will be replaced by space characters.
   procedure al_add_config_comment
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      comment : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_add_config_comment, "al_add_config_comment");

   -- Gets a pointer to an internal character buffer that will only remain valid as long as the ALLEGRO_CONFIG structure is not destroyed. Copy the value if you need a copy. The section can be NULL or "" for the global section. Returns NULL if the section or key do not exist.
   function al_get_config_value
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_config_value, "al_get_config_value");

   -- Read a configuration file from disk. Returns NULL on error. The configuration structure should be destroyed with al_destroy_config.
   function al_load_config_file (filename : Interfaces.C.Strings.chars_ptr) return ALLEGRO_CONFIG;
   pragma Import (C, al_load_config_file, "al_load_config_file");

   -- Read a configuration file from an already open file.
   --
   -- Returns NULL on error. The configuration structure should be destroyed with al_destroy_config. The file remains open afterwards.
   function al_load_config_file_f (file : Allegro5.File.ALLEGRO_FILE) return ALLEGRO_CONFIG;
   pragma Import (C, al_load_config_file_f, "al_load_config_file_f");

   -- Write out a configuration file to disk. Returns true on success, false on error.
   function al_save_config_file (filename : Interfaces.C.Strings.chars_ptr; config : ALLEGRO_CONFIG) return Extensions.bool;
   pragma Import (C, al_save_config_file, "al_save_config_file");

   -- Write out a configuration file to an already open file.
   --
   -- Returns true on success, false on error. The file remains open afterwards.
   function al_save_config_file_f (file : Allegro5.File.ALLEGRO_FILE; config : ALLEGRO_CONFIG) return Extensions.bool;
   pragma Import (C, al_save_config_file_f, "al_save_config_file_f");

   -- Merge one configuration structure into another. Values in configuration 'add' override those in 'master'. 'master' is modified. Comments from 'add' are not retained.
   procedure al_merge_config_into (master : ALLEGRO_CONFIG; add : ALLEGRO_CONFIG);
   pragma Import (C, al_merge_config_into, "al_merge_config_into");

   -- Merge two configuration structures, and return the result as a new configuration. Values in configuration 'cfg2' override those in 'cfg1'. Neither of the input configuration structures are modified. Comments from 'cfg2' are not retained.
   function al_merge_config (cfg1 : ALLEGRO_CONFIG; cfg2 : ALLEGRO_CONFIG) return ALLEGRO_CONFIG;
   pragma Import (C, al_merge_config, "al_merge_config");

   -- Free the resources used by a configuration structure. Does nothing if passed NULL.
   procedure al_destroy_config (config : ALLEGRO_CONFIG);
   pragma Import (C, al_destroy_config, "al_destroy_config");

   -- Returns the name of the first section in the given config file. Usually this will return an empty string for the global section. The iterator parameter will receive an opaque iterator which is used by al_get_next_config_section to iterate over the remaining sections.
   --
   -- The returned string and the iterator are only valid as long as no change is made to the passed ALLEGRO_CONFIG.
   function al_get_first_config_section (config : ALLEGRO_CONFIG; iterator : ALLEGRO_CONFIG_SECTION) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_section, "al_get_first_config_section");

   -- Returns the name of the next section in the given config file or NULL if there are no more sections. The iterator must have been obtained with al_get_first_config_section first.
   function al_get_next_config_section (iterator : ALLEGRO_CONFIG_SECTION) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_section, "al_get_next_config_section");

   -- Returns the name of the first key in the given section in the given config or NULL if the section is empty. The iterator works like the one for al_get_first_config_section.
   --
   -- The returned string and the iterator are only valid as long as no change is made to the passed ALLEGRO_CONFIG.
   function al_get_first_config_entry
     (config : ALLEGRO_CONFIG;
      section : Interfaces.C.Strings.chars_ptr;
      iterator : ALLEGRO_CONFIG_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_first_config_entry, "al_get_first_config_entry");

   -- Returns the next key for the iterator obtained by al_get_first_config_entry. The iterator works like the one for al_get_next_config_section.
   function al_get_next_config_entry (iterator : ALLEGRO_CONFIG_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_next_config_entry, "al_get_next_config_entry");

private
   type ALLEGRO_CONFIG is new System.Address;
   type ALLEGRO_CONFIG_SECTION is new System.Address;
   type ALLEGRO_CONFIG_ENTRY is new System.Address;

end Allegro5.Config;
