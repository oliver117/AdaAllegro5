with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

with Allegro5.Config;
with Allegro5.Path;

package Allegro5.System is

   function al_init return Extensions.bool;

   type ALLEGRO_SYSTEM is new Standard.System.Address;

   function al_install_system (version : int; atexit_ptr : access function (arg1 : access procedure) return int) return Extensions.bool;
   pragma Import (C, al_install_system, "al_install_system");

   procedure al_uninstall_system;
   pragma Import (C, al_uninstall_system, "al_uninstall_system");

   function al_is_system_installed return Extensions.bool;
   pragma Import (C, al_is_system_installed, "al_is_system_installed");

   function al_get_system_driver return ALLEGRO_SYSTEM;
   pragma Import (C, al_get_system_driver, "al_get_system_driver");

   function al_get_system_config return Config.ALLEGRO_CONFIG;
   pragma Import (C, al_get_system_config, "al_get_system_config");

   function al_get_standard_path (id : int) return Path.ALLEGRO_PATH;
   pragma Import (C, al_get_standard_path, "al_get_standard_path");

   procedure al_set_exe_name (path : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_exe_name, "al_set_exe_name");

   procedure al_set_org_name (org_name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_org_name, "al_set_org_name");

   procedure al_set_app_name (app_name : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_set_app_name, "al_set_app_name");

   function al_get_org_name return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_org_name, "al_get_org_name");

   function al_get_app_name return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_app_name, "al_get_app_name");

   function al_inhibit_screensaver (inhibit : Extensions.bool) return Extensions.bool;
   pragma Import (C, al_inhibit_screensaver, "al_inhibit_screensaver");

private

   function atexit(arg1 : access procedure) return int;

end Allegro5.System;
