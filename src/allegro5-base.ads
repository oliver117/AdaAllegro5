with Interfaces.C; use Interfaces.C;
with stdint;
with System;

package Allegro5.Base is

   function AL_ID
     (a    : int;
      b    : int;
      c    : int;
      d    : int)
      return int;

   ALLEGRO_VERSION        : constant := 5;
   ALLEGRO_SUB_VERSION    : constant := 0;
   ALLEGRO_WIP_VERSION    : constant := 10;
   ALLEGRO_RELEASE_NUMBER : constant := 1;
   ALLEGRO_VERSION_STR    : constant String := "5.0.10";
   ALLEGRO_DATE_STR       : constant String := "2013";
   ALLEGRO_DATE           : constant := 20130616;
   ALLEGRO_VERSION_INT    : constant := ALLEGRO_VERSION * 2 ** 24 +
                                        ALLEGRO_SUB_VERSION * 2 ** 16 +
                                        ALLEGRO_WIP_VERSION * 2 ** 8 +
                                        ALLEGRO_RELEASE_NUMBER;
   ALLEGRO_PI             : constant := 3.14159265358979323846;

   -- Returns the (compiled) version of the Allegro library, packed into a
   --single integer as groups of 8 bits in the form (major << 24) | (minor <<
   --16) | (revision << 8) | release.
   function al_get_allegro_version return  stdint.uint32_t;
   pragma Import (C, al_get_allegro_version, "al_get_allegro_version");

   -- This function is useful in cases where you don't have a main() function
   --but want to run Allegro (mostly useful in a wrapper library). Under
   --Windows and Linux this is no problem because you simply can call
   --al_install_system. But some other system (like OSX) don't allow calling
   --al_install_system in the main thread. al_run_main will know what to do in
   --that case.
   --
   -- The passed argc and argv will simply be passed on to user_main and the
   --return value of user_main will be returned.
   function al_run_main
     (argc      : int;
      argv      : System.Address;
      user_main : access function
     (arg1 : int;
      arg2 : System.Address)
      return int)
      return      int;
   pragma Import (C, al_run_main, "al_run_main");

end Allegro5.Base;
