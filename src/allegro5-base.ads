with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

package Allegro5.Base is

   function AL_ID (a : int; b : int; c : int; d : int) return int;

   ALLEGRO_VERSION : constant := 5;
   ALLEGRO_SUB_VERSION : constant := 0;
   ALLEGRO_WIP_VERSION : constant := 10;
   ALLEGRO_RELEASE_NUMBER : constant := 1;
   ALLEGRO_VERSION_STR : constant String := "5.0.10";
   ALLEGRO_DATE_STR : constant String := "2013";
   ALLEGRO_DATE : constant := 20130616;
   ALLEGRO_VERSION_INT : int := ALLEGRO_VERSION * 2 ** 24 + ALLEGRO_SUB_VERSION * 2 ** 16 + ALLEGRO_WIP_VERSION * 2 ** 8 + ALLEGRO_RELEASE_NUMBER;
   ALLEGRO_PI : constant := 3.14159265358979323846;



   function al_get_allegro_version return Unsigned_32;
   pragma Import (C, al_get_allegro_version, "al_get_allegro_version");

   function al_run_main
     (argc : int;
      argv : System.Address;
      arg3 : access function (arg1 : int; arg2 : System.Address) return int) return int;
   pragma Import (C, al_run_main, "al_run_main");

end Allegro5.Base;
