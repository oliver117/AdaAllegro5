with Interfaces.C; use Interfaces.C;
with System;

package Allegro5.TLS is

   subtype ALLEGRO_STATE_FLAGS is unsigned;
   ALLEGRO_STATE_NEW_DISPLAY_PARAMETERS : constant ALLEGRO_STATE_FLAGS := 1;
   ALLEGRO_STATE_NEW_BITMAP_PARAMETERS : constant ALLEGRO_STATE_FLAGS := 2;
   ALLEGRO_STATE_DISPLAY : constant ALLEGRO_STATE_FLAGS := 4;
   ALLEGRO_STATE_TARGET_BITMAP : constant ALLEGRO_STATE_FLAGS := 8;
   ALLEGRO_STATE_BLENDER : constant ALLEGRO_STATE_FLAGS := 16;
   ALLEGRO_STATE_NEW_FILE_INTERFACE : constant ALLEGRO_STATE_FLAGS := 32;
   ALLEGRO_STATE_TRANSFORM : constant ALLEGRO_STATE_FLAGS := 64;
   ALLEGRO_STATE_BITMAP : constant ALLEGRO_STATE_FLAGS := 10;
   ALLEGRO_STATE_ALL : constant ALLEGRO_STATE_FLAGS := 65535;

   subtype ALLEGRO_STATE_u_tls_array is Interfaces.C.char_array (0 .. 1023);
   type ALLEGRO_STATE is record
      u_tls : aliased ALLEGRO_STATE_u_tls_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_STATE);

   procedure al_store_state (state : access ALLEGRO_STATE; flags : int);
   pragma Import (C, al_store_state, "al_store_state");

   procedure al_restore_state (state : System.Address);
   pragma Import (C, al_restore_state, "al_restore_state");

end Allegro5.TLS;
