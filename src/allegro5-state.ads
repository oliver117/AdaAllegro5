with Interfaces.C; use Interfaces.C;
with System;

package Allegro5.State is

   -- Flags which can be passed to al_store_state/al_restore_state
   -- as bit combinations. See al_store_state for the list of flags.
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

   -- Opaque type which is passed to al_store_state/al_restore_state.
   type ALLEGRO_STATE is new System.Address;

   -- Stores part of the state of the current thread in
   --the given ALLEGRO_STATE objects.
   procedure al_store_state (state : ALLEGRO_STATE; flags : int);
   pragma Import (C, al_store_state, "al_store_state");

   -- Restores part of the state of the current thread
   --from the given ALLEGRO_STATE object.
   procedure al_restore_state (state : ALLEGRO_STATE);
   pragma Import (C, al_restore_state, "al_restore_state");

end Allegro5.State;
