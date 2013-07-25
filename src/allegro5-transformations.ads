with Interfaces.C; use Interfaces.C;
with System;

package Allegro5.Transformations is

   type ALLEGRO_TRANSFORM_m_array is array (0 .. 3, 0 .. 3) of aliased float;
   type ALLEGRO_TRANSFORM is record
      m : aliased ALLEGRO_TRANSFORM_m_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TRANSFORM);

   procedure al_use_transform (trans : System.Address);
   pragma Import (C, al_use_transform, "al_use_transform");

   procedure al_copy_transform (dest : access ALLEGRO_TRANSFORM; src : System.Address);
   pragma Import (C, al_copy_transform, "al_copy_transform");

   procedure al_identity_transform (trans : access ALLEGRO_TRANSFORM);
   pragma Import (C, al_identity_transform, "al_identity_transform");

   procedure al_build_transform
     (trans : access ALLEGRO_TRANSFORM;
      x : float;
      y : float;
      sx : float;
      sy : float;
      theta : float);
   pragma Import (C, al_build_transform, "al_build_transform");

   procedure al_translate_transform
     (trans : access ALLEGRO_TRANSFORM;
      x : float;
      y : float);
   pragma Import (C, al_translate_transform, "al_translate_transform");

   procedure al_rotate_transform (trans : access ALLEGRO_TRANSFORM; theta : float);
   pragma Import (C, al_rotate_transform, "al_rotate_transform");

   procedure al_scale_transform
     (trans : access ALLEGRO_TRANSFORM;
      sx : float;
      sy : float);
   pragma Import (C, al_scale_transform, "al_scale_transform");

   procedure al_transform_coordinates
     (trans : System.Address;
      x : access float;
      y : access float);
   pragma Import (C, al_transform_coordinates, "al_transform_coordinates");

   procedure al_compose_transform (trans : access ALLEGRO_TRANSFORM; other : System.Address);
   pragma Import (C, al_compose_transform, "al_compose_transform");

   function al_get_current_transform return System.Address;
   pragma Import (C, al_get_current_transform, "al_get_current_transform");

   procedure al_invert_transform (trans : access ALLEGRO_TRANSFORM);
   pragma Import (C, al_invert_transform, "al_invert_transform");

   function al_check_inverse (trans : System.Address; tol : float) return int;
   pragma Import (C, al_check_inverse, "al_check_inverse");

end Allegro5.Transformations;
