with Interfaces.C; use Interfaces.C;

package Allegro5.Transformations is

   type ALLEGRO_TRANSFORM_m_array is array (0 .. 3, 0 .. 3) of aliased float;

   -- Defines the generic transformation type, a 4x4 matrix.
   --2D transforms use only a small subsection of this matrix, namely the top
   --left 2x2 matrix, and the right most 2x1 matrix, for a total of 6 values.
   type ALLEGRO_TRANSFORM is record
      m : aliased ALLEGRO_TRANSFORM_m_array;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TRANSFORM);

   -- Sets the transformation to be used for the the drawing operations on the
   --target bitmap (each bitmap maintains its own transformation).
   --Every drawing operation after this call will be transformed using
   --this transformation. Call this function with an identity transformation
   --to return to the default behaviour.

   -- This function does nothing if there is no target bitmap.

   -- The parameter is passed by reference as an optimization to avoid the
   --overhead of stack copying. The reference will not be stored in the
   --Allegro library so it is safe to pass references to local variables.
   procedure al_use_transform (trans : ALLEGRO_TRANSFORM);
   pragma Import (C, al_use_transform, "al_use_transform");

   -- Makes a copy of a transformation.
   procedure al_copy_transform (dest : access ALLEGRO_TRANSFORM; src : ALLEGRO_TRANSFORM);
   pragma Import (C, al_copy_transform, "al_copy_transform");

   -- Sets the transformation to be the identity transformation. This is the
   --default transformation. Use al_use_transform on an identity transformation
   --to return to the default.
   procedure al_identity_transform (trans : access ALLEGRO_TRANSFORM);
   pragma Import (C, al_identity_transform, "al_identity_transform");

   -- Builds a transformation given some parameters. This call is equivalent to
   --calling the transformations in this order: make identity, scale,
   --rotate, translate. This method is faster, however, than actually
   --calling those functions.
   procedure al_build_transform
     (trans : access ALLEGRO_TRANSFORM;
      x : float;
      y : float;
      sx : float;
      sy : float;
      theta : float);
   pragma Import (C, al_build_transform, "al_build_transform");

   -- Apply a translation to a transformation.
   procedure al_translate_transform
     (trans : access ALLEGRO_TRANSFORM;
      x : float;
      y : float);
   pragma Import (C, al_translate_transform, "al_translate_transform");

   -- Apply a rotation to a transformation.
   procedure al_rotate_transform (trans : access ALLEGRO_TRANSFORM; theta : float);
   pragma Import (C, al_rotate_transform, "al_rotate_transform");

   -- Apply a scale to a transformation.
   procedure al_scale_transform
     (trans : access ALLEGRO_TRANSFORM;
      sx : float;
      sy : float);
   pragma Import (C, al_scale_transform, "al_scale_transform");

   -- Transform a pair of coordinates.
   procedure al_transform_coordinates
     (trans : ALLEGRO_TRANSFORM;
      x : access float;
      y : access float);
   pragma Import (C, al_transform_coordinates, "al_transform_coordinates");

   -- Compose (combine) two transformations by a matrix multiplication.

   --    trans := trans other

   -- Note that the order of matrix multiplications is important. The effect
   --of applying the combined transform will be as if first applying trans
   --and then applying other and not the other way around.
   procedure al_compose_transform (trans : access ALLEGRO_TRANSFORM; other : ALLEGRO_TRANSFORM);
   pragma Import (C, al_compose_transform, "al_compose_transform");

   -- Returns the transformation of the current target bitmap, as set by
   --al_use_transform. If there is no target bitmap, this function returns NULL.
   function al_get_current_transform return ALLEGRO_TRANSFORM;
   pragma Import (C, al_get_current_transform, "al_get_current_transform");

   -- Inverts the passed transformation. If the transformation is nearly
   --singular (close to not having an inverse) then the returned transformation
   --may be invalid. Use al_check_inverse to ascertain if the transformation
   --has an inverse before inverting it if you are in doubt.
   procedure al_invert_transform (trans : access ALLEGRO_TRANSFORM);
   pragma Import (C, al_invert_transform, "al_invert_transform");

   -- Checks if the transformation has an inverse using the supplied tolerance.
   --Tolerance should be a small value between 0 and 1, with 1e-7 being
   --sufficient for most applications.

   -- In this function tolerance specifies how close the determinant can be
   --to 0 (if the determinant is 0, the transformation has no inverse).
   --Thus the smaller the tolerance you specify, the "worse" transformations
   --will pass this test. Using a tolerance of 1e-7 will catch errors greater
   --than 1/1000's of a pixel, but let smaller errors pass. That means that
   --if you transformed a point by a transformation and then transformed it
   --again by the inverse transformation that passed this check, the resultant
   --point should less than 1/1000's of a pixel away from the original point.

   -- Note that this check is superfluous most of the time if you never touched
   --the transformation matrix values yourself. The only thing that would
   --cause the transformation to not have an inverse is if you applied
   --a 0 (or very small) scale to the transformation or you have a really
   --large translation. As long as the scale is comfortably above 0, the
   --transformation will be invertible.
   function al_check_inverse (trans : ALLEGRO_TRANSFORM; tol : float) return int;
   pragma Import (C, al_check_inverse, "al_check_inverse");

end Allegro5.Transformations;
