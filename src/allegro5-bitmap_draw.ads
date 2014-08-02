with Interfaces.C; use Interfaces.C;

with Allegro5.Bitmap;
with Allegro5.Color;

package Allegro5.Bitmap_Draw is

   -- Draws an unscaled, unrotated bitmap at the given position to the current
   --target bitmap (see al_set_target_bitmap).
   --
   -- flags can be a combination of:
   --
   -- ALLEGRO_FLIP_HORIZONTAL - flip the bitmap about the y-axis
   -- ALLEGRO_FLIP_VERTICAL - flip the bitmap about the x-axis
   procedure al_draw_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      dx     : Float;
      dy     : Float;
      flags  : int);
   pragma Import (C, al_draw_bitmap, "al_draw_bitmap");

   -- Draws a region of the given bitmap to the target bitmap.
   --
   -- sx - source x
   -- sy - source y
   -- sw - source width (width of region to blit)
   -- sh - source height (height of region to blit)
   -- dx - destination x
   -- dy - destination y
   -- flags - same as for al_draw_bitmap
   procedure al_draw_bitmap_region
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      sx     : Float;
      sy     : Float;
      sw     : Float;
      sh     : Float;
      dx     : Float;
      dy     : Float;
      flags  : int);
   pragma Import (C, al_draw_bitmap_region, "al_draw_bitmap_region");

   -- Draws a scaled version of the given bitmap to the target bitmap.
   --
   -- sx - source x
   -- sy - source y
   -- sw - source width
   -- sh - source height
   -- dx - destination x
   -- dy - destination y
   -- dw - destination width
   -- dh - destination height
   -- flags - same as for al_draw_bitmap
   procedure al_draw_scaled_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      sx     : Float;
      sy     : Float;
      sw     : Float;
      sh     : Float;
      dx     : Float;
      dy     : Float;
      dw     : Float;
      dh     : Float;
      flags  : int);
   pragma Import (C, al_draw_scaled_bitmap, "al_draw_scaled_bitmap");

   -- Draws a rotated version of the given bitmap to the target bitmap. The
   --bitmap is rotated by 'angle' radians clockwise.
   --
   -- The point at cx/cy relative to the upper left corner of the bitmap will
   --be drawn at dx/dy and the bitmap is rotated around this point. If cx,cy
   --is 0,0 the bitmap will rotate around its upper left corner.
   --
   -- cx - center x (relative to the bitmap)
   -- cy - center y (relative to the bitmap)
   -- dx - destination x
   -- dy - destination y
   -- angle - angle by which to rotate (radians)
   -- flags - same as for al_draw_bitmap
   procedure al_draw_rotated_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      cx     : Float;
      cy     : Float;
      dx     : Float;
      dy     : Float;
      angle  : Float;
      flags  : int);
   pragma Import (C, al_draw_rotated_bitmap, "al_draw_rotated_bitmap");

   -- Like al_draw_rotated_bitmap, but can also scale the bitmap.
   --
   -- The point at cx/cy in the bitmap will be drawn at dx/dy and the bitmap
   --is rotated and scaled around this point.
   --
   -- cx - center x
   -- cy - center y
   -- dx - destination x
   -- dy - destination y
   -- xscale - how much to scale on the x-axis (e.g. 2 for twice the size)
   -- yscale - how much to scale on the y-axis
   -- angle - angle by which to rotate (radians)
   -- flags - same as for al_draw_bitmap
   procedure al_draw_scaled_rotated_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      cx     : Float;
      cy     : Float;
      dx     : Float;
      dy     : Float;
      xscale : Float;
      yscale : Float;
      angle  : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_scaled_rotated_bitmap,
      "al_draw_scaled_rotated_bitmap");

   -- Like al_draw_bitmap but multiplies all colors in the bitmap with the
   --given color.
   procedure al_draw_tinted_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      tint   : Color.ALLEGRO_COLOR;
      dx     : Float;
      dy     : Float;
      flags  : int);
   pragma Import (C, al_draw_tinted_bitmap, "al_draw_tinted_bitmap");

   -- Like al_draw_bitmap_region but multiplies all colors in the bitmap with
   --the given color.
   procedure al_draw_tinted_bitmap_region
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      tint   : Color.ALLEGRO_COLOR;
      sx     : Float;
      sy     : Float;
      sw     : Float;
      sh     : Float;
      dx     : Float;
      dy     : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_tinted_bitmap_region,
      "al_draw_tinted_bitmap_region");

   -- Like al_draw_scaled_bitmap but multiplies all colors in the bitmap with
   --the given color.
   procedure al_draw_tinted_scaled_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      tint   : Color.ALLEGRO_COLOR;
      sx     : Float;
      sy     : Float;
      sw     : Float;
      sh     : Float;
      dx     : Float;
      dy     : Float;
      dw     : Float;
      dh     : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_tinted_scaled_bitmap,
      "al_draw_tinted_scaled_bitmap");

   -- Like al_draw_rotated_bitmap but multiplies all colors in the bitmap with
   --the given color.
   procedure al_draw_tinted_rotated_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      tint   : Color.ALLEGRO_COLOR;
      cx     : Float;
      cy     : Float;
      dx     : Float;
      dy     : Float;
      angle  : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_tinted_rotated_bitmap,
      "al_draw_tinted_rotated_bitmap");

   -- Like al_draw_scaled_rotated_bitmap but multiplies all colors in the
   --bitmap with the given color.
   procedure al_draw_tinted_scaled_rotated_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      tint   : Color.ALLEGRO_COLOR;
      cx     : Float;
      cy     : Float;
      dx     : Float;
      dy     : Float;
      xscale : Float;
      yscale : Float;
      angle  : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_tinted_scaled_rotated_bitmap,
      "al_draw_tinted_scaled_rotated_bitmap");

   -- Like al_draw_tinted_scaled_rotated_bitmap but you specify an area within
   --the bitmap to be drawn.
   procedure al_draw_tinted_scaled_rotated_bitmap_region
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      sx     : Float;
      sy     : Float;
      sw     : Float;
      sh     : Float;
      tint   : Color.ALLEGRO_COLOR;
      cx     : Float;
      cy     : Float;
      dx     : Float;
      dy     : Float;
      xscale : Float;
      yscale : Float;
      angle  : Float;
      flags  : int);
   pragma Import
     (C,
      al_draw_tinted_scaled_rotated_bitmap_region,
      "al_draw_tinted_scaled_rotated_bitmap_region");

end Allegro5.Bitmap_Draw;
