with Allegro5.Color;

package Allegro5.Drawing is

   -- Clear the complete target bitmap, but confined by the clipping rectangle.
   procedure al_clear_to_color (color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_clear_to_color, "al_clear_to_color");

   -- Draws a single pixel at x, y. This function, unlike al_put_pixel, does
   --blending and, unlike al_put_blended_pixel, respects the transformations.
   --This function can be slow if called often; if you need to draw a lot of
   --pixels consider using al_draw_prim with ALLEGRO_PRIM_POINT_LIST from
   --the primitives addon.
   procedure al_draw_pixel
     (x : float;
      y : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_pixel, "al_draw_pixel");

end Allegro5.Drawing;
