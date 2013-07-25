with Interfaces.C; use Interfaces.C;
with System;
with Allegro5.Color;

package Allegro5.Bitmap_Draw is

   procedure al_draw_bitmap
     (bitmap : System.Address;
      dx : float;
      dy : float;
      flags : int);
   pragma Import (C, al_draw_bitmap, "al_draw_bitmap");

   procedure al_draw_bitmap_region
     (bitmap : System.Address;
      sx : float;
      sy : float;
      sw : float;
      sh : float;
      dx : float;
      dy : float;
      flags : int);
   pragma Import (C, al_draw_bitmap_region, "al_draw_bitmap_region");

   procedure al_draw_scaled_bitmap
     (bitmap : System.Address;
      sx : float;
      sy : float;
      sw : float;
      sh : float;
      dx : float;
      dy : float;
      dw : float;
      dh : float;
      flags : int);
   pragma Import (C, al_draw_scaled_bitmap, "al_draw_scaled_bitmap");

   procedure al_draw_rotated_bitmap
     (bitmap : System.Address;
      cx : float;
      cy : float;
      dx : float;
      dy : float;
      angle : float;
      flags : int);
   pragma Import (C, al_draw_rotated_bitmap, "al_draw_rotated_bitmap");

   procedure al_draw_scaled_rotated_bitmap
     (bitmap : System.Address;
      cx : float;
      cy : float;
      dx : float;
      dy : float;
      xscale : float;
      yscale : float;
      angle : float;
      flags : int);
   pragma Import (C, al_draw_scaled_rotated_bitmap, "al_draw_scaled_rotated_bitmap");

   procedure al_draw_tinted_bitmap
     (bitmap : System.Address;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      dx : float;
      dy : float;
      flags : int);
   pragma Import (C, al_draw_tinted_bitmap, "al_draw_tinted_bitmap");

   procedure al_draw_tinted_bitmap_region
     (bitmap : System.Address;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      sx : float;
      sy : float;
      sw : float;
      sh : float;
      dx : float;
      dy : float;
      flags : int);
   pragma Import (C, al_draw_tinted_bitmap_region, "al_draw_tinted_bitmap_region");

   procedure al_draw_tinted_scaled_bitmap
     (bitmap : System.Address;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      sx : float;
      sy : float;
      sw : float;
      sh : float;
      dx : float;
      dy : float;
      dw : float;
      dh : float;
      flags : int);
   pragma Import (C, al_draw_tinted_scaled_bitmap, "al_draw_tinted_scaled_bitmap");

   procedure al_draw_tinted_rotated_bitmap
     (bitmap : System.Address;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      cx : float;
      cy : float;
      dx : float;
      dy : float;
      angle : float;
      flags : int);
   pragma Import (C, al_draw_tinted_rotated_bitmap, "al_draw_tinted_rotated_bitmap");

   procedure al_draw_tinted_scaled_rotated_bitmap
     (bitmap : System.Address;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      cx : float;
      cy : float;
      dx : float;
      dy : float;
      xscale : float;
      yscale : float;
      angle : float;
      flags : int);
   pragma Import (C, al_draw_tinted_scaled_rotated_bitmap, "al_draw_tinted_scaled_rotated_bitmap");

   procedure al_draw_tinted_scaled_rotated_bitmap_region
     (bitmap : System.Address;
      sx : float;
      sy : float;
      sw : float;
      sh : float;
      tint : Allegro5.Color.ALLEGRO_COLOR;
      cx : float;
      cy : float;
      dx : float;
      dy : float;
      xscale : float;
      yscale : float;
      angle : float;
      flags : int);
   pragma Import (C, al_draw_tinted_scaled_rotated_bitmap_region, "al_draw_tinted_scaled_rotated_bitmap_region");

end Allegro5.Bitmap_Draw;
