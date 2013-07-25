with Interfaces.C; use Interfaces.C;
with Allegro5.Color;


package Allegro5.Drawing is

   procedure al_clear_to_color (color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_clear_to_color, "al_clear_to_color");

   procedure al_draw_pixel
     (x : float;
      y : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_pixel, "al_draw_pixel");

end Allegro5.Drawing;
