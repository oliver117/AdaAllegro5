with Interfaces.C; use Interfaces.C;

package Allegro5.Blender is

   type ALLEGRO_BLEND_MODE is (
      ALLEGRO_ZERO,
      ALLEGRO_ONE,
      ALLEGRO_ALPHA,
      ALLEGRO_INVERSE_ALPHA,
      ALLEGRO_SRC_COLOR,
      ALLEGRO_DEST_COLOR,
      ALLEGRO_INVERSE_SRC_COLOR,
      ALLEGRO_INVERSE_DEST_COLOR,
      ALLEGRO_NUM_BLEND_MODES);
   pragma Convention (C, ALLEGRO_BLEND_MODE);

   type ALLEGRO_BLEND_OPERATIONS is (
      ALLEGRO_ADD,
      ALLEGRO_SRC_MINUS_DEST,
      ALLEGRO_DEST_MINUS_SRC,
      ALLEGRO_NUM_BLEND_OPERATIONS);
   pragma Convention (C, ALLEGRO_BLEND_OPERATIONS);

   -- Sets the function to use for blending for the current thread.
   --
   -- Blending means, the source and destination colors are combined in
   --drawing operations.
   --
   -- Assume the source color (e.g. color of a rectangle to draw, or pixel of
   --a bitmap to draw) is given as its red/green/blue/alpha components (if the
   --bitmap has no alpha it always is assumed to be fully opaque, so 255 for
   --8-bit or 1.0 for floating point): sr, sg, sb, sa. And this color is drawn
   --to a destination, which already has a color: dr, dg, db, da.
   procedure al_set_blender (op : int; source : int; dest : int);
   pragma Import (C, al_set_blender, "al_set_blender");

   -- Returns the active blender for the current thread. You can pass NULL for
   --values you are not interested in.
   procedure al_get_blender
     (op     : out int;
      source : out int;
      dest   : out int);
   pragma Import (C, al_get_blender, "al_get_blender");

   -- Like al_set_blender, but allows specifying a separate blending operation
   --for the alpha channel. This is useful if your target bitmap also has an
   --alpha channel and the two alpha channels need to be combined in a
   --different way than the color components.
   procedure al_set_separate_blender
     (op           : int;
      source       : int;
      dest         : int;
      alpha_op     : int;
      alpha_source : int;
      alpha_dest   : int);
   pragma Import (C, al_set_separate_blender, "al_set_separate_blender");

   -- Returns the active blender for the current thread. You can pass NULL for
   --values you are not interested in.
   procedure al_get_separate_blender
     (op         : out int;
      source     : out int;
      dest       : out int;
      alpha_op   : out int;
      alpha_src  : out int;
      alpha_dest : out int);
   pragma Import (C, al_get_separate_blender, "al_get_separate_blender");

end Allegro5.Blender;
