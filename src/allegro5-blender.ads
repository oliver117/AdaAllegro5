with Interfaces.C; use Interfaces.C;


package Allegro5.Blender is

   type ALLEGRO_BLEND_MODE is 
     (ALLEGRO_ZERO,
      ALLEGRO_ONE,
      ALLEGRO_ALPHA,
      ALLEGRO_INVERSE_ALPHA,
      ALLEGRO_SRC_COLOR,
      ALLEGRO_DEST_COLOR,
      ALLEGRO_INVERSE_SRC_COLOR,
      ALLEGRO_INVERSE_DEST_COLOR,
      ALLEGRO_NUM_BLEND_MODES);
   pragma Convention (C, ALLEGRO_BLEND_MODE);

   type ALLEGRO_BLEND_OPERATIONS is 
     (ALLEGRO_ADD,
      ALLEGRO_SRC_MINUS_DEST,
      ALLEGRO_DEST_MINUS_SRC,
      ALLEGRO_NUM_BLEND_OPERATIONS);
   pragma Convention (C, ALLEGRO_BLEND_OPERATIONS);

   procedure al_set_blender
     (op : int;
      source : int;
      dest : int);
   pragma Import (C, al_set_blender, "al_set_blender");

   procedure al_get_blender
     (op : access int;
      source : access int;
      dest : access int);
   pragma Import (C, al_get_blender, "al_get_blender");

   procedure al_set_separate_blender
     (op : int;
      source : int;
      dest : int;
      alpha_op : int;
      alpha_source : int;
      alpha_dest : int);
   pragma Import (C, al_set_separate_blender, "al_set_separate_blender");

   procedure al_get_separate_blender
     (op : access int;
      source : access int;
      dest : access int;
      alpha_op : access int;
      alpha_src : access int;
      alpha_dest : access int);
   pragma Import (C, al_get_separate_blender, "al_get_separate_blender");

end Allegro5.Blender;
