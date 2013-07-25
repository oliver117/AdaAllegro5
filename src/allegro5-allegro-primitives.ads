with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with stdint;
with System;

with Allegro5.Bitmap;
with Allegro5.Color;

use Allegro5;


package Allegro5.Allegro.Primitives is

   ALLEGRO_VERTEX_CACHE_SIZE : constant := 256;
   ALLEGRO_PRIM_QUALITY : constant := 10;

   type ALLEGRO_PRIM_TYPE is
     (ALLEGRO_PRIM_LINE_LIST,
      ALLEGRO_PRIM_LINE_STRIP,
      ALLEGRO_PRIM_LINE_LOOP,
      ALLEGRO_PRIM_TRIANGLE_LIST,
      ALLEGRO_PRIM_TRIANGLE_STRIP,
      ALLEGRO_PRIM_TRIANGLE_FAN,
      ALLEGRO_PRIM_POINT_LIST,
      ALLEGRO_PRIM_NUM_TYPES);
   pragma Convention (C, ALLEGRO_PRIM_TYPE);

   subtype ALLEGRO_PRIM_ATTR is unsigned;
   ALLEGRO_PRIM_POSITION : constant ALLEGRO_PRIM_ATTR := 1;
   ALLEGRO_PRIM_COLOR_ATTR : constant ALLEGRO_PRIM_ATTR := 2;
   ALLEGRO_PRIM_TEX_COORD : constant ALLEGRO_PRIM_ATTR := 3;
   ALLEGRO_PRIM_TEX_COORD_PIXEL : constant ALLEGRO_PRIM_ATTR := 4;
   ALLEGRO_PRIM_ATTR_NUM : constant ALLEGRO_PRIM_ATTR := 5;

   type ALLEGRO_PRIM_STORAGE is
     (ALLEGRO_PRIM_FLOAT_2,
      ALLEGRO_PRIM_FLOAT_3,
      ALLEGRO_PRIM_SHORT_2);
   pragma Convention (C, ALLEGRO_PRIM_STORAGE);

   type ALLEGRO_VERTEX_ELEMENT is record
      attribute : aliased int;
      storage : aliased int;
      offset : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_VERTEX_ELEMENT);

   type ALLEGRO_VERTEX_DECL is new System.Address;

   type ALLEGRO_VERTEX is record
      x : aliased float;
      y : aliased float;
      z : aliased float;
      u : aliased float;
      v : aliased float;
      color : aliased Allegro5.Color.ALLEGRO_COLOR;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_VERTEX);

   function al_get_allegro_primitives_version return stdint.uint32_t;
   pragma Import (C, al_get_allegro_primitives_version, "al_get_allegro_primitives_version");

   function al_init_primitives_addon return Extensions.bool;
   pragma Import (C, al_init_primitives_addon, "al_init_primitives_addon");

   procedure al_shutdown_primitives_addon;
   pragma Import (C, al_shutdown_primitives_addon, "al_shutdown_primitives_addon");

   function al_draw_prim
     (vtxs : System.Address;
      decl : ALLEGRO_VERTEX_DECL;
      texture : Bitmap.ALLEGRO_BITMAP;
      start : int;
      c_end : int;
      c_type : int) return int;
   pragma Import (C, al_draw_prim, "al_draw_prim");

   function al_draw_indexed_prim
     (vtxs : System.Address;
      decl : ALLEGRO_VERTEX_DECL;
      texture : Bitmap.ALLEGRO_BITMAP;
      indices : access int;
      num_vtx : int;
      c_type : int) return int;
   pragma Import (C, al_draw_indexed_prim, "al_draw_indexed_prim");

   function al_create_vertex_decl (elements : ALLEGRO_VERTEX_ELEMENT; stride : int) return ALLEGRO_VERTEX_DECL;
   pragma Import (C, al_create_vertex_decl, "al_create_vertex_decl");

   procedure al_destroy_vertex_decl (decl : ALLEGRO_VERTEX_DECL);
   pragma Import (C, al_destroy_vertex_decl, "al_destroy_vertex_decl");

   procedure al_draw_soft_triangle
     (v1 : access ALLEGRO_VERTEX;
      v2 : access ALLEGRO_VERTEX;
      v3 : access ALLEGRO_VERTEX;
      state : stdint.uintptr_t;
      init : access procedure
        (arg1 : stdint.uintptr_t;
         arg2 : access ALLEGRO_VERTEX;
         arg3 : access ALLEGRO_VERTEX;
         arg4 : access ALLEGRO_VERTEX);
      first : access procedure
        (arg1 : stdint.uintptr_t;
         arg2 : int;
         arg3 : int;
         arg4 : int;
         arg5 : int);
      step : access procedure (arg1 : stdint.uintptr_t; arg2 : int);
      draw : access procedure
        (arg1 : stdint.uintptr_t;
         arg2 : int;
         arg3 : int;
         arg4 : int));
   pragma Import (C, al_draw_soft_triangle, "al_draw_soft_triangle");

   procedure al_draw_soft_line
     (v1 : access ALLEGRO_VERTEX;
      v2 : access ALLEGRO_VERTEX;
      state : stdint.uintptr_t;
      first : access procedure
        (arg1 : stdint.uintptr_t;
         arg2 : int;
         arg3 : int;
         arg4 : access ALLEGRO_VERTEX;
         arg5 : access ALLEGRO_VERTEX);
      step : access procedure (arg1 : stdint.uintptr_t; arg2 : int);
      draw : access procedure
        (arg1 : stdint.uintptr_t;
         arg2 : int;
         arg3 : int));
   pragma Import (C, al_draw_soft_line, "al_draw_soft_line");

   procedure al_draw_line
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_line, "al_draw_line");

   procedure al_draw_triangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      x3 : float;
      y3 : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_triangle, "al_draw_triangle");

   procedure al_draw_rectangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_rectangle, "al_draw_rectangle");

   procedure al_draw_rounded_rectangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      rx : float;
      ry : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_rounded_rectangle, "al_draw_rounded_rectangle");

   procedure al_calculate_arc
     (dest : access float;
      stride : int;
      cx : float;
      cy : float;
      rx : float;
      ry : float;
      start_theta : float;
      delta_theta : float;
      thickness : float;
      num_segments : int);
   pragma Import (C, al_calculate_arc, "al_calculate_arc");

   procedure al_draw_circle
     (cx : float;
      cy : float;
      r : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_circle, "al_draw_circle");

   procedure al_draw_ellipse
     (cx : float;
      cy : float;
      rx : float;
      ry : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_ellipse, "al_draw_ellipse");

   procedure al_draw_arc
     (cx : float;
      cy : float;
      r : float;
      start_theta : float;
      delta_theta : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_arc, "al_draw_arc");

   procedure al_draw_elliptical_arc
     (cx : float;
      cy : float;
      rx : float;
      ry : float;
      start_theta : float;
      delta_theta : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_elliptical_arc, "al_draw_elliptical_arc");

   procedure al_draw_pieslice
     (cx : float;
      cy : float;
      r : float;
      start_theta : float;
      delta_theta : float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_pieslice, "al_draw_pieslice");

   procedure al_calculate_spline
     (dest : access float;
      stride : int;
      points : access float;
      thickness : float;
      num_segments : int);
   pragma Import (C, al_calculate_spline, "al_calculate_spline");

   procedure al_draw_spline
     (points : access float;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float);
   pragma Import (C, al_draw_spline, "al_draw_spline");

   procedure al_calculate_ribbon
     (dest : access float;
      dest_stride : int;
      points : access float;
      points_stride : int;
      thickness : float;
      num_segments : int);
   pragma Import (C, al_calculate_ribbon, "al_calculate_ribbon");

   procedure al_draw_ribbon
     (points : access float;
      points_stride : int;
      color : Allegro5.Color.ALLEGRO_COLOR;
      thickness : float;
      num_segments : int);
   pragma Import (C, al_draw_ribbon, "al_draw_ribbon");

   procedure al_draw_filled_triangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      x3 : float;
      y3 : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_triangle, "al_draw_filled_triangle");

   procedure al_draw_filled_rectangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_rectangle, "al_draw_filled_rectangle");

   procedure al_draw_filled_ellipse
     (cx : float;
      cy : float;
      rx : float;
      ry : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_ellipse, "al_draw_filled_ellipse");

   procedure al_draw_filled_circle
     (cx : float;
      cy : float;
      r : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_circle, "al_draw_filled_circle");

   procedure al_draw_filled_pieslice
     (cx : float;
      cy : float;
      r : float;
      start_theta : float;
      delta_theta : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_pieslice, "al_draw_filled_pieslice");

   procedure al_draw_filled_rounded_rectangle
     (x1 : float;
      y1 : float;
      x2 : float;
      y2 : float;
      rx : float;
      ry : float;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_draw_filled_rounded_rectangle, "al_draw_filled_rounded_rectangle");

end Allegro5.Allegro.Primitives;
