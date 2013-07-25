with Interfaces.C; use Interfaces.C;


package Allegro5.Color is

   type ALLEGRO_COLOR is record
      r : aliased float;
      g : aliased float;
      b : aliased float;
      a : aliased float;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_COLOR);

   type ALLEGRO_PIXEL_FORMAT is
     (ALLEGRO_PIXEL_FORMAT_ANY,
      ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA,
      ALLEGRO_PIXEL_FORMAT_ARGB_8888,
      ALLEGRO_PIXEL_FORMAT_RGBA_8888,
      ALLEGRO_PIXEL_FORMAT_ARGB_4444,
      ALLEGRO_PIXEL_FORMAT_RGB_888,
      ALLEGRO_PIXEL_FORMAT_RGB_565,
      ALLEGRO_PIXEL_FORMAT_RGB_555,
      ALLEGRO_PIXEL_FORMAT_RGBA_5551,
      ALLEGRO_PIXEL_FORMAT_ARGB_1555,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888,
      ALLEGRO_PIXEL_FORMAT_XBGR_8888,
      ALLEGRO_PIXEL_FORMAT_BGR_888,
      ALLEGRO_PIXEL_FORMAT_BGR_565,
      ALLEGRO_PIXEL_FORMAT_BGR_555,
      ALLEGRO_PIXEL_FORMAT_RGBX_8888,
      ALLEGRO_PIXEL_FORMAT_XRGB_8888,
      ALLEGRO_PIXEL_FORMAT_ABGR_F32,
      ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE,
      ALLEGRO_PIXEL_FORMAT_RGBA_4444,
      ALLEGRO_NUM_PIXEL_FORMATS);
   pragma Convention (C, ALLEGRO_PIXEL_FORMAT);

   function al_map_rgb
     (r : unsigned_char;
      g : unsigned_char;
      b : unsigned_char) return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgb, "al_map_rgb");

   function al_map_rgba
     (r : unsigned_char;
      g : unsigned_char;
      b : unsigned_char;
      a : unsigned_char) return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgba, "al_map_rgba");

   function al_map_rgb_f
     (r : float;
      g : float;
      b : float) return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgb_f, "al_map_rgb_f");

   function al_map_rgba_f
     (r : float;
      g : float;
      b : float;
      a : float) return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgba_f, "al_map_rgba_f");

   procedure al_unmap_rgb
     (color : ALLEGRO_COLOR;
      r : access unsigned_char;
      g : access unsigned_char;
      b : access unsigned_char);
   pragma Import (C, al_unmap_rgb, "al_unmap_rgb");

   procedure al_unmap_rgba
     (color : ALLEGRO_COLOR;
      r : access unsigned_char;
      g : access unsigned_char;
      b : access unsigned_char;
      a : access unsigned_char);
   pragma Import (C, al_unmap_rgba, "al_unmap_rgba");

   procedure al_unmap_rgb_f
     (color : ALLEGRO_COLOR;
      r : access float;
      g : access float;
      b : access float);
   pragma Import (C, al_unmap_rgb_f, "al_unmap_rgb_f");

   procedure al_unmap_rgba_f
     (color : ALLEGRO_COLOR;
      r : access float;
      g : access float;
      b : access float;
      a : access float);
   pragma Import (C, al_unmap_rgba_f, "al_unmap_rgba_f");

   function al_get_pixel_size (format : int) return int;
   pragma Import (C, al_get_pixel_size, "al_get_pixel_size");

   function al_get_pixel_format_bits (format : int) return int;
   pragma Import (C, al_get_pixel_format_bits, "al_get_pixel_format_bits");

end Allegro5.Color;
