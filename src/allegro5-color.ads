with Interfaces.C; use Interfaces.C;

package Allegro5.Color is

   -- An ALLEGRO_COLOR structure describes a color in a device independent
   --way. Use al_map_rgb et al. and al_unmap_rgb et al. to translate from and
   --to various color representations.
   type ALLEGRO_COLOR is record
      r : aliased Float;
      g : aliased Float;
      b : aliased Float;
      a : aliased Float;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_COLOR);

   -- Pixel formats. Each pixel format specifies the exact size and bit layout
   --of a pixel in memory. Components are specified from high bits to low
   --bits, so for example a fully opaque red pixel in ARGB_8888 format is
   --0xFFFF0000.
   type ALLEGRO_PIXEL_FORMAT is (
      ALLEGRO_PIXEL_FORMAT_ANY,
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

   -- Convert r, g, b (ranging from 0-255) into an ALLEGRO_COLOR, using 255
   --for alpha.
   function al_map_rgb
     (r    : unsigned_char;
      g    : unsigned_char;
      b    : unsigned_char)
      return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgb, "al_map_rgb");

   -- Convert r, g, b, a (ranging from 0-255) into an ALLEGRO_COLOR.
   function al_map_rgba
     (r    : unsigned_char;
      g    : unsigned_char;
      b    : unsigned_char;
      a    : unsigned_char)
      return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgba, "al_map_rgba");

   -- Convert r, g, b, (ranging from 0.0f-1.0f) into an ALLEGRO_COLOR, using
   --1.0f for alpha.
   function al_map_rgb_f
     (r    : Float;
      g    : Float;
      b    : Float)
      return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgb_f, "al_map_rgb_f");

   -- Convert r, g, b, a (ranging from 0.0f-1.0f) into an ALLEGRO_COLOR.
   function al_map_rgba_f
     (r    : Float;
      g    : Float;
      b    : Float;
      a    : Float)
      return ALLEGRO_COLOR;
   pragma Import (C, al_map_rgba_f, "al_map_rgba_f");

   -- Retrieves components of an ALLEGRO_COLOR, ignoring alpha Components will
   --range from 0-255.
   procedure al_unmap_rgb
     (color : ALLEGRO_COLOR;
      r     : access unsigned_char;
      g     : access unsigned_char;
      b     : access unsigned_char);
   pragma Import (C, al_unmap_rgb, "al_unmap_rgb");

   -- Retrieves components of an ALLEGRO_COLOR. Components will range from
   --0-255.
   procedure al_unmap_rgba
     (color : ALLEGRO_COLOR;
      r     : access unsigned_char;
      g     : access unsigned_char;
      b     : access unsigned_char;
      a     : access unsigned_char);
   pragma Import (C, al_unmap_rgba, "al_unmap_rgba");

   -- Retrieves components of an ALLEGRO_COLOR, ignoring alpha. Components
   --will range from 0.0f-1.0f.
   procedure al_unmap_rgb_f
     (color : ALLEGRO_COLOR;
      r     : access Float;
      g     : access Float;
      b     : access Float);
   pragma Import (C, al_unmap_rgb_f, "al_unmap_rgb_f");

   -- Retrieves components of an ALLEGRO_COLOR. Components will range from
   --0.0f-1.0f.
   procedure al_unmap_rgba_f
     (color : ALLEGRO_COLOR;
      r     : access Float;
      g     : access Float;
      b     : access Float;
      a     : access Float);
   pragma Import (C, al_unmap_rgba_f, "al_unmap_rgba_f");

   -- Return the number of bytes that a pixel of the given format occupies.
   function al_get_pixel_size (format : int) return int;
   pragma Import (C, al_get_pixel_size, "al_get_pixel_size");

   -- Return the number of bits that a pixel of the given format occupies.
   function al_get_pixel_format_bits (format : int) return int;
   pragma Import (C, al_get_pixel_format_bits, "al_get_pixel_format_bits");

end Allegro5.Color;
