with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with stdint;
with System;

with Allegro5.Bitmap;
with Allegro5.Color;
with Allegro5.UTF8;

use  Allegro5;

package Allegro5.Allegro.Font is

   type ALLEGRO_FONT_VTABLE;

   -- A handle identifying any kind of font. Usually you will create it with
   --al_load_font which supports loading all kinds of TrueType fonts supported
   --by the FreeType library. If you instead pass the filename of a bitmap
   --file, it will be loaded with al_load_bitmap and a font in Allegro's
   --bitmap font format will be created from it with al_grab_font_from_bitmap.
   type ALLEGRO_FONT is record
      data   : System.Address;
      height : aliased int;
      vtable : access ALLEGRO_FONT_VTABLE;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FONT);

   type ALLEGRO_FONT_VTABLE is record
      font_height         : access function (f : ALLEGRO_FONT) return int;
      font_ascent         : access function (f : ALLEGRO_FONT) return int;
      font_descent        : access function (f : ALLEGRO_FONT) return int;
      char_length         : access function
        (f    : ALLEGRO_FONT;
         ch   : int)
         return int;
      text_length         : access function
        (f    : ALLEGRO_FONT;
         text : access constant UTF8.ALLEGRO_USTR)
         return int;
      render_char         : access function
        (f     : ALLEGRO_FONT;
         color : Allegro5.Color.ALLEGRO_COLOR;
         ch    : int;
         x     : Float;
         y     : Float)
         return  int;
      render              : access function
        (f     : ALLEGRO_FONT;
         color : Allegro5.Color.ALLEGRO_COLOR;
         text  : access constant UTF8.ALLEGRO_USTR;
         x     : Float;
         y     : Float)
         return  int;
      destroy             : access procedure (f : access ALLEGRO_FONT);
      get_text_dimensions : access procedure
        (f    : ALLEGRO_FONT;
         text : access constant UTF8.ALLEGRO_USTR;
         bbx  : access int;
         bby  : access int;
         bbw  : access int;
         bbh  : access int);
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FONT_VTABLE);

   subtype ALLEGRO_ALIGN is int;
   ALLEGRO_ALIGN_LEFT    : ALLEGRO_ALIGN := 0;
   ALLEGRO_ALIGN_CENTRE  : ALLEGRO_ALIGN := 1;
   ALLEGRO_ALIGN_CENTER  : ALLEGRO_ALIGN := 1;
   ALLEGRO_ALIGN_RIGHT   : ALLEGRO_ALIGN := 2;
   ALLEGRO_ALIGN_INTEGER : ALLEGRO_ALIGN := 4;

   -- Informs Allegro of a new font file type, telling it how to load files of
   --this format.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The load_font argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_font_loader
     (ext    : Interfaces.C.Strings.chars_ptr;
      loader : access function
     (filename : Interfaces.C.Strings.chars_ptr;
      size     : int;
      flags    : int)
      return     access ALLEGRO_FONT)
      return   Extensions.bool;
   pragma Import (C, al_register_font_loader, "al_register_font_loader");

   -- Load a bitmap font from. It does this by first calling al_load_bitmap
   --and then al_grab_font_from_bitmap. If you want to for example load an old
   --A4 font, you could load the bitmap yourself, then call
   --al_convert_mask_to_alpha on it and only then pass it to
   --al_grab_font_from_bitmap.
   function al_load_bitmap_font
     (filename : Interfaces.C.Strings.chars_ptr)
      return     access ALLEGRO_FONT;
   pragma Import (C, al_load_bitmap_font, "al_load_bitmap_font");

   -- Loads a font from disk. This will use al_load_bitmap_font if you pass
   --the name of a known bitmap format, or else al_load_ttf_font.
   --
   -- Bitmap and TTF fonts are affected by the current bitmap flags at the
   --time the font is loaded.
   function al_load_font
     (filename : Interfaces.C.Strings.chars_ptr;
      size     : int;
      flags    : int)
      return     access ALLEGRO_FONT;
   pragma Import (C, al_load_font, "al_load_font");

   -- Creates a new font from an Allegro bitmap. You can delete the bitmap
   --after the function returns as the font will contain a copy for itself.
   --
   -- Parameters:
   --
   -- bmp: The bitmap with the glyphs drawn onto it
   -- n: Number of unicode ranges in the bitmap.
   -- ranges: 'n' pairs of first and last unicode point to map glyphs to for
   --each range.
   function al_grab_font_from_bitmap
     (bmp    : Bitmap.ALLEGRO_BITMAP;
      n      : int;
      ranges : access int)
      return   access ALLEGRO_FONT;
   pragma Import (C, al_grab_font_from_bitmap, "al_grab_font_from_bitmap");

   -- Creates a monochrome bitmap font (8x8 pixels per character).
   --
   -- This font is primarily intended to be used for displaying information in
   --environments or during early runtime states where no external font data
   --is available or loaded (e.g. for debugging).
   function al_create_builtin_font return access ALLEGRO_FONT;
   pragma Import (C, al_create_builtin_font, "al_create_builtin_font");

   -- Like al_draw_text, except the text is passed as an ALLEGRO_USTR instead
   --of a NUL-terminated char array.
   procedure al_draw_ustr
     (font  : access ALLEGRO_FONT;
      color : Allegro5.Color.ALLEGRO_COLOR;
      x     : Float;
      y     : Float;
      flags : int;
      ustr  : access constant UTF8.ALLEGRO_USTR);
   pragma Import (C, al_draw_ustr, "al_draw_ustr");

   -- Writes the NUL-terminated string text onto the target bitmap at position
   --x, y, using the specified font.
   --
   -- The flags parameter can be 0 or one of the following flags:
   --
   -- ALLEGRO_ALIGN_LEFT - Draw the text left-aligned (same as 0).
   -- ALLEGRO_ALIGN_CENTRE - Draw the text centered around the given position.
   -- ALLEGRO_ALIGN_RIGHT - Draw the text right-aligned to the given position.
   -- It can also be combined with this flag:
   --
   -- ALLEGRO_ALIGN_INTEGER - Always draw text aligned to an integer pixel
   --position. This is formerly the default behaviour.
   procedure al_draw_text
     (font  : access ALLEGRO_FONT;
      color : Allegro5.Color.ALLEGRO_COLOR;
      x     : Float;
      y     : Float;
      flags : int;
      text  : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_draw_text, "al_draw_text");

   -- Like al_draw_text, but justifies the string to the region x1-x2.
   --
   -- The diff parameter is the maximum amount of horizontal space to allow
   --between words. If justisfying the text would exceed diff pixels, or the
   --string contains less than two words, then the string will be drawn left
   --aligned.
   --
   -- The flags parameter can be 0 or one of the following flags:
   --
   -- ALLEGRO_ALIGN_INTEGER - Draw text aligned to integer pixel positions.
   procedure al_draw_justified_text
     (font   : access ALLEGRO_FONT;
      color  : Allegro5.Color.ALLEGRO_COLOR;
      x1     : Float;
      x2     : Float;
      y      : Float;
      diff   : Float;
      flags  : int;
      format : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_draw_justified_text, "al_draw_justified_text");

   -- Like al_draw_justified_text, except the text is passed as an
   --ALLEGRO_USTR instead of a NUL-terminated char array.
   procedure al_draw_justified_ustr
     (font  : access ALLEGRO_FONT;
      color : Allegro5.Color.ALLEGRO_COLOR;
      x1    : Float;
      x2    : Float;
      y     : Float;
      diff  : Float;
      flags : int;
      text  : access constant UTF8.ALLEGRO_USTR);
   pragma Import (C, al_draw_justified_ustr, "al_draw_justified_ustr");

   -- Formatted text output, using a printf() style format string. All
   --parameters have the same meaning as with al_draw_text otherwise.
   procedure al_draw_textf
     (font   : access ALLEGRO_FONT;
      color  : Allegro5.Color.ALLEGRO_COLOR;
      x      : Float;
      y      : Float;
      flags  : int;
      format : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_draw_textf, "al_draw_textf");

   -- Formatted text output, using a printf() style format string. All
   --parameters have the same meaning as with al_draw_justified_text otherwise.
   procedure al_draw_justified_textf
     (font   : access ALLEGRO_FONT;
      color  : Allegro5.Color.ALLEGRO_COLOR;
      x1     : Float;
      x2     : Float;
      y      : Float;
      diff   : Float;
      flags  : int;
      format : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_draw_justified_textf, "al_draw_justified_textf");

   -- Calculates the length of a string in a particular font, in pixels.
   function al_get_text_width
     (f    : access ALLEGRO_FONT;
      str  : Interfaces.C.Strings.chars_ptr)
      return int;
   pragma Import (C, al_get_text_width, "al_get_text_width");

   -- Like al_get_text_width but expects an ALLEGRO_USTR.
   function al_get_ustr_width
     (f    : access ALLEGRO_FONT;
      ustr : access constant UTF8.ALLEGRO_USTR)
      return int;
   pragma Import (C, al_get_ustr_width, "al_get_ustr_width");

   -- Returns the usual height of a line of text in the specified font. For
   --bitmap fonts this is simply the height of all glyph bitmaps. For truetype
   --fonts it is whatever the font file specifies. In particular, some special
   --glyphs may be higher than the height returned here.
   function al_get_font_line_height (f : access ALLEGRO_FONT) return int;
   pragma Import (C, al_get_font_line_height, "al_get_font_line_height");

   -- Returns the ascent of the specified font.
   function al_get_font_ascent (f : access ALLEGRO_FONT) return int;
   pragma Import (C, al_get_font_ascent, "al_get_font_ascent");

   -- Returns the descent of the specified font.
   function al_get_font_descent (f : access ALLEGRO_FONT) return int;
   pragma Import (C, al_get_font_descent, "al_get_font_descent");

   -- Frees the memory being used by a font structure. Does nothing if passed
   --NULL.
   procedure al_destroy_font (f : access ALLEGRO_FONT);
   pragma Import (C, al_destroy_font, "al_destroy_font");

   -- Sometimes, the al_get_ustr_width and al_get_font_line_height functions
   --are not enough for exact text placement, so this function returns some
   --additional information.
   procedure al_get_ustr_dimensions
     (f    : access ALLEGRO_FONT;
      text : access constant UTF8.ALLEGRO_USTR;
      bbx  : access int;
      bby  : access int;
      bbw  : access int;
      bbh  : access int);
   pragma Import (C, al_get_ustr_dimensions, "al_get_ustr_dimensions");

   -- Sometimes, the al_get_text_width and al_get_font_line_height functions
   --are not enough for exact text placement, so this function returns some
   --additional information.
   --
   -- Returned variables (all in pixel):
   --
   -- x, y - Offset to upper left corner of bounding box.
   -- w, h - Dimensions of bounding box.
   -- Note that glyphs may go to the left and upwards of the X, in which case
   --x and y will have negative values.
   procedure al_get_text_dimensions
     (f    : access ALLEGRO_FONT;
      text : Interfaces.C.Strings.chars_ptr;
      bbx  : access int;
      bby  : access int;
      bbw  : access int;
      bbh  : access int);
   pragma Import (C, al_get_text_dimensions, "al_get_text_dimensions");

   -- Initialise the font addon.
   --
   -- Note that if you intend to load bitmap fonts, you will need to
   --initialise allegro_image separately (unless you are using another library
   --to load images).
   procedure al_init_font_addon;
   pragma Import (C, al_init_font_addon, "al_init_font_addon");

   -- Shut down the font addon. This is done automatically at program exit,
   --but can be called any time the user wishes as well.
   procedure al_shutdown_font_addon;
   pragma Import (C, al_shutdown_font_addon, "al_shutdown_font_addon");

   -- Returns the (compiled) version of the addon, in the same format as
   --al_get_allegro_version.
   function al_get_allegro_font_version return  stdint.uint32_t;
   pragma Import
     (C,
      al_get_allegro_font_version,
      "al_get_allegro_font_version");

end Allegro5.Allegro.Font;
