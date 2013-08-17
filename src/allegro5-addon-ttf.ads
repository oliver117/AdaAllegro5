with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

with stdint;

with Allegro5.Addon.Font;
with Allegro5.File;

package Allegro5.Addon.TTF is

   -- Do not use any kerning even if the font file supports it.
   ALLEGRO_TTF_NO_KERNING : constant := 1;

   --  Load as a monochrome font (which means no anti-aliasing of the font is
   --done).
   ALLEGRO_TTF_MONOCHROME : constant := 2;

   -- isable the Auto Hinter which is enabled by default in newer versions of
   --FreeType.
   ALLEGRO_TTF_NO_AUTOHINT : constant := 4;

   -- Loads a TrueType font from a file using the FreeType library. Quoting
   --from the FreeType FAQ this means support for many different font formats:
   --
   -- TrueType, OpenType, Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others
   --
   -- The size parameter determines the size the font will be rendered at,
   --specified in pixels. The standard font size is measured in units per EM,
   --if you instead want to specify the size as the total height of glyphs in
   --pixels, pass it as a negative value.
   function al_load_ttf_font
     (filename : Interfaces.C.Strings.chars_ptr;
      size     : int;
      flags    : int)
      return     access Addon.Font.ALLEGRO_FONT;
   pragma Import (C, al_load_ttf_font, "al_load_ttf_font");

   -- Like al_load_ttf_font, but the font is read from the file handle. The
   --filename is only used to find possible additional files next to a font
   --file.
   function al_load_ttf_font_f
     (file     : Allegro5.File.ALLEGRO_FILE;
      filename : Interfaces.C.Strings.chars_ptr;
      size     : int;
      flags    : int)
      return     access Addon.Font.ALLEGRO_FONT;
   pragma Import (C, al_load_ttf_font_f, "al_load_ttf_font_f");

   -- Like al_load_ttf_font, except it takes separate width and height
   --parameters instead of a single size parameter.
   --
   -- If the height is a positive value, and the width zero or positive, then
   --font will be stretched according to those parameters. The width must not
   --be negative if the height is positive.
   --
   -- As with al_load_ttf_font, the height may be a negative value to specify
   --the total height in pixels. Then the width must also be a negative value,
   --or zero.
   --
   -- The behaviour is undefined the height is positive while width is
   --negative, or if the height is negative while the width is positive.
   function al_load_ttf_font_stretch
     (filename : Interfaces.C.Strings.chars_ptr;
      w        : int;
      h        : int;
      flags    : int)
      return     access Addon.Font.ALLEGRO_FONT;
   pragma Import (C, al_load_ttf_font_stretch, "al_load_ttf_font_stretch");

   -- Like al_load_ttf_font_stretch, but the font is read from the file
   --handle. The filename is only used to find possible additional files next
   --to a font file.
   function al_load_ttf_font_stretch_f
     (file     : Allegro5.File.ALLEGRO_FILE;
      filename : Interfaces.C.Strings.chars_ptr;
      w        : int;
      h        : int;
      flags    : int)
      return     access Addon.Font.ALLEGRO_FONT;
   pragma Import
     (C,
      al_load_ttf_font_stretch_f,
      "al_load_ttf_font_stretch_f");

   -- Call this after al_init_font_addon to make al_load_font recognize ".ttf"
   --and other formats supported by al_load_ttf_font.
   --
   -- Returns true on success, false on failure.
   function al_init_ttf_addon return  Extensions.bool;
   pragma Import (C, al_init_ttf_addon, "al_init_ttf_addon");

   -- Unloads the ttf addon again. You normally don't need to call this.
   procedure al_shutdown_ttf_addon;
   pragma Import (C, al_shutdown_ttf_addon, "al_shutdown_ttf_addon");

   -- Returns the (compiled) version of the addon, in the same format as
   --al_get_allegro_version.
   function al_get_allegro_ttf_version return  stdint.uint32_t;
   pragma Import
     (C,
      al_get_allegro_ttf_version,
      "al_get_allegro_ttf_version");

end Allegro5.Addon.TTF;
