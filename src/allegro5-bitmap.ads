with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Color;

package Allegro5.Bitmap is

   -- Abstract type representing a bitmap (2D image).
   type ALLEGRO_BITMAP is private;
   No_Bitmap : constant ALLEGRO_BITMAP;

   -- Sets the pixel format for newly created bitmaps. The default format is 0
   --and means the display driver will choose the best format.
   procedure al_set_new_bitmap_format (format : int);
   pragma Import (C, al_set_new_bitmap_format, "al_set_new_bitmap_format");

   -- Sets the flags to use for newly created bitmaps.
   procedure al_set_new_bitmap_flags (flags : int);
   pragma Import (C, al_set_new_bitmap_flags, "al_set_new_bitmap_flags");

   -- Returns the format used for newly created bitmaps.
   function al_get_new_bitmap_format return int;
   pragma Import (C, al_get_new_bitmap_format, "al_get_new_bitmap_format");

   -- Returns the flags used for newly created bitmaps.
   function al_get_new_bitmap_flags return int;
   pragma Import (C, al_get_new_bitmap_flags, "al_get_new_bitmap_flags");

   -- A convenience function which does the same as
   --
   --al_set_new_bitmap_flags(al_get_new_bitmap_flags() | flag);
   procedure al_add_new_bitmap_flag (flag : int);
   pragma Import (C, al_add_new_bitmap_flag, "al_add_new_bitmap_flag");

   -- Returns the width of a bitmap in pixels.
   function al_get_bitmap_width (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_width, "al_get_bitmap_width");

   -- Returns the height of a bitmap in pixels.
   function al_get_bitmap_height (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_height, "al_get_bitmap_height");

   -- Returns the pixel format of a bitmap.
   function al_get_bitmap_format (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_format, "al_get_bitmap_format");

   -- Return the flags used to create the bitmap.
   function al_get_bitmap_flags (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_flags, "al_get_bitmap_flags");

   -- Creates a new bitmap using the bitmap format and flags for the current
   --thread. Blitting between bitmaps of differing formats, or blitting
   --between memory bitmaps and display bitmaps may be slow.
   --
   -- Unless you set the ALLEGRO_MEMORY_BITMAP flag, the bitmap is created for
   --the current display. Blitting to another display may be slow.
   --
   -- If a display bitmap is created, there may be limitations on the allowed
   --dimensions. For example a DirectX or OpenGL backend usually has a maximum
   --allowed texture size - so if bitmap creation fails for very large
   --dimensions, you may want to re-try with a smaller bitmap. Some platforms
   --also dictate a minimum texture size, which is relevant if you plan to use
   --this bitmap with the primitives addon. If you try to create a bitmap
   --smaller than this, this call will not fail but the returned bitmap will
   --be a section of a larger bitmap with the minimum size. The minimum size
   --that will work on all platforms is 32 by 32.
   --
   -- Some platforms do not directly support display bitmaps whose dimensions
   --are not powers of two. Allegro handles this by creating a larger bitmap
   --that has dimensions that are powers of two and then returning a section
   --of that bitmap with the dimensions you requested. This can be relevant if
   --you plan to use this bitmap with the primitives addon but shouldn't be an
   --issue otherwise.
   function al_create_bitmap (w : int; h : int) return ALLEGRO_BITMAP;
   pragma Import (C, al_create_bitmap, "al_create_bitmap");

   -- Destroys the given bitmap, freeing all resources used by it. This
   --function does nothing if the bitmap argument is NULL.
   --
   -- As a convenience, if the calling thread is currently targets the bitmap
   --then the bitmap will be untargeted first. The new target bitmap is
   --unspecified. (since: 5.0.10, 5.1.6)
   --
   -- Otherwise, it is an error to destroy a bitmap while it (or a sub-bitmap)
   --is the target bitmap of any thread.
   procedure al_destroy_bitmap (bitmap : ALLEGRO_BITMAP);
   pragma Import (C, al_destroy_bitmap, "al_destroy_bitmap");

   -- Draw a single pixel on the target bitmap. This operation is slow on
   --non-memory bitmaps. Consider locking the bitmap if you are going to use
   --this function multiple times on the same bitmap. This function is not
   --affected by the transformations or the color blenders.
   procedure al_put_pixel
     (x     : int;
      y     : int;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_put_pixel, "al_put_pixel");

   -- Like al_put_pixel, but the pixel color is blended using the current
   --blenders before being drawn.
   procedure al_put_blended_pixel
     (x     : int;
      y     : int;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_put_blended_pixel, "al_put_blended_pixel");

   -- Get a pixel's color value from the specified bitmap. This operation is
   --slow on non-memory bitmaps. Consider locking the bitmap if you are going
   --to use this function multiple times on the same bitmap.
   function al_get_pixel
     (bitmap : ALLEGRO_BITMAP;
      x      : int;
      y      : int)
      return   Color.ALLEGRO_COLOR;
   pragma Import (C, al_get_pixel, "al_get_pixel");

   -- Convert the given mask color to an alpha channel in the bitmap. Can be
   --used to convert older 4.2-style bitmaps with magic pink to alpha-ready
   --bitmaps.
   procedure al_convert_mask_to_alpha
     (bitmap     : ALLEGRO_BITMAP;
      mask_color : Color.ALLEGRO_COLOR);
   pragma Import (C, al_convert_mask_to_alpha, "al_convert_mask_to_alpha");

   -- Set the region of the target bitmap or display that pixels get clipped
   --to. The default is to clip pixels to the entire bitmap.
   procedure al_set_clipping_rectangle
     (x      : int;
      y      : int;
      width  : int;
      height : int);
   pragma Import (C, al_set_clipping_rectangle, "al_set_clipping_rectangle");

   -- Equivalent to calling `al_set_clipping_rectangle(0, 0, w, h)' where w
   --and h are the width and height of the target bitmap respectively.
   --
   -- Does nothing if there is no target bitmap.
   procedure al_reset_clipping_rectangle;
   pragma Import
     (C,
      al_reset_clipping_rectangle,
      "al_reset_clipping_rectangle");

   -- Gets the clipping rectangle of the target bitmap.
   procedure al_get_clipping_rectangle
     (x : access int;
      y : access int;
      w : access int;
      h : access int);
   pragma Import (C, al_get_clipping_rectangle, "al_get_clipping_rectangle");

   -- Creates a sub-bitmap of the parent, at the specified coordinates and of
   --the specified size. A sub-bitmap is a bitmap that shares drawing memory
   --with a pre-existing (parent) bitmap, but possibly with a different size
   --and clipping settings.
   --
   -- The sub-bitmap may originate off or extend past the parent bitmap.
   --
   -- See the discussion in al_get_backbuffer about using sub-bitmaps of the
   --backbuffer.
   --
   -- The parent bitmap's clipping rectangles are ignored.
   --
   -- If a sub-bitmap was not or cannot be created then NULL is returned.
   --
   -- Note that destroying parents of sub-bitmaps will not destroy the
   --sub-bitmaps; instead the sub-bitmaps become invalid and should no longer
   --be used.
   function al_create_sub_bitmap
     (parent : ALLEGRO_BITMAP;
      x      : int;
      y      : int;
      w      : int;
      h      : int)
      return   ALLEGRO_BITMAP;
   pragma Import (C, al_create_sub_bitmap, "al_create_sub_bitmap");

   -- Returns true if the specified bitmap is a sub-bitmap, false otherwise.
   function al_is_sub_bitmap
     (bitmap : ALLEGRO_BITMAP)
      return   Extensions.bool;
   pragma Import (C, al_is_sub_bitmap, "al_is_sub_bitmap");

   -- Returns the bitmap this bitmap is a sub-bitmap of. Returns NULL if this bitmap is not a sub-bitmap.
   function al_get_parent_bitmap
     (bitmap : ALLEGRO_BITMAP)
      return   ALLEGRO_BITMAP;
   pragma Import (C, al_get_parent_bitmap, "al_get_parent_bitmap");

   -- Create a new bitmap with al_create_bitmap, and copy the pixel data from the old bitmap across.
   function al_clone_bitmap (bitmap : ALLEGRO_BITMAP) return ALLEGRO_BITMAP;
   pragma Import (C, al_clone_bitmap, "al_clone_bitmap");

private
   type ALLEGRO_BITMAP is new System.Address;
   No_Bitmap : constant ALLEGRO_BITMAP := ALLEGRO_BITMAP (System.Null_Address);
end Allegro5.Bitmap;
