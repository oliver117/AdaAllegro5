with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Color;


package Allegro5.Bitmap is

   subtype ALLEGRO_BITMAP is Extensions.opaque_structure_def;

   procedure al_set_new_bitmap_format (format : int);
   pragma Import (C, al_set_new_bitmap_format, "al_set_new_bitmap_format");

   procedure al_set_new_bitmap_flags (flags : int);
   pragma Import (C, al_set_new_bitmap_flags, "al_set_new_bitmap_flags");

   function al_get_new_bitmap_format return int;
   pragma Import (C, al_get_new_bitmap_format, "al_get_new_bitmap_format");

   function al_get_new_bitmap_flags return int;
   pragma Import (C, al_get_new_bitmap_flags, "al_get_new_bitmap_flags");

   procedure al_add_new_bitmap_flag (flag : int);
   pragma Import (C, al_add_new_bitmap_flag, "al_add_new_bitmap_flag");

   function al_get_bitmap_width (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_width, "al_get_bitmap_width");

   function al_get_bitmap_height (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_height, "al_get_bitmap_height");

   function al_get_bitmap_format (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_format, "al_get_bitmap_format");

   function al_get_bitmap_flags (bitmap : ALLEGRO_BITMAP) return int;
   pragma Import (C, al_get_bitmap_flags, "al_get_bitmap_flags");

   function al_create_bitmap (w : int; h : int) return ALLEGRO_BITMAP;
   pragma Import (C, al_create_bitmap, "al_create_bitmap");

   procedure al_destroy_bitmap (bitmap : ALLEGRO_BITMAP);
   pragma Import (C, al_destroy_bitmap, "al_destroy_bitmap");

   procedure al_put_pixel
     (x : int;
      y : int;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_put_pixel, "al_put_pixel");

   procedure al_put_blended_pixel
     (x : int;
      y : int;
      color : Allegro5.Color.ALLEGRO_COLOR);
   pragma Import (C, al_put_blended_pixel, "al_put_blended_pixel");

   function al_get_pixel
     (bitmap : ALLEGRO_BITMAP;
      x : int;
      y : int) return Color.ALLEGRO_COLOR;
   pragma Import (C, al_get_pixel, "al_get_pixel");

   procedure al_convert_mask_to_alpha (bitmap : ALLEGRO_BITMAP; mask_color : Color.ALLEGRO_COLOR);
   pragma Import (C, al_convert_mask_to_alpha, "al_convert_mask_to_alpha");

   procedure al_set_clipping_rectangle
     (x : int;
      y : int;
      width : int;
      height : int);
   pragma Import (C, al_set_clipping_rectangle, "al_set_clipping_rectangle");

   procedure al_reset_clipping_rectangle;
   pragma Import (C, al_reset_clipping_rectangle, "al_reset_clipping_rectangle");

   procedure al_get_clipping_rectangle
     (x : access int;
      y : access int;
      w : access int;
      h : access int);
   pragma Import (C, al_get_clipping_rectangle, "al_get_clipping_rectangle");

   function al_create_sub_bitmap
     (parent : ALLEGRO_BITMAP;
      x : int;
      y : int;
      w : int;
      h : int) return ALLEGRO_BITMAP;
   pragma Import (C, al_create_sub_bitmap, "al_create_sub_bitmap");

   function al_is_sub_bitmap (bitmap : ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_is_sub_bitmap, "al_is_sub_bitmap");

   function al_get_parent_bitmap (bitmap : ALLEGRO_BITMAP) return ALLEGRO_BITMAP;
   pragma Import (C, al_get_parent_bitmap, "al_get_parent_bitmap");

   function al_clone_bitmap (bitmap : ALLEGRO_BITMAP) return ALLEGRO_BITMAP;
   pragma Import (C, al_clone_bitmap, "al_clone_bitmap");

end Allegro5.Bitmap;
