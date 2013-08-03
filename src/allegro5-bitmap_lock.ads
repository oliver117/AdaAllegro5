with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Bitmap;

package Allegro5.Bitmap_Lock is

   -- Users who wish to manually edit or read from a bitmap are required to
   --lock it first. The ALLEGRO_LOCKED_REGION structure represents the locked
   --region of the bitmap. This call will work with any bitmap, including
   --memory bitmaps.
   type ALLEGRO_LOCKED_REGION is record
      data       : System.Address;
      format     : aliased int;
      pitch      : aliased int;
      pixel_size : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_LOCKED_REGION);

   -- Lock an entire bitmap for reading or writing. If the bitmap is a display
   --bitmap it will be updated from system memory after the bitmap is unlocked
   --(unless locked read only). Returns NULL if the bitmap cannot be locked,
   --e.g. the bitmap was locked previously and not unlocked.
   --
   -- Flags are:
   --
   -- ALLEGRO_LOCK_READONLY - The locked region will not be written to. This
   --can be faster if the bitmap is a video texture, as it can be discarded
   --after the lock instead of uploaded back to the card.
   --
   -- ALLEGRO_LOCK_WRITEONLY - The locked region will not be read from. This
   --can be faster if the bitmap is a video texture, as no data need to be
   --read from the video card. You are required to fill in all pixels before
   --unlocking the bitmap again, so be careful when using this flag.
   --
   -- ALLEGRO_LOCK_READWRITE - The locked region can be written to and read
   --from. Use this flag if a partial number of pixels need to be written to,
   --even if reading is not needed.
   --
   -- 'format' indicates the pixel format that the returned buffer will be in.
   --To lock in the same format as the bitmap stores it's data internally,
   --call with al_get_bitmap_format(bitmap) as the format or use
   --ALLEGRO_PIXEL_FORMAT_ANY. Locking in the native format will usually be
   --faster.
   function al_lock_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      format : int;
      flags  : int)
      return   access ALLEGRO_LOCKED_REGION;
   pragma Import (C, al_lock_bitmap, "al_lock_bitmap");

   -- Like al_lock_bitmap, but only locks a specific area of the bitmap. If
   --the bitmap is a display bitmap, only that area of the texture will be
   --updated when it is unlocked. Locking only the region you indend to modify
   --will be faster than locking the whole bitmap.
   function al_lock_bitmap_region
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      x      : int;
      y      : int;
      width  : int;
      height : int;
      format : int;
      flags  : int)
      return   access ALLEGRO_LOCKED_REGION;
   pragma Import (C, al_lock_bitmap_region, "al_lock_bitmap_region");

   -- Unlock a previously locked bitmap or bitmap region. If the bitmap is a
   --display bitmap, the texture will be updated to match the system memory
   --copy (unless it was locked read only).
   procedure al_unlock_bitmap (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP);
   pragma Import (C, al_unlock_bitmap, "al_unlock_bitmap");

   -- Returns whether or not a bitmap is already locked.
   function al_is_bitmap_locked
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return   Extensions.bool;
   pragma Import (C, al_is_bitmap_locked, "al_is_bitmap_locked");

end Allegro5.Bitmap_Lock;
