with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Bitmap;

use Allegro5;

package Allegro5.Bitmap_Lock is

   type ALLEGRO_LOCKED_REGION is record
      data : System.Address;
      format : aliased int;
      pitch : aliased int;
      pixel_size : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_LOCKED_REGION);

   function al_lock_bitmap
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      format : int;
      flags : int) return access ALLEGRO_LOCKED_REGION;
   pragma Import (C, al_lock_bitmap, "al_lock_bitmap");

   function al_lock_bitmap_region
     (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP;
      x : int;
      y : int;
      width : int;
      height : int;
      format : int;
      flags : int) return access ALLEGRO_LOCKED_REGION;
   pragma Import (C, al_lock_bitmap_region, "al_lock_bitmap_region");

   procedure al_unlock_bitmap (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP);
   pragma Import (C, al_unlock_bitmap, "al_unlock_bitmap");

   function al_is_bitmap_locked (bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) return Extensions.bool;
   pragma Import (C, al_is_bitmap_locked, "al_is_bitmap_locked");

end Allegro5.Bitmap_Lock;
