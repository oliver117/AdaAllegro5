with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Bitmap;
with Allegro5.Display;

use Allegro5;


package Allegro5.Mouse_Cursor is

   type ALLEGRO_MOUSE_CURSOR is new System.Address;

   type ALLEGRO_SYSTEM_MOUSE_CURSOR is 
     (ALLEGRO_SYSTEM_MOUSE_CURSOR_NONE,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_DEFAULT,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_ARROW,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_BUSY,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_QUESTION,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_EDIT,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_MOVE,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_N,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_W,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_S,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_E,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NW,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SW,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_SE,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_RESIZE_NE,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_PROGRESS,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_PRECISION,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_LINK,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_ALT_SELECT,
      ALLEGRO_SYSTEM_MOUSE_CURSOR_UNAVAILABLE,
      ALLEGRO_NUM_SYSTEM_MOUSE_CURSORS);
   pragma Convention (C, ALLEGRO_SYSTEM_MOUSE_CURSOR);

   function al_create_mouse_cursor
     (sprite : Bitmap.ALLEGRO_BITMAP;
      xfocus : int;
      yfocus : int) return ALLEGRO_MOUSE_CURSOR;
   pragma Import (C, al_create_mouse_cursor, "al_create_mouse_cursor");

   procedure al_destroy_mouse_cursor (arg1 : ALLEGRO_MOUSE_CURSOR);
   pragma Import (C, al_destroy_mouse_cursor, "al_destroy_mouse_cursor");

   function al_set_mouse_cursor (display : Allegro5.Display.ALLEGRO_DISPLAY; cursor : ALLEGRO_MOUSE_CURSOR) return Extensions.bool;
   pragma Import (C, al_set_mouse_cursor, "al_set_mouse_cursor");

   function al_set_system_mouse_cursor (display : Allegro5.Display.ALLEGRO_DISPLAY; cursor_id : ALLEGRO_SYSTEM_MOUSE_CURSOR) return Extensions.bool;
   pragma Import (C, al_set_system_mouse_cursor, "al_set_system_mouse_cursor");

   function al_show_mouse_cursor (display : Allegro5.Display.ALLEGRO_DISPLAY) return Extensions.bool;
   pragma Import (C, al_show_mouse_cursor, "al_show_mouse_cursor");

   function al_hide_mouse_cursor (display : Allegro5.Display.ALLEGRO_DISPLAY) return Extensions.bool;
   pragma Import (C, al_hide_mouse_cursor, "al_hide_mouse_cursor");

end Allegro5.Mouse_Cursor;
