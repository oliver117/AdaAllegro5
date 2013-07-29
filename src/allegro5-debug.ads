with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Allegro5.Debug is

   -- Register a function to be called when an internal Allegro assertion
   --fails. Pass NULL to reset to the default behaviour, which is to do
   --whatever the standard assert() macro does.
   procedure al_register_assert_handler
     (handler : access procedure
     (expr : Interfaces.C.Strings.chars_ptr;
      file : Interfaces.C.Strings.chars_ptr;
      line : int;
      func : Interfaces.C.Strings.chars_ptr));
   pragma Import
     (C,
      al_register_assert_handler,
      "al_register_assert_handler");

end Allegro5.Debug;
