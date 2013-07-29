with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Allegro5.Debug is

   --  arg-macro: procedure ALLEGRO_TRACE_CHANNEL_LEVEL (channel, x)
   --    1 ? (void) 0 : _al_trace_suffix
   --  arg-macro: procedure ALLEGRO_TRACE_LEVEL (x)
   --    ALLEGRO_TRACE_CHANNEL_LEVEL(__al_debug_channel, x)

   ALLEGRO_DEBUG : constant := ALLEGRO_TRACE_LEVEL(0);
   ALLEGRO_INFO : constant := ALLEGRO_TRACE_LEVEL(1);
   ALLEGRO_WARN : constant := ALLEGRO_TRACE_LEVEL(2);
   ALLEGRO_ERROR : constant := ALLEGRO_TRACE_LEVEL(3);

   --  unsupported macro: ALLEGRO_ASSERT(e) ((e) ? (void) 0 :
   --(_al_user_assert_handler) ? _al_user_assert_handler(#e, __FILE__,
   --__LINE__, __func__) : assert(e))
   --  unsupported macro: ALLEGRO_ASSERT_CONCAT_(a,b) a ##b
   --  arg-macro: procedure ALLEGRO_ASSERT_CONCAT (a, b)
   --    ALLEGRO_ASSERT_CONCAT_(a, b)
   --  unsupported macro: ALLEGRO_STATIC_ASSERT(module,e) struct
   --ALLEGRO_ASSERT_CONCAT(static_assert_ ##module ##_line_, __LINE__) {
   --unsigned int bf : !!(e); }

   --  skipped func _al_trace_prefix

   --  skipped func _al_trace_suffix

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
