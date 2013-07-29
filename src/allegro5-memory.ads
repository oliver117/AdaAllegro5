with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with stdint;


package Allegro5.Memory is

   --  arg-macro: function al_malloc (n)
   --    return al_malloc_with_context((n), __LINE__, __FILE__, __func__);
   --function al_malloc_with_context (stdint.size_t n; int line; Interfaces.C.Strings.chars_ptr file, Interfaces.C.Strings.chars_ptr func) return System.Address;



   --  arg-macro: function al_free (p)
   --    return al_free_with_context((p), __LINE__, __FILE__, __func__);


   --  arg-macro: function al_realloc (p, n)
   --    return al_realloc_with_context((p), (n), __LINE__, __FILE__, __func__);


   --  arg-macro: function al_calloc (c, n)
   --    return al_calloc_with_context((c), (n), __LINE__, __FILE__, __func__);
   type ALLEGRO_MEMORY_INTERFACE is record
      mi_malloc : access function
           (arg1 : stdint.size_t;
            arg2 : int;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : Interfaces.C.Strings.chars_ptr) return System.Address;
      mi_free : access procedure
           (arg1 : System.Address;
            arg2 : int;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : Interfaces.C.Strings.chars_ptr);
      mi_realloc : access function
           (arg1 : System.Address;
            arg2 : stdint.size_t;
            arg3 : int;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : Interfaces.C.Strings.chars_ptr) return System.Address;
      mi_calloc : access function
           (arg1 : stdint.size_t;
            arg2 : stdint.size_t;
            arg3 : int;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : Interfaces.C.Strings.chars_ptr) return System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MEMORY_INTERFACE);

   procedure al_set_memory_interface (iface : access ALLEGRO_MEMORY_INTERFACE);
   pragma Import (C, al_set_memory_interface, "al_set_memory_interface");

   function al_malloc_with_context
     (n : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_malloc_with_context, "al_malloc_with_context");

   procedure al_free_with_context
     (ptr : System.Address;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_free_with_context, "al_free_with_context");

   function al_realloc_with_context
     (ptr : System.Address;
      n : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_realloc_with_context, "al_realloc_with_context");

   function al_calloc_with_context
     (count : stdint.size_t;
      n : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_calloc_with_context, "al_calloc_with_context");

end Allegro5.Memory;
