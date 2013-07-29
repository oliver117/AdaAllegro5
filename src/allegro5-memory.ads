with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with stdint;
with System;

package Allegro5.Memory is

   type ALLEGRO_MEMORY_INTERFACE is record
      mi_malloc  : access function
        (n    : stdint.size_t;
         line : int;
         file : Interfaces.C.Strings.chars_ptr;
         func : Interfaces.C.Strings.chars_ptr)
         return System.Address;
      mi_free    : access procedure
        (ptr  : System.Address;
         line : int;
         file : Interfaces.C.Strings.chars_ptr;
         func : Interfaces.C.Strings.chars_ptr);
      mi_realloc : access function
        (ptr  : System.Address;
         n    : stdint.size_t;
         line : int;
         file : Interfaces.C.Strings.chars_ptr;
         func : Interfaces.C.Strings.chars_ptr)
         return System.Address;
      mi_calloc  : access function
        (count : stdint.size_t;
         n     : stdint.size_t;
         line  : int;
         file  : Interfaces.C.Strings.chars_ptr;
         func  : Interfaces.C.Strings.chars_ptr)
         return  System.Address;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_MEMORY_INTERFACE);

   -- Override the memory management functions with implementations of
   --al_malloc_with_context, al_free_with_context, al_realloc_with_context and
   --al_calloc_with_context. The context arguments may be used for debugging.
   --
   -- If the pointer is NULL, the default behaviour will be restored.
   procedure al_set_memory_interface
     (iface : access ALLEGRO_MEMORY_INTERFACE);
   pragma Import (C, al_set_memory_interface, "al_set_memory_interface");

   -- This calls malloc() from the Allegro library (this matters on Windows),
   --unless overridden with al_set_memory_interface,
   --
   -- Generally you should use the al_malloc subprogram renaming declaration.
   function al_malloc_with_context
     (n    : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr)
      return System.Address;
   pragma Import (C, al_malloc_with_context, "al_malloc_with_context");

   -- Like malloc() in the C standard library, but the implementation may be
   --overridden.
   --
   -- This is a subprogram renaming declaration.
   function al_malloc
     (n    : stdint.size_t;
      line : int                            := -1;
      file : Interfaces.C.Strings.chars_ptr :=
      Interfaces.C.Strings.New_String ("NOT_SUPPORTED");
      func : Interfaces.C.Strings.chars_ptr :=
      Interfaces.C.Strings.New_String ("NOT_SUPPORTED"))
      return System.Address renames al_malloc_with_context;

   -- This calls free() from the Allegro library (this matters on Windows),
   --unless overridden with al_set_memory_interface.
   --
   -- Generally you should use the al_free subprogram renaming declaration.
   procedure al_free_with_context
     (ptr  : System.Address;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, al_free_with_context, "al_free_with_context");

   -- Like free() in the C standard library, but the implementation may be
   --overridden.
   --
   -- Additionally, on Windows, a memory block allocated by one DLL must be
   --freed from the same DLL. In the few places where an Allegro function
   --returns a pointer that must be freed, you must use al_free for
   --portability to Windows.
   --
   -- This is a subprogram renaming declaration.
   procedure al_free
     (ptr  : System.Address;
      line : int                            := -1;
      file : Interfaces.C.Strings.chars_ptr :=
      Interfaces.C.Strings.New_String ("NOT_SUPPORTED");
      func : Interfaces.C.Strings.chars_ptr :=
      Interfaces.C.Strings.New_String ("NOT_SUPPORTED")) renames
     al_free_with_context;

   -- This calls realloc() from the Allegro library (this matters on Windows),
   --unless overridden with al_set_memory_interface,
   --
   -- Generally you should use the al_realloc subprogram renaming declaration.
   function al_realloc_with_context
     (ptr  : System.Address;
      n    : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr)
      return System.Address;
   pragma Import (C, al_realloc_with_context, "al_realloc_with_context");

   -- Like realloc() in the C standard library, but the implementation may be
   --overridden.
   --
   -- This is a subprogram renaming declaration.
   function al_realloc
     (ptr  : System.Address;
      n    : stdint.size_t;
      line : int;
      file : Interfaces.C.Strings.chars_ptr;
      func : Interfaces.C.Strings.chars_ptr)
      return System.Address renames al_realloc_with_context;

   -- This calls calloc() from the Allegro library (this matters on Windows),
   --unless overridden with al_set_memory_interface,
   --
   -- Generally you should use the al_calloc subprogram renaming declaration.
   function al_calloc_with_context
     (count : stdint.size_t;
      n     : stdint.size_t;
      line  : int;
      file  : Interfaces.C.Strings.chars_ptr;
      func  : Interfaces.C.Strings.chars_ptr)
      return  System.Address;
   pragma Import (C, al_calloc_with_context, "al_calloc_with_context");

   -- Like calloc() in the C standard library, but the implementation may be
   --overridden.
   --
   -- This is a subprogram renaming declaration.
   function al_calloc
     (count : stdint.size_t;
      n     : stdint.size_t;
      line  : int;
      file  : Interfaces.C.Strings.chars_ptr;
      func  : Interfaces.C.Strings.chars_ptr)
      return  System.Address renames al_calloc_with_context;

end Allegro5.Memory;
