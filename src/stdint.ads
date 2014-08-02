with Interfaces.C;

package stdint is
   subtype time_t is Interfaces.C.long; -- FIX
   subtype off_t is Interfaces.C.long; -- FIX

   type uintptr_t is mod 2 ** Standard'Address_Size;
   for uintptr_t'Size use Standard'Address_Size;

   type intptr_t is range -2 ** (Standard'Address_Size - 1) .. 2 ** (Standard'Address_Size - 1) - 1;
   for intptr_t'Size use Standard'Address_Size;
end stdint;
