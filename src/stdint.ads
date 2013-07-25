with System;
with Interfaces.C;

package stdint is
   type uint16_t is mod 2 ** 16;
   for uint16_t'Size use 16;

   type int16_t is range -2 ** 15 .. 2 ** 15 - 1;
   for int16_t'Size use 16;

   type uint32_t is mod 2 ** 32;
   for uint32_t'Size use 32;

   type int32_t is range -2 ** 31 .. 2 ** 31 - 1;
   for int32_t'Size use 32;

   type uint64_t is mod 2 ** 64;
   for uint64_t'Size use 64;

   type int64_t is range -2 ** 63 .. 2 ** 63 - 1;
   for int64_t'Size use 64;

   subtype size_t is Interfaces.C.long; -- FIX
   subtype time_t is Interfaces.C.long; -- FIX
   subtype off_t is Interfaces.C.long; -- FIX

   type uintptr_t is mod 2 ** Standard'Address_Size;
   for uintptr_t'Size use Standard'Address_Size;

   type intptr_t is range -2 ** (Standard'Address_Size - 1) .. 2 ** (Standard'Address_Size - 1) - 1;
   for intptr_t'Size use Standard'Address_Size;
end stdint;
