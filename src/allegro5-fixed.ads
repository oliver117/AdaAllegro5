with stdint;

package Allegro5.Fixed is

   -- A fixed point number.
   --
   -- Allegro provides some routines for working with fixed point numbers, and
   --defines the type al_fixed to be a signed 32-bit integer. The high word is
   --used for the integer part and the low word for the fraction, giving a
   --range of -32768 to 32767 and an accuracy of about four or five decimal
   --places. Fixed point numbers can be assigned, compared, added, subtracted,
   --negated and shifted (for multiplying or dividing by powers of two) using
   --the normal integer operators, but you should take care to use the
   --appropriate conversion routines when mixing fixed point with integer or
   --floating point values. Writing fixed_point_1 + fixed_point_2 is OK, but
   --fixed_point + integer is not.
   --
   -- The only advantage of fixed point math routines is that you don't
   --require a floating point coprocessor to use them. This was great in the
   --time period of i386 and i486 machines, but stopped being so useful with
   --the coming of the Pentium class of processors. From Pentium onwards, CPUs
   --have increased their strength in floating point operations, equaling or
   --even surpassing integer math performance.
   --
   -- Depending on the type of operations your program may need, using
   --floating point types may be faster than fixed types if you are targeting
   --a specific machine class. Many embedded processors have no FPUs so fixed
   --point maths can be useful there.
   subtype al_fixed is stdint.int32_t;

   -- This constant gives a ratio which can be used to convert a fixed point
   --number in binary angle format to a fixed point number in radians.
   al_fixtorad_r : aliased al_fixed;
   pragma Import (C, al_fixtorad_r, "al_fixtorad_r");

   -- This constant gives a ratio which can be used to convert a fixed point
   --number in radians to a fixed point number in binary angle format.
   al_radtofix_r : aliased al_fixed;
   pragma Import (C, al_radtofix_r, "al_radtofix_r");

end Allegro5.Fixed;
