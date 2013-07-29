with Interfaces.C;   use Interfaces.C;
with Allegro5.Fixed; use Allegro5;

package Allegro5.Inline_Fmaths is

   -- Converts a floating point value to fixed point. Unlike al_itofix, this
   --function clamps values which could overflow the type conversion, setting
   --Allegro's errno to ERANGE in the process if this happens.
   function al_ftofix (x : double) return Fixed.al_fixed;
   pragma Import (C, al_ftofix, "al_ftofix");

   -- Converts fixed point to floating point.
   function al_fixtof (x : Fixed.al_fixed) return double;
   pragma Import (C, al_fixtof, "al_fixtof");

   -- Although fixed point numbers can be added with the normal + integer
   --operator, that doesn't provide any protection against overflow. If
   --overflow is a problem, you should use this function instead. It is slower
   --than using integer operators, but if an overflow occurs it will set
   --Allegro's errno and clamp the result, rather than just letting it wrap.
   function al_fixadd
     (x    : Fixed.al_fixed;
      y    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixadd, "al_fixadd");

   -- Although fixed point numbers can be subtracted with the normal - integer
   --operator, that doesn't provide any protection against overflow. If
   --overflow is a problem, you should use this function instead. It is slower
   --than using integer operators, but if an overflow occurs it will set
   --Allegro's errno and clamp the result, rather than just letting it wrap.
   function al_fixsub
     (x    : Fixed.al_fixed;
      y    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixsub, "al_fixsub");

   -- A fixed point value can be multiplied or divided by an integer with the
   --normal * and / operators. To multiply two fixed point values, though, you
   --must use this function.
   --
   -- If an overflow occurs, Allegro's errno will be set and the maximum
   --possible value will be returned, but errno is not cleared if the
   --operation is successful. This means that if you are going to test for
   --overflow you should call al_set_errno(0) before calling al_fixmul.
   function al_fixmul
     (x    : Fixed.al_fixed;
      y    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixmul, "al_fixmul");

   -- A fixed point value can be divided by an integer with the normal /
   --operator. To divide two fixed point values, though, you must use this
   --function. If a division by zero occurs, Allegro's errno will be set and
   --the maximum possible value will be returned, but errno is not cleared if
   --the operation is successful. This means that if you are going to test for
   --division by zero you should call al_set_errno(0) before calling al_fixdiv.
   function al_fixdiv
     (x    : Fixed.al_fixed;
      y    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixdiv, "al_fixdiv");

   -- Returns the greatest integer not greater than x. That is, it rounds
   --towards negative infinity.
   function al_fixfloor (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixfloor, "al_fixfloor");

   -- Returns the smallest integer not less than x. That is, it rounds towards
   --positive infinity.
   function al_fixceil (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixceil, "al_fixceil");

   -- Converts an integer to fixed point. This is the same thing as x<<16.
   --Remember that overflows (trying to convert an integer greater than 32767)
   --and underflows (trying to convert an integer lesser than -32768) are not
   --detected even in debug builds! The values simply "wrap around".
   function al_itofix (x : int) return Fixed.al_fixed;
   pragma Import (C, al_itofix, "al_itofix");

   -- Converts fixed point to integer, rounding as required to the nearest
   --integer.
   function al_fixtoi (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixtoi, "al_fixtoi");

   -- This function finds the cosine of a value using a lookup table. The
   --input value must be a fixed point binary angle.
   function al_fixcos (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixcos, "al_fixcos");

   -- This function finds the sine of a value using a lookup table. The input
   --value must be a fixed point binary angle.
   function al_fixsin (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixsin, "al_fixsin");

   -- This function finds the tangent of a value using a lookup table. The
   --input value must be a fixed point binary angle.
   function al_fixtan (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixtan, "al_fixtan");

   -- This function finds the inverse cosine of a value using a lookup table.
   --The input value must be a fixed point radian. The inverse cosine is
   --defined only in the domain from -1 to 1. Outside of this input range, the
   --function will set Allegro's errno to EDOM and return zero.
   function al_fixacos (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixacos, "al_fixacos");

   -- This function finds the inverse sine of a value using a lookup table.
   --The input value must be a fixed point value. The inverse sine is defined
   --only in the domain from -1 to 1. Outside of this input range, the
   --function will set Allegro's errno to EDOM and return zero.
   function al_fixasin (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixasin, "al_fixasin");

end Allegro5.Inline_Fmaths;
