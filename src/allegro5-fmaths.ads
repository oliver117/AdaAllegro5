with Allegro5.Fixed; use Allegro5;

package Allegro5.Fmaths is

   -- This finds out the non negative square root of x. If x is negative,
   --Allegro's errno is set to EDOM and the function returns zero.
   function al_fixsqrt (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixsqrt, "al_fixsqrt");

   -- Fixed point hypotenuse (returns the square root of x*x + y*y). This
   --should be better than calculating the formula yourself manually, since
   --the error is much smaller.
   function al_fixhypot
     (x    : Fixed.al_fixed;
      y    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixhypot, "al_fixhypot");

   -- This function finds the inverse tangent of a value using a lookup table.
   --The input value must be a fixed point radian. The inverse tangent is the
   --value whose tangent is x.
   function al_fixatan (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixatan, "al_fixatan");

   -- This is a fixed point version of the libc atan2() routine. It computes
   --the arc tangent of y / x, but the signs of both arguments are used to
   --determine the quadrant of the result, and x is permitted to be zero. This
   --function is useful to convert Cartesian coordinates to polar coordinates.
   function al_fixatan2
     (y    : Fixed.al_fixed;
      x    : Fixed.al_fixed)
      return Fixed.al_fixed;
   pragma Import (C, al_fixatan2, "al_fixatan2");

end Allegro5.Fmaths;
