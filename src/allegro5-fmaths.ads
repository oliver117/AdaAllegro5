with Interfaces.C; use Interfaces.C;
with Allegro5.Fixed;
use Allegro5;

package Allegro5.Fmaths is

   function al_fixsqrt (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixsqrt, "al_fixsqrt");

   function al_fixhypot (x : Fixed.al_fixed; y : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixhypot, "al_fixhypot");

   function al_fixatan (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixatan, "al_fixatan");

   function al_fixatan2 (y : Fixed.al_fixed; x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixatan2, "al_fixatan2");

end Allegro5.Fmaths;
