with Interfaces.C; use Interfaces.C;
with Allegro5.Fixed;
use Allegro5;


package Allegro5.Inline_Fmaths is

   function al_ftofix (x : double) return Fixed.al_fixed;
   pragma Import (C, al_ftofix, "al_ftofix");

   function al_fixtof (x : Fixed.al_fixed) return double;
   pragma Import (C, al_fixtof, "al_fixtof");

   function al_fixadd (x : Fixed.al_fixed; y : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixadd, "al_fixadd");

   function al_fixsub (x : Fixed.al_fixed; y : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixsub, "al_fixsub");

   function al_fixmul (x : Fixed.al_fixed; y : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixmul, "al_fixmul");

   function al_fixdiv (x : Fixed.al_fixed; y : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixdiv, "al_fixdiv");

   function al_fixfloor (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixfloor, "al_fixfloor");

   function al_fixceil (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixceil, "al_fixceil");

   function al_itofix (x : int) return Fixed.al_fixed;
   pragma Import (C, al_itofix, "al_itofix");

   function al_fixtoi (x : Fixed.al_fixed) return int;
   pragma Import (C, al_fixtoi, "al_fixtoi");

   function al_fixcos (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixcos, "al_fixcos");

   function al_fixsin (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixsin, "al_fixsin");

   function al_fixtan (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixtan, "al_fixtan");

   function al_fixacos (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixacos, "al_fixacos");

   function al_fixasin (x : Fixed.al_fixed) return Fixed.al_fixed;
   pragma Import (C, al_fixasin, "al_fixasin");

end Allegro5.Inline_Fmaths;
