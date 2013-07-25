with Interfaces.C; use Interfaces.C;
with stdint;


package Allegro5.Fixed is

   subtype al_fixed is stdint.int32_t;

   al_fixtorad_r : aliased al_fixed;
   pragma Import (C, al_fixtorad_r, "al_fixtorad_r");

   al_radtofix_r : aliased al_fixed;
   pragma Import (C, al_radtofix_r, "al_radtofix_r");

end Allegro5.Fixed;
