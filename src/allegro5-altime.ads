with Interfaces.C; use Interfaces.C;
with stdint;


package Allegro5.Altime is

   type ALLEGRO_TIMEOUT is record
      uu_pad1_u_u : aliased stdint.uint64_t;
      uu_pad2_u_u : aliased stdint.uint64_t;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TIMEOUT);

   function al_get_time return double;
   pragma Import (C, al_get_time, "al_get_time");

   procedure al_rest (seconds : double);
   pragma Import (C, al_rest, "al_rest");

   procedure al_init_timeout (timeout : access ALLEGRO_TIMEOUT; seconds : double);
   pragma Import (C, al_init_timeout, "al_init_timeout");

end Allegro5.Altime;
