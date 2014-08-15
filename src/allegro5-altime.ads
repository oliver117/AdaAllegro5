with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package Allegro5.Altime is

   -- Represent a timeout value. The size of the structure is known so can be
   --statically allocated. The contents are private.
   type ALLEGRO_TIMEOUT is record
      uu_pad1_u_u : aliased Unsigned_64;
      uu_pad2_u_u : aliased Unsigned_64;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_TIMEOUT);

   -- Return the number of seconds since the Allegro library was initialised.
   --The return value is undefined if Allegro is uninitialised. The resolution
   --depends on the used driver, but typically can be in the order of
   --microseconds.
   function al_get_time return double;
   pragma Import (C, al_get_time, "al_get_time");

   -- Waits for the specified number seconds. This tells the system to pause
   --the current thread for the given amount of time. With some operating
   --systems, the accuracy can be in the order of 10ms.
   procedure al_rest (seconds : double);
   pragma Import (C, al_rest, "al_rest");

   -- Set timeout value of some number of seconds after the function call.
   procedure al_init_timeout
     (timeout : out ALLEGRO_TIMEOUT;
      seconds : double);
   pragma Import (C, al_init_timeout, "al_init_timeout");

end Allegro5.Altime;
