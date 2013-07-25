with Interfaces.C; use Interfaces.C;


package Allegro5.Timer is

   type ALLEGRO_TIMER is new System.Address;

   function ALLEGRO_USECS_TO_SECS (x : double) return double is
   begin
      return x / 1_000_000.0;
   end ALLEGRO_USECS_TO_SECS;

   function ALLEGRO_MSECS_TO_SECS (x : double) return double is
   begin
      return x / 1_000.0;
   end ALLEGRO_USECS_TO_SECS;

   function ALLEGRO_BPS_TO_SECS (x : double) return double is
   begin
      return 1.0 / x;
   end ALLEGRO_USECS_TO_SECS;

   function ALLEGRO_BPM_TO_SECS (x : double) return double is
   begin
      return 60.0 / x;
   end ALLEGRO_USECS_TO_SECS;

end Allegro5.Timer;
