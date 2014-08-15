with Interfaces.C; use Interfaces.C;

package body Allegro5.Thick.Altime is

   package A5A renames Allegro5.Altime;

   --------------
   -- Get_Time --
   --------------

   function Get_Time
      return Long_Float
   is
   begin
      return Long_Float (A5A.al_get_time);
   end Get_Time;

   ----------
   -- Rest --
   ----------

   procedure Rest
     (Seconds : Long_Float)
   is
   begin
      A5A.al_rest (double (Seconds));
   end Rest;

   ------------------
   -- Init_Timeout --
   ------------------

   procedure Init_Timeout
     (Timeout : out Allegro5.Altime.ALLEGRO_TIMEOUT;
      Seconds :     Long_Float)
   is
   begin
      A5A.al_init_timeout (Timeout, double (Seconds));
   end Init_Timeout;

end Allegro5.Thick.Altime;
