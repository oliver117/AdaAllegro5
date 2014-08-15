with Allegro5.Altime;

package Allegro5.Thick.Altime is

   function Get_Time return Long_Float with
      Inline => True;

   procedure Rest (Seconds : Long_Float) with
      Inline => True;

   procedure Init_Timeout
     (Timeout : out Allegro5.Altime.ALLEGRO_TIMEOUT;
      Seconds :     Long_Float) with
      Inline => True;

end Allegro5.Thick.Altime;
