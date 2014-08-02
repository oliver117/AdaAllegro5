
package body Allegro5.Events is

   function ALLEGRO_EVENT_TYPE_IS_USER (t : unsigned) return Extensions.bool is
      Is_User : Extensions.bool;
   begin
      if t >= 512 then
         Is_User := 1;
      else
         Is_User := 0;
      end if;

      return Is_User;
   end ALLEGRO_EVENT_TYPE_IS_USER;

end Allegro5.Events;
