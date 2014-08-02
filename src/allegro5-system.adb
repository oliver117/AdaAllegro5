
with Allegro5.Base;

package body Allegro5.System is

   function al_init return Extensions.bool is
   begin
      return al_install_system (version    => Allegro5.Base.ALLEGRO_VERSION_INT,
                                atexit_ptr => atexit'Access);
   end al_init;

end Allegro5.System;
