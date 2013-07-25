with Interfaces.C; use Interfaces.C;


package Allegro5.Error is

   function al_get_errno return int;
   pragma Import (C, al_get_errno, "al_get_errno");

   procedure al_set_errno (errnum : int);
   pragma Import (C, al_set_errno, "al_set_errno");

end Allegro5.Error;
