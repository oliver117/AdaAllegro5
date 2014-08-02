with Interfaces.C; use Interfaces.C;

package Allegro5.Error is

   -- Some Allegro functions will set an error number as well as returning an
   --error code. Call this function to retrieve the last error number set
   --for the calling thread.
   function al_get_errno return int;
   pragma Import (C, al_get_errno, "al_get_errno");

   -- Set the error number for for the calling thread.
   procedure al_set_errno (errnum : int);
   pragma Import (C, al_set_errno, "al_set_errno");

end Allegro5.Error;
