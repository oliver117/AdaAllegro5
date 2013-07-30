with Interfaces.C; use Interfaces.C;

package body Allegro5.Base is

   function AL_ID (a : int; b : int; c : int; d : int) return int is
   begin
      return a * 2 ** 24 + b * 2 ** 16 + c * 2 ** 8 + d;
   end AL_ID;

end Allegro5.Base;
