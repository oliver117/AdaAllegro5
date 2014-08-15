with Allegro5.Blender;

with Interfaces.C; use Interfaces.C;

package body Allegro5.Thick.Blender is

   package A5B renames Allegro5.Blender;

   -----------------
   -- Set_Blender --
   -----------------

   procedure Set_Blender
     (Op : Integer;
      Source : Integer;
      Dest : Integer)
   is
   begin
      A5B.al_set_blender (int (Op), int (Source), int (Dest));
   end Set_Blender;

   -----------------
   -- Get_Blender --
   -----------------

   procedure Get_Blender
     (Op     : out Integer;
      Source : out Integer;
      Dest   : out Integer)
   is
   begin
      A5B.al_get_blender (int (Op), int (Source), int (Dest));
   end Get_Blender;

   --------------------------
   -- Set_Separate_Blender --
   --------------------------

   procedure Set_Separate_Blender
     (Op           : Integer;
      Source       : Integer;
      Dest         : Integer;
      Alpha_Op     : Integer;
      Alpha_Source : Integer;
      Alpha_Dest   : Integer)
   is
   begin
      A5B.al_set_separate_blender (int (Op), int (Source), int (Dest), int (Alpha_Op), int (Alpha_Source), int (Alpha_Dest));
   end Set_Separate_Blender;

   --------------------------
   -- Get_Separate_Blender --
   --------------------------

   procedure Get_Separate_Blender
     (Op         : out Integer;
      Source     : out Integer;
      Dest       : out Integer;
      Alpha_Op   : out Integer;
      Alpha_Src  : out Integer;
      Alpha_Dest : out Integer)
   is
   begin
      A5B.al_get_separate_blender (int (Op), int (Source), int (Dest), int (Alpha_Op), int (Alpha_Src), int (Alpha_Dest));
   end Get_Separate_Blender;

end Allegro5.Thick.Blender;
