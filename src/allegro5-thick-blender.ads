package Allegro5.Thick.Blender is

   procedure Set_Blender (Op : Integer; Source : Integer; Dest : Integer) with
      Inline => True;

   procedure Get_Blender
     (Op     : out Integer;
      Source : out Integer;
      Dest   : out Integer) with
      Inline => True;

   procedure Set_Separate_Blender
     (Op           : Integer;
      Source       : Integer;
      Dest         : Integer;
      Alpha_Op     : Integer;
      Alpha_Source : Integer;
      Alpha_Dest   : Integer) with
      Inline => True;

   procedure Get_Separate_Blender
     (Op         : out Integer;
      Source     : out Integer;
      Dest       : out Integer;
      Alpha_Op   : out Integer;
      Alpha_Src  : out Integer;
      Alpha_Dest : out Integer) with
      Inline => True;

end Allegro5.Thick.Blender;
