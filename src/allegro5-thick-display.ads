with Allegro5.Bitmap;
with Allegro5.Display;
with Allegro5.Events;

package Allegro5.Thick.Display is

   package A5D renames Allegro5.Display;

   type Allegro_Display is tagged record
      D : A5D.ALLEGRO_DISPLAY;
   end record;

   procedure Set_New_Display_Refresh_Rate (Refresh_Rate : Integer) with
      Inline => True;

   procedure Set_New_Display_Flags (Flags : Integer) with
      Inline => True;

   function Get_New_Display_Refresh_Rate return Integer with
      Inline => True;

   function Get_New_Display_Flags return Integer with
      Inline => True;

   function Get_Display_Width (Display : Allegro_Display) return Integer with
      Inline => True;

   function Get_Display_Height (Display : Allegro_Display) return Integer with
      Inline => True;

   function Get_Display_Format (Display : Allegro_Display) return Integer with
      Inline => True;

   function Get_Display_Refresh_Rate
     (Display : Allegro_Display) return Integer with
      Inline => True;

   function Get_Display_Flags (Display : Allegro_Display) return Integer with
      Inline => True;

   function Set_Display_Flag
     (Display : Allegro_Display;
      Flag    : Integer;
      On_Off  : Boolean) return Boolean with
      Inline => True;

   function Toogle_Display_Flag
     (Display : Allegro_Display;
      Flag    : Integer;
      On_Off  : Boolean) return Boolean with
      Inline => True;

   function Create_Display
     (W : Integer;
      H : Integer) return Allegro_Display with
      Inline => True;

   procedure Destroy_Display (Display : Allegro_Display) with
      Inline => True;

   function Get_Current_Display return Allegro_Display with
      Inline => True;

   procedure Set_Target_Bitmap (Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) with
      Inline => True;

   procedure Set_Target_Backbuffer (Display : Allegro_Display) with
      Inline => True;

   function Get_Backbuffer
     (Display : Allegro_Display) return Allegro5.Bitmap.ALLEGRO_BITMAP with
      Inline => True;

   function Get_Target_Bitmap return Bitmap.ALLEGRO_BITMAP with
      Inline => True;

   function Acknowledge_Resize (Display : Allegro_Display) return Boolean with
      Inline => True;

   function Resize_Display
     (Display : Allegro_Display;
      Width   : Integer;
      Height  : Integer) return Boolean with
      Inline => True;

   procedure Flip_Display with
      Inline => True;

   procedure Update_Display_Region
     (X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer) with
      Inline => True;

   function Is_Compatible_Bitmap
     (Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) return Boolean with
      Inline => True;

   function Wait_For_VSync return Boolean with
      Inline => True;

   function Get_Display_Event_Source
     (Display : Allegro_Display) return access Events.ALLEGRO_EVENT_SOURCE with
      Inline => True;

   procedure Set_Display_Icon
     (Display : Allegro_Display;
      Icon    : Bitmap.ALLEGRO_BITMAP) with
      Inline => True;

   procedure Set_Display_Icons
     (Display   : Allegro_Display;
      Num_Icons : Integer;
      Icons     : Bitmap.ALLEGRO_BITMAP) with
      Inline => True;

   function Get_New_Display_Adapter return Integer with
      Inline => True;

   procedure Set_New_Display_Adapter (Adapter : Integer) with
      Inline => True;

   procedure Set_New_Window_Position (X : Integer; Y : Integer) with
      Inline => True;

   procedure Get_New_Window_Position (X : out Integer; Y : out Integer) with
      Inline => True;

   procedure Set_Window_Position
     (Display : Allegro_Display;
      X       : Integer;
      Y       : Integer) with
      Inline => True;

   procedure Get_Window_Position
     (Display :     Allegro_Display;
      X       : out Integer;
      Y       : out Integer) with
      Inline => True;

   procedure Set_Window_Title (Display : Allegro_Display; Title : String) with
      Inline => True;

   procedure Set_New_Display_Option
     (Option     : A5D.ALLEGRO_DISPLAY_OPTIONS;
      Value      : Integer;
      Importance : Integer) with
      Inline => True;

   function Get_New_Display_Option
     (Option     :     Integer;
      Importance : out Integer) return Integer with
      Inline => True;

   procedure Reset_New_Display_Options with
      Inline => True;

   function Get_Display_Option
     (Display : Allegro_Display;
      Option  : A5D.ALLEGRO_DISPLAY_OPTIONS) return Integer with
      Inline => True;

   procedure Hold_Bitmap_Drawing (Hold : Boolean) with
      Inline => True;

   function Is_Bitmap_Drawing_Held return Boolean with
      Inline => True;

end Allegro5.Thick.Display;
