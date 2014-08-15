with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;


package body Allegro5.Thick.Display is

   ----------------------------------
   -- Set_New_Display_Refresh_Rate --
   ----------------------------------

   procedure Set_New_Display_Refresh_Rate (Refresh_Rate : Integer) is
   begin
      A5D.al_set_new_display_refresh_rate (int (Refresh_Rate));
   end Set_New_Display_Refresh_Rate;

   ---------------------------
   -- Set_New_Display_Flags --
   ---------------------------

   procedure Set_New_Display_Flags (Flags : Integer) is
   begin
      A5D.al_set_new_display_flags (int (Flags));
   end Set_New_Display_Flags;

   ----------------------------------
   -- Get_New_Display_Refresh_Rate --
   ----------------------------------

   function Get_New_Display_Refresh_Rate return Integer is
   begin
      return Integer (A5D.al_get_new_display_refresh_rate);
   end Get_New_Display_Refresh_Rate;

   ---------------------------
   -- Get_New_Display_Flags --
   ---------------------------

   function Get_New_Display_Flags return Integer is
   begin
      return Integer (A5D.al_get_new_display_flags);
   end Get_New_Display_Flags;

   -----------------------
   -- Get_Display_Width --
   -----------------------

   function Get_Display_Width (Display : Allegro_Display) return Integer is
   begin
      return Integer (A5D.al_get_display_width (Display.D));
   end Get_Display_Width;

   ------------------------
   -- Get_Display_Height --
   ------------------------

   function Get_Display_Height (Display : Allegro_Display) return Integer is
   begin
      return Integer (A5D.al_get_display_height (Display.D));
   end Get_Display_Height;

   ------------------------
   -- Get_Display_Format --
   ------------------------

   function Get_Display_Format (Display : Allegro_Display) return Integer is
   begin
      return Integer (A5D.al_get_display_format (Display.D));
   end Get_Display_Format;

   ------------------------------
   -- Get_Display_Refresh_Rate --
   ------------------------------

   function Get_Display_Refresh_Rate
     (Display : Allegro_Display)
      return Integer
   is
   begin
      return Integer (A5D.al_get_display_refresh_rate (Display.D));
   end Get_Display_Refresh_Rate;

   -----------------------
   -- Get_Display_Flags --
   -----------------------

   function Get_Display_Flags (Display : Allegro_Display) return Integer is
   begin
      return Integer (A5D.al_get_display_flags (Display.D));
   end Get_Display_Flags;

   ----------------------
   -- Set_Display_Flag --
   ----------------------

   function Set_Display_Flag
     (Display : Allegro_Display;
      Flag    : Integer;
      On_Off  : Boolean)
      return Boolean
   is
   begin
      return 0 /= (A5D.al_set_display_flag (Display.D, int (Flag), Extensions.bool (Boolean'Pos (On_Off))));
   end Set_Display_Flag;

   -------------------------
   -- Toogle_Display_Flag --
   -------------------------

   function Toogle_Display_Flag
     (Display : Allegro_Display;
      Flag    : Integer;
      On_Off  : Boolean)
      return Boolean
   is
   begin
      return 0 /= (A5D.al_toggle_display_flag (Display.D, int (Flag), Extensions.bool (Boolean'Pos (On_Off))));
   end Toogle_Display_Flag;

   --------------------
   -- Create_Display --
   --------------------

   function Create_Display
     (W : Integer;
      H : Integer)
      return Allegro_Display
   is
   begin
      return Allegro_Display'(D => A5D.al_create_display (int (W), int (H)));
   end Create_Display;

   ---------------------
   -- Destroy_Display --
   ---------------------

   procedure Destroy_Display (Display : Allegro_Display) is
   begin
      A5D.al_destroy_display (Display.D);
   end Destroy_Display;

   -------------------------
   -- Get_Current_Display --
   -------------------------

   function Get_Current_Display return Allegro_Display is
   begin
      return Allegro_Display'(D => A5D.al_get_current_display);
   end Get_Current_Display;

   -----------------------
   -- Set_Target_Bitmap --
   -----------------------

   procedure Set_Target_Bitmap (Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP) is
   begin
      A5D.al_set_target_bitmap (Bitmap);
   end Set_Target_Bitmap;

   ---------------------------
   -- Set_Target_Backbuffer --
   ---------------------------

   procedure Set_Target_Backbuffer (Display : Allegro_Display) is
   begin
      A5D.al_set_target_backbuffer (Display.D);
   end Set_Target_Backbuffer;

   --------------------
   -- Get_Backbuffer --
   --------------------

   function Get_Backbuffer
     (Display : Allegro_Display)
      return Allegro5.Bitmap.ALLEGRO_BITMAP
   is
   begin
      return A5D.al_get_backbuffer (Display.D);
   end Get_Backbuffer;

   -----------------------
   -- Get_Target_Bitmap --
   -----------------------

   function Get_Target_Bitmap return Bitmap.ALLEGRO_BITMAP is
   begin
      return A5D.al_get_target_bitmap;
   end Get_Target_Bitmap;

   ------------------------
   -- Acknowledge_Resize --
   ------------------------

   function Acknowledge_Resize (Display : Allegro_Display) return Boolean is
   begin
      return 0 /= (A5D.al_acknowledge_resize (Display.D));
   end Acknowledge_Resize;

   --------------------
   -- Resize_Display --
   --------------------

   function Resize_Display
     (Display : Allegro_Display;
      Width   : Integer;
      Height  : Integer)
      return Boolean
   is
   begin
      return 0 /= (A5D.al_resize_display (Display.D, int (Width), int (Height)));
   end Resize_Display;

   ------------------
   -- Flip_Display --
   ------------------

   procedure Flip_Display is
   begin
      A5D.al_flip_display;
   end Flip_Display;

   ---------------------------
   -- Update_Display_Region --
   ---------------------------

   procedure Update_Display_Region
     (X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
   begin
      A5D.al_update_display_region (int (X), int (Y), int (Width), int (Height));
   end Update_Display_Region;

   --------------------------
   -- Is_Compatible_Bitmap --
   --------------------------

   function Is_Compatible_Bitmap
     (Bitmap : Allegro5.Bitmap.ALLEGRO_BITMAP)
      return Boolean
   is
   begin
      return 0 /= (A5D.al_is_compatible_bitmap (Bitmap));
   end Is_Compatible_Bitmap;

   --------------------
   -- Wait_For_VSync --
   --------------------

   function Wait_For_VSync return Boolean is
   begin
      return 0 /= (A5D.al_wait_for_vsync);
   end Wait_For_VSync;

   ------------------------------
   -- Get_Display_Event_Source --
   ------------------------------

   function Get_Display_Event_Source
     (Display : Allegro_Display)
      return access Events.ALLEGRO_EVENT_SOURCE
   is
   begin
      return A5D.al_get_display_event_source (Display.D);
   end Get_Display_Event_Source;

   ----------------------
   -- Set_Display_Icon --
   ----------------------

   procedure Set_Display_Icon
     (Display : Allegro_Display;
      Icon    : Bitmap.ALLEGRO_BITMAP)
   is
   begin
      A5D.al_set_display_icon (Display.D, Icon);
   end Set_Display_Icon;

   -----------------------
   -- Set_Display_Icons --
   -----------------------

   procedure Set_Display_Icons
     (Display   : Allegro_Display;
      Num_Icons : Integer;
      Icons     : Bitmap.ALLEGRO_BITMAP)
   is
   begin
      A5D.al_set_display_icons (Display.D, int (Num_Icons), Icons);
   end Set_Display_Icons;

   -----------------------------
   -- Get_New_Display_Adapter --
   -----------------------------

   function Get_New_Display_Adapter return Integer is
   begin
      return Integer (A5D.al_get_new_display_adapter);
   end Get_New_Display_Adapter;

   -----------------------------
   -- Set_New_Display_Adapter --
   -----------------------------

   procedure Set_New_Display_Adapter (Adapter : Integer) is
   begin
      A5D.al_set_new_display_adapter (int (Adapter));
   end Set_New_Display_Adapter;

   -----------------------------
   -- Set_New_Window_Position --
   -----------------------------

   procedure Set_New_Window_Position (X : Integer; Y : Integer) is
   begin
      A5D.al_set_new_window_position (int (X), int (Y));
   end Set_New_Window_Position;

   -----------------------------
   -- Get_New_Window_Position --
   -----------------------------

   procedure Get_New_Window_Position (X : out Integer; Y : out Integer) is
   begin
      A5D.al_get_new_window_position (int (X), int (Y));
   end Get_New_Window_Position;

   -------------------------
   -- Set_Window_Position --
   -------------------------

   procedure Set_Window_Position
     (Display : Allegro_Display;
      X       : Integer;
      Y       : Integer)
   is
   begin
      A5D.al_set_window_position (Display.D, int (X), int (Y));
   end Set_Window_Position;

   -------------------------
   -- Get_Window_Position --
   -------------------------

   procedure Get_Window_Position
     (Display :     Allegro_Display;
      X       : out Integer;
      Y       : out Integer)
   is
   begin
      A5D.al_get_window_position (Display.D, int (X), int (Y));
   end Get_Window_Position;

   ----------------------
   -- Set_Window_Title --
   ----------------------

   procedure Set_Window_Title (Display : Allegro_Display; Title : String) is
      use Interfaces.C.Strings;
      Title_C : chars_ptr := New_String (Title);
   begin
      A5D.al_set_window_title (Display.D, Title_C);

      Free (Title_C);
   end Set_Window_Title;

   ----------------------------
   -- Set_New_Display_Option --
   ----------------------------

   procedure Set_New_Display_Option
     (Option     : A5D.ALLEGRO_DISPLAY_OPTIONS;
      Value      : Integer;
      Importance : Integer)
   is
   begin
      A5D.al_set_new_display_option (Option, int (Value), int (Importance));
   end Set_New_Display_Option;

   ----------------------------
   -- Get_New_Display_Option --
   ----------------------------

   function Get_New_Display_Option
     (Option     :     Integer;
      Importance : out Integer)
      return Integer
   is
   begin
      return Integer (A5D.al_get_new_display_option (int (Option), int (Importance)));
   end Get_New_Display_Option;

   -------------------------------
   -- Reset_New_Display_Options --
   -------------------------------

   procedure Reset_New_Display_Options is
   begin
      A5D.al_reset_new_display_options;
   end Reset_New_Display_Options;

   ------------------------
   -- Get_Display_Option --
   ------------------------

   function Get_Display_Option
     (Display : Allegro_Display;
      Option  : A5D.ALLEGRO_DISPLAY_OPTIONS)
      return Integer
   is
   begin
      return Integer (A5D.al_get_display_option (Display.D, Option));
   end Get_Display_Option;

   -------------------------
   -- Hold_Bitmap_Drawing --
   -------------------------

   procedure Hold_Bitmap_Drawing (Hold : Boolean) is
   begin
      A5D.al_hold_bitmap_drawing (Extensions.bool (Boolean'Pos (Hold)));
   end Hold_Bitmap_Drawing;

   ----------------------------
   -- Is_Bitmap_Drawing_Held --
   ----------------------------

   function Is_Bitmap_Drawing_Held return Boolean is
   begin
      return 0 /= (A5D.al_is_bitmap_drawing_held);
   end Is_Bitmap_Drawing_Held;

end Allegro5.Thick.Display;
