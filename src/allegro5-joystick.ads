with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

limited with Allegro5.Events;

package Allegro5.Joystick is

   -- This is an abstract data type representing a physical joystick.
   type ALLEGRO_JOYSTICK is private;

   type ALLEGRO_JOYSTICK_STATE_axis_array is array (0 .. 2) of aliased Float;

   type stick is record
      axis : aliased ALLEGRO_JOYSTICK_STATE_axis_array;
   end record;
   pragma Convention (C, stick);

   type ALLEGRO_JOYSTICK_STATE_stick_array is array (0 .. 7) of aliased stick;
   type ALLEGRO_JOYSTICK_STATE_button_array is array (0 .. 31) of aliased int;

   -- This is a structure that is used to hold a "snapshot" of a joystick's
   --axes and buttons at a particular instant. All fields public and read-only.
   type ALLEGRO_JOYSTICK_STATE is record
      stick  : aliased ALLEGRO_JOYSTICK_STATE_stick_array;
      button : aliased ALLEGRO_JOYSTICK_STATE_button_array;
   end record;
   pragma Convention (C, ALLEGRO_JOYSTICK_STATE);

   -- ALLEGRO_JOYFLAG_DIGITAL - the stick provides digital input
   -- ALLEGRO_JOYFLAG_ANALOGUE - the stick provides analogue input
   --
   -- (this enum is a holdover from the old API and may be removed)
   subtype ALLEGRO_JOYFLAGS is unsigned;
   ALLEGRO_JOYFLAG_DIGITAL  : constant ALLEGRO_JOYFLAGS := 1;
   ALLEGRO_JOYFLAG_ANALOGUE : constant ALLEGRO_JOYFLAGS := 2;

   -- Install a joystick driver, returning true if successful. If a joystick
   --driver was already installed, returns true immediately.
   function al_install_joystick return  Extensions.bool;
   pragma Import (C, al_install_joystick, "al_install_joystick");

   -- Uninstalls the active joystick driver. All outstanding ALLEGRO_JOYSTICK
   --structures are invalidated. If no joystick driver was active, this
   --function does nothing.
   --
   -- This function is automatically called when Allegro is shut down.
   procedure al_uninstall_joystick;
   pragma Import (C, al_uninstall_joystick, "al_uninstall_joystick");

   -- Returns true if al_install_joystick was called successfully.
   function al_is_joystick_installed return  Extensions.bool;
   pragma Import (C, al_is_joystick_installed, "al_is_joystick_installed");

   -- Allegro is able to cope with users connecting and disconnected joystick
   --devices on-the-fly. On existing platforms, the joystick event source will
   --generate an event of type ALLEGRO_EVENT_JOYSTICK_CONFIGURATION when a
   --device is plugged in or unplugged. In response, you should call
   --al_reconfigure_joysticks.
   --
   -- Afterwards, the number returned by al_get_num_joysticks may be
   --different, and the handles returned by al_get_joystick may be different
   --or be ordered differently.
   --
   -- All ALLEGRO_JOYSTICK handles remain valid, but handles for disconnected
   --devices become inactive: their states will no longer update, and
   --al_get_joystick will not return the handle. Handles for devices which
   --remain connected will continue to represent the same devices. Previously
   --inactive handles may become active again, being reused to represent newly
   --connected devices.
   --
   -- Returns true if the joystick configuration changed, otherwise returns
   --false.
   --
   -- It is possible that on some systems, Allegro won't be able to generate
   --ALLEGRO_EVENT_JOYSTICK_CONFIGURATION events. If your game has an input
   --configuration screen or similar, you may wish to call
   --al_reconfigure_joysticks when entering that screen.
   function al_reconfigure_joysticks return  Extensions.bool;
   pragma Import (C, al_reconfigure_joysticks, "al_reconfigure_joysticks");

   -- Return the number of joysticks currently on the system (or potentially
   --on the system). This number can change after al_reconfigure_joysticks is
   --called, in order to support hotplugging.
   --
   -- Returns 0 if there is no joystick driver installed.
   function al_get_num_joysticks return int;
   pragma Import (C, al_get_num_joysticks, "al_get_num_joysticks");

   -- Get a handle for a joystick on the system. The number may be from 0 to
   --al_get_num_joysticks-1. If successful a pointer to a joystick object is
   --returned, which represents a physical device. Otherwise NULL is returned.
   --
   -- The handle and the index are only incidentally linked. After
   --al_reconfigure_joysticks is called, al_get_joystick may return handles in
   --a different order, and handles which represent disconnected devices will
   --not be returned.
   function al_get_joystick (joyn : int) return ALLEGRO_JOYSTICK;
   pragma Import (C, al_get_joystick, "al_get_joystick");

   -- This function currently does nothing.
   procedure al_release_joystick (joy : ALLEGRO_JOYSTICK);
   pragma Import (C, al_release_joystick, "al_release_joystick");

   -- Return if the joystick handle is "active", i.e. in the current
   --configuration, the handle represents some physical device plugged into
   --the system. al_get_joystick returns active handles. After
   --reconfiguration, active handles may become inactive, and vice versa.
   function al_get_joystick_active
     (joy  : ALLEGRO_JOYSTICK)
      return Extensions.bool;
   pragma Import (C, al_get_joystick_active, "al_get_joystick_active");

   -- Return the name of the given joystick.
   function al_get_joystick_name
     (joy  : ALLEGRO_JOYSTICK)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_name, "al_get_joystick_name");

   -- Return the number of "sticks" on the given joystick. A stick has one or
   --more axes.
   function al_get_joystick_num_sticks (joy : ALLEGRO_JOYSTICK) return int;
   pragma Import
     (C,
      al_get_joystick_num_sticks,
      "al_get_joystick_num_sticks");

   -- Return the flags of the given "stick". If the stick doesn't exist, NULL
   --is returned. Indices begin from 0.
   function al_get_joystick_stick_flags
     (joy   : ALLEGRO_JOYSTICK;
      stick : int)
      return  int;
   pragma Import
     (C,
      al_get_joystick_stick_flags,
      "al_get_joystick_stick_flags");

   -- Return the name of the given "stick". If the stick doesn't exist, NULL
   --is returned.
   function al_get_joystick_stick_name
     (joy   : ALLEGRO_JOYSTICK;
      stick : int)
      return  Interfaces.C.Strings.chars_ptr;
   pragma Import
     (C,
      al_get_joystick_stick_name,
      "al_get_joystick_stick_name");

   -- Return the number of axes on the given "stick". If the stick doesn't
   --exist, 0 is returned.
   function al_get_joystick_num_axes
     (joy   : ALLEGRO_JOYSTICK;
      stick : int)
      return  int;
   pragma Import (C, al_get_joystick_num_axes, "al_get_joystick_num_axes");

   -- Return the name of the given axis. If the axis doesn't exist, NULL is
   --returned. Indices begin from 0.
   function al_get_joystick_axis_name
     (joy   : ALLEGRO_JOYSTICK;
      stick : int;
      axis  : int)
      return  Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_joystick_axis_name, "al_get_joystick_axis_name");

   -- Return the number of buttons on the joystick.
   function al_get_joystick_num_buttons (joy : ALLEGRO_JOYSTICK) return int;
   pragma Import
     (C,
      al_get_joystick_num_buttons,
      "al_get_joystick_num_buttons");

   -- Return the name of the given button. If the button doesn't exist, NULL
   --is returned. Indices begin from 0.
   function al_get_joystick_button_name
     (joy     : ALLEGRO_JOYSTICK;
      buttonn : int)
      return    Interfaces.C.Strings.chars_ptr;
   pragma Import
     (C,
      al_get_joystick_button_name,
      "al_get_joystick_button_name");

   -- Get the current joystick state.
   procedure al_get_joystick_state
     (joy       : ALLEGRO_JOYSTICK;
      ret_state : ALLEGRO_JOYSTICK_STATE);
   pragma Import (C, al_get_joystick_state, "al_get_joystick_state");

   -- Returns the global joystick event source. All joystick events are
   --generated by this event source.
   function al_get_joystick_event_source return access
     Allegro5.Events.ALLEGRO_EVENT_SOURCE;
   pragma Import
     (C,
      al_get_joystick_event_source,
      "al_get_joystick_event_source");

private
   type ALLEGRO_JOYSTICK is new System.Address;

end Allegro5.Joystick;
