with Allegro5;
with Allegro5.Addon;
with Allegro5.Addon.ACodec;
with Allegro5.Addon.Audio;
with Allegro5.Addon.Font;
with Allegro5.Addon.Image;
with Allegro5.Addon.Primitives;
with Allegro5.Addon.TTF;

with Allegro5.Altime;
with Allegro5.Base;
with Allegro5.Bitmap;
with allegro5.Bitmap_Draw;
with Allegro5.Bitmap_IO;
with Allegro5.Bitmap_Lock;
with allegro5.Blender;
with Allegro5.Color;
with Allegro5.Config;
with Allegro5.Debug;
with Allegro5.Display;
with Allegro5.Drawing;
with Allegro5.Error;
with Allegro5.Events;
with Allegro5.File;
with Allegro5.Fixed;
with Allegro5.Fmaths;
with Allegro5.Fshook;
with Allegro5.Fullscreen_Mode;
with Allegro5.Inline_Fmaths;
with Allegro5.Joystick;
with Allegro5.Keyboard;
with Allegro5.Keycodes;
with Allegro5.Memory;
with Allegro5.Monitor;
with Allegro5.Mouse;
with Allegro5.Mouse_Cursor;
with Allegro5.Path;
with Allegro5.State;
with Allegro5.System;
with Allegro5.Threads;
with Allegro5.Timer;
with Allegro5.Transformations;
with Allegro5.UTF8;

procedure Build_Check is
begin
   -- This file's purpose is to simply include all compilation units
   -- of this project, so that it might be used as a build target in order
   -- to check all the files for errors.
   null;
end Build_Check;
