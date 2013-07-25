with Interfaces.C; use Interfaces.C;


package Allegro5.Keycodes is

   subtype ALLEGRO_KEYCODE is unsigned;
   ALLEGRO_KEY_A : constant ALLEGRO_KEYCODE := 1;
   ALLEGRO_KEY_B : constant ALLEGRO_KEYCODE := 2;
   ALLEGRO_KEY_C : constant ALLEGRO_KEYCODE := 3;
   ALLEGRO_KEY_D : constant ALLEGRO_KEYCODE := 4;
   ALLEGRO_KEY_E : constant ALLEGRO_KEYCODE := 5;
   ALLEGRO_KEY_F : constant ALLEGRO_KEYCODE := 6;
   ALLEGRO_KEY_G : constant ALLEGRO_KEYCODE := 7;
   ALLEGRO_KEY_H : constant ALLEGRO_KEYCODE := 8;
   ALLEGRO_KEY_I : constant ALLEGRO_KEYCODE := 9;
   ALLEGRO_KEY_J : constant ALLEGRO_KEYCODE := 10;
   ALLEGRO_KEY_K : constant ALLEGRO_KEYCODE := 11;
   ALLEGRO_KEY_L : constant ALLEGRO_KEYCODE := 12;
   ALLEGRO_KEY_M : constant ALLEGRO_KEYCODE := 13;
   ALLEGRO_KEY_N : constant ALLEGRO_KEYCODE := 14;
   ALLEGRO_KEY_O : constant ALLEGRO_KEYCODE := 15;
   ALLEGRO_KEY_P : constant ALLEGRO_KEYCODE := 16;
   ALLEGRO_KEY_Q : constant ALLEGRO_KEYCODE := 17;
   ALLEGRO_KEY_R : constant ALLEGRO_KEYCODE := 18;
   ALLEGRO_KEY_S : constant ALLEGRO_KEYCODE := 19;
   ALLEGRO_KEY_T : constant ALLEGRO_KEYCODE := 20;
   ALLEGRO_KEY_U : constant ALLEGRO_KEYCODE := 21;
   ALLEGRO_KEY_V : constant ALLEGRO_KEYCODE := 22;
   ALLEGRO_KEY_W : constant ALLEGRO_KEYCODE := 23;
   ALLEGRO_KEY_X : constant ALLEGRO_KEYCODE := 24;
   ALLEGRO_KEY_Y : constant ALLEGRO_KEYCODE := 25;
   ALLEGRO_KEY_Z : constant ALLEGRO_KEYCODE := 26;
   ALLEGRO_KEY_0 : constant ALLEGRO_KEYCODE := 27;
   ALLEGRO_KEY_1 : constant ALLEGRO_KEYCODE := 28;
   ALLEGRO_KEY_2 : constant ALLEGRO_KEYCODE := 29;
   ALLEGRO_KEY_3 : constant ALLEGRO_KEYCODE := 30;
   ALLEGRO_KEY_4 : constant ALLEGRO_KEYCODE := 31;
   ALLEGRO_KEY_5 : constant ALLEGRO_KEYCODE := 32;
   ALLEGRO_KEY_6 : constant ALLEGRO_KEYCODE := 33;
   ALLEGRO_KEY_7 : constant ALLEGRO_KEYCODE := 34;
   ALLEGRO_KEY_8 : constant ALLEGRO_KEYCODE := 35;
   ALLEGRO_KEY_9 : constant ALLEGRO_KEYCODE := 36;
   ALLEGRO_KEY_PAD_0 : constant ALLEGRO_KEYCODE := 37;
   ALLEGRO_KEY_PAD_1 : constant ALLEGRO_KEYCODE := 38;
   ALLEGRO_KEY_PAD_2 : constant ALLEGRO_KEYCODE := 39;
   ALLEGRO_KEY_PAD_3 : constant ALLEGRO_KEYCODE := 40;
   ALLEGRO_KEY_PAD_4 : constant ALLEGRO_KEYCODE := 41;
   ALLEGRO_KEY_PAD_5 : constant ALLEGRO_KEYCODE := 42;
   ALLEGRO_KEY_PAD_6 : constant ALLEGRO_KEYCODE := 43;
   ALLEGRO_KEY_PAD_7 : constant ALLEGRO_KEYCODE := 44;
   ALLEGRO_KEY_PAD_8 : constant ALLEGRO_KEYCODE := 45;
   ALLEGRO_KEY_PAD_9 : constant ALLEGRO_KEYCODE := 46;
   ALLEGRO_KEY_F1 : constant ALLEGRO_KEYCODE := 47;
   ALLEGRO_KEY_F2 : constant ALLEGRO_KEYCODE := 48;
   ALLEGRO_KEY_F3 : constant ALLEGRO_KEYCODE := 49;
   ALLEGRO_KEY_F4 : constant ALLEGRO_KEYCODE := 50;
   ALLEGRO_KEY_F5 : constant ALLEGRO_KEYCODE := 51;
   ALLEGRO_KEY_F6 : constant ALLEGRO_KEYCODE := 52;
   ALLEGRO_KEY_F7 : constant ALLEGRO_KEYCODE := 53;
   ALLEGRO_KEY_F8 : constant ALLEGRO_KEYCODE := 54;
   ALLEGRO_KEY_F9 : constant ALLEGRO_KEYCODE := 55;
   ALLEGRO_KEY_F10 : constant ALLEGRO_KEYCODE := 56;
   ALLEGRO_KEY_F11 : constant ALLEGRO_KEYCODE := 57;
   ALLEGRO_KEY_F12 : constant ALLEGRO_KEYCODE := 58;
   ALLEGRO_KEY_ESCAPE : constant ALLEGRO_KEYCODE := 59;
   ALLEGRO_KEY_TILDE : constant ALLEGRO_KEYCODE := 60;
   ALLEGRO_KEY_MINUS : constant ALLEGRO_KEYCODE := 61;
   ALLEGRO_KEY_EQUALS : constant ALLEGRO_KEYCODE := 62;
   ALLEGRO_KEY_BACKSPACE : constant ALLEGRO_KEYCODE := 63;
   ALLEGRO_KEY_TAB : constant ALLEGRO_KEYCODE := 64;
   ALLEGRO_KEY_OPENBRACE : constant ALLEGRO_KEYCODE := 65;
   ALLEGRO_KEY_CLOSEBRACE : constant ALLEGRO_KEYCODE := 66;
   ALLEGRO_KEY_ENTER : constant ALLEGRO_KEYCODE := 67;
   ALLEGRO_KEY_SEMICOLON : constant ALLEGRO_KEYCODE := 68;
   ALLEGRO_KEY_QUOTE : constant ALLEGRO_KEYCODE := 69;
   ALLEGRO_KEY_BACKSLASH : constant ALLEGRO_KEYCODE := 70;
   ALLEGRO_KEY_BACKSLASH2 : constant ALLEGRO_KEYCODE := 71;
   ALLEGRO_KEY_COMMA : constant ALLEGRO_KEYCODE := 72;
   ALLEGRO_KEY_FULLSTOP : constant ALLEGRO_KEYCODE := 73;
   ALLEGRO_KEY_SLASH : constant ALLEGRO_KEYCODE := 74;
   ALLEGRO_KEY_SPACE : constant ALLEGRO_KEYCODE := 75;
   ALLEGRO_KEY_INSERT : constant ALLEGRO_KEYCODE := 76;
   ALLEGRO_KEY_DELETE : constant ALLEGRO_KEYCODE := 77;
   ALLEGRO_KEY_HOME : constant ALLEGRO_KEYCODE := 78;
   ALLEGRO_KEY_END : constant ALLEGRO_KEYCODE := 79;
   ALLEGRO_KEY_PGUP : constant ALLEGRO_KEYCODE := 80;
   ALLEGRO_KEY_PGDN : constant ALLEGRO_KEYCODE := 81;
   ALLEGRO_KEY_LEFT : constant ALLEGRO_KEYCODE := 82;
   ALLEGRO_KEY_RIGHT : constant ALLEGRO_KEYCODE := 83;
   ALLEGRO_KEY_UP : constant ALLEGRO_KEYCODE := 84;
   ALLEGRO_KEY_DOWN : constant ALLEGRO_KEYCODE := 85;
   ALLEGRO_KEY_PAD_SLASH : constant ALLEGRO_KEYCODE := 86;
   ALLEGRO_KEY_PAD_ASTERISK : constant ALLEGRO_KEYCODE := 87;
   ALLEGRO_KEY_PAD_MINUS : constant ALLEGRO_KEYCODE := 88;
   ALLEGRO_KEY_PAD_PLUS : constant ALLEGRO_KEYCODE := 89;
   ALLEGRO_KEY_PAD_DELETE : constant ALLEGRO_KEYCODE := 90;
   ALLEGRO_KEY_PAD_ENTER : constant ALLEGRO_KEYCODE := 91;
   ALLEGRO_KEY_PRINTSCREEN : constant ALLEGRO_KEYCODE := 92;
   ALLEGRO_KEY_PAUSE : constant ALLEGRO_KEYCODE := 93;
   ALLEGRO_KEY_ABNT_C1 : constant ALLEGRO_KEYCODE := 94;
   ALLEGRO_KEY_YEN : constant ALLEGRO_KEYCODE := 95;
   ALLEGRO_KEY_KANA : constant ALLEGRO_KEYCODE := 96;
   ALLEGRO_KEY_CONVERT : constant ALLEGRO_KEYCODE := 97;
   ALLEGRO_KEY_NOCONVERT : constant ALLEGRO_KEYCODE := 98;
   ALLEGRO_KEY_AT : constant ALLEGRO_KEYCODE := 99;
   ALLEGRO_KEY_CIRCUMFLEX : constant ALLEGRO_KEYCODE := 100;
   ALLEGRO_KEY_COLON2 : constant ALLEGRO_KEYCODE := 101;
   ALLEGRO_KEY_KANJI : constant ALLEGRO_KEYCODE := 102;
   ALLEGRO_KEY_PAD_EQUALS : constant ALLEGRO_KEYCODE := 103;
   ALLEGRO_KEY_BACKQUOTE : constant ALLEGRO_KEYCODE := 104;
   ALLEGRO_KEY_SEMICOLON2 : constant ALLEGRO_KEYCODE := 105;
   ALLEGRO_KEY_COMMAND : constant ALLEGRO_KEYCODE := 106;
   ALLEGRO_KEY_UNKNOWN : constant ALLEGRO_KEYCODE := 107;

   ALLEGRO_KEY_MODIFIERS : constant ALLEGRO_KEYCODE := 215;
   ALLEGRO_KEY_LSHIFT : constant ALLEGRO_KEYCODE := 215;
   ALLEGRO_KEY_RSHIFT : constant ALLEGRO_KEYCODE := 216;
   ALLEGRO_KEY_LCTRL : constant ALLEGRO_KEYCODE := 217;
   ALLEGRO_KEY_RCTRL : constant ALLEGRO_KEYCODE := 218;
   ALLEGRO_KEY_ALT : constant ALLEGRO_KEYCODE := 219;
   ALLEGRO_KEY_ALTGR : constant ALLEGRO_KEYCODE := 220;
   ALLEGRO_KEY_LWIN : constant ALLEGRO_KEYCODE := 221;
   ALLEGRO_KEY_RWIN : constant ALLEGRO_KEYCODE := 222;
   ALLEGRO_KEY_MENU : constant ALLEGRO_KEYCODE := 223;
   ALLEGRO_KEY_SCROLLLOCK : constant ALLEGRO_KEYCODE := 224;
   ALLEGRO_KEY_NUMLOCK : constant ALLEGRO_KEYCODE := 225;
   ALLEGRO_KEY_CAPSLOCK : constant ALLEGRO_KEYCODE := 226;
   ALLEGRO_KEY_MAX : constant ALLEGRO_KEYCODE := 227;

   subtype ALLEGRO_KEYMOD is unsigned;
   ALLEGRO_KEYMOD_SHIFT : constant ALLEGRO_KEYMOD := 1;
   ALLEGRO_KEYMOD_CTRL : constant ALLEGRO_KEYMOD := 2;
   ALLEGRO_KEYMOD_ALT : constant ALLEGRO_KEYMOD := 4;
   ALLEGRO_KEYMOD_LWIN : constant ALLEGRO_KEYMOD := 8;
   ALLEGRO_KEYMOD_RWIN : constant ALLEGRO_KEYMOD := 16;
   ALLEGRO_KEYMOD_MENU : constant ALLEGRO_KEYMOD := 32;
   ALLEGRO_KEYMOD_ALTGR : constant ALLEGRO_KEYMOD := 64;
   ALLEGRO_KEYMOD_COMMAND : constant ALLEGRO_KEYMOD := 128;
   ALLEGRO_KEYMOD_SCROLLLOCK : constant ALLEGRO_KEYMOD := 256;
   ALLEGRO_KEYMOD_NUMLOCK : constant ALLEGRO_KEYMOD := 512;
   ALLEGRO_KEYMOD_CAPSLOCK : constant ALLEGRO_KEYMOD := 1024;
   ALLEGRO_KEYMOD_INALTSEQ : constant ALLEGRO_KEYMOD := 2048;
   ALLEGRO_KEYMOD_ACCENT1 : constant ALLEGRO_KEYMOD := 4096;
   ALLEGRO_KEYMOD_ACCENT2 : constant ALLEGRO_KEYMOD := 8192;
   ALLEGRO_KEYMOD_ACCENT3 : constant ALLEGRO_KEYMOD := 16384;
   ALLEGRO_KEYMOD_ACCENT4 : constant ALLEGRO_KEYMOD := 32768;

end Allegro5.Keycodes;
