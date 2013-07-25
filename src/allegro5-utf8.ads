with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with Interfaces.C.Extensions;
with stdint;

package Allegro5.UTF8 is

   type u_al_tagbstring;
   subtype ALLEGRO_USTR is u_al_tagbstring;

   subtype ALLEGRO_USTR_INFO is u_al_tagbstring;

   type u_al_tagbstring is record
      mlen : aliased int;
      slen : aliased int;
      data : access unsigned_char;
   end record;
   pragma Convention (C_Pass_By_Copy, u_al_tagbstring);

   function al_ustr_new (s : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_new, "al_ustr_new");

   function al_ustr_new_from_buffer (s : Interfaces.C.Strings.chars_ptr; size : stdint.size_t) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_new_from_buffer, "al_ustr_new_from_buffer");

   function al_ustr_newf (fmt : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_newf, "al_ustr_newf");

   procedure al_ustr_free (us : access ALLEGRO_USTR);
   pragma Import (C, al_ustr_free, "al_ustr_free");

   function al_cstr (us : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_cstr, "al_cstr");

   procedure al_ustr_to_buffer
     (us : System.Address;
      buffer : Interfaces.C.Strings.chars_ptr;
      size : int);
   pragma Import (C, al_ustr_to_buffer, "al_ustr_to_buffer");

   function al_cstr_dup (us : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_cstr_dup, "al_cstr_dup");

   function al_ustr_dup (us : System.Address) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_dup, "al_ustr_dup");

   function al_ustr_dup_substr
     (us : System.Address;
      start_pos : int;
      end_pos : int) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_dup_substr, "al_ustr_dup_substr");

   function al_ustr_empty_string return System.Address;
   pragma Import (C, al_ustr_empty_string, "al_ustr_empty_string");

   function al_ref_cstr (info : access ALLEGRO_USTR_INFO; s : Interfaces.C.Strings.chars_ptr) return System.Address;
   pragma Import (C, al_ref_cstr, "al_ref_cstr");

   function al_ref_buffer
     (info : access ALLEGRO_USTR_INFO;
      s : Interfaces.C.Strings.chars_ptr;
      size : stdint.size_t) return System.Address;
   pragma Import (C, al_ref_buffer, "al_ref_buffer");

   function al_ref_ustr
     (info : access ALLEGRO_USTR_INFO;
      us : System.Address;
      start_pos : int;
      end_pos : int) return System.Address;
   pragma Import (C, al_ref_ustr, "al_ref_ustr");

   function al_ustr_size (us : System.Address) return stdint.size_t;
   pragma Import (C, al_ustr_size, "al_ustr_size");

   function al_ustr_length (us : System.Address) return stdint.size_t;
   pragma Import (C, al_ustr_length, "al_ustr_length");

   function al_ustr_offset (us : System.Address; index : int) return int;
   pragma Import (C, al_ustr_offset, "al_ustr_offset");

   function al_ustr_next (us : System.Address; pos : access int) return Extensions.bool;
   pragma Import (C, al_ustr_next, "al_ustr_next");

   function al_ustr_prev (us : System.Address; pos : access int) return Extensions.bool;
   pragma Import (C, al_ustr_prev, "al_ustr_prev");

   function al_ustr_get (us : System.Address; pos : int) return stdint.int32_t;
   pragma Import (C, al_ustr_get, "al_ustr_get");

   function al_ustr_get_next (us : System.Address; pos : access int) return stdint.int32_t;
   pragma Import (C, al_ustr_get_next, "al_ustr_get_next");

   function al_ustr_prev_get (us : System.Address; pos : access int) return stdint.int32_t;
   pragma Import (C, al_ustr_prev_get, "al_ustr_prev_get");

   function al_ustr_insert
     (us1 : access ALLEGRO_USTR;
      pos : int;
      us2 : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_insert, "al_ustr_insert");

   function al_ustr_insert_cstr
     (us : access ALLEGRO_USTR;
      pos : int;
      us2 : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_insert_cstr, "al_ustr_insert_cstr");

   function al_ustr_insert_chr
     (us : access ALLEGRO_USTR;
      pos : int;
      c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_ustr_insert_chr, "al_ustr_insert_chr");

   function al_ustr_append (us1 : access ALLEGRO_USTR; us2 : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_append, "al_ustr_append");

   function al_ustr_append_cstr (us : access ALLEGRO_USTR; s : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_append_cstr, "al_ustr_append_cstr");

   function al_ustr_append_chr (us : access ALLEGRO_USTR; c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_ustr_append_chr, "al_ustr_append_chr");

   function al_ustr_appendf (us : access ALLEGRO_USTR; fmt : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_appendf, "al_ustr_appendf");

   function al_ustr_vappendf
     (us : access ALLEGRO_USTR;
      fmt : Interfaces.C.Strings.chars_ptr;
      ap : access System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_vappendf, "al_ustr_vappendf");

   function al_ustr_remove_chr (us : access ALLEGRO_USTR; pos : int) return Extensions.bool;
   pragma Import (C, al_ustr_remove_chr, "al_ustr_remove_chr");

   function al_ustr_remove_range
     (us : access ALLEGRO_USTR;
      start_pos : int;
      end_pos : int) return Extensions.bool;
   pragma Import (C, al_ustr_remove_range, "al_ustr_remove_range");

   function al_ustr_truncate (us : access ALLEGRO_USTR; start_pos : int) return Extensions.bool;
   pragma Import (C, al_ustr_truncate, "al_ustr_truncate");

   function al_ustr_ltrim_ws (us : access ALLEGRO_USTR) return Extensions.bool;
   pragma Import (C, al_ustr_ltrim_ws, "al_ustr_ltrim_ws");

   function al_ustr_rtrim_ws (us : access ALLEGRO_USTR) return Extensions.bool;
   pragma Import (C, al_ustr_rtrim_ws, "al_ustr_rtrim_ws");

   function al_ustr_trim_ws (us : access ALLEGRO_USTR) return Extensions.bool;
   pragma Import (C, al_ustr_trim_ws, "al_ustr_trim_ws");

   function al_ustr_assign (us1 : access ALLEGRO_USTR; us2 : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_assign, "al_ustr_assign");

   function al_ustr_assign_substr
     (us1 : access ALLEGRO_USTR;
      us2 : System.Address;
      start_pos : int;
      end_pos : int) return Extensions.bool;
   pragma Import (C, al_ustr_assign_substr, "al_ustr_assign_substr");

   function al_ustr_assign_cstr (us1 : access ALLEGRO_USTR; s : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_assign_cstr, "al_ustr_assign_cstr");

   function al_ustr_set_chr
     (us : access ALLEGRO_USTR;
      pos : int;
      c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_ustr_set_chr, "al_ustr_set_chr");

   function al_ustr_replace_range
     (us1 : access ALLEGRO_USTR;
      start_pos1 : int;
      end_pos1 : int;
      us2 : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_replace_range, "al_ustr_replace_range");

   function al_ustr_find_chr
     (us : System.Address;
      start_pos : int;
      c : stdint.int32_t) return int;
   pragma Import (C, al_ustr_find_chr, "al_ustr_find_chr");

   function al_ustr_rfind_chr
     (us : System.Address;
      start_pos : int;
      c : stdint.int32_t) return int;
   pragma Import (C, al_ustr_rfind_chr, "al_ustr_rfind_chr");

   function al_ustr_find_set
     (us : System.Address;
      start_pos : int;
      c_accept : System.Address) return int;
   pragma Import (C, al_ustr_find_set, "al_ustr_find_set");

   function al_ustr_find_set_cstr
     (us : System.Address;
      start_pos : int;
      c_accept : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_ustr_find_set_cstr, "al_ustr_find_set_cstr");

   function al_ustr_find_cset
     (us : System.Address;
      start_pos : int;
      reject : System.Address) return int;
   pragma Import (C, al_ustr_find_cset, "al_ustr_find_cset");

   function al_ustr_find_cset_cstr
     (us : System.Address;
      start_pos : int;
      reject : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_ustr_find_cset_cstr, "al_ustr_find_cset_cstr");

   function al_ustr_find_str
     (haystack : System.Address;
      start_pos : int;
      needle : System.Address) return int;
   pragma Import (C, al_ustr_find_str, "al_ustr_find_str");

   function al_ustr_find_cstr
     (haystack : System.Address;
      start_pos : int;
      needle : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_ustr_find_cstr, "al_ustr_find_cstr");

   function al_ustr_rfind_str
     (haystack : System.Address;
      start_pos : int;
      needle : System.Address) return int;
   pragma Import (C, al_ustr_rfind_str, "al_ustr_rfind_str");

   function al_ustr_rfind_cstr
     (haystack : System.Address;
      start_pos : int;
      needle : Interfaces.C.Strings.chars_ptr) return int;
   pragma Import (C, al_ustr_rfind_cstr, "al_ustr_rfind_cstr");

   function al_ustr_find_replace
     (us : access ALLEGRO_USTR;
      start_pos : int;
      find : System.Address;
      replace : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_find_replace, "al_ustr_find_replace");

   function al_ustr_find_replace_cstr
     (us : access ALLEGRO_USTR;
      start_pos : int;
      find : Interfaces.C.Strings.chars_ptr;
      replace : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_find_replace_cstr, "al_ustr_find_replace_cstr");

   function al_ustr_equal (us1 : System.Address; us2 : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_equal, "al_ustr_equal");

   function al_ustr_compare (u : System.Address; v : System.Address) return int;
   pragma Import (C, al_ustr_compare, "al_ustr_compare");

   function al_ustr_ncompare
     (us1 : System.Address;
      us2 : System.Address;
      n : int) return int;
   pragma Import (C, al_ustr_ncompare, "al_ustr_ncompare");

   function al_ustr_has_prefix (u : System.Address; v : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_has_prefix, "al_ustr_has_prefix");

   function al_ustr_has_prefix_cstr (u : System.Address; s : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_has_prefix_cstr, "al_ustr_has_prefix_cstr");

   function al_ustr_has_suffix (u : System.Address; v : System.Address) return Extensions.bool;
   pragma Import (C, al_ustr_has_suffix, "al_ustr_has_suffix");

   function al_ustr_has_suffix_cstr (us1 : System.Address; s : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_ustr_has_suffix_cstr, "al_ustr_has_suffix_cstr");

   function al_utf8_width (c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_utf8_width, "al_utf8_width");

   function al_utf8_encode (s : Interfaces.C.Strings.chars_ptr; c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_utf8_encode, "al_utf8_encode");

   function al_ustr_new_from_utf16 (s : access stdint.uint16_t) return access ALLEGRO_USTR;
   pragma Import (C, al_ustr_new_from_utf16, "al_ustr_new_from_utf16");

   function al_ustr_size_utf16 (us : System.Address) return stdint.size_t;
   pragma Import (C, al_ustr_size_utf16, "al_ustr_size_utf16");

   function al_ustr_encode_utf16
     (us : System.Address;
      s : access stdint.uint16_t;
      n : stdint.size_t) return stdint.size_t;
   pragma Import (C, al_ustr_encode_utf16, "al_ustr_encode_utf16");

   function al_utf16_width (c : int) return stdint.size_t;
   pragma Import (C, al_utf16_width, "al_utf16_width");

   function al_utf16_encode (s : access stdint.uint16_t; c : stdint.int32_t) return stdint.size_t;
   pragma Import (C, al_utf16_encode, "al_utf16_encode");

end Allegro5.UTF8;
