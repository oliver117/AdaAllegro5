with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with stdint;

with Allegro5.File;

package Allegro5.Fshook is

   EOF : constant := -1;

   type ALLEGRO_FS_INTERFACE;

   -- Opaque filesystem entry object. Represents a file or
   --a directory (check with al_get_fs_entry_mode). There are no user
   --accessible member variables.
   type ALLEGRO_FS_ENTRY is record
      vtable : access constant ALLEGRO_FS_INTERFACE;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FS_ENTRY);

   -- Filesystem modes/types
   subtype ALLEGRO_FILE_MODE is unsigned;
   ALLEGRO_FILEMODE_READ    : constant ALLEGRO_FILE_MODE := 1;
   ALLEGRO_FILEMODE_WRITE   : constant ALLEGRO_FILE_MODE := 2;
   ALLEGRO_FILEMODE_EXECUTE : constant ALLEGRO_FILE_MODE := 4;
   ALLEGRO_FILEMODE_HIDDEN  : constant ALLEGRO_FILE_MODE := 8;
   ALLEGRO_FILEMODE_ISFILE  : constant ALLEGRO_FILE_MODE := 16;
   ALLEGRO_FILEMODE_ISDIR   : constant ALLEGRO_FILE_MODE := 32;

   -- The available functions you can provide for a filesystem.
   type ALLEGRO_FS_INTERFACE is record
      fs_create_entry : access function
        (path : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_FS_ENTRY;
      fs_destroy_entry : access procedure (e : access ALLEGRO_FS_ENTRY);
      fs_entry_name    : access function
        (e : access ALLEGRO_FS_ENTRY) return Interfaces.C.Strings.chars_ptr;
      fs_update_entry : access function
        (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_entry_mode : access function
        (e : access ALLEGRO_FS_ENTRY) return Unsigned_32;
      fs_entry_atime : access function
        (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_mtime : access function
        (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_ctime : access function
        (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
      fs_entry_size : access function
        (e : access ALLEGRO_FS_ENTRY) return stdint.off_t;
      fs_entry_exists : access function
        (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_remove_entry : access function
        (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_open_directory : access function
        (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_read_directory : access function
        (e : access ALLEGRO_FS_ENTRY) return access ALLEGRO_FS_ENTRY;
      fs_close_directory : access function
        (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
      fs_filename_exists : access function
        (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_remove_filename : access function
        (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_get_current_directory : access function
        return Interfaces.C.Strings.chars_ptr;
      fs_change_directory : access function
        (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_make_directory : access function
        (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
      fs_open_file : access function
        (e    : access ALLEGRO_FS_ENTRY;
         mode : Interfaces.C.Strings.chars_ptr) return File.ALLEGRO_FILE;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_FS_INTERFACE);

   -- Creates an ALLEGRO_FS_ENTRY object pointing to path on the filesystem.
   --'path' can be a file or a directory and must not be NULL.
   function al_create_fs_entry
     (path : Interfaces.C.Strings.chars_ptr) return access ALLEGRO_FS_ENTRY;
   pragma Import (C, al_create_fs_entry, "al_create_fs_entry");

   -- Destroys a fs entry handle. The file or directory represented by it
   --is not destroyed. If the entry was opened, it is closed before
   --being destroyed.

   -- Does nothing if passed NULL.
   procedure al_destroy_fs_entry (e : access ALLEGRO_FS_ENTRY);
   pragma Import (C, al_destroy_fs_entry, "al_destroy_fs_entry");

   -- Returns the entry's filename path. Note that the filesystem encoding may
   --not be known and the conversion to UTF-8 could in very rare cases cause
   --this to return an invalid path. Therefore it's always safest to access
   --the file over its ALLEGRO_FS_ENTRY and not the path.

   -- On success returns a read only string which you must not modify
   --or destroy. Returns NULL on failure.

   -- Note: prior to 5.1.5 it was written: "... the path will not be
   --an absolute path if the entry wasn't created from an absolute path".
   --This is no longer true.
   function al_get_fs_entry_name
     (e : access ALLEGRO_FS_ENTRY) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_fs_entry_name, "al_get_fs_entry_name");

   -- Updates file status information for a filesystem entry. File status
   --information is automatically updated when the entry is created, however
   --you may update it again with this function, e.g. in case it changed.

   -- Returns true on success, false on failure. Fills in errno
   --to indicate the error.
   function al_update_fs_entry
     (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_update_fs_entry, "al_update_fs_entry");

   -- Returns the entry's mode flags, i.e. permissions and whether the entry
   --refers to a file or directory.
   function al_get_fs_entry_mode
     (e : access ALLEGRO_FS_ENTRY) return Unsigned_32;
   pragma Import (C, al_get_fs_entry_mode, "al_get_fs_entry_mode");

   -- Returns the time in seconds since the epoch since the
   --entry was last accessed.

   -- Warning: some filesystem either don't support this flag, or people
   --turn it off to increase performance. It may not be valid
   --in all circumstances.
   function al_get_fs_entry_atime
     (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_atime, "al_get_fs_entry_atime");

   -- Returns the time in seconds since the epoch since
   --the entry was last modified.
   function al_get_fs_entry_mtime
     (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_mtime, "al_get_fs_entry_mtime");

   -- Returns the time in seconds since the epoch this entry was created on
   --the filesystem.
   function al_get_fs_entry_ctime
     (e : access ALLEGRO_FS_ENTRY) return stdint.time_t;
   pragma Import (C, al_get_fs_entry_ctime, "al_get_fs_entry_ctime");

   -- Returns the time in seconds since the epoch since the
   --entry was last modified.
   function al_get_fs_entry_size
     (e : access ALLEGRO_FS_ENTRY) return stdint.off_t;
   pragma Import (C, al_get_fs_entry_size, "al_get_fs_entry_size");

   -- Check if the given entry exists on in the filesystem. Returns true
   --if it does exist or false if it doesn't exist, or an error occurred.
   --Error is indicated in Allegro's errno.
   function al_fs_entry_exists
     (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_fs_entry_exists, "al_fs_entry_exists");

   -- Delete this filesystem entry from the filesystem. Only files and empty
   --directories may be deleted.

   -- Returns true on success, and false on failure, error is indicated
   --in Allegro's errno.
   function al_remove_fs_entry
     (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_remove_fs_entry, "al_remove_fs_entry");

   -- Opens a directory entry object. You must call this before
   --using al_read_directory on an entry and you must call al_close_directory
   --when you no longer need it.

   -- Returns true on success.
   function al_open_directory
     (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_open_directory, "al_open_directory");

   -- Reads the next directory item and returns a filesystem entry for it.

   -- Returns NULL if there are no more entries or if an error occurs.
   --Call al_destroy_fs_entry on the returned entry when you are done with it.
   function al_read_directory
     (e : access ALLEGRO_FS_ENTRY) return access ALLEGRO_FS_ENTRY;
   pragma Import (C, al_read_directory, "al_read_directory");

   -- Closes a previously opened directory entry object.

   -- Returns true on success, false on failure and fills in Allegro's errno
   --to indicate the error.
   function al_close_directory
     (e : access ALLEGRO_FS_ENTRY) return Extensions.bool;
   pragma Import (C, al_close_directory, "al_close_directory");

   -- Check if the path exists on the filesystem, without creating
   --an ALLEGRO_FS_ENTRY object explicitly.
   function al_filename_exists
     (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_filename_exists, "al_filename_exists");

   -- Delete the given path from the filesystem, which may be a file or an
   --empty directory. This is the same as al_remove_fs_entry, except it expects
   --the path as a string.

   -- Returns true on success, and false on failure. Allegro's errno is filled
   --in to indicate the error.
   function al_remove_filename
     (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_remove_filename, "al_remove_filename");

   -- Returns the path to the current working directory, or NULL on failure.
   --The returned path is dynamically allocated and must be
   --destroyed with al_free.

   -- Allegro's errno is filled in to indicate the error if there is a failure.
   --This function may not be implemented on some (virtual) filesystems.
   function al_get_current_directory return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, al_get_current_directory, "al_get_current_directory");

   -- Changes the current working directory to 'path'.

   -- Returns true on success, false on error.
   function al_change_directory
     (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_change_directory, "al_change_directory");

   -- Creates a new directory on the filesystem. This function also creates
   --any parent directories as needed.

   -- Returns true on success (including if the directory already exists),
   --otherwise returns false on error. Fills in Allegro's errno
   --to indicate the error.
   function al_make_directory
     (path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;
   pragma Import (C, al_make_directory, "al_make_directory");

   -- Open an ALLEGRO_FILE handle to a filesystem entry, for the
   --given access mode. This is like calling al_fopen with the name of the
   --filesystem entry, but uses the appropriate file interface, not whatever
   --was set with the latest call to al_set_new_file_interface.

   -- Returns the handle on success, NULL on error.
   function al_open_fs_entry
     (e    : access ALLEGRO_FS_ENTRY;
      mode : Interfaces.C.Strings.chars_ptr) return File.ALLEGRO_FILE;
   pragma Import (C, al_open_fs_entry, "al_open_fs_entry");

   -- Return a pointer to the ALLEGRO_FS_INTERFACE table in effect
   --for the calling thread.
   function al_get_fs_interface return ALLEGRO_FS_INTERFACE;
   pragma Import (C, al_get_fs_interface, "al_get_fs_interface");

   -- Set the ALLEGRO_FS_INTERFACE table for the calling thread.
   procedure al_set_fs_interface (vtable : ALLEGRO_FS_INTERFACE);
   pragma Import (C, al_set_fs_interface, "al_set_fs_interface");

   -- Return the ALLEGRO_FS_INTERFACE table to the default,
   --for the calling thread.
   procedure al_set_standard_fs_interface;
   pragma Import
     (C,
      al_set_standard_fs_interface,
      "al_set_standard_fs_interface");

end Allegro5.Fshook;
