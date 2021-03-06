with Interfaces; use Interfaces;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;

package Allegro5.Addon.ACodec is

   -- This function registers all the known audio file type handlers for
   --al_load_sample, al_save_sample, al_load_audio_stream, etc.
   --
   -- Depending on what libraries are available, the full set of recognised
   --extensions is: .wav, .flac, .ogg, .it, .mod, .s3m, .xm.
   --
   -- Limitations:
   --
   -- Saving is only supported for wav files.
   --
   -- Wav file loader currently only supports 8/16 bit little endian PCM
   --files. 16 bits are used when saving wav files. Use flac files if more
   --precision is required.
   --
   -- Module files (.it, .mod, .s3m, .xm) are often composed with streaming in
   --mind, and sometimes cannot be easily rendered into a finite length
   --sample. Therefore they cannot be loaded with
   --al_load_sample/al_load_sample_f and must be streamed with
   --al_load_audio_stream or al_load_audio_stream_f.
   --
   -- Return true on success.
   function al_init_acodec_addon return  Extensions.bool;
   pragma Import (C, al_init_acodec_addon, "al_init_acodec_addon");

   -- Returns the (compiled) version of the addon, in the same format as
   --al_get_allegro_version.
   function al_get_allegro_acodec_version return Unsigned_32;
   pragma Import
     (C,
      al_get_allegro_acodec_version,
      "al_get_allegro_acodec_version");

end Allegro5.Addon.ACodec;
