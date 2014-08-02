with Interfaces; use Interfaces;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

limited with Allegro5.Events;
with Allegro5.File;

use  Allegro5;

package Allegro5.Addon.Audio is

   ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT : constant := 513;
   ALLEGRO_EVENT_AUDIO_STREAM_FINISHED : constant := 514;

   ALLEGRO_MAX_CHANNELS : constant := 8;

   -- A special value for the pan property of samples and audio streams. Use
   --this value to disable panning on samples and audio streams, and play them
   --without attentuation implied by panning support.
   --
   -- ALLEGRO_AUDIO_PAN_NONE is different from a pan value of 0.0 (centered)
   --because, when panning is enabled, we try to maintain a constant sound
   --power level as a sample is panned from left to right. A sound coming out
   --of one speaker should sound as loud as it does when split over two
   --speakers. As a consequence, a sample with pan value 0.0 will be 3 dB
   --softer than the original level.
   ALLEGRO_AUDIO_PAN_NONE : constant := -1000.0;

   -- Sample depth and type, and signedness. Mixers only use 32-bit signed
   --float (-1..+1), or 16-bit signed integers. The unsigned value is a
   --bit-flag applied to the depth value.
   subtype ALLEGRO_AUDIO_DEPTH is unsigned;
   ALLEGRO_AUDIO_DEPTH_INT8     : constant ALLEGRO_AUDIO_DEPTH := 0;
   ALLEGRO_AUDIO_DEPTH_INT16    : constant ALLEGRO_AUDIO_DEPTH := 1;
   ALLEGRO_AUDIO_DEPTH_INT24    : constant ALLEGRO_AUDIO_DEPTH := 2;
   ALLEGRO_AUDIO_DEPTH_FLOAT32  : constant ALLEGRO_AUDIO_DEPTH := 3;
   ALLEGRO_AUDIO_DEPTH_UNSIGNED : constant ALLEGRO_AUDIO_DEPTH := 8;
   ALLEGRO_AUDIO_DEPTH_UINT8    : constant ALLEGRO_AUDIO_DEPTH := 8;
   ALLEGRO_AUDIO_DEPTH_UINT16   : constant ALLEGRO_AUDIO_DEPTH := 9;
   ALLEGRO_AUDIO_DEPTH_UINT24   : constant ALLEGRO_AUDIO_DEPTH := 10;

   -- Speaker configuration (mono, stereo, 2.1, etc).
   subtype ALLEGRO_CHANNEL_CONF is unsigned;
   ALLEGRO_CHANNEL_CONF_1   : constant ALLEGRO_CHANNEL_CONF := 16;
   ALLEGRO_CHANNEL_CONF_2   : constant ALLEGRO_CHANNEL_CONF := 32;
   ALLEGRO_CHANNEL_CONF_3   : constant ALLEGRO_CHANNEL_CONF := 48;
   ALLEGRO_CHANNEL_CONF_4   : constant ALLEGRO_CHANNEL_CONF := 64;
   ALLEGRO_CHANNEL_CONF_5_1 : constant ALLEGRO_CHANNEL_CONF := 81;
   ALLEGRO_CHANNEL_CONF_6_1 : constant ALLEGRO_CHANNEL_CONF := 97;
   ALLEGRO_CHANNEL_CONF_7_1 : constant ALLEGRO_CHANNEL_CONF := 113;

   -- Sample and stream playback mode.
   subtype ALLEGRO_PLAYMODE is unsigned;
   ALLEGRO_PLAYMODE_ONCE            : constant ALLEGRO_PLAYMODE := 256;
   ALLEGRO_PLAYMODE_LOOP            : constant ALLEGRO_PLAYMODE := 257;
   ALLEGRO_PLAYMODE_BIDIR           : constant ALLEGRO_PLAYMODE := 258;
   u_ALLEGRO_PLAYMODE_STREAM_ONCE   : constant ALLEGRO_PLAYMODE := 259;
   u_ALLEGRO_PLAYMODE_STREAM_ONEDIR : constant ALLEGRO_PLAYMODE := 260;

   -- ALLEGRO_MIXER_QUALITY_POINT - point sampling
   -- ALLEGRO_MIXER_QUALITY_LINEAR - linear interpolation
   -- ALLEGRO_MIXER_QUALITY_CUBIC - cubic interpolation
   subtype ALLEGRO_MIXER_QUALITY is unsigned;
   ALLEGRO_MIXER_QUALITY_POINT  : constant ALLEGRO_MIXER_QUALITY := 272;
   ALLEGRO_MIXER_QUALITY_LINEAR : constant ALLEGRO_MIXER_QUALITY := 273;
   ALLEGRO_MIXER_QUALITY_CUBIC  : constant ALLEGRO_MIXER_QUALITY := 274;

   -- An ALLEGRO_SAMPLE object stores the data necessary for playing
   --pre-defined digital audio. It holds information pertaining to data
   --length, frequency, channel configuration, etc. You can have an
   --ALLEGRO_SAMPLE object playing multiple times simultaneously. The object
   --holds a user-specified PCM data buffer, of the format the object is
   --created with.
   type ALLEGRO_SAMPLE is private;

   -- An ALLEGRO_SAMPLE_ID represents a sample being played via
   --al_play_sample. It can be used to later stop the sample with
   --al_stop_sample.
   type ALLEGRO_SAMPLE_ID is record
      u_index : aliased int;
      u_id    : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, ALLEGRO_SAMPLE_ID);

   -- An ALLEGRO_SAMPLE_INSTANCE object represents a playable instance of a
   --predefined sound effect. It holds information pertaining to the looping
   --mode, loop start/end points, playing position, etc. An instance uses the
   --data from an ALLEGRO_SAMPLE object. Multiple instances may be created
   --from the same ALLEGRO_SAMPLE. An ALLEGRO_SAMPLE must not be destroyed
   --while there are instances which reference it.
   --
   -- To be played, an ALLEGRO_SAMPLE_INSTANCE object must be attached to an
   --ALLEGRO_VOICE object, or to an ALLEGRO_MIXER object which is itself
   --attached to an ALLEGRO_VOICE object (or to another ALLEGRO_MIXER object
   --which is attached to an ALLEGRO_VOICE object, etc).
   type ALLEGRO_SAMPLE_INSTANCE is private;

   -- An ALLEGRO_AUDIO_STREAM object is used to stream generated audio to the
   --sound device, in real-time. This is done by reading from a buffer, which
   --is split into a number of fragments. Whenever a fragment has finished
   --playing, the user can refill it with new data.
   --
   -- As with ALLEGRO_SAMPLE_INSTANCE objects, streams store information
   --necessary for playback, so you may not play the same stream multiple
   --times simultaneously. Streams also need to be attached to an
   --ALLEGRO_VOICE object, or to an ALLEGRO_MIXER object which, eventually,
   --reaches an ALLEGRO_VOICE object.
   --
   -- While playing, you must periodically fill fragments with new audio data.
   --To know when a new fragment is ready to be filled, you can either
   --directly check with al_get_available_audio_stream_fragments, or listen to
   --events from the stream.
   --
   -- You can register an audio stream event source to an event queue; see
   --al_get_audio_stream_event_source. An ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT
   --event is generated whenever a new fragment is ready. When you receive an
   --event, use al_get_audio_stream_fragment to obtain a pointer to the
   --fragment to be filled. The size and format are determined by the
   --parameters passed to al_create_audio_stream.
   --
   -- If you're late with supplying new data, the stream will be silent until
   --new data is provided. You must call al_drain_audio_stream when you're
   --finished with supplying data to the stream.
   --
   -- If the stream is created by al_load_audio_stream then it can also
   --generate an ALLEGRO_EVENT_AUDIO_STREAM_FINISHED event if it reaches the
   --end of the file and is not set to loop.
   type ALLEGRO_AUDIO_STREAM is private;

   -- A mixer is a type of stream which mixes together attached streams into a
   --single buffer.
   type ALLEGRO_MIXER is private;

   -- A voice represents an audio device on the system, which may be a real
   --device, or an abstract device provided by the operating system. To play
   --back audio, you would attach a mixer or sample or stream to a voice.
   type ALLEGRO_VOICE is private;

   -- Create a sample data structure from the supplied buffer. If free_buf is
   --true then the buffer will be freed with al_free when the sample data
   --structure is destroyed. For portability (especially Windows), the buffer
   --should have been allocated with al_malloc. Otherwise you should free the
   --sample data yourself.
   function al_create_sample
     (buf       : System.Address;
      samples   : unsigned;
      freq      : unsigned;
      depth     : ALLEGRO_AUDIO_DEPTH;
      chan_conf : ALLEGRO_CHANNEL_CONF;
      free_buf  : Extensions.bool)
      return      ALLEGRO_SAMPLE;
   pragma Import (C, al_create_sample, "al_create_sample");

   -- Free the sample data structure. If it was created with the free_buf
   --parameter set to true, then the buffer will be freed with al_free.
   --
   -- This function will stop any sample instances which may be playing the
   --buffer referenced by the ALLEGRO_SAMPLE.
   procedure al_destroy_sample (spl : ALLEGRO_SAMPLE);
   pragma Import (C, al_destroy_sample, "al_destroy_sample");

   -- Creates a sample stream, using the supplied data. This must be attached
   --to a voice or mixer before it can be played. The argument may be NULL.
   --You can then set the data later with al_set_sample.
   function al_create_sample_instance
     (data : ALLEGRO_SAMPLE)
      return ALLEGRO_SAMPLE_INSTANCE;
   pragma Import (C, al_create_sample_instance, "al_create_sample_instance");

   -- Detaches the sample stream from anything it may be attached to and frees
   --it (the sample data is not freed!).
   procedure al_destroy_sample_instance (spl : ALLEGRO_SAMPLE_INSTANCE);
   pragma Import
     (C,
      al_destroy_sample_instance,
      "al_destroy_sample_instance");

   -- Return the frequency of the sample.
   function al_get_sample_frequency (spl : ALLEGRO_SAMPLE) return unsigned;
   pragma Import (C, al_get_sample_frequency, "al_get_sample_frequency");

   -- Return the length of the sample in sample values.
   function al_get_sample_length (spl : ALLEGRO_SAMPLE) return unsigned;
   pragma Import (C, al_get_sample_length, "al_get_sample_length");

   -- Return the audio depth.
   function al_get_sample_depth
     (spl  : ALLEGRO_SAMPLE)
      return ALLEGRO_AUDIO_DEPTH;
   pragma Import (C, al_get_sample_depth, "al_get_sample_depth");

   -- Return the channel configuration.
   function al_get_sample_channels
     (spl  : ALLEGRO_SAMPLE)
      return ALLEGRO_CHANNEL_CONF;
   pragma Import (C, al_get_sample_channels, "al_get_sample_channels");

   -- Return a pointer to the raw sample data.
   function al_get_sample_data (spl : ALLEGRO_SAMPLE) return System.Address;
   pragma Import (C, al_get_sample_data, "al_get_sample_data");

   -- Return the frequency of the sample instance.
   function al_get_sample_instance_frequency
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return unsigned;
   pragma Import
     (C,
      al_get_sample_instance_frequency,
      "al_get_sample_instance_frequency");

   -- Return the length of the sample instance in sample values.
   function al_get_sample_instance_length
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return unsigned;
   pragma Import
     (C,
      al_get_sample_instance_length,
      "al_get_sample_instance_length");

   -- Set the length of the sample instance in sample values.
   --
   -- Return true on success, false on failure. Will fail if the sample
   --instance is currently playing.
   function al_get_sample_instance_position
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return unsigned;
   pragma Import
     (C,
      al_get_sample_instance_position,
      "al_get_sample_instance_position");

   -- Return the relative playback speed.
   function al_get_sample_instance_speed
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Float;
   pragma Import
     (C,
      al_get_sample_instance_speed,
      "al_get_sample_instance_speed");

   -- Return the playback gain.
   function al_get_sample_instance_gain
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Float;
   pragma Import
     (C,
      al_get_sample_instance_gain,
      "al_get_sample_instance_gain");

   -- Get the pan value.
   function al_get_sample_instance_pan
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Float;
   pragma Import
     (C,
      al_get_sample_instance_pan,
      "al_get_sample_instance_pan");

   -- Return the length of the sample instance in seconds, assuming a playback
   --speed of 1.0.
   function al_get_sample_instance_time
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Float;
   pragma Import
     (C,
      al_get_sample_instance_time,
      "al_get_sample_instance_time");

   -- Return the audio depth.
   function al_get_sample_instance_depth
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return ALLEGRO_AUDIO_DEPTH;
   pragma Import
     (C,
      al_get_sample_instance_depth,
      "al_get_sample_instance_depth");

   -- Return the channel configuration.
   function al_get_sample_instance_channels
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return ALLEGRO_CHANNEL_CONF;
   pragma Import
     (C,
      al_get_sample_instance_channels,
      "al_get_sample_instance_channels");

   -- Return the playback mode.
   function al_get_sample_instance_playmode
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return ALLEGRO_PLAYMODE;
   pragma Import
     (C,
      al_get_sample_instance_playmode,
      "al_get_sample_instance_playmode");

   -- Return true if the sample instance is playing.
   function al_get_sample_instance_playing
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Extensions.bool;
   pragma Import
     (C,
      al_get_sample_instance_playing,
      "al_get_sample_instance_playing");

   -- Return whether the sample instance is attached to something.
   function al_get_sample_instance_attached
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Extensions.bool;
   pragma Import
     (C,
      al_get_sample_instance_attached,
      "al_get_sample_instance_attached");

   -- Get the playback position of a sample instance.
   function al_set_sample_instance_position
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : unsigned)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_position,
      "al_set_sample_instance_position");

   -- Set the length of the sample instance in sample values.
   --
   -- Return true on success, false on failure. Will fail if the sample
   --instance is currently playing.
   function al_set_sample_instance_length
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : unsigned)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_length,
      "al_set_sample_instance_length");

   -- Set the relative playback speed. 1.0 is normal speed.
   --
   -- Return true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_sample_instance_speed
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : Float)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_speed,
      "al_set_sample_instance_speed");

   -- Set the playback gain.
   --
   -- Returns true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_sample_instance_gain
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : Float)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_gain,
      "al_set_sample_instance_gain");

   -- Set the pan value on a sample instance. A value of -1.0 means to play
   --the sample only through the left speaker; +1.0 means only through the
   --right speaker; 0.0 means the sample is centre balanced. A special value
   --ALLEGRO_AUDIO_PAN_NONE disables panning and plays the sample at its
   --original level. This will be louder than a pan value of 0.0.
   --
   -- Returns true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_sample_instance_pan
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : Float)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_pan,
      "al_set_sample_instance_pan");

   -- Set the playback mode.
   --
   -- Returns true on success, false on failure.
   function al_set_sample_instance_playmode
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : ALLEGRO_PLAYMODE)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_playmode,
      "al_set_sample_instance_playmode");

   -- Change whether the sample instance is playing.
   --
   -- Returns true on success, false on failure.
   function al_set_sample_instance_playing
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      val  : Extensions.bool)
      return Extensions.bool;
   pragma Import
     (C,
      al_set_sample_instance_playing,
      "al_set_sample_instance_playing");

   -- Detach the sample instance from whatever it's attached to, if anything.
   --
   -- Returns true on success.
   function al_detach_sample_instance
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Extensions.bool;
   pragma Import (C, al_detach_sample_instance, "al_detach_sample_instance");

   -- Change the sample data that a sample instance plays. This can be quite
   --an involved process.
   --
   -- First, the sample is stopped if it is not already.
   --
   -- Next, if data is NULL, the sample is detached from its parent (if any).
   --
   -- If data is not NULL, the sample may be detached and reattached to its
   --parent (if any). This is not necessary if the old sample data and new
   --sample data have the same frequency, depth and channel configuration.
   --Reattaching may not always succeed.
   --
   -- On success, the sample remains stopped. The playback position and loop
   --end points are reset to their default values. The loop mode remains
   --unchanged.
   --
   -- Returns true on success, false on failure. On failure, the sample will
   --be stopped and detached from its parent.
   function al_set_sample
     (spl  : ALLEGRO_SAMPLE_INSTANCE;
      data : ALLEGRO_SAMPLE)
      return Extensions.bool;
   pragma Import (C, al_set_sample, "al_set_sample");

   -- Return the sample data that the sample instance plays.
   --
   -- Note this returns a pointer to an internal structure, not the
   --ALLEGRO_SAMPLE that you may have passed to al_set_sample. You may,
   --however, check which sample buffer is being played by the sample instance
   --with al_get_sample_data, and so on.
   function al_get_sample
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return ALLEGRO_SAMPLE;
   pragma Import (C, al_get_sample, "al_get_sample");

   -- Play an instance of a sample data. Returns true on success, false on
   --failure.
   function al_play_sample_instance
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Extensions.bool;
   pragma Import (C, al_play_sample_instance, "al_play_sample_instance");

   -- Stop an sample instance playing.
   function al_stop_sample_instance
     (spl  : ALLEGRO_SAMPLE_INSTANCE)
      return Extensions.bool;
   pragma Import (C, al_stop_sample_instance, "al_stop_sample_instance");

   -- Creates an ALLEGRO_AUDIO_STREAM. The stream will be set to play by
   --default. It will feed audio data from a buffer, which is split into a
   --number of fragments.
   --
   -- Parameters:
   --
   -- fragment_count - How many fragments to use for the audio stream. Usually
   --only two fragments are required - splitting the audio buffer in two
   --halves. But it means that the only time when new data can be supplied is
   --whenever one half has finished playing. When using many fragments, you
   --usually will use fewer samples for one, so there always will be (small)
   --fragments available to be filled with new data.
   --
   -- frag_samples - The size of a fragment in samples. See note below.
   --
   -- freq - The frequency, in Hertz.
   --
   -- depth - Must be one of the values listed for ALLEGRO_AUDIO_DEPTH.
   --
   -- chan_conf - Must be one of the values listed for ALLEGRO_CHANNEL_CONF.
   function al_create_audio_stream
     (buffer_count : size_t;
      samples      : unsigned;
      freq         : unsigned;
      depth        : ALLEGRO_AUDIO_DEPTH;
      chan_conf    : ALLEGRO_CHANNEL_CONF)
      return         ALLEGRO_AUDIO_STREAM;
   pragma Import (C, al_create_audio_stream, "al_create_audio_stream");

   -- Destroy an audio stream which was created with al_create_audio_stream or
   --al_load_audio_stream.
   procedure al_destroy_audio_stream (stream : ALLEGRO_AUDIO_STREAM);
   pragma Import (C, al_destroy_audio_stream, "al_destroy_audio_stream");

   -- You should call this to finalise an audio stream that you will no longer
   --be feeding, to wait for all pending buffers to finish playing. The
   --stream's playing state will change to false.
   procedure al_drain_audio_stream (stream : ALLEGRO_AUDIO_STREAM);  -- AA5/add
                                                                     --ons/audi
                                                                     --o/allegr
                                                                     --o5/alleg
                                                                     --ro_audio
                                                                     --.h:230
   pragma Import (C, al_drain_audio_stream, "al_drain_audio_stream");

   -- Return the stream frequency.
   function al_get_audio_stream_frequency
     (stream : ALLEGRO_AUDIO_STREAM)
      return   unsigned;
   pragma Import
     (C,
      al_get_audio_stream_frequency,
      "al_get_audio_stream_frequency");

   -- Return the stream length in samples.
   function al_get_audio_stream_length
     (stream : ALLEGRO_AUDIO_STREAM)
      return   unsigned;
   pragma Import
     (C,
      al_get_audio_stream_length,
      "al_get_audio_stream_length");

   -- Returns the number of fragments this stream uses. This is the same value
   --as passed to al_create_audio_stream when a new stream is created.
   function al_get_audio_stream_fragments
     (stream : ALLEGRO_AUDIO_STREAM)
      return   unsigned;
   pragma Import
     (C,
      al_get_audio_stream_fragments,
      "al_get_audio_stream_fragments");

   -- Returns the number of available fragments in the stream, that is,
   --fragments which are not currently filled with data for playback.
   function al_get_available_audio_stream_fragments
     (stream : ALLEGRO_AUDIO_STREAM)
      return   unsigned;
   pragma Import
     (C,
      al_get_available_audio_stream_fragments,
      "al_get_available_audio_stream_fragments");

   -- Return the relative playback speed.
   function al_get_audio_stream_speed
     (stream : ALLEGRO_AUDIO_STREAM)
      return   Float;
   pragma Import (C, al_get_audio_stream_speed, "al_get_audio_stream_speed");

   -- Return the playback gain.
   function al_get_audio_stream_gain
     (stream : ALLEGRO_AUDIO_STREAM)
      return   Float;
   pragma Import (C, al_get_audio_stream_gain, "al_get_audio_stream_gain");

   -- Get the pan value.
   function al_get_audio_stream_pan
     (stream : ALLEGRO_AUDIO_STREAM)
      return   Float;
   pragma Import (C, al_get_audio_stream_pan, "al_get_audio_stream_pan");

   -- Return the stream channel configuration.
   function al_get_audio_stream_channels
     (stream : ALLEGRO_AUDIO_STREAM)
      return   ALLEGRO_CHANNEL_CONF;
   pragma Import
     (C,
      al_get_audio_stream_channels,
      "al_get_audio_stream_channels");

   -- Return the stream audio depth.
   function al_get_audio_stream_depth
     (stream : ALLEGRO_AUDIO_STREAM)
      return   ALLEGRO_AUDIO_DEPTH;
   pragma Import (C, al_get_audio_stream_depth, "al_get_audio_stream_depth");

   -- Return the playback mode.
   function al_get_audio_stream_playmode
     (stream : ALLEGRO_AUDIO_STREAM)
      return   ALLEGRO_PLAYMODE;
   pragma Import
     (C,
      al_get_audio_stream_playmode,
      "al_get_audio_stream_playmode");

   -- Return true if the stream is playing.
   function al_get_audio_stream_playing
     (spl  : ALLEGRO_AUDIO_STREAM)
      return Extensions.bool;
   pragma Import
     (C,
      al_get_audio_stream_playing,
      "al_get_audio_stream_playing");

   -- Return whether the stream is attached to something.
   function al_get_audio_stream_attached
     (spl  : ALLEGRO_AUDIO_STREAM)
      return Extensions.bool;
   pragma Import
     (C,
      al_get_audio_stream_attached,
      "al_get_audio_stream_attached");

   -- When using Allegro's audio streaming, you will use this function to
   --continuously provide new sample data to a stream.
   --
   -- If the stream is ready for new data, the function will return the
   --address of an internal buffer to be filled with audio data. The length
   --and format of the buffer are specified with al_create_audio_stream or can
   --be queried with the various functions described here. Once the buffer is
   --filled, you must signal this to Allegro by passing the buffer to
   --al_set_audio_stream_fragment.
   --
   -- If the stream is not ready for new data, the function will return NULL.
   function al_get_audio_stream_fragment
     (stream : ALLEGRO_AUDIO_STREAM)
      return   System.Address;
   pragma Import
     (C,
      al_get_audio_stream_fragment,
      "al_get_audio_stream_fragment");

   -- Set the relative playback speed. 1.0 is normal speed.
   --
   -- Return true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_audio_stream_speed
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : Float)
      return   Extensions.bool;
   pragma Import (C, al_set_audio_stream_speed, "al_set_audio_stream_speed");

   -- Set the playback gain.
   --
   -- Returns true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_audio_stream_gain
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : Float)
      return   Extensions.bool;
   pragma Import (C, al_set_audio_stream_gain, "al_set_audio_stream_gain");

   -- Set the pan value on an audio stream. A value of -1.0 means to play the
   --stream only through the left speaker; +1.0 means only through the right
   --speaker; 0.0 means the sample is centre balanced. A special value
   --ALLEGRO_AUDIO_PAN_NONE disables panning and plays the stream at its
   --original level. This will be louder than a pan value of 0.0.
   --
   -- Returns true on success, false on failure. Will fail if the sample
   --instance is attached directly to a voice.
   function al_set_audio_stream_pan
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : Float)
      return   Extensions.bool;
   pragma Import (C, al_set_audio_stream_pan, "al_set_audio_stream_pan");

   -- Set the playback mode.
   --
   -- Returns true on success, false on failure.
   function al_set_audio_stream_playmode
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : ALLEGRO_PLAYMODE)
      return   Extensions.bool;
   pragma Import
     (C,
      al_set_audio_stream_playmode,
      "al_set_audio_stream_playmode");

   -- Change whether the stream is playing.
   --
   -- Returns true on success, false on failure.
   function al_set_audio_stream_playing
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : Extensions.bool)
      return   Extensions.bool;
   pragma Import
     (C,
      al_set_audio_stream_playing,
      "al_set_audio_stream_playing");

   -- Detach the stream from whatever it's attached to, if anything.
   function al_detach_audio_stream
     (stream : ALLEGRO_AUDIO_STREAM)
      return   Extensions.bool;
   pragma Import (C, al_detach_audio_stream, "al_detach_audio_stream");

   -- This function needs to be called for every successful call of
   --al_get_audio_stream_fragment to indicate that the buffer is filled with
   --new data.
   function al_set_audio_stream_fragment
     (stream : ALLEGRO_AUDIO_STREAM;
      val    : System.Address)
      return   Extensions.bool;
   pragma Import
     (C,
      al_set_audio_stream_fragment,
      "al_set_audio_stream_fragment");

   -- Set the streaming file playing position to the beginning. Returns true
   --on success. Currently this can only be called on streams created with
   --al_load_audio_stream, al_load_audio_stream_f and the format-specific
   --functions underlying those functions.
   function al_rewind_audio_stream
     (stream : ALLEGRO_AUDIO_STREAM)
      return   Extensions.bool;
   pragma Import (C, al_rewind_audio_stream, "al_rewind_audio_stream");

   -- Set the streaming file playing position to time. Returns true on
   --success. Currently this can only be called on streams created with
   --al_load_audio_stream, al_load_audio_stream_f and the format-specific
   --functions underlying those functions.
   function al_seek_audio_stream_secs
     (stream : ALLEGRO_AUDIO_STREAM;
      time   : double)
      return   Extensions.bool;
   pragma Import (C, al_seek_audio_stream_secs, "al_seek_audio_stream_secs");

   -- Return the position of the stream in seconds. Currently this can only be
   --called on streams created with al_load_audio_stream.
   function al_get_audio_stream_position_secs
     (stream : ALLEGRO_AUDIO_STREAM)
      return   double;
   pragma Import
     (C,
      al_get_audio_stream_position_secs,
      "al_get_audio_stream_position_secs");

   -- Return the length of the stream in seconds, if known. Otherwise returns
   --zero.
   --
   -- Currently this can only be called on streams created with
   --al_load_audio_stream, al_load_audio_stream_f and the format-specific
   --functions underlying those functions.
   function al_get_audio_stream_length_secs
     (stream : ALLEGRO_AUDIO_STREAM)
      return   double;
   pragma Import
     (C,
      al_get_audio_stream_length_secs,
      "al_get_audio_stream_length_secs");

   -- Sets the loop points for the stream in seconds. Currently this can only
   --be called on streams created with al_load_audio_stream,
   --al_load_audio_stream_f and the format-specific functions underlying those
   --functions.
   function al_set_audio_stream_loop_secs
     (stream : ALLEGRO_AUDIO_STREAM;
      start  : double;
      c_end  : double)
      return   Extensions.bool;
   pragma Import
     (C,
      al_set_audio_stream_loop_secs,
      "al_set_audio_stream_loop_secs");

   -- Retrieve the associated event source.
   --
   -- See al_get_audio_stream_fragment for a description of the
   --ALLEGRO_EVENT_AUDIO_STREAM_FRAGMENT event that audio streams emit.
   function al_get_audio_stream_event_source
     (stream : ALLEGRO_AUDIO_STREAM)
      return   access Events.ALLEGRO_EVENT_SOURCE;
   pragma Import
     (C,
      al_get_audio_stream_event_source,
      "al_get_audio_stream_event_source");

   -- Creates a mixer stream, to attach sample streams or other mixers to. It
   --will mix into a buffer at the requested frequency and channel count.
   --
   -- The only supported audio depths are ALLEGRO_AUDIO_DEPTH_FLOAT32 and
   --ALLEGRO_AUDIO_DEPTH_INT16 (not yet complete).
   --
   -- Returns true on success, false on error.
   function al_create_mixer
     (freq      : unsigned;
      depth     : ALLEGRO_AUDIO_DEPTH;
      chan_conf : ALLEGRO_CHANNEL_CONF)
      return      ALLEGRO_MIXER;
   pragma Import (C, al_create_mixer, "al_create_mixer");

   -- Destroys the mixer stream.
   procedure al_destroy_mixer (mixer : ALLEGRO_MIXER);
   pragma Import (C, al_destroy_mixer, "al_destroy_mixer");

   -- Attach a sample instance to a mixer. The instance must not already be
   --attached to anything.
   --
   -- Returns true on success, false on failure.
   function al_attach_sample_instance_to_mixer
     (spl   : ALLEGRO_SAMPLE_INSTANCE;
      mixer : ALLEGRO_MIXER)
      return  Extensions.bool;
   pragma Import
     (C,
      al_attach_sample_instance_to_mixer,
      "al_attach_sample_instance_to_mixer");

   -- Attach a stream to a mixer.
   --
   -- Returns true on success, false on failure.
   function al_attach_audio_stream_to_mixer
     (stream : ALLEGRO_AUDIO_STREAM;
      mixer  : ALLEGRO_MIXER)
      return   Extensions.bool;
   pragma Import
     (C,
      al_attach_audio_stream_to_mixer,
      "al_attach_audio_stream_to_mixer");

   -- Attaches a mixer onto another mixer. The same rules as with
   --al_attach_sample_instance_to_mixer apply, with the added caveat that both
   --mixers must be the same frequency. Returns true on success, false on
   --error.
   --
   -- Currently both mixers must have the same audio depth, otherwise the
   --function fails.
   function al_attach_mixer_to_mixer
     (stream : ALLEGRO_MIXER;
      mixer  : ALLEGRO_MIXER)
      return   Extensions.bool;
   pragma Import (C, al_attach_mixer_to_mixer, "al_attach_mixer_to_mixer");

   -- Sets a post-processing filter function that's called after the attached
   --streams have been mixed. The buffer's format will be whatever the mixer
   --was created with. The sample count and user-data pointer is also passed.
   function al_set_mixer_postprocess_callback
     (mixer                : ALLEGRO_MIXER;
      pp_callback          : access procedure
     (buf     : System.Address;
      samples : unsigned;
      data    : System.Address);
      pp_callback_userdata : System.Address)
      return                 Extensions.bool;
   pragma Import
     (C,
      al_set_mixer_postprocess_callback,
      "al_set_mixer_postprocess_callback");

   -- Return the mixer frequency.
   function al_get_mixer_frequency (mixer : ALLEGRO_MIXER) return unsigned;
   pragma Import (C, al_get_mixer_frequency, "al_get_mixer_frequency");

   -- Return the mixer channel configuration.
   function al_get_mixer_channels
     (mixer : ALLEGRO_MIXER)
      return  ALLEGRO_CHANNEL_CONF;
   pragma Import (C, al_get_mixer_channels, "al_get_mixer_channels");

   -- Return the mixer audio depth.
   function al_get_mixer_depth
     (mixer : ALLEGRO_MIXER)
      return  ALLEGRO_AUDIO_DEPTH;
   pragma Import (C, al_get_mixer_depth, "al_get_mixer_depth");

   -- Return the mixer quality.
   function al_get_mixer_quality
     (mixer : ALLEGRO_MIXER)
      return  ALLEGRO_MIXER_QUALITY;
   pragma Import (C, al_get_mixer_quality, "al_get_mixer_quality");

   -- Return the mixer gain (amplification factor). The default is 1.0.
   function al_get_mixer_gain (mixer : ALLEGRO_MIXER) return Float;
   pragma Import (C, al_get_mixer_gain, "al_get_mixer_gain");

   -- Return true if the mixer is playing.
   function al_get_mixer_playing
     (mixer : ALLEGRO_MIXER)
      return  Extensions.bool;
   pragma Import (C, al_get_mixer_playing, "al_get_mixer_playing");

   -- Return true if the mixer is attached to something.
   function al_get_mixer_attached
     (mixer : ALLEGRO_MIXER)
      return  Extensions.bool;
   pragma Import (C, al_get_mixer_attached, "al_get_mixer_attached");

   -- Set the mixer frequency. This will only work if the mixer is not
   --attached to anything.
   --
   -- Returns true on success, false on failure.
   function al_set_mixer_frequency
     (mixer : ALLEGRO_MIXER;
      val   : unsigned)
      return  Extensions.bool;
   pragma Import (C, al_set_mixer_frequency, "al_set_mixer_frequency");

   -- Set the mixer quality. This can only succeed if the mixer does not have
   --anything attached to it.
   --
   -- Returns true on success, false on failure.
   function al_set_mixer_quality
     (mixer : ALLEGRO_MIXER;
      val   : ALLEGRO_MIXER_QUALITY)
      return  Extensions.bool;
   pragma Import (C, al_set_mixer_quality, "al_set_mixer_quality");

   -- Set the mixer gain (amplification factor).
   --
   -- Returns true on success, false on failure.
   function al_set_mixer_gain
     (mixer : ALLEGRO_MIXER;
      gain  : Float)
      return  Extensions.bool;
   pragma Import (C, al_set_mixer_gain, "al_set_mixer_gain");

   -- Change whether the mixer is playing.
   --
   -- Returns true on success, false on failure.
   function al_set_mixer_playing
     (mixer : ALLEGRO_MIXER;
      val   : Extensions.bool)
      return  Extensions.bool;
   pragma Import (C, al_set_mixer_playing, "al_set_mixer_playing");

   -- Detach the mixer from whatever it is attached to, if anything.
   function al_detach_mixer (mixer : ALLEGRO_MIXER) return Extensions.bool;
   pragma Import (C, al_detach_mixer, "al_detach_mixer");

   -- Creates a voice structure and allocates a voice from the digital sound
   --driver. The passed frequency, sample format and channel configuration are
   --used as a hint to what kind of data will be sent to the voice. However,
   --the underlying sound driver is free to use non-matching values. For
   --example it may be the native format of the sound hardware. If a mixer is
   --attached to the voice, the mixer will convert from the mixer's format to
   --the voice format and care does not have to be taken for this.
   --
   -- However if you access the voice directly, make sure to not rely on the
   --parameters passed to this function, but instead query the returned voice
   --for the actual settings.
   function al_create_voice
     (freq      : unsigned;
      depth     : ALLEGRO_AUDIO_DEPTH;
      chan_conf : ALLEGRO_CHANNEL_CONF)
      return      ALLEGRO_VOICE;
   pragma Import (C, al_create_voice, "al_create_voice");

   -- Destroys the voice and deallocates it from the digital driver. Does
   --nothing if the voice is NULL.
   procedure al_destroy_voice (voice : ALLEGRO_VOICE);
   pragma Import (C, al_destroy_voice, "al_destroy_voice");

   -- Attaches a sample to a voice, and allows it to play. The sample's volume
   --and loop mode will be ignored, and it must have the same frequency and
   --depth (including signed-ness) as the voice. This function may fail if the
   --selected driver doesn't support preloading sample data.
   --
   -- At this time, we don't recommend attaching samples directly to voices.
   --Use a mixer in between.
   --
   -- Returns true on success, false on failure.
   function al_attach_sample_instance_to_voice
     (spl   : ALLEGRO_SAMPLE_INSTANCE;
      voice : ALLEGRO_VOICE)
      return  Extensions.bool;
   pragma Import
     (C,
      al_attach_sample_instance_to_voice,
      "al_attach_sample_instance_to_voice");

   -- Attaches an audio stream to a voice. The same rules as
   --al_attach_sample_instance_to_voice apply. This may fail if the driver
   --can't create a voice with the buffer count and buffer size the stream
   --uses.
   --
   -- An audio stream attached directly to a voice has a number of
   --limitations. The audio stream plays immediately and cannot be stopped.
   --The stream position, speed, gain, panning, cannot be changed. At this
   --time, we don't recommend attaching audio streams directly to voices. Use
   --a mixer in between.
   --
   -- Returns true on success, false on failure.
   function al_attach_audio_stream_to_voice
     (stream : ALLEGRO_AUDIO_STREAM;
      voice  : ALLEGRO_VOICE)
      return   Extensions.bool;
   pragma Import
     (C,
      al_attach_audio_stream_to_voice,
      "al_attach_audio_stream_to_voice");

   -- Attaches a mixer to a voice. The same rules as
   --al_attach_sample_instance_to_voice apply, with the exception of the depth
   --requirement.
   --
   -- Returns true on success, false on failure.
   function al_attach_mixer_to_voice
     (mixer : ALLEGRO_MIXER;
      voice : ALLEGRO_VOICE)
      return  Extensions.bool;
   pragma Import (C, al_attach_mixer_to_voice, "al_attach_mixer_to_voice");

   -- Detaches the mixer or sample or stream from the voice.
   procedure al_detach_voice (voice : ALLEGRO_VOICE);
   pragma Import (C, al_detach_voice, "al_detach_voice");

   -- Return the frequency of the voice, e.g. 44100.
   function al_get_voice_frequency (voice : ALLEGRO_VOICE) return unsigned;
   pragma Import (C, al_get_voice_frequency, "al_get_voice_frequency");

   -- When the voice has a non-streaming object attached to it, e.g. a sample,
   --returns the voice's current sample position. Otherwise, returns zero.
   function al_get_voice_position (voice : ALLEGRO_VOICE) return unsigned;
   pragma Import (C, al_get_voice_position, "al_get_voice_position");

   -- Return the channel configuration of the voice.
   function al_get_voice_channels
     (voice : ALLEGRO_VOICE)
      return  ALLEGRO_CHANNEL_CONF;
   pragma Import (C, al_get_voice_channels, "al_get_voice_channels");

   -- Return the audio depth of the voice.
   function al_get_voice_depth
     (voice : ALLEGRO_VOICE)
      return  ALLEGRO_AUDIO_DEPTH;
   pragma Import (C, al_get_voice_depth, "al_get_voice_depth");

   -- Return true if the voice is currently playing.
   function al_get_voice_playing
     (voice : ALLEGRO_VOICE)
      return  Extensions.bool;
   pragma Import (C, al_get_voice_playing, "al_get_voice_playing");

   -- Set the voice position. This can only work if the voice has a
   --non-streaming object attached to it, e.g. a sample instance.
   --
   -- Returns true on success, false on failure.
   function al_set_voice_position
     (voice : ALLEGRO_VOICE;
      val   : unsigned)
      return  Extensions.bool;
   pragma Import (C, al_set_voice_position, "al_set_voice_position");

   -- Change whether a voice is playing or not. This can only work if the
   --voice has a non-streaming object attached to it, e.g. a sample instance.
   --On success the voice's current sample position is reset.
   --
   -- Returns true on success, false on failure.
   function al_set_voice_playing
     (voice : ALLEGRO_VOICE;
      val   : Extensions.bool)
      return  Extensions.bool;
   pragma Import (C, al_set_voice_playing, "al_set_voice_playing");

   -- Install the audio subsystem.
   --
   -- Returns true on success, false on failure.
   --
   -- Note: most users will call al_reserve_samples and al_init_acodec_addon
   --after this.
   function al_install_audio return  Extensions.bool;
   pragma Import (C, al_install_audio, "al_install_audio");

   -- Uninstalls the audio subsystem.
   procedure al_uninstall_audio;
   pragma Import (C, al_uninstall_audio, "al_uninstall_audio");

   -- Returns true if al_install_audio was called previously and returned
   --successfully.
   function al_is_audio_installed return  Extensions.bool;
   pragma Import (C, al_is_audio_installed, "al_is_audio_installed");

   -- Returns the (compiled) version of the addon, in the same format as
   --al_get_allegro_version.
   function al_get_allegro_audio_version return  Unsigned_32;
   pragma Import
     (C,
      al_get_allegro_audio_version,
      "al_get_allegro_audio_version");

   -- Return the number of channels for the given channel configuration, which
   --is one of the values listed under ALLEGRO_CHANNEL_CONF.
   function al_get_channel_count
     (conf : ALLEGRO_CHANNEL_CONF)
      return size_t;
   pragma Import (C, al_get_channel_count, "al_get_channel_count");

   -- Return the size of a sample, in bytes, for the given format. The format
   --is one of the values listed under ALLEGRO_AUDIO_DEPTH.
   function al_get_audio_depth_size
     (conf : ALLEGRO_AUDIO_DEPTH)
      return size_t;
   pragma Import (C, al_get_audio_depth_size, "al_get_audio_depth_size");

   -- Reserves a number of sample instances, attaching them to the default
   --mixer. If no default mixer is set when this function is called, then it
   --will automatically create a voice with an attached mixer, which becomes
   --the default mixer.
   --
   -- Returns true on success, false on error. al_install_audio must have been
   --called first.
   function al_reserve_samples
     (reserve_samples : int)
      return            Extensions.bool;
   pragma Import (C, al_reserve_samples, "al_reserve_samples");

   -- Return the default mixer, or NULL if one has not been set. Although
   --different configurations of mixers and voices can be used, in most cases
   --a single mixer attached to a voice is what you want. The default mixer is
   --used by al_play_sample.
   function al_get_default_mixer return ALLEGRO_MIXER;
   pragma Import (C, al_get_default_mixer, "al_get_default_mixer");

   -- Sets the default mixer. All samples started with al_play_sample will be
   --stopped. If you are using your own mixer, this should be called before
   --al_reserve_samples.
   --
   -- Returns true on success, false on error.
   function al_set_default_mixer
     (mixer : ALLEGRO_MIXER)
      return  Extensions.bool;
   pragma Import (C, al_set_default_mixer, "al_set_default_mixer");

   -- Restores Allegro's default mixer. All samples started with
   --al_play_sample will be stopped. Returns true on success, false on error.
   function al_restore_default_mixer return  Extensions.bool;
   pragma Import (C, al_restore_default_mixer, "al_restore_default_mixer");

   -- Plays a sample on one of the sample instances created by
   --al_reserve_samples. Returns true on success, false on failure. Playback
   --may fail because all the reserved sample instances are currently used.
   --
   -- Parameters:
   --
   -- gain - relative volume at which the sample is played; 1.0 is normal.
   -- pan - 0.0 is centred, -1.0 is left, 1.0 is right, or
   --ALLEGRO_AUDIO_PAN_NONE.
   -- speed - relative speed at which the sample is played; 1.0 is normal.
   -- loop - ALLEGRO_PLAYMODE_ONCE, ALLEGRO_PLAYMODE_LOOP, or
   --ALLEGRO_PLAYMODE_BIDIR
   -- ret_id - if non-NULL the variable which this points to will be assigned
   --an id representing the sample being played.
   function al_play_sample
     (spl    : ALLEGRO_SAMPLE;
      gain   : Float;
      pan    : Float;
      speed  : Float;
      c_loop : ALLEGRO_PLAYMODE;
      ret_id : access ALLEGRO_SAMPLE_ID)
      return   Extensions.bool;
   pragma Import (C, al_play_sample, "al_play_sample");

   -- Stop the sample started by al_play_sample.
   procedure al_stop_sample (spl_id : access ALLEGRO_SAMPLE_ID);
   pragma Import (C, al_stop_sample, "al_stop_sample");

   -- Stop all samples started by al_play_sample.
   procedure al_stop_samples;
   pragma Import (C, al_stop_samples, "al_stop_samples");

   -- Register a handler for al_load_sample. The given function will be used
   --to handle the loading of sample files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_sample_loader
     (ext    : Interfaces.C.Strings.chars_ptr;
      loader : access function
     (filename : Interfaces.C.Strings.chars_ptr)
      return     ALLEGRO_SAMPLE)
      return   Extensions.bool;
   pragma Import (C, al_register_sample_loader, "al_register_sample_loader");

   -- Register a handler for al_save_sample. The given function will be used
   --to handle the saving of sample files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The saver argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_sample_saver
     (ext   : Interfaces.C.Strings.chars_ptr;
      saver : access function
     (filename : Interfaces.C.Strings.chars_ptr;
      spl      : ALLEGRO_SAMPLE)
      return     Extensions.bool)
      return  Extensions.bool;
   pragma Import (C, al_register_sample_saver, "al_register_sample_saver");

   -- Register a handler for al_load_audio_stream. The given function will be
   --used to open streams from files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The stream_loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_audio_stream_loader
     (ext           : Interfaces.C.Strings.chars_ptr;
      stream_loader : access function
     (filename     : Interfaces.C.Strings.chars_ptr;
      buffer_count : size_t;
      samples      : unsigned)
      return         ALLEGRO_AUDIO_STREAM)
      return          Extensions.bool;
   pragma Import
     (C,
      al_register_audio_stream_loader,
      "al_register_audio_stream_loader");

   -- Register a handler for al_load_sample_f. The given function will be used
   --to handle the loading of sample files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_sample_loader_f
     (ext    : Interfaces.C.Strings.chars_ptr;
      loader : access function
     (fp   : File.ALLEGRO_FILE)
      return ALLEGRO_SAMPLE)
      return   Extensions.bool;
   pragma Import
     (C,
      al_register_sample_loader_f,
      "al_register_sample_loader_f");

   -- Register a handler for al_save_sample_f. The given function will be used
   --to handle the saving of sample files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The saver argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_sample_saver_f
     (ext   : Interfaces.C.Strings.chars_ptr;
      saver : access function
     (fp   : File.ALLEGRO_FILE;
      spl  : ALLEGRO_SAMPLE)
      return Extensions.bool)
      return  Extensions.bool;
   pragma Import
     (C,
      al_register_sample_saver_f,
      "al_register_sample_saver_f");

   -- Register a handler for al_load_audio_stream_f. The given function will
   --be used to open streams from files with the given extension.
   --
   -- The extension should include the leading dot ('.') character. It will be
   --matched case-insensitively.
   --
   -- The stream_loader argument may be NULL to unregister an entry.
   --
   -- Returns true on success, false on error. Returns false if unregistering
   --an entry that doesn't exist.
   function al_register_audio_stream_loader_f
     (ext           : Interfaces.C.Strings.chars_ptr;
      stream_loader : access function
     (fp           : File.ALLEGRO_FILE;
      buffer_count : size_t;
      samples      : unsigned)
      return         ALLEGRO_AUDIO_STREAM)
      return          Extensions.bool;
   pragma Import
     (C,
      al_register_audio_stream_loader_f,
      "al_register_audio_stream_loader_f");

   -- Loads a few different audio file formats based on their extension.
   --
   -- Note that this stores the entire file in memory at once, which may be
   --time consuming. To read the file as it is needed, use
   --al_load_audio_stream.
   --
   -- Returns the sample on success, NULL on failure.
   function al_load_sample
     (filename : Interfaces.C.Strings.chars_ptr)
      return     ALLEGRO_SAMPLE;
   pragma Import (C, al_load_sample, "al_load_sample");

   -- Writes a sample into a file. Currently, wav is the only supported
   --format, and the extension must be ".wav".
   --
   -- Returns true on success, false on error.
   function al_save_sample
     (filename : Interfaces.C.Strings.chars_ptr;
      spl      : ALLEGRO_SAMPLE)
      return     Extensions.bool;
   pragma Import (C, al_save_sample, "al_save_sample");

   -- Loads an audio file from disk as it is needed.
   --
   -- Unlike regular streams, the one returned by this function need not be
   --fed by the user; the library will automatically read more of the file as
   --it is needed. The stream will contain buffer_count buffers with samples
   --samples.
   --
   -- The audio stream will start in the playing state. It should be attached
   --to a voice or mixer to generate any output. See ALLEGRO_AUDIO_STREAM for
   --more details.
   --
   -- Returns the stream on success, NULL on failure.
   function al_load_audio_stream
     (filename     : Interfaces.C.Strings.chars_ptr;
      buffer_count : size_t;
      samples      : unsigned)
      return         ALLEGRO_AUDIO_STREAM;
   pragma Import (C, al_load_audio_stream, "al_load_audio_stream");

   -- Loads an audio file from an ALLEGRO_FILE stream into an ALLEGRO_SAMPLE.
   --The file type is determined by the passed 'ident' parameter, which is a
   --file name extension including the leading dot.
   --
   -- Note that this stores the entire file in memory at once, which may be
   --time consuming. To read the file as it is needed, use
   --al_load_audio_stream_f.
   --
   -- Returns the sample on success, NULL on failure. The file remains open
   --afterwards.
   function al_load_sample_f
     (fp    : File.ALLEGRO_FILE;
      ident : Interfaces.C.Strings.chars_ptr)
      return  ALLEGRO_SAMPLE;
   pragma Import (C, al_load_sample_f, "al_load_sample_f");

   -- Writes a sample into a ALLEGRO_FILE filestream. Currently, wav is the
   --only supported format, and the extension must be ".wav".
   --
   -- Returns true on success, false on error. The file remains open
   --afterwards.
   function al_save_sample_f
     (fp    : File.ALLEGRO_FILE;
      ident : Interfaces.C.Strings.chars_ptr;
      spl   : ALLEGRO_SAMPLE)
      return  Extensions.bool;
   pragma Import (C, al_save_sample_f, "al_save_sample_f");

   -- Loads an audio file from ALLEGRO_FILE stream as it is needed.
   --
   -- Unlike regular streams, the one returned by this function need not be
   --fed by the user; the library will automatically read more of the file as
   --it is needed. The stream will contain buffer_count buffers with samples
   --samples.
   --
   -- The file type is determined by the passed 'ident' parameter, which is a
   --file name extension including the leading dot.
   --
   -- The audio stream will start in the playing state. It should be attached
   --to a voice or mixer to generate any output. See ALLEGRO_AUDIO_STREAM for
   --more details.
   --
   -- Returns the stream on success, NULL on failure. On success the file
   --should be considered owned by the audio stream, and will be closed when
   --the audio stream is destroyed. On failure the file will be closed.
   function al_load_audio_stream_f
     (fp           : File.ALLEGRO_FILE;
      ident        : Interfaces.C.Strings.chars_ptr;
      buffer_count : size_t;
      samples      : unsigned)
      return         ALLEGRO_AUDIO_STREAM;
   pragma Import (C, al_load_audio_stream_f, "al_load_audio_stream_f");

private
   type ALLEGRO_SAMPLE is new System.Address;
   type ALLEGRO_SAMPLE_INSTANCE is new System.Address;
   type ALLEGRO_AUDIO_STREAM is new System.Address;
   type ALLEGRO_MIXER is new System.Address;
   type ALLEGRO_VOICE is new System.Address;

end Allegro5.Addon.Audio;
