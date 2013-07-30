with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

with Allegro5.Altime;

package Allegro5.Threads is

   type ALLEGRO_THREAD is new System.Address;
   type ALLEGRO_MUTEX is new System.Address;
   type ALLEGRO_COND is new System.Address;

   function al_create_thread (proc : access function (thread : ALLEGRO_THREAD; arg : System.Address) return System.Address; arg : System.Address) return ALLEGRO_THREAD;
   pragma Import (C, al_create_thread, "al_create_thread");

   procedure al_start_thread (outer : ALLEGRO_THREAD);
   pragma Import (C, al_start_thread, "al_start_thread");

   procedure al_join_thread (outer : ALLEGRO_THREAD; ret_value : System.Address);
   pragma Import (C, al_join_thread, "al_join_thread");

   procedure al_set_thread_should_stop (outer : ALLEGRO_THREAD);
   pragma Import (C, al_set_thread_should_stop, "al_set_thread_should_stop");

   function al_get_thread_should_stop (outer : ALLEGRO_THREAD) return Extensions.bool;
   pragma Import (C, al_get_thread_should_stop, "al_get_thread_should_stop");

   procedure al_destroy_thread (thread : ALLEGRO_THREAD);
   pragma Import (C, al_destroy_thread, "al_destroy_thread");

   procedure al_run_detached_thread (proc : access function (arg1 : System.Address) return System.Address; arg : System.Address);
   pragma Import (C, al_run_detached_thread, "al_run_detached_thread");

   function al_create_mutex return ALLEGRO_MUTEX;
   pragma Import (C, al_create_mutex, "al_create_mutex");

   function al_create_mutex_recursive return ALLEGRO_MUTEX;
   pragma Import (C, al_create_mutex_recursive, "al_create_mutex_recursive");

   procedure al_lock_mutex (mutex : ALLEGRO_MUTEX);
   pragma Import (C, al_lock_mutex, "al_lock_mutex");

   procedure al_unlock_mutex (mutex : ALLEGRO_MUTEX);
   pragma Import (C, al_unlock_mutex, "al_unlock_mutex");

   procedure al_destroy_mutex (mutex : ALLEGRO_MUTEX);
   pragma Import (C, al_destroy_mutex, "al_destroy_mutex");

   function al_create_cond return ALLEGRO_MUTEX;
   pragma Import (C, al_create_cond, "al_create_cond");

   procedure al_destroy_cond (cond : ALLEGRO_MUTEX);
   pragma Import (C, al_destroy_cond, "al_destroy_cond");

   procedure al_wait_cond (cond : ALLEGRO_MUTEX; mutex : ALLEGRO_MUTEX);
   pragma Import (C, al_wait_cond, "al_wait_cond");

   function al_wait_cond_until
     (cond : ALLEGRO_COND;
      mutex : ALLEGRO_MUTEX;
      timeout : access constant Allegro5.Altime.ALLEGRO_TIMEOUT) return int;
   pragma Import (C, al_wait_cond_until, "al_wait_cond_until");

   procedure al_broadcast_cond (cond : ALLEGRO_COND);
   pragma Import (C, al_broadcast_cond, "al_broadcast_cond");

   procedure al_signal_cond (cond : ALLEGRO_COND);
   pragma Import (C, al_signal_cond, "al_signal_cond");

end Allegro5.Threads;
