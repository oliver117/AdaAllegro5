AdaAllegro5
===========

A thin binding to the Allegro 5 open source, cross-platform
game programming libary.

http://alleg.sourceforge.net/

The Allegro 5.0 reference manual can be found here:

http://alleg.sourceforge.net/a5docs/5.0.10/

**Note:** The binding is generated directly from the allegro header files thus
the package names do not necessarily correspond to the manual's sections.

## TODO

### testing

* everything

### fix

* create display flags
* al_inhibit_screensaver is missing (Display)
* constants for al_get_standard_path
* ALLEGRO_KEY_MAX? (keycodes)
* change stdint defs <- mostly fixed
* utf8 - va_list
* merge fmaths_inline and fmaths?
* move al_(get|set)_new_display_adapter to monitor?
* add missing flags in font addon package
* bitmap_lock - pixel format (is in color)
* hyperlinks to the manual instead of inline docs?

### feature

* create bindings to addons

## inline docs for:

 * Allegro5.Addon.Primitives
 * Allegro5.File
 * Allegro5.Path
 * Allegro5.Threads
 * Allegro5.UTF8
