/** 
 ** Copyright (c) 2006 Ingo Ruhnke <grumbel@gmx.de>
 ** 
 ** This software is provided 'as-is', without any express or implied
 ** warranty. In no event will the authors be held liable for any
 ** damages arising from the use of this software.
 ** 
 ** Permission is granted to anyone to use this software for any
 ** purpose, including commercial applications, and to alter it and
 ** redistribute it freely, subject to the following restrictions:
 ** 
 **   1. The origin of this software must not be misrepresented; you
 **      must not claim that you wrote the original software. If you
 **      use this software in a product, an acknowledgment in the
 **      product documentation would be appreciated but is not
 **      required.
 ** 
 **   2. Altered source versions must be plainly marked as such, and
 **      must not be misrepresented as being the original software.
 ** 
 **   3. This notice may not be removed or altered from any source
 **      distribution.
 ** 
 */

#ifndef _SDL_TTY_H
#define _SDL_TTY_H

#include <SDL.h>

/* Set up for C function definitions, even when using C++ */
#ifdef __cplusplus
extern "C" {
#endif

#define TTY_MAJOR_VERSION  0;
#define TTY_MINOR_VERSION  0;
#define TTY_PATH_VERSION   1;

/** Reuse SDL functions for error reporting */
#define TTY_SetError	SDL_SetError
#define TTY_GetError	SDL_GetError

/** 
 */
typedef struct TTY_Font
{
  /** 
   *  Surface containing the glyphs, glyphs have to be fixed width and
   *  run from left to right, seperating the glyphs to multiple lines
   *  is allowed
   */
  SDL_Surface* surface;

  /**
   *  Map characters to there index inside the font file,
   *  ie. translable['a'] will give the index of 'a'
   */
  char transtbl[256];

  int  glyph_width;
  int  glyph_height;
}  TTY_Font;

typedef struct TTY
{
  /** 
   *  The buffer containing all characters for display, access via
   *  framebuffer[y][x]
   */
  char** framebuffer;

  /** 
   *  The font that is used at default for rendering the framebuffer
   */
  TTY_Font* font;

  /** 
   *  The width of the framebuffer 
   */
  int width;

  /** 
   *  The height of the framebuffer 
   */
  int height;

  /** 
   *  The cursors x position in the framebuffer in screen coordinates,
   *  use TTY_SetCursor() to avoid throuble when scrolling
   */
  int cursor_x;

  /** 
   *  The cursors y position in the framebuffer in screen coordinates,
   *  use TTY_SetCursor() to avoid throuble when scrolling
   */
  int cursor_y;

  /**
   *  The character that should be used to display the cursor
   */
  int cursor_character;

  /** 
   *  If set the cursor is displayed
   */
  int print_cursor;

  /** 
   *  The scroll offset in x direction
   */
  int scroll_x;

  /** 
   *  The scroll offset in x direction
   */
  int scroll_y;

}  TTY;

#define TTY_CreateRGBSurface(name) SDL_CreateRGBSurfaceFrom( name##_data, \
                                      name##_width, name##_height, name##_bpp, name##_pitch, \
                                      name##_rmask, name##_gmask,  name##_bmask, name##_amask )

/**
 *  Creates a font from an SDL_Surface. The letter with index 0 is at
 *  the top/left of the image, 
 *
 *  @param surface      The SDL_Surface that contains all letters
 *  @param glyph_width  The width of a glyph
 *  @param glyph_height The height of a glyph
 *  @param letters      The letters that are present in the font
 */
TTY_Font* TTY_CreateFont(SDL_Surface* surface, int glyph_width, int glyph_height, const char* letters);
void      TTY_FreeFont(TTY_Font* font);

/**
 *  Calculate the position of character \a idx in the surface used by
 *  \a font and write the result to \a rect
 */
void TTY_GetGlypth(TTY_Font* font, char idx, SDL_Rect* rect);

enum {
  FNT_ALIGN_LEFT     = (1<<0),
  FNT_ALIGN_RIGHT    = (1<<1),
  FNT_ALIGN_H_CENTER = FNT_ALIGN_LEFT | FNT_ALIGN_RIGHT,

  FNT_ALIGN_TOP      = (1<<2),
  FNT_ALIGN_BOTTOM   = (1<<3),
  FNT_ALIGN_V_CENTER = FNT_ALIGN_TOP | FNT_ALIGN_BOTTOM,

  FNT_ALIGN_CENTER = FNT_ALIGN_H_CENTER | FNT_ALIGN_V_CENTER
};

/** 
 * Return the height, ie. lines * font_height, of a given text in
 * pixel
 */
int FNT_GetTextHeight(TTY_Font* font, const char* text);

/** 
 * Return the width, ie. the width of the longest line, of a given
 * text in pixel
 */
int FNT_GetTextWidth(TTY_Font* font, const char* text);

/** 
 * Return the width of the given line (everything after a \0 or a \n is ignored)
 */
int FNT_GetTextLineWidth(TTY_Font* font, const char* text);

/** 
 *  Print the given string to the screen at the given coordinates using \a font.
 */
void TTY_Print(TTY_Font* font, SDL_Surface* screen, int x, int y, Uint32 flags, const char *str);

/** 
 *  Print to the screen at the given coordinates, while handling \a
 *  fmt in a printf like manner
 */
void TTY_Printf(TTY_Font* font, SDL_Surface* screen, int x, int y, Uint32 flags, const char *fmt, ...)
  __attribute__ ((format (printf, 6, 7)));

/** 
 *  Allocate a new TTY with the given dimensions and font.
 *
 *  @param width  The width of the terminal in characters
 *  @param height The height of the terminal in characters
 *  @param font   The font used for displaying the terminal, the font
 *                will not be freed upon TTY_Free()

 */
TTY* TTY_Create(int width, int height, TTY_Font* font);

/**
 *  Deallocate a given TTY
 */
void TTY_Free(TTY* tty);

/**
 *  The the current cursor position to \a x, \a y, if x or y are
 *  outside the range of the TTY, they automatically wrap around 
 */
void TTY_SetCursor(TTY* tty, int x, int y);

void TTY_SetScrollOffset(TTY* tty, int scroll_x, int scroll_y);
void TTY_GetScrollOffset(TTY* tty, int* scroll_x, int* scroll_y);

/**
 *  Write the current cursor position to \a x and \a y 
 */
void TTY_GetCursor(TTY* tty, int* x, int* y);

/**
 *  Use character chr as the cursor
 */
void TTY_SetCursorCharacter(TTY* tty, int chr);

/**
 *  If \a i is set, display a cursor, if \a i is 0, don't display it
 */
void TTY_EnableVisibleCursor(TTY* tty, int i);

/** 
 *  Clear the tty's framebuffer
 */
void TTY_Clear(TTY* tty);

/** 
 *  Append the content of \a buffer to the given \a tty, \a len gives
 *  the length of the buffer.
 */
void TTY_write(TTY* tty, const char* buffer, int len);

/**
 *  Append the content of \a buffer to the given \a tty, \a buffer
 *  must be '\0' terminated.
 */
void TTY_print(TTY* tty, const char* buffer);

/**
 *  Append chr to \a tty
 */
void TTY_putchar(TTY* tty, char chr);

/**
 *  Put\a chr at the current cursor position without advancing the cursor
 */
void TTY_putchar_nomove(TTY* tty, char chr);

/**
 *  printf like way to append content to \a tty
 */
void TTY_printf(TTY* tty, const char *fmt, ...)  __attribute__ ((format (printf, 2, 3)));

/**
 *  Blit \a tty given to the surface \a screen at the given coordinates \a x and \a y
 */
void TTY_Blit(TTY* tty, SDL_Surface* screen, int x, int y);

/* Ends C function definitions when using C++ */
#ifdef __cplusplus
}
#endif

#endif

/* EOF */
