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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <SDL.h>
#include <SDL_image.h>
#include "SDL_tty.h"
#include <string.h>

static int modulo(int x, int y)
{
  int xmody = x - (x / y) * y;
  if (xmody && ((y ^ xmody) < 0)) {
    xmody += y;
  }
  return xmody;
}

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char *
make_message(const char *fmt, va_list ap)
{
  /* Guess we need no more than 100 bytes. */
  int n, size = 100;
  char *p, *np;
 
  if ((p = malloc (size)) == NULL)
    return NULL;

  while (1) {
    /* Try to print in the allocated space. */
    //va_start(ap, fmt);

    n = vsnprintf (p, size, fmt, ap);
    va_end(ap);
    /* If that worked, return the string. */
    if (n > -1 && n < size)
      return p;
    /* Else try again with more space. */
    if (n > -1)    /* glibc 2.1 */
      size = n+1; /* precisely what is needed */
    else           /* glibc 2.0 */
      size *= 2;  /* twice the old size */
    if ((np = realloc (p, size)) == NULL) {
      free(p);
      return NULL;
    } else {
      p = np;
    }
  }
}

TTY_Font*
TTY_CreateFont(SDL_Surface* surface, int glyph_width, int glyph_height, const char* letters)
{
  int i;
  TTY_Font* font = (TTY_Font*)malloc(sizeof(TTY_Font));

  font->surface = SDL_DisplayFormatAlpha(surface);

  if (!font->surface) 
    {
      TTY_SetError("TTY_CreateFont: conversation of surface failed");
      return 0;
    }
  
  memset(font->transtbl, 0, 256);
  for(i = 0; letters[i] != '\0'; ++i)
    font->transtbl[(int)letters[i]] = i;

  font->glyph_width  = glyph_width;
  font->glyph_height = glyph_height;

  return font;
}

void
TTY_FreeFont(TTY_Font* font)
{
  SDL_FreeSurface(font->surface);
  free(font);
}

void
TTY_GetGlyph(TTY_Font* font, char idx, SDL_Rect* rect)
{
  idx = font->transtbl[(int)idx];

  rect->x = (idx % (font->surface->w / font->glyph_width)) * font->glyph_width;
  rect->y = (idx / (font->surface->w / font->glyph_width)) * font->glyph_height;

  rect->w = font->glyph_width;
  rect->h = font->glyph_height;
}

void TTY_Printf(TTY_Font* font, SDL_Surface* screen, int x, int y, Uint32 flags, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  char* str = make_message(fmt, ap);
  TTY_Print(font, screen, x, y, flags, str);
  free(str);
}

int FNT_GetTextHeight(TTY_Font* font, const char* text)
{
  int lines = 1;
  int i;
  for(i = 0; text[i] != '\0'; ++i)
    {
      if (text[i] == '\n')
        {
          lines += 1;
        }
    }
  return lines * font->glyph_height;
}

int FNT_GetTextWidth(TTY_Font* font, const char* text)
{
  int longest_line = 0;
  int line = 0;
  int i;
  for(i = 0; text[i] != '\0'; ++i)
    {
      if (text[i] == '\n')
        {
          if (line > longest_line)
            longest_line = line;
          line = 0;
        }
      else
        {
          line += 1;
        }
    }

  if (line > longest_line)
    longest_line = line;

  return longest_line * font->glyph_width;
}

int FNT_GetTextLineWidth(TTY_Font* font, const char* text)
{
  int i;
  for(i = 0; text[i] != '\0' && text[i] != '\n'; ++i);
  return i * font->glyph_width;
}

void
TTY_Print(TTY_Font* font, SDL_Surface* screen, int x, int y, Uint32 flags, const char *str)
{
  SDL_Rect src_rect;
  SDL_Rect dst_rect;

  int x_of = 0;
  int y_of = 0;

  int i;

  int text_width  = FNT_GetTextWidth(font, str);
  int text_height = FNT_GetTextHeight(font, str);

  if ((flags & FNT_ALIGN_LEFT) && (flags & FNT_ALIGN_RIGHT))
    {
      x -= text_width/2;
    }
  else if (flags & FNT_ALIGN_LEFT)
    {
    }
  else if (flags & FNT_ALIGN_RIGHT)
    {
      x -= text_width;
    }

  if ((flags & FNT_ALIGN_TOP) && (flags & FNT_ALIGN_BOTTOM))
    {
      y -= text_height/2;
    }
  else if (flags & FNT_ALIGN_TOP)
    {
    }
  else if (flags & FNT_ALIGN_BOTTOM)
    {
      y -= text_height;
    }

  for(i = 0; str[i] != '\0'; ++i)
    {
      if (str[i] == '\n')
        {
          if ((flags & FNT_ALIGN_LEFT) && (flags & FNT_ALIGN_RIGHT))
            {
              x_of = text_width/2 - FNT_GetTextLineWidth(font, str+i+1)/2;
            }
          else if (flags & FNT_ALIGN_LEFT)
            {
              x_of = 0;
            }
          else if (flags & FNT_ALIGN_RIGHT)
            {
              x_of = text_width - FNT_GetTextLineWidth(font, str+i+1);
            }

          y_of += font->glyph_height;
        }
      else
        {
          TTY_GetGlyph(font, str[i], &src_rect);
  
          dst_rect.x = x + x_of;
          dst_rect.y = y + y_of;
  
          SDL_BlitSurface(font->surface, &src_rect, screen, &dst_rect);

          x_of += font->glyph_width;
        }
    }
}

TTY*
TTY_Create(int width, int height, TTY_Font* font)
{
  int i;
  TTY* tty = (TTY*)malloc(sizeof(TTY));

  // SDL_Surface* temp = TTY_CreateRGBSurface(font8x12);
  // SDL_Surface* temp = IMG_Load("c64_16x16.png");
      
  //tty->font = TTY_CreateFont(temp, 16, 16, 
  //                         "\x7f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  //                         "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~");
  tty->font = font;

  //SDL_FreeSurface(temp);
  
  /* Create Framebuffer */
  tty->framebuffer = (char**)malloc(sizeof(char*) * height);
  for(i = 0; i < height; ++i)
    tty->framebuffer[i] = (char*)malloc(sizeof(char) * width);
  
  tty->width  = width;
  tty->height = height;

  tty->cursor_character = 0;
  tty->print_cursor     = 0;

  tty->scroll_x = 0;
  tty->scroll_y = 0;

  tty->cursor_x = 0;
  tty->cursor_y = 0;
  
  TTY_Clear(tty);

  return tty;
}

void
TTY_Free(TTY* tty)
{
  int i;
  
  // TTY_FreeFont(tty->font);

  for(i = 0; i < tty->height; ++i)
    free(tty->framebuffer[i]);
  free(tty->framebuffer);
  
  free(tty);
}

void TTY_SetScrollOffset(TTY* tty, int scroll_x, int scroll_y)
{
  tty->scroll_x = scroll_x;
  tty->scroll_y = scroll_y;
}

void TTY_GetScrollOffset(TTY* tty, int* scroll_x, int* scroll_y)
{
  *scroll_x = tty->scroll_x;
  *scroll_y = tty->scroll_y;
}

void TTY_SetCursor(TTY* tty, int x, int y)
{
  tty->cursor_x = modulo(x + tty->scroll_x, tty->width);
  tty->cursor_y = modulo(y + tty->scroll_y, tty->height);
}

void TTY_GetCursor(TTY* tty, int* x, int* y)
{
  *x = tty->cursor_x - tty->scroll_x;
  *y = tty->cursor_y - tty->scroll_y;
}

void TTY_Clear(TTY* tty)
{ 
  int y;
  for(y = 0; y < tty->height; ++y)
    memset(tty->framebuffer[y], 0, tty->width);
}

void TTY_putchar_nomove(TTY* tty, char chr)
{
  if (chr != '\n' && chr != '\r')
    tty->framebuffer[tty->cursor_y][tty->cursor_x] = chr;
}

void TTY_putchar(TTY* tty, char chr)
{
  if (chr == '\n')
    {
      tty->cursor_x = tty->scroll_x;
      tty->cursor_y = modulo(tty->cursor_y  + 1, tty->height);

      if (modulo(tty->cursor_y - tty->scroll_y, tty->height) == 0)
        {
          tty->scroll_y = modulo(tty->scroll_y + 1, tty->height);
          memset(tty->framebuffer[tty->cursor_y], 0, tty->width);
        }
    }
  else if (chr == '\r')
    {
      tty->cursor_x = tty->scroll_x;
    }
  else
    {      
      tty->framebuffer[tty->cursor_y][tty->cursor_x] = chr;

      tty->cursor_x += 1;
      if (tty->cursor_x == tty->width)
        {
          tty->cursor_x = tty->scroll_x;
          tty->cursor_y = modulo(tty->cursor_y  + 1, tty->height);

          if (modulo(tty->cursor_y - tty->scroll_y, tty->height) == 0)
            {
              tty->scroll_y = modulo(tty->scroll_y + 1, tty->height);
              memset(tty->framebuffer[tty->cursor_y], 0, tty->width);
            }
        }
    }
}

void TTY_write(TTY* tty, const char* buffer, int len)
{
  int i;
  for(i = 0; i < len; ++i)
    TTY_putchar(tty, buffer[i]);
}

void TTY_print(TTY* tty, const char* buffer)
{
  TTY_write(tty, buffer, strlen(buffer));
}

void TTY_SetCursorCharacter(TTY* tty, int chr)
{
  tty->cursor_character = chr;
}

void TTY_EnableVisibleCursor(TTY* tty, int i)
{
  tty->print_cursor = i;
}

void TTY_Blit(TTY* tty, SDL_Surface* screen, int screen_x, int screen_y)
{
  SDL_Rect src_rect;
  SDL_Rect dst_rect;

  int x;
  int y;
  for(y = 0; y < tty->height; ++y)
    {
      for(x = 0; x < tty->width; ++x)
        {
          if (tty->print_cursor)
            {
              if (modulo(x + tty->scroll_x, tty->width) == tty->cursor_x &&
                  modulo(y + tty->scroll_y, tty->height) == tty->cursor_y &&
                  (SDL_GetTicks()/200) % 2 == 0)
                {
                  TTY_GetGlyph(tty->font, tty->cursor_character, &src_rect);

                  dst_rect.x = screen_x + x * tty->font->glyph_width;
                  dst_rect.y = screen_y + y * tty->font->glyph_height;

                  SDL_BlitSurface(tty->font->surface, &src_rect, screen, &dst_rect);
                }
              else
                {
                  char chr = tty->framebuffer[modulo(y + tty->scroll_y, tty->height)][modulo(x + tty->scroll_x, tty->width)];
                  if (chr)
                    {
                      TTY_GetGlyph(tty->font, chr, &src_rect);

                      dst_rect.x = screen_x + x * tty->font->glyph_width;
                      dst_rect.y = screen_y + y * tty->font->glyph_height;

                      SDL_BlitSurface(tty->font->surface, &src_rect, screen, &dst_rect);
                    }
                }
            }
          else
            {          
              char chr = tty->framebuffer[modulo(y + tty->scroll_y, tty->height)][modulo(x + tty->scroll_x, tty->width)];
              if (chr)
                {
                  TTY_GetGlyph(tty->font, chr, &src_rect);

                  dst_rect.x = screen_x + x * tty->font->glyph_width;
                  dst_rect.y = screen_y + y * tty->font->glyph_height;

                  SDL_BlitSurface(tty->font->surface, &src_rect, screen, &dst_rect);
                }
            }
        }
    }
}

void TTY_printf(TTY* tty, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  char* str = make_message(fmt, ap);
  TTY_print(tty, str);
  free(str);
}

/* EOF */
