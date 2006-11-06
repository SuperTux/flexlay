/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
** 
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#ifndef HEADER_SURFACE_GRAPHIC_CONTEXT_HPP
#define HEADER_SURFACE_GRAPHIC_CONTEXT_HPP

#include "SDL.h"
#include "math/point.hpp"
#include "math/rect.hpp"
#include "color.hpp"
#include "graphic_context.hpp"

/** */
class SurfaceGraphicContext : public GraphicContext
{
private:
  /** the SDL_Surface onto which all drawing command will land */
  SDL_Surface* target;

  /** The region into which all drawing commands will land, drawing to
      (0, 0) will land on (region.left, region.top)  */
  Rect region;

  bool anti_aliasing;

public:
  SurfaceGraphicContext(SDL_Surface* surface, const Rect& region);
  ~SurfaceGraphicContext();
  
  void set_anti_aliasing(bool t);

  void fill_rect(const Rect& rect, const Color& color);
  void draw_rect(const Rect& rect, const Color& color);
  
  void fill_circle(const Point& pos, int radius, const Color& color);
  void draw_circle(const Point& pos, int radius, const Color& color);

  void draw_line(const Point& p1, const Point& p2, const Color& color);
  
  void blit(SDL_Surface* source, const Point& pos);
  void blit(SDL_Surface* source, const Rect& src_rect, const Point& pos);

  SDL_Surface* get_surface() { return target; }
private:
  SurfaceGraphicContext (const SurfaceGraphicContext&);
  SurfaceGraphicContext& operator= (const SurfaceGraphicContext&);
};

#endif

/* EOF */
