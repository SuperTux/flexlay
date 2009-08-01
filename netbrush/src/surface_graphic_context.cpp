/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "SDL_gfx/SDL_gfxPrimitives.h"
#include "surface_graphic_context.hpp"

SurfaceGraphicContext::SurfaceGraphicContext(SDL_Surface* target_, 
                                             const Rect&  region_)
  : target(target_),
    region(region_),
    anti_aliasing(false)
{
}

SurfaceGraphicContext::~SurfaceGraphicContext()
{
  SDL_FreeSurface(target);
}

void
SurfaceGraphicContext::fill_rect(const Rect& rect, const Color& color)
{
  boxRGBA(target,
          rect.left  + region.left, rect.top    + region.top, 
          rect.right + region.left, rect.bottom + region.top,
          color.r, color.g, color.b, color.a);
}

void
SurfaceGraphicContext::draw_rect(const Rect& rect, const Color& color)
{
  boxRGBA(target,
          rect.left  + region.left, rect.top    + region.top, 
          rect.right + region.left, rect.bottom + region.top,
          color.r, color.g, color.b, color.a);
}
  
void
SurfaceGraphicContext::fill_circle(const Point& pos, int radius, const Color& color)
{
  filledCircleRGBA(target,
                   pos.x + region.left,
                   pos.y + region.top, 
                   radius,
                   color.r, color.g, color.b, color.a);
}

void
SurfaceGraphicContext::draw_circle(const Point& pos, int radius, const Color& color)
{
  circleRGBA(target,
             pos.x + region.left, pos.y + region.top, 
             radius,
             color.r, color.g, color.b, color.a);
}

void
SurfaceGraphicContext::draw_line(const Point& p1, const Point& p2, const Color& color)
{
  aalineRGBA(target,
             p1.x + region.left, p1.y + region.top,
             p2.x + region.left, p2.y + region.top,
             color.r, color.g, color.b, color.a);
}

void
SurfaceGraphicContext::blit(SDL_Surface* source, const Point& pos)
{
  SDL_Rect target_rect;
  target_rect.x = pos.x + region.left;
  target_rect.y = pos.y + region.top;

  SDL_BlitSurface(source, 0, target, &target_rect);
}

void
SurfaceGraphicContext::blit(SDL_Surface* source, const Rect& src_rect, const Point& pos)
{
  // FIXME: add clipping
  SDL_Rect source_rect;
  source_rect.x = src_rect.left;
  source_rect.y = src_rect.right;
  source_rect.w = src_rect.get_width();
  source_rect.h = src_rect.get_height();

  SDL_Rect target_rect;
  target_rect.x = pos.x + region.left;
  target_rect.y = pos.y + region.top;

  SDL_BlitSurface(source, &source_rect, target, &target_rect);  
}

/* EOF */
