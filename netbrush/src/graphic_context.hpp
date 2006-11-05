/*  $Id$
**   __      __ __             ___        __   __ __   __
**  /  \    /  \__| ____    __| _/_______/  |_|__|  | |  |   ____
**  \   \/\/   /  |/    \  / __ |/  ___/\   __\  |  | |  | _/ __ \
**   \        /|  |   |  \/ /_/ |\___ \  |  | |  |  |_|  |_\  ___/
**    \__/\  / |__|___|  /\____ /____  > |__| |__|____/____/\___  >
**         \/          \/      \/    \/                         \/
**  Copyright (C) 2005 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef HEADER_GRAPHIC_CONTEXT_HPP
#define HEADER_GRAPHIC_CONTEXT_HPP

#include "math/rect.hpp"
#include "math/point.hpp"
#include "color.hpp"

/** */
class GraphicContext
{
private:
public:
  GraphicContext() {}
  virtual ~GraphicContext() {}

  virtual void fill_rect(const Rect& rect, const Color& color) =0;
  virtual void draw_rect(const Rect& rect, const Color& color) =0;

  virtual void fill_circle(const Point& pos, int radius, const Color& color) =0;
  virtual void draw_circle(const Point& pos, int radius, const Color& color) =0;
  
  virtual void blit(SDL_Surface* source, const Point& pos) =0;
  virtual void blit(SDL_Surface* source, const Rect& src_rect, const Point& pos) =0;

  virtual void draw_line(const Point& p1, const Point& p2, const Color& color) =0;

  virtual SDL_Surface* get_surface() =0;
private:
  GraphicContext (const GraphicContext&);
  GraphicContext& operator= (const GraphicContext&);
};

#endif

/* EOF */
