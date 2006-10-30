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

#include <iostream>
#include "SDL_gfx/SDL_gfxPrimitives.h"

#include "drawing_parameter.hpp"
#include "drawing_context.hpp"
#include "stroke.hpp"
#include "screen_buffer.hpp"
#include "globals.hpp"
#include "client_state.hpp"

ClientState::ClientState(int id_)
  :id(id_), current_stroke(0), draw_param(0)
{    
  draw_param = new DrawingParameter();
}

ClientState::~ClientState()
{
  delete draw_param;
  delete current_stroke; 
}

void
ClientState::set_tool(DrawingParameter::Tool tool)
{
  draw_param->tool = tool;
}

void
ClientState::set_color(const Color& color)
{
  draw_param->color = color;
}

void
ClientState::set_opacity(Uint8 o)
{
  draw_param->opacity = o;
}

void
ClientState::set_brush(const std::string& filename) 
{
  draw_param->set_brush(filename);
}

void
ClientState::set_generic_brush(BrushShape shape,
                               float  radius,
                               int    spikes,        /* 2 - 20     */
                               float  hardness,      /* 0.0 - 1.0  */
                               float  aspect_ratio,  /* y/x (1.0f - 20.0f)       */
                               float  angle)
{
  draw_param->generic_brush.shape        = shape;
  draw_param->generic_brush.radius       = radius;
  draw_param->generic_brush.spikes       = spikes;
  draw_param->generic_brush.hardness     = hardness;
  draw_param->generic_brush.aspect_ratio = aspect_ratio;
  draw_param->generic_brush.angle        = angle;
  
  draw_param->brush_file = "";
  delete draw_param->brush_buffer;
  draw_param->brush_buffer = draw_param->generic_brush.generate();
}

void
ClientState::stroke_begin() {
  //std::cout << "CL" << id << " stroke_begin" << std::endl;
  current_stroke = new Stroke();
}

void
ClientState::stroke_end() {
  //std::cout << "CL" << id << " stroke_end" << std::endl;
  if (current_stroke)
    {
      //std::cout << "# drawing stroke" << std::endl;
      draw_ctx->draw_stroke(*current_stroke, draw_param);

      delete current_stroke;
      current_stroke = 0;
    }
  else
    {
      std::cout << "# got stroke_end before stroke_begin" << std::endl;
    }
}

void
ClientState::dab(unsigned int time, int x, int y) 
{
  //std::cout << "CL" << id << " dab " << time << " " << x << " " << y << std::endl;
  if (current_stroke)
    {
      current_stroke->add_dab(Dab(x, y));
    }
}

void
ClientState::copy_region(const Rect& rect, const Point& target)
{ // FIXME: use float position here instead of int
  SDL_Surface* surface = draw_ctx->get_surface(rect);
  draw_ctx->blit(surface, target);
  SDL_FreeSurface(surface);
}

void
ClientState::fill_rect(const Rect& rect)
{
  // FIXME: Should move this into DrawingContext
  boxRGBA(draw_ctx->get_surface(),
          rect.left, rect.top, rect.right, rect.bottom,
          draw_param->color.r, draw_param->color.g, draw_param->color.b, 
          draw_param->opacity);
  screen_buffer->mark_dirty(rect);
}

/* EOF */
