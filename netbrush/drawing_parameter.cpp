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

#include "grayscale_buffer.hpp"
#include "drawing_parameter.hpp"

DrawingParameter::DrawingParameter()
  : brush_surface(0), brush_buffer(0), 
    color(0, 0, 0), 
    opacity(128),
    spacing(1.0f)
{
  set_brush("brush_3x3.png");
}

void
DrawingParameter::set_brush(const std::string& filename)
{
  SDL_Surface* new_brush = IMG_Load(filename.c_str());
  if (new_brush)
    {
      if (brush_surface)
        SDL_FreeSurface(brush_surface);

      brush_surface = new_brush;
      brush_file    = filename;  

      delete brush_buffer;
      brush_buffer = new GrayscaleBuffer(brush_surface);
    }
  else
    {
      client_error("Couldn't load file " + filename);
    }
}

std::string
DrawingParameter::get_brush() const 
{
  return brush_file;
}

SDL_Surface*
DrawingParameter::get_brush_surface() const 
{ 
  return brush_surface;
}

GrayscaleBuffer*
DrawingParameter::get_brush_buffer() const
{
  return brush_buffer;
}

int
DrawingParameter::thickness() const
{
  return brush_buffer->get_width();
}

/* EOF */
