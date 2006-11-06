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

#ifndef HEADER_DRAWING_PARAMETER_HPP
#define HEADER_DRAWING_PARAMETER_HPP

#include "SDL.h"
#include "SDL_image.h"
#include "debug.hpp"
#include "color.hpp"
#include "generic_brush.hpp"

class GrayscaleBuffer;

class DrawingParameter
{
public:
  enum Tool { TOOL_AIRBRUSH, TOOL_PAINTBRUSH, TOOL_COLOR_PICKER };
  Tool             tool;
  std::string      brush_file;
  GenericBrush     generic_brush;
  SDL_Surface*     brush_surface;
  GrayscaleBuffer* brush_buffer;
  Color            color;
  Uint8            opacity;
  float            spacing;
  
public:
  DrawingParameter();

  float get_spacing();
  void set_brush(const std::string& filename);
  std::string get_brush() const;
  SDL_Surface* get_brush_surface() const;
  GrayscaleBuffer* get_brush_buffer() const;
  int thickness() const;
};

#endif 

/* EOF */
