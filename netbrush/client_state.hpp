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

#ifndef HEADER_CLIENT_STATE_HPP
#define HEADER_CLIENT_STATE_HPP

#include <string>
#include "brushmask.hpp"

class Color;
class Stroke;
class DrawingParameter;

class ClientState
{
private:
  int id;
  Stroke* current_stroke;
  DrawingParameter* draw_param;

public:
  ClientState(int id_);
  ~ClientState();

  void set_opacity(Uint8 o);
  void set_color(const Color& color);
  void set_generic_brush(BrushShape shape,
                         float  radius,
                         int    spikes,        /* 2 - 20     */
                         float  hardness,      /* 0.0 - 1.0  */
                         float  aspect_ratio,  /* y/x (1.0f - 20.0f)       */
                         float  angle);
  void set_brush(const std::string& filename);
  void stroke_begin();
  void stroke_end();

  void dab(unsigned int time, int x, int y);
};

#endif

/* EOF */
