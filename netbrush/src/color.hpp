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

#ifndef HEADER_COLOR_HPP
#define HEADER_COLOR_HPP

#include "SDL.h"

struct Color;

struct HSVColor
{
public:
  Uint8 hue;
  Uint8 saturation;
  Uint8 value;
  
  HSVColor();
  static HSVColor from_rgb(const Color& color);
};

struct Color
{
  Uint8 r;
  Uint8 g;
  Uint8 b;

  Color();
  Color(Uint8 r_, Uint8 g_, Uint8 b_);
  static Color from_hsv(Uint8 hue, Uint8 saturation, Uint8 value);
  static Color from_hue(Uint8 hue);
  void apply_value_saturation(Uint8 value, Uint8 saturation);
};

#endif

/* EOF */
