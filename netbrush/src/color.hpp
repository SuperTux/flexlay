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

#ifndef HEADER_COLOR_HPP
#define HEADER_COLOR_HPP

#include <iosfwd>
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
  Uint8 a;

  Color();
  Color(Uint8 r_, Uint8 g_, Uint8 b_, Uint8 a_ = 255);
  static Color from_hsv(Uint8 hue, Uint8 saturation, Uint8 value);
  static Color from_hue(Uint8 hue);
  void apply_value_saturation(Uint8 value, Uint8 saturation);
};

std::ostream& operator<<(std::ostream& s, const Color&    rgba);
std::ostream& operator<<(std::ostream& s, const HSVColor& hsv);

#endif

/* EOF */
