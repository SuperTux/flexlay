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

#include <algorithm>
#include <iostream>
#include "color.hpp"

HSVColor::HSVColor()
  : hue(0), saturation(0), value(0)
{
}

HSVColor
HSVColor::from_rgb(const Color& color)
{ // untested
  HSVColor hsvcolor;
    
  Uint8 min = std::min(std::min(color.r, color.g), color.b);
  Uint8 max = std::max(std::max(color.r, color.g), color.b);

  if (min == max)
    {
      hsvcolor.hue = 0;
    }
  else
    {
      if (max == color.r)
        hsvcolor.hue = (0 + (color.g - color.b) / (max - min)) * 255 / 6;
      else if (max == color.g)
        hsvcolor.hue = (2 + (color.b - color.r) / (max - min)) * 255 / 6;
      else if (max == color.b)
        hsvcolor.hue = (4 + (color.r - color.g) / (max - min)) * 255 / 6;
    }

  if (max == 0)
    hsvcolor.saturation = 0;
  else
    hsvcolor.saturation = 255 * (max - min) / max;

  hsvcolor.value = 255 * max;

  return hsvcolor;
}

Color::Color()
  : r(0), g(0), b(0)
{}

Color::Color(Uint8 r_, Uint8 g_, Uint8 b_, Uint8 a_)
  : r(r_), g(g_), b(b_), a(a_)
{}

Color
Color::from_hsv(Uint8 hue, Uint8 saturation, Uint8 value)
{
  Color color = Color::from_hue(hue);
  color.apply_value_saturation(value, saturation);
  return color;
}
  
  

Color
Color::from_hue(Uint8 hue)
{
  static Color colors[] = { Color(255,   0,   0),
                            Color(255,   0, 255),
                            Color(  0,   0, 255),
                            Color(  0, 255, 255),
                            Color(  0, 255,   0),
                            Color(255, 255,   0),
                            Color(255,   0,   0) };
  
  int seg_len = (255/6);
  int seg  = (hue / seg_len);
  int prog = (hue % seg_len);

  return Color((((seg_len - prog) * colors[seg].r) + (prog * colors[seg+1].r))/seg_len,
               (((seg_len - prog) * colors[seg].g) + (prog * colors[seg+1].g))/seg_len,
               (((seg_len - prog) * colors[seg].b) + (prog * colors[seg+1].b))/seg_len);
}

void
Color::apply_value_saturation(Uint8 value, Uint8 saturation)
{
  r = (0*value + (255-value) * ((255 * saturation + r * (255 - saturation))/255))/255;
  g = (0*value + (255-value) * ((255 * saturation + g * (255 - saturation))/255))/255;
  b = (0*value + (255-value) * ((255 * saturation + b * (255 - saturation))/255))/255;
}

std::ostream& operator<<(std::ostream& s, const Color& color)
{
  return (s << "RGBA("
          << int(color.r) << ", " << int(color.g) << ", " << int(color.b) << ", " << int(color.a) 
          << ")");
}

std::ostream& operator<<(std::ostream& s, const HSVColor& hsv)
{
  return (s << "HSV("
          << int(hsv.hue) << ", " << int(hsv.saturation) << ", " << int(hsv.value)
          << ")");
}

/* EOF */
