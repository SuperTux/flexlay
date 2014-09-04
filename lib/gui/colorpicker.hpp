// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_COLORPICKER_HPP
#define HEADER_FLEXLAY_COLORPICKER_HPP

#include <boost/signals2.hpp>

#include "color.hpp"

class ColorPickerAlpha;
class ColorPickerBrightness;
class ColorPickerHue;
class Rect;

class ColorPicker : public CL_Component
{
protected:
  ~ColorPicker() {}
private:
  boost::signals2::signal<void (Color)> on_color_change;
  std::vector<CL_Slot> slots;
  Color color;

  ColorPickerHue*     hue;
  ColorPickerBrightness* brightness;
  ColorPickerAlpha*      alpha;

  void update_brightness_color(Color color);
  void update_alpha_color(float alpha);
public:
  ColorPicker(const Rect& rect, CL_Component* parent);

  void draw();

  boost::signals2::signal<void (Color)>& sig_color_change();

  Color get_color();
  void set_color(const Color& color);
};

#endif

/* EOF */
