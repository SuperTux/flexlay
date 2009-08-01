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

#ifndef HEADER_CONTROLLER_HPP
#define HEADER_CONTROLLER_HPP

#include "saturation_value_picker.hpp"
#include "alpha_picker.hpp"
#include "color_display.hpp"
#include "hue_picker.hpp"
#include "generic_brush.hpp"

class SliderWidget;
class TextView;
class BrushWidget;

/** */
class Controller
{
private:
  SaturationValuePicker* saturation_value_picker;
  HuePicker*             hue_picker;
  AlphaPicker*           alpha_picker;
  ColorDisplay*          color_display;

  BrushWidget* brush_widget;
  TextView* text_view;

  SliderWidget* radius_slider;
  SliderWidget* spike_slider;
  SliderWidget* hardness_slider;
  SliderWidget* aspect_ratio_slider;
  SliderWidget* angle_slider;
  
  Uint8 hue;
  Uint8 saturation;
  Uint8 value;

public:
  Controller();
  ~Controller();

  void set_color(const Color& color);
  void set_color_hue(Uint8 hue);
  void set_color_value_saturation(Uint8 value, Uint8 saturation);

  void set_generic_brush_shape(BrushShape shape);
  void set_generic_brush_radius(float radius);
  void set_generic_brush_spikes(int spikes);
  void set_generic_brush_hardness(float hardness);
  void set_generic_brush_aspect_ratio(float aspect_ratio);
  void set_generic_brush_angle(float angle);
  void set_generic_brush(const GenericBrush& brush);

  void puts(const std::string& str);

  void update_mouse_cursor();
  void save_png(const std::string& filename);
private:
  Controller (const Controller&);
  Controller& operator= (const Controller&);
};

#endif

/* EOF */
