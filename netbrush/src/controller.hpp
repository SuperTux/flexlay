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

#ifndef HEADER_CONTROLLER_HPP
#define HEADER_CONTROLLER_HPP

#include "saturation_value_picker.hpp"
#include "alpha_picker.hpp"
#include "hue_picker.hpp"
#include "generic_brush.hpp"

class SliderWidget;

/** */
class Controller
{
private:
  SaturationValuePicker* saturation_value_picker;
  HuePicker*             hue_picker;
  AlphaPicker*           alpha_picker;

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

  void set_color_hue(Uint8 hue);
  void set_color_value_saturation(Uint8 value, Uint8 saturation);

  void set_generic_brush_shape(BrushShape shape);
  void set_generic_brush_radius(float radius);
  void set_generic_brush_spikes(int spikes);
  void set_generic_brush_hardness(float hardness);
  void set_generic_brush_aspect_ratio(float aspect_ratio);
  void set_generic_brush_angle(float angle);
  void set_generic_brush(const GenericBrush& brush);

  void update_mouse_cursor();
private:
  Controller (const Controller&);
  Controller& operator= (const Controller&);
};

#endif

/* EOF */
