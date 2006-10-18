//  $Id$
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#ifndef HEADER_COLORPICKER_HXX
#define HEADER_COLORPICKER_HXX

#include <ClanLib/Display/display.h>
#include <ClanLib/Display/color.h>
#include <ClanLib/Display/gradient.h>

class ColorPickerHue;
class ColorPickerAlpha;
class ColorPickerBrightness;

/** */
class ColorPicker : public CL_Component
{
protected:
  ~ColorPicker() {}
private:
  CL_Signal_v1<CL_Color> on_color_change;
  std::vector<CL_Slot> slots;
  CL_Color color;

  ColorPickerHue*     hue;
  ColorPickerBrightness* brightness;
  ColorPickerAlpha*      alpha;

  void update_brightness_color(CL_Color color);
  void update_alpha_color(float alpha);
public:
  ColorPicker(const CL_Rect& rect, CL_Component* parent);
  
  void draw();

  CL_Signal_v1<CL_Color>& sig_color_change();

  CL_Color get_color();
  void set_color(const CL_Color& color);
};

#endif

/* EOF */
