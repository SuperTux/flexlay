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

#ifndef HEADER_SLIDER_HXX
#define HEADER_SLIDER_HXX

#include <vector>
#include <ClanLib/GUI/component.h>
#include <ClanLib/Core/Math/rect.h>

/** Little Slider GUI component */
class Slider : public CL_Component
{
protected:
  virtual ~Slider();
private:
  std::vector<CL_Slot> slots;
  float start;
  float end;
  float value;
  bool pressed;

  CL_Signal_v1<float> on_change;
  
  void update_mouse(const CL_InputEvent& event);
public:
  Slider(const CL_Rect& rect, CL_Component* parent);

  CL_Signal_v1<float>& sig_on_change();

  void set_range(float start, float end);
  void set_value(float value);

  void draw();
  
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);
};

#endif

/* EOF */
