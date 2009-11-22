//  Flexlay - A Generic 2D Game Editor
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_EDITOR_MAP_COMPONENT_HPP
#define HEADER_FLEXLAY_EDITOR_MAP_COMPONENT_HPP

#include <ClanLib/GUI/component.h>
#include "workspace.hpp"

class Scrollbar;
class EditorMapComponentImpl;

/** Object which represents a level, quirled together with the GUI
    stuff */
class EditorMapComponent : public CL_Component
{
private:
  static EditorMapComponent* current_; 
protected:
  virtual ~EditorMapComponent();
public:
  static EditorMapComponent* current() { return current_; } 

  EditorMapComponent(const CL_Rect& rect, CL_Component* parent);
 
  Workspace get_workspace() const;
  void      set_workspace(Workspace m);

  void  set_zoom(float z);
  void  zoom_to(CL_Rectf rect);
  void  zoom_out(CL_Point pos);
  void  zoom_in (CL_Point pos);

  void move_to(int x, int y);
  void move_to_x(float x);
  void move_to_y(float y);

  CL_Signal_v2<int, int>& sig_on_key(const std::string& str);

  CL_Pointf screen2world(const CL_Point& pos);

  CL_Rectf get_clip_rect();

  GraphicContextState& get_gc_state();

private:
  boost::shared_ptr<EditorMapComponentImpl> impl;
};

#endif

/* EOF */
