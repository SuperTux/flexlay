//  $Id: view_component.hxx,v 1.1 2003/10/11 12:15:59 grumbel Exp $
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

#ifndef HEADER_VIEW_COMPONENT_HXX
#define HEADER_VIEW_COMPONENT_HXX

#include <ClanLib/Signals/slot.h>
#include <ClanLib/GUI/component.h>

class View;
          
/** clanGUI wrapper for View class, used for the debug GUI in the game */
class ViewComponent : public CL_Component
{
private:
  View* view;
  std::vector<CL_Slot> slots;
public:
  ViewComponent(CL_Component* parent, View* view);

  void on_input_down(const CL_InputEvent& event);
  void on_input_up(const CL_InputEvent& event);
private:
  ViewComponent (const ViewComponent&);
  ViewComponent& operator= (const ViewComponent&);
};

#endif

/* EOF */
