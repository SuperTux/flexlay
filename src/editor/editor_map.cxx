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

#include <ClanLib/Display/display.h>
#include "editor_map.hxx"

EditorMap::EditorMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0),
                         CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent)
{
}

EditorMap::~EditorMap()
{
}

void
EditorMap::update(float delta)
{
}

void
EditorMap::draw()
{
}

void
EditorMap::mouse_up  (const CL_InputEvent& event)
{
}

void
EditorMap::mouse_down(const CL_InputEvent& event)
{
}

void
EditorMap::mouse_move(const CL_InputEvent& event)
{
}

/* EOF */
