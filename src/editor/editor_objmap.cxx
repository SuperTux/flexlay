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
#include "editor_objmap.hxx"

extern CL_ResourceManager* resources;

EditorObjMap::EditorObjMap(CL_Component* parent)
  : CL_Component(CL_Rect(CL_Point(0, 0),
                         CL_Size(CL_Display::get_width(), CL_Display::get_height())),
                 parent)
{
  slots.connect(sig_paint(),      this, &EditorObjMap::draw);
  slots.connect(sig_mouse_up(),   this, &EditorObjMap::mouse_up);
  slots.connect(sig_mouse_down(), this, &EditorObjMap::mouse_down);
  slots.connect(sig_mouse_move(), this, &EditorObjMap::mouse_move);

  Obj obj;
  obj.sprite = CL_Sprite("igel", resources);
  obj.pos    = CL_Point(100, 100);
  objects.push_back(obj);
}

EditorObjMap::~EditorObjMap()
{
}

void
EditorObjMap::update(float delta)
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i).sprite.update(delta);
    }
}

void
EditorObjMap::draw()
{
  for(Objs::iterator i = objects.begin(); i != objects.end(); ++i)
    {
      (*i).sprite.draw((*i).pos.x, (*i).pos.y);
    }
}

void
EditorObjMap::mouse_up  (const CL_InputEvent& event)
{
}

void
EditorObjMap::mouse_down(const CL_InputEvent& event)
{
}

void
EditorObjMap::mouse_move(const CL_InputEvent& event)
{
}

/* EOF */
