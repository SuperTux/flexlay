//  $Id: scripting.cxx,v 1.5 2003/09/11 18:58:19 grumbel Exp $
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

#include <ClanLib/gui.h>
#include <iostream>
#include "../scm_functor.hxx"
#include "../globals.hxx"
#include "editor.hxx"
#include "tile_selector.hxx"
#include "editor_tilemap.hxx"
#include "scripting.hxx"

void
editor_set_brush_tile(int i)
{
  Editor::current()->get_editor_tilemap()->brush_tile = i;
}

int
editor_get_brush_tile()
{
  return Editor::current()->get_editor_tilemap()->brush_tile;
}

CL_Component*
editor_add_button(int x, int y, int w, int h, const char* text, SCM func)
{
  CL_Component* manager = Editor::current()->get_component();
  CL_SlotContainer* slot_container = Editor::current()->get_slot_container();
 
  CL_Button* button = new CL_Button(CL_Rect(CL_Point(x, y), CL_Size(w, h)),
                                    text, manager);
  SCMFunctor* functor = new SCMFunctor(func);
  slot_container->connect(button->sig_clicked(), functor, &SCMFunctor::call);

  return button;
}

CL_Component* 
editor_add_label(int x, int y, const char* text)
{
  CL_Component* manager = Editor::current()->get_component();
  return new CL_Label(CL_Point(x, y), text, manager);
}

CL_Component*
editor_add_window(int x, int y, int w, int h, const char* title)
{
  CL_Component* manager = Editor::current()->get_component();
  return new CL_Window(CL_Rect(CL_Point(x, y), CL_Size(w, h)), title, manager);
}

CL_Component*
editor_add_inputbox(int x, int y, int w, int h, const char* text)
{
  CL_Component* manager = Editor::current()->get_component();
  return new CL_InputBox(CL_Rect(CL_Point(x,y), CL_Size(w, h)), text, manager);
}

void
editor_quit()
{
  Editor::current()->get_component()->quit();
}

int
screen_get_width()
{
  return CL_Display::get_width();
}

int
screen_get_height()
{
  return CL_Display::get_height();
}

void tilemap_set_active_layer(int i)
{
  Editor::current()->get_editor_tilemap()->set_active_layer(i);
}

void editor_set_tool(SCM func)
{
  
}

void tile_selector_create(int x, int y, int w, int h)
{
  CL_Window* window = new CL_Window(CL_Rect(CL_Point(x, y),
                                            CL_Size(w*(TILE_SIZE/2), h*(TILE_SIZE/2) + 32)),
                                    "TileSelector", Editor::current()->get_component());
  new TileSelector(w, h, window->get_client_area());
}

CL_Component*
push_component(CL_Component* c)
{
  Editor::current()->push_component(c);
  return c;
}

void
pop_component()
{
  Editor::current()->pop_component();
}

void window_close(CL_Component* comp)
{
  comp->close();
}

const char* 
inputbox_get_text(CL_Component* comp)
{
  CL_InputBox* box = dynamic_cast<CL_InputBox*>(comp);
  if (box)
    {
      return box->get_text().c_str();
    }
  else
    {
      return 0;
    }
}

void
component_hide(CL_Component* comp)
{
  comp->show(false);
}

void
component_show(CL_Component* comp)
{
  comp->show(true);
}

CL_Component* window_get_client_area(CL_Component* comp)
{
  return comp->get_client_area();
}

SCM map_get_data(int i)
{
  Field<EditorTile*>* field = Editor::current()->get_editor_tilemap()->get_map(i);
  if (field)
    {
      std::cout << ": " << field->get_width() << "x" << field->get_height() 
                << " " << field->size() << std::endl;

      SCM vec = SCM_EOL;
      for (Field<EditorTile*>::iterator i = field->begin(); i != field->end(); ++i)
        {
          vec = gh_cons(gh_int2scm((*i)->get_id()), vec);
        }
      return gh_reverse(vec);
    }
  else

    {
      return SCM_BOOL_F;
    }
}

int map_get_width()
{
  return Editor::current()->get_editor_tilemap()->get_width();
}

int map_get_height()
{
  return Editor::current()->get_editor_tilemap()->get_height();
}

void map_set_size(int w, int h)
{
  //return Editor::current()->get_editor_tilemap()->get_height();
}

void
editor_load(const char* filename)
{
  Editor::current()->get_editor_tilemap()->load(filename);
}

/* EOF */
