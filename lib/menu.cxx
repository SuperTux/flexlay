//  $Id$
//
//  Flexlay - A Generic 2D Game Editor
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

#include <iostream>
#include <string>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include "fonts.hxx"
#include "box.hxx"
#include "menu.hxx"

class MenuItem;

class MenuImpl
{
public:
  Menu* parent;
  std::vector<CL_Slot> slots;

  typedef std::vector<MenuItem*> Items;
  Items items;
  
  int current_item;

  int width;
  int height;

  void draw();
  void recalc_size();
  int  get_width();
  int  get_height();

  void on_mouse_move(const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
};

class MenuItem
{
protected:
  MenuImpl* parent;
  CL_Signal_v0 on_clicked;

public:
  MenuItem(MenuImpl* parent_) 
    : parent(parent_) {}

  virtual ~MenuItem() {}

  virtual void draw(int x, int y, bool active) =0;
  virtual int get_width() =0;
  virtual int get_height() =0;

  CL_Signal_v0& sig_clicked() { return on_clicked; }
};

class SeperatorMenuItem : public MenuItem
{
public:
  SeperatorMenuItem(MenuImpl* parent_) 
    : MenuItem(parent_)
  {}
  virtual ~SeperatorMenuItem() {}

  void draw(int x, int y, bool active) 
  {
    CL_Display::fill_rect(CL_Rect(CL_Point(x, y), CL_Size(parent->get_width()-7, 2)), 
                          CL_Color(150, 150, 150));
    CL_Display::fill_rect(CL_Rect(CL_Point(x, y+1), CL_Size(parent->get_width()-7, 1)),
                          CL_Color(255, 255, 255));
  }

  int get_width()  { return 10; }
  int get_height() { return 2; }
};

class TextMenuItem : public MenuItem
{
private:
  CL_Sprite sprite;
  std::string text;

public:
  TextMenuItem(const CL_Sprite& sprite_, const std::string& text_, MenuImpl* parent_)
    : MenuItem(parent_),
      sprite(sprite_),
      text(text_) 
  {
    sprite.set_alignment(origin_center);
  }

  virtual ~TextMenuItem() {}

  void draw(int x, int y, bool active) {
    if (active)
      CL_Display::fill_rect(CL_Rect(CL_Point(x, y-2), CL_Size(parent->get_width() - 7, 18)), 
                            CL_Color(255, 255, 255));
    if (sprite)
      {
        sprite.draw(x+10, y+7);
      }
    Fonts::verdana11.draw(x+24, y, text);
  }
  int get_width()  { return Fonts::verdana11.bounding_rect(0, 0, text).get_width() + 16; }
  int get_height() { return Fonts::verdana11.get_height(); }
};

Menu::Menu(const CL_Point& pos, CL_Component* parent)
  : CL_Component(CL_Rect(pos, CL_Size(1,1)), parent),
    impl(new MenuImpl())
{
  impl->parent = this;

  impl->width  = 1;
  impl->height = 1;

  impl->slots.push_back(sig_paint().connect(impl.get(), &MenuImpl::draw));
  impl->slots.push_back(sig_mouse_move().connect(impl.get(), &MenuImpl::on_mouse_move));
  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &MenuImpl::on_mouse_down));

  show(false);
}

Menu::~Menu()
{
  clear();
}

void
Menu::clear()
{
  for(MenuImpl::Items::iterator i = impl->items.begin(); i != impl->items.end(); ++i)
    delete *i;
  impl->items.clear();
}

MenuItemHandle
Menu::add_seperator()
{
  impl->items.push_back(new SeperatorMenuItem(impl.get()));
  impl->recalc_size();
  return impl->items.size()-1;
}

MenuItemHandle
Menu::add_item(const CL_Sprite& sprite, const std::string& name)
{
  impl->items.push_back(new TextMenuItem(sprite, name, impl.get()));
  impl->recalc_size();
  return impl->items.size()-1;
}

MenuItemHandle
Menu::add_submenu(const std::string& name, const Menu& submenu)
{
  impl->recalc_size();
  return -1;
}

void
MenuImpl::recalc_size()
{
  height = 0;
  width = 0;

  for(Items::iterator i = items.begin(); i != items.end(); ++i)
    width = std::max(width, (*i)->get_width());

  for(Items::iterator i = items.begin(); i != items.end(); ++i)
    height += (*i)->get_height() + 6;
  
  width  += 12 + 24;
  height += 8;

  parent->set_size(width, height);
}

void
MenuImpl::draw()
{
  CL_Display::push_translate(parent->get_screen_x(), parent->get_screen_y());

  Box::draw_window(CL_Rect(CL_Point(0, 0), 
                           CL_Size(parent->get_width(),
                                   parent->get_height())));
  int x_pos = 3;
  int y_pos = 6;

  for(int i = 0; i < int(items.size()); ++i)
    {
      if (i == current_item)
        items[i]->draw(x_pos, y_pos, true);
      else
        items[i]->draw(x_pos, y_pos, false);
      y_pos += items[i]->get_height() + 6;
    }
  CL_Display::pop_modelview();
}

int
MenuImpl::get_width()
{
  return width;
}

int
MenuImpl::get_height()
{
  return height;
}

void
MenuImpl::on_mouse_down(const CL_InputEvent& event)
{
  if (current_item != -1)
    {
      std::cout << "Current_Item: " << current_item << std::endl;
      items[current_item]->sig_clicked()();
    }
  parent->release_mouse();
  parent->show(false);
}

void
MenuImpl::on_mouse_move(const CL_InputEvent& event)
{
  if (parent->has_mouse_over())
    {
      int y_pos = 6;

      for(int i = 0; i < int(items.size()); ++i)
        {
          y_pos += items[i]->get_height() + 6;      
          if (y_pos > event.mouse_pos.y)
            {
              current_item = i;
              return;
            }
        }
      current_item = -1;
    }
  else
    {
      current_item = -1;
    }
}

CL_Signal_v0&
Menu::sig_clicked(MenuItemHandle item)
{
  return impl->items[item]->sig_clicked();
}

void
Menu::run()
{
  // FIXME: Make menu act sane on == 0 items
  if (impl->items.size() > 0)
    {
      show(true);
      capture_mouse();
      raise();
    }
}

/* EOF */
