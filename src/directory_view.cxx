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

#include <assert.h>
#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Core/IOData/directory_scanner.h>
#include <ClanLib/Display/font.h>
#include <ClanLib/Display/display.h>
#include "fonts.hxx"
#include "directory_view.hxx"

class DirectoryViewEntry
{
public:
  std::string name;
  bool directory;
  bool hidden;
};

struct DirectoryViewSorter
{
  bool operator()(const DirectoryViewEntry& lhs, const DirectoryViewEntry& rhs)
  {
    if (lhs.directory > rhs.directory)
      return true;
    else if (lhs.directory < rhs.directory)
      return false;
    else
      return lhs.name < rhs.name;
  }
};

class DirectoryViewImpl
{
public:
  DirectoryView* parent;

  std::vector<CL_Slot> slots;

  std::string path;
  CL_Signal_v1<std::string> sig_on_click;
  typedef std::vector<DirectoryViewEntry> Items;
  Items items;
  int current_item;
  int column_width;
  int num_columns;

  void update_items();
  void draw();
  int get_item(const CL_Point& pos);
  void on_mouse_move(const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
};

DirectoryView::DirectoryView(const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new DirectoryViewImpl())
{ 
  impl->parent = this;

  //  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &DirectoryViewImpl::draw));
  impl->slots.push_back(sig_paint().connect(impl.get(), &DirectoryViewImpl::draw));
  impl->slots.push_back(sig_mouse_move().connect(impl.get(), &DirectoryViewImpl::on_mouse_move));
  impl->slots.push_back(sig_mouse_down().connect(impl.get(), &DirectoryViewImpl::on_mouse_down));

  impl->current_item = -1;
}

DirectoryView::~DirectoryView()
{
}

void
DirectoryView::set_directory(const std::string& path_)
{
  impl->path = path_;
  impl->update_items();
}

CL_Signal_v1<std::string>&
DirectoryView::sig_on_click()
{
  return impl->sig_on_click;
}

void 
DirectoryViewImpl::draw()
{
  CL_Font font = Fonts::verdana11; 

  int horizontal_spacing = 10;
  int vertical_spacing   = 5;
  int x_pos = 0;
  int y_pos = 0;

  CL_Display::clear(CL_Color(255, 255, 0));

  int j = 0;
  for(Items::iterator i = items.begin(); i != items.begin()+50 && i != items.end(); ++i)
    {
      if (current_item && current_item < int(items.size()) && j == current_item)
        {
          CL_Rect rect = font.bounding_rect(x_pos * (column_width + horizontal_spacing) + 1, 
                                            y_pos * (font.get_height() + vertical_spacing) + 1,
                                            i->name);
          CL_Display::fill_rect(CL_Rect(rect.left-5, rect.top-3,
                                        rect.left+5+column_width, rect.bottom+3),
                                CL_Color(250, 200, 0));
        }

      // draw item
      if (!i->directory)
        {
          font.draw(x_pos * (column_width + horizontal_spacing), 
                    y_pos * (font.get_height() + vertical_spacing),
                    i->name);
        }
      else
        {
          font.draw(x_pos * (column_width + horizontal_spacing), 
                    y_pos * (font.get_height() + vertical_spacing),
                    "[" + i->name + "]");
        }

      x_pos += 1;
      if (x_pos >= num_columns)
        {
          x_pos = 0;
          y_pos += 1;
        }
      ++j;
    }
}

int
DirectoryViewImpl::get_item(const CL_Point& pos)
{
  CL_Font font = Fonts::verdana11; 

  int horizontal_spacing = 10;
  int vertical_spacing   = 5;

  return (pos.x / (column_width + horizontal_spacing))
    + num_columns * (pos.y / (font.get_height() + vertical_spacing));    
}

void 
DirectoryViewImpl::on_mouse_down(const CL_InputEvent& event)
{
  current_item = get_item(event.mouse_pos);
  if (current_item >= 0 && current_item < int(items.size()))
    {
      if (items[current_item].directory)
      parent->set_directory(path + "/" + items[current_item].name);
    }
}

void 
DirectoryViewImpl::on_mouse_move(const CL_InputEvent& event)
{
  current_item = get_item(event.mouse_pos);
}

void
DirectoryViewImpl::update_items()
{
  items.clear();
  CL_DirectoryScanner scanner;

  scanner.scan(path);
  while(scanner.next())
    {
      DirectoryViewEntry entry;
      entry.name = scanner.get_name();
      entry.hidden = (scanner.get_name()[0] == '.');
      entry.directory = scanner.is_directory();
      items.push_back(entry);
    }

  std::sort(items.begin(), items.end(), DirectoryViewSorter());

  CL_Font font = Fonts::verdana11; 

  column_width = 60; // min_colum_width
  for(Items::iterator i = items.begin(); i != items.end(); ++i)
    {
      CL_Rect rect = font.bounding_rect(0, 0, i->name + "[]");
      column_width = std::max(column_width, rect.get_width());
    }

  num_columns = parent->get_width()/column_width;
}

/* EOF */
