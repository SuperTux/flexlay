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

#include <iostream>
#include <ClanLib/Display/font.h>
#include "fonts.hxx"
#include "field.hxx"
#include "console.hxx"

class ConsoleImpl
{
public:
  std::vector<CL_Slot> slots;

  CL_Size size;

  /** Complete log of everything that got written to the console */
  std::string full_buffer;

  /** Buffer of the stuff currently visible on the screen */
  Field<char> screen;

  CL_Point cursor_pos;

  CL_Font font;

  ConsoleImpl(int w, int h);
  void putchar(char c);
  void draw();
};

ConsoleImpl::ConsoleImpl(int w, int h)
  : size(w, h),
    screen(w, h),
    cursor_pos(0, 0)
{
}

void
ConsoleImpl::draw()
{
  //std::cout << "ConsoleImpl::draw()" << std::endl;

  int font_w = font.get_width("W");
  int font_h = font.get_height();

  for(int y = 0; y < size.height; ++y)
    for(int x = 0; x < size.width; ++x)
      {
        font.draw_character(x * font_w, y * font_h, screen.at(x, y));
      }
}

Console::Console(/*const CL_Font& font,*/ const CL_Rect& rect, CL_Component* parent)
  : CL_Component(rect, parent),
    impl(new ConsoleImpl(40, 24))
{
  impl->font = Fonts::verdana11_yellow;
  impl->slots.push_back(sig_paint().connect(impl.get(), &ConsoleImpl::draw));
}

Console::~Console()
{
  
}

void
Console::clearscr()
{
  for(int y = 0; y < impl->size.height; ++y)
    for(int x = 0; x < impl->size.width; ++x)
      impl->screen.at(x, y) = 0;
}

void
ConsoleImpl::putchar(char c)
{
  full_buffer += c; 

  if (c == '\n')
    {
      cursor_pos.x = 0;
      cursor_pos.y += 1;
    }
  else
    {
      screen.at(cursor_pos.x, cursor_pos.y) = c;

      cursor_pos.x += 1;

      if (cursor_pos.x >= size.width)
        cursor_pos.x = 0;
    }

  // Move all content one line up
  if (cursor_pos.y >= size.height)
    screen.resize(size.width, size.height, 0, -1);
}

void
Console::write(const std::string& str)
{
  std::cout << str << std::flush;

  for(std::string::const_iterator i = str.begin(); i != str.end(); ++i)
    {
      if (*i != 0)
        impl->putchar(*i);
    }
}

/* EOF */
