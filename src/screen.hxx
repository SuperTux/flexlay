//  $Id: screen.hxx,v 1.1 2003/09/20 21:55:57 grumbel Exp $
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

#ifndef HEADER_SCREEN_HXX
#define HEADER_SCREEN_HXX

/** */
class Screen
{
private:
  bool do_quit;
  bool do_pause;
public:
  Screen();
  virtual ~Screen() {}

  virtual void draw() =0;
  virtual void update(float delta) =0;

  virtual void on_startup() {}
  virtual void on_shutdown() {}

  void display();

  void quit() { do_quit = true; }
  void set_pause(bool p) { do_pause = p; }
  bool get_pause() { return do_pause; }
private:
  Screen (const Screen&);
  Screen& operator= (const Screen&);
};

#endif

/* EOF */
