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

#ifndef HEADER_FLEXLAY_ICON_HPP
#define HEADER_FLEXLAY_ICON_HPP

#include <ClanLib/GUI/component.h>
#include <ClanLib/Display/sprite.h>
#include <boost/shared_ptr.hpp>

class IconImpl;

/** */
class Icon : public CL_Component
{
protected:
  virtual ~Icon() {}
public:
  Icon(const CL_Rect& rect, const CL_Sprite& sprite, const std::string& tooltip, CL_Component* parent);

  void disable();
  void enable();

  void set_up();
  void set_down();
  
  CL_Signal_v0& sig_clicked();

private:
  Icon (const Icon&);
  Icon& operator= (const Icon&);

  boost::shared_ptr<IconImpl> impl;
};

#endif

/* EOF */
