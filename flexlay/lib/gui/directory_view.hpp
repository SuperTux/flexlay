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

#ifndef HEADER_FLEXLAY_DIRECTORY_VIEW_HPP
#define HEADER_FLEXLAY_DIRECTORY_VIEW_HPP

#include <ClanLib/GUI/component.h>
#include <boost/shared_ptr.hpp>

class DirectoryViewImpl;

class DirectoryView : public CL_Component
{
private:
protected:
  virtual ~DirectoryView();
public:
  DirectoryView(const CL_Rect& rect, CL_Component* parent);

  /** Set the directory which should be shown */
  void set_directory(const std::string& path_);

  /** Call the callback on click on a directory item */
  CL_Signal_v1<std::string>& sig_on_click();

private:
  boost::shared_ptr<DirectoryViewImpl> impl;
};

#endif

/* EOF */
