//  $Id: editor.hxx,v 1.9 2003/11/04 22:48:51 grumbel Exp $
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

#ifndef HEADER_EDITOR_HXX
#define HEADER_EDITOR_HXX

#include <vector>

class TileEditor;
class EditorMap;
class GUIManager;

/** FIXME: Obsolete class, should be removed, scripting can then use GUIManager directly */
class Editor
{
private:
  GUIManager* manager;

public:
  Editor();
  ~Editor();

  GUIManager*  get_gui_manager() const { return manager; }
  
  void run();
};

#endif

/* EOF */
