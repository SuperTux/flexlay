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
class ToolManager;
class EditorMap;
class GUIManager;
class Command;

/** */
class Editor
{
private:
  GUIManager* manager;

  ToolManager* tool_manager;

  std::vector<Command*> undo_stack;
  std::vector<Command*> redo_stack;

  static Editor* current_;
public:
  static Editor* current() { return current_; }

  Editor();
  ~Editor();

  ToolManager* get_tool_manager() const { return tool_manager; }

  void run();

  // FIXME: Move undo stuff to EditorMap or some other per map class

  /** Execute a command and place it on the undo stack, commands given
      to this function will be deleted by the Editor class, so they
      have to be new'ed */
  void execute(Command* command);

  /** Move backward in the undo stack */
  void undo();

  /** Move forward in the undo stack */
  void redo();
private:
  Editor (const Editor&);
  Editor& operator= (const Editor&);
};

#endif

/* EOF */
