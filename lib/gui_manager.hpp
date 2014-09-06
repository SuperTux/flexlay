// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_GUI_MANAGER_HPP
#define HEADER_FLEXLAY_GUI_MANAGER_HPP

#include <memory>

class ButtonPanel;
class EditorMapComponent;
class FileDialog;
class GUIManagerImpl;
class GenericDialog;
class Menubar;
class Minimap;
class QMainWindow;
class Rect;

class GUIManager
{
public:
  GUIManager();
  ~GUIManager();

  void run();
  void quit();

  Menubar* create_menubar();
  ButtonPanel* create_button_panel(bool horizontal);
  GenericDialog* create_generic_dialog(const std::string& title);
  EditorMapComponent* create_editor_map_component();
  Minimap* create_minimap(EditorMapComponent* parent, const Rect& rect);
  FileDialog* create_filedialog(const std::string& titel,
                                const std::string& ok_label, const std::string& cancel_label);

private:
  std::unique_ptr<QMainWindow> m_window;
};

#endif

/* EOF */
