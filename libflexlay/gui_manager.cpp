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

#include "gui_manager.hpp"

#include <QApplication>
#include <QMainWindow>
#include <QMenuBar>
#include <QGridLayout>
#include <QScrollBar>
#include <QToolBar>

#include "globals.hpp"
#include "gui/button_panel.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/editor_map_widget.hpp"
#include "gui/file_dialog.hpp"
#include "gui/generic_dialog.hpp"
#include "gui/menubar.hpp"
#include "gui/minimap.hpp"

GUIManager::GUIManager() :
  m_window(new QMainWindow)
{
}

GUIManager::~GUIManager()
{
}

void
GUIManager::run()
{
  m_window->show();
  QApplication::instance()->exec();
}

void
GUIManager::quit()
{
  QCoreApplication::quit();
}

Menubar*
GUIManager::create_menubar()
{
  QMenuBar* menubar = m_window->menuBar();
  menubar->show();
  return new Menubar(menubar);
}

ButtonPanel*
GUIManager::create_button_panel(bool horizontal)
{
  QToolBar* toolbar = m_window->addToolBar(QString("Toolbar Title?"));
  return new ButtonPanel(toolbar);
}

GenericDialog*
GUIManager::create_generic_dialog(const std::string& title)
{
  return new GenericDialog(title, m_window.get());
}

EditorMapComponent*
GUIManager::create_editor_map_component()
{
  QWidget* central = new QWidget;

  QGridLayout* layout = new QGridLayout(central);
  layout->setContentsMargins(0, 0, 0, 0);
  layout->setHorizontalSpacing(0);
  layout->setVerticalSpacing(0);

  m_window->setCentralWidget(central);

  //QWidget* dummy = new QWidget;
  //layout->addWidget(dummy, 0, 0);

  EditorMapComponent* editor = new EditorMapComponent(nullptr);
  layout->addWidget(editor->get_editormap_widget(), 0, 0);

  QScrollBar* scroll_horz = new QScrollBar(Qt::Horizontal);
  QScrollBar* scroll_vert = new QScrollBar(Qt::Vertical);

  layout->addWidget(scroll_horz, 1, 0);
  layout->addWidget(scroll_vert, 0, 1);

//dummy->setStyleSheet("background-color:black;");

return editor;
}

Minimap*
GUIManager::create_minimap(EditorMapComponent* parent)
{
  return new Minimap(parent);
}

FileDialog*
GUIManager::create_filedialog(const std::string& titel,
                              const std::string& ok_label, const std::string& cancel_label)
{
  return new FileDialog(titel, ok_label, cancel_label);
}

/* EOF */
