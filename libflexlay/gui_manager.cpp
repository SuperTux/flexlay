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
#include <QDockWidget>
#include <QGridLayout>
#include <QMainWindow>
#include <QMenuBar>
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
#include "gui/object_selector.hpp"
#include "gui/tile_selector.hpp"

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
  QToolBar* toolbar = new QToolBar;
  m_window->addToolBar(horizontal ? Qt::TopToolBarArea : Qt::LeftToolBarArea, toolbar);
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

  EditorMapComponent* editor = new EditorMapComponent(nullptr);
  QVBoxLayout* layout = new QVBoxLayout();
  layout->setContentsMargins(0,0,0,0);
  layout->addWidget(editor->get_widget());
  central->setLayout(layout);

  m_window->setCentralWidget(central);

  return editor;
}

Minimap*
GUIManager::create_minimap(EditorMapComponent* parent)
{
  QDockWidget* dockwidget = new QDockWidget("Minimap");
  Minimap* minimap = new Minimap(parent);
  dockwidget->setWidget(minimap->get_widget());

  m_window->addDockWidget(Qt::BottomDockWidgetArea, dockwidget);
  return minimap;
}

FileDialog*
GUIManager::create_filedialog(const std::string& titel,
                              const std::string& ok_label, const std::string& cancel_label)
{
  return new FileDialog(titel, ok_label, cancel_label);
}

ObjectSelector*
GUIManager::create_object_selector(int w, int h)
{
  QDockWidget* dockwidget = new QDockWidget("Object Selector");
  ObjectSelector* object_selector = new ObjectSelector(w, h, nullptr);
  dockwidget->setWidget(object_selector->get_widget());

  m_window->addDockWidget(Qt::RightDockWidgetArea, dockwidget);
  return object_selector;
}

TileSelector*
GUIManager::create_tile_selector()
{
  QDockWidget* dockwidget = new QDockWidget("Tile Selector");
  TileSelector* tile_selector = new TileSelector;
  dockwidget->setWidget(tile_selector->get_widget());

  m_window->addDockWidget(Qt::RightDockWidgetArea, dockwidget);
  return tile_selector;
}

/* EOF */
