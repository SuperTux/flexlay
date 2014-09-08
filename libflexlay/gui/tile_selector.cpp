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

#include "gui/tile_selector.hpp"

#include <QScrollArea>
#include <QVBoxLayout>
#include <QComboBox>
#include <iostream>

#include "gui/tile_selector_widget.hpp"

TileSelector::TileSelector() :
  m_combobox(),
  m_widget(),
  m_scroll_area()
{
  m_combobox = new QComboBox;

  m_scroll_area = new QScrollArea;
  m_scroll_area->setWidgetResizable(true);
  m_scroll_area->setWidget(m_widget);

  m_widget = new TileSelectorWidget(m_scroll_area->viewport());
  m_scroll_area->setWidget(m_widget);

  m_box = new QWidget;
  m_layout = new QVBoxLayout(m_box);
  m_layout->setContentsMargins(0, 0, 0, 0);
  m_layout->addWidget(m_combobox);
  m_layout->addWidget(m_scroll_area);

  QObject::connect(m_combobox, static_cast<void (QComboBox::*)(const QString&)>(&QComboBox::activated),
                   [this](const QString& text){
                     auto tiles = m_tiles[text.toStdString()];
                     std::cout << "Setting tiles: " << text.toStdString() << " - " << tiles.size() << std::endl;
                     m_widget->set_tiles(tiles);
                     m_scroll_area->update();
                   });
}

TileSelector::~TileSelector()
{
}

void
TileSelector::set_tileset(Tileset t)
{
  m_widget->set_tileset(t);
}

void
TileSelector::set_tiles(const Tiles& t)
{
  m_widget->set_tiles(t);
  m_combobox->clear();
  m_combobox->hide();
}

void
TileSelector::set_tiles(const std::string& name, const Tiles& t)
{
  m_tiles[name] = t;
  m_combobox->addItem(QString::fromStdString(name), QVariant(QString::fromStdString(name)));
  m_combobox->show();
}

TileSelector::Tiles
TileSelector::get_tiles() const
{
  return m_widget->get_tiles();
}

void
TileSelector::set_scale(float s)
{
  m_widget->set_scale(s);
}

QWidget*
TileSelector::get_widget() const
{
  return m_box;
}

/* EOF */
