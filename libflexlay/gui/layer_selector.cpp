// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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

#include "gui/layer_selector.hpp"

#include <QList>
#include <QListView>
#include <QVBoxLayout>
#include <QTreeView>
#include <QStandardItemModel>
#include <QToolBar>
#include <QScrollArea>

LayerSelector::LayerSelector() :
  m_vbox(),
  m_tree_view()
{
  QStandardItemModel* model = new QStandardItemModel;
  model->setHorizontalHeaderItem(0, new QStandardItem( "Visible" ) );
  model->setHorizontalHeaderItem(1, new QStandardItem( "Layer" ) );
  
  model->appendRow(QList<QStandardItem*>({
        new QStandardItem(QIcon("../data/images/icons16/resize1.png"), "Wrong"),
          new QStandardItem("Background")
          }));

  model->appendRow(QList<QStandardItem*>({
          new QStandardItem("Eye"),
          new QStandardItem("Interactive")
          }));

  model->appendRow(QList<QStandardItem*>({
          new QStandardItem("Eye"),
          new QStandardItem("Foreground")
          }));
  m_vbox = new QWidget;

  // Use QTreeWidget instead!?
  m_tree_view = new QTreeView;
  m_tree_view->setModel(model);

  m_scroll_area = new QScrollArea;

  m_toolbar = new QToolBar;
  m_toolbar->addAction("Hide All");
  m_toolbar->addAction("Show All");

  m_layout = new QVBoxLayout(m_vbox);
  m_layout->setContentsMargins(0, 0, 0, 0);
  m_layout->addWidget(m_scroll_area);
  m_layout->addWidget(m_toolbar);
  
  m_scroll_area->setWidgetResizable(true);
  m_scroll_area->setWidget(m_tree_view);
}

QWidget*
LayerSelector::get_widget() const
{
  return m_vbox;
}

/* EOF */
