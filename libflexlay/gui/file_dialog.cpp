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

#include "gui/file_dialog.hpp"

#include <QFileDialog>
#include <iostream>

#include "math/rect.hpp"

FileDialog::FileDialog(const std::string& title,
                       const std::string& ok_label,
                       const std::string& cancel_label) :
  m_callback(),
  m_file_dialog(new QFileDialog())
{
  m_file_dialog->setFileMode(QFileDialog::ExistingFile);
}

FileDialog::~FileDialog()
{
}

void
FileDialog::set_filename(const std::string& filename)
{
}

std::string
FileDialog::get_filename() const
{
  return {};
}

void
FileDialog::run(std::function<void(std::string)> func)
{
  m_callback = func;
  if (m_callback)
  {
    QObject::connect(m_file_dialog, &QFileDialog::fileSelected, [this](const QString& filename)
                     {
                       std::cout << "FileDialog callback called: " << filename.toStdString() << std::endl;
                       m_callback(filename.toStdString());
                     });
  }

  m_file_dialog->show();
}

/* EOF */
