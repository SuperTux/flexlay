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

#include "math/rect.hpp"

FileDialog::FileDialog(const std::string& title,
                       const std::string& ok_label,
                       const std::string& cancel_label) :
  m_callback(),
  m_file_dialog()
{
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
#ifdef GRUMBEL
  m_callback = func;
  m_inputbox->set_focus();
  m_window->show();
#endif
}

void
FileDialog::on_ok()
{
#ifdef GRUMBEL
  if (m_callback)
  {
    m_callback(m_inputbox->get_text());
  }
#endif
}

void
FileDialog::on_cancel()
{
}

/* EOF */
