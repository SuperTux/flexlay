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

#include <ClanLib/GUI/button.h>
#include <ClanLib/GUI/inputbox.h>

#include "math/rect.hpp"
#include "gui/window.hpp"

FileDialog::FileDialog(const std::string& title,
                       const std::string& ok_label, const std::string& cancel_label,
                       CL_Component* parent) :
  m_window(),
  m_inputbox(),
  m_ok_button(),
  m_cancel_button(),
  m_callback(),
  m_slots()
{
  m_window = new Window(Rect(Point(120, 200), Size(560, 100)), title, parent);
  m_inputbox = new CL_InputBox(Rect(Point(10, 10), Size(530, 25)).to_cl(),
                               m_window->get_client_area());
  m_ok_button = new CL_Button(Rect(Point(490, 35), Size(50, 25)).to_cl(), ok_label,
                              m_window->get_client_area());
  m_cancel_button = new CL_Button(Rect(Point(430, 35), Size(50, 25)).to_cl(), cancel_label,
                                  m_window->get_client_area());
  m_window->hide();
}

FileDialog::~FileDialog()
{
}

void
FileDialog::set_filename(const std::string& filename)
{
  m_inputbox->set_text(filename);
}

std::string
FileDialog::get_filename() const
{
  return m_inputbox->get_text();
}

void
FileDialog::run(std::function<void(std::string)> func)
{
  m_slots.push_back(m_ok_button->sig_clicked().connect_functor([this]{ on_ok(); }));
  m_slots.push_back(m_inputbox->sig_return_pressed().connect_functor([this]{ on_ok(); }));
  m_slots.push_back(m_cancel_button->sig_clicked().connect_functor([this]{ on_cancel(); }));

  m_callback = func;
  m_inputbox->set_focus();
  m_window->show();
}

void
FileDialog::on_ok()
{
  m_window->hide();
  if (m_callback)
  {
    m_callback(m_inputbox->get_text());
  }
}

void
FileDialog::on_cancel()
{
  m_window->hide();
}

/* EOF */
