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

#include "gui/generic_dialog.hpp"

#include <ClanLib/GUI/button.h>
#include <ClanLib/GUI/checkbox.h>
#include <ClanLib/GUI/inputbox.h>
#include <ClanLib/GUI/label.h>
#include <ClanLib/GUI/radiobutton.h>
#include <ClanLib/GUI/radiogroup.h>

#include "math/rect.hpp"
#include "gui/window.hpp"

GenericDialog::GenericDialog(const std::string& title, CL_Component* parent) :
  m_window(new Window(Rect(Point(100, 100), Size(400, 100)), title, parent)),
  m_ok(new CL_Button(Rect(Point(290, 35), Size(50, 25)).to_cl(), "Ok",
                     m_window->get_client_area())),
  m_cancel(new CL_Button(Rect(Point(230, 35), Size(50, 25)).to_cl(), "Cancel",
                         m_window->get_client_area())),
  m_items(),
  m_ok_callback()
{
  m_slots.push_back(m_cancel->sig_clicked().connect_functor([this]{ on_cancel(); }));
  m_slots.push_back(m_ok->sig_clicked().connect_functor([this]{ on_ok(); }));
}

void
GenericDialog::add_label(const std::string& text)
{
  m_items.emplace_back(TYPE_LABEL,
                       new CL_Label(Point(10, 10).to_cl(), text, m_window->get_client_area()),
                       nullptr);
  update_layout();
}

void
GenericDialog::add_float(const std::string& name, float value)
{
  CL_Label* label = new CL_Label(Point(10, 10).to_cl(), name,
                                 m_window->get_client_area());
  CL_InputBox* inputbox = new CL_InputBox(Rect(Point(110, 10), Size(200, 25)).to_cl(),
                                          m_window->get_client_area());

  inputbox->set_text(std::to_string(value));

  m_items.emplace_back(TYPE_FLOAT, label, inputbox);
  update_layout();
}

void
GenericDialog::add_bool(const std::string& name, bool value)
{
  CL_Label* label = new CL_Label(Point(10, 10).to_cl(), name,
                                 m_window->get_client_area());
  CL_CheckBox* checkbox = new CL_CheckBox(Point(110, 10).to_cl(), "",
                                          m_window->get_client_area());

  if (value)
  {
    checkbox->set_checked();
  }

  m_items.emplace_back(TYPE_BOOL, label, checkbox);
  update_layout();
}

void
GenericDialog::add_int(const std::string& name, int value)
{
  CL_Label* label = new CL_Label(Point(10, 10).to_cl(), name,
                                 m_window->get_client_area());
  CL_InputBox* inputbox = new CL_InputBox(Rect(Point(110, 10), Size(200, 25)).to_cl(),
                                          m_window->get_client_area());

  inputbox->set_text(std::to_string(value));

  m_items.emplace_back(TYPE_INT, label, inputbox);
  update_layout();
}

void
GenericDialog::add_string(const std::string& name, const std::string& value)
{
  CL_Label* label = new CL_Label(Point(10, 10).to_cl(), name,
                                 m_window->get_client_area());
  CL_InputBox* inputbox = new CL_InputBox(Rect(Point(110, 10), Size(200, 25)).to_cl(),
                                          m_window->get_client_area());

  inputbox->set_text(value);

  m_items.emplace_back(TYPE_STRING, label, inputbox);
  update_layout();
}

void
GenericDialog::add_enum(const std::string& name,
                        const std::vector<std::string>& types,
                        const std::string& value)
{
  CL_Label* label = new CL_Label(Point(10, 10).to_cl(), name,
                                 m_window->get_client_area());
  CL_RadioGroup* group = new CL_RadioGroup();
  for(auto& type : types)
  {
    CL_RadioButton* radio = new CL_RadioButton(Point(0, 0).to_cl(), type,
                                               m_window->get_client_area());
    radio->set_checked(type == value);
    group->add(radio);
  }

  m_items.emplace_back(TYPE_ENUM, label, nullptr, group);
  update_layout();
}

void
GenericDialog::update_layout()
{
  int y = 10;

  for(auto& item : m_items)
  {
    item.label->set_position(10, y);
    if (item.type == TYPE_ENUM)
    {
      y += 5;
      for(auto& child : item.group->get_buttons())
      {
        child->set_position(110, y);
        y += 20;
      }
    }
    else
    {
      if (item.body)
      {
        item.body->set_position(110, y);
      }
      y += 25;
    }
  }
  y += 5;

  m_cancel->set_position(200, y);
  m_ok->set_position(260, y);
  m_window->set_size(330, y + 60);
}

void
GenericDialog::on_cancel()
{
  m_window->hide();
}

void
GenericDialog::on_ok()
{
  m_window->hide();
  if (m_ok_callback)
  {
    m_ok_callback();
  }
}

void
GenericDialog::set_ok_callback(std::function<void ()> callback)
{
  m_ok_callback = callback;
}

std::vector<PropertyValue>
GenericDialog::get_values() const
{
  std::vector<PropertyValue> result;

  for(auto& item : m_items)
  {
    switch(item.type)
    {
      case TYPE_LABEL:
        // ignored
        break;

      case TYPE_ENUM:
        {
          int idx = 0;
          for(auto& button : item.group->get_buttons())
          {
            if (button == item.group->get_toggled())
            {
              result.emplace_back(idx);
              break;
            }
            idx += 1;
          }
        }
        break;
        
      case TYPE_BOOL:
        result.emplace_back(static_cast<bool>(std::stoi(static_cast<CL_InputBox*>(item.body)->get_text())));
        break;

      case TYPE_INT:
        result.emplace_back(std::stoi(static_cast<CL_InputBox*>(item.body)->get_text()));
        break;

      case TYPE_FLOAT:
        result.emplace_back(std::stof(static_cast<CL_InputBox*>(item.body)->get_text()));
        break;

      case TYPE_STRING:
        result.emplace_back(static_cast<CL_InputBox*>(item.body)->get_text());
        break;
    }
  }

  return result;
}

  /* EOF */
