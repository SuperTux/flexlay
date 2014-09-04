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

#ifndef HEADER_GENERIC_DIALOG_HPP
#define HEADER_GENERIC_DIALOG_HPP

#include <string>
#include <vector>

#include <ClanLib/Signals/slot.h>

class CL_RadioGroup;
class CL_Label;
class CL_Button;
class CL_Component;
class Window;

class GenericDialog
{
private:
  Window* m_window;
  CL_Button* m_ok;
  CL_Button* m_cancel;
  std::vector<CL_Slot> m_slots;
  enum Type {
    TYPE_LABEL,
    TYPE_STRING,
    TYPE_BOOL,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_ENUM
  };
  struct Item
  {
    Type type;
    CL_Component* label;
    CL_Component* body;
    CL_RadioGroup* group;

    Item(Type type_, CL_Component* label_, CL_Component* body_, CL_RadioGroup* group_ = nullptr) :
      type(type_), label(label_), body(body_), group(group_)
    {}
  };
  std::vector<Item> m_items;

public:
  GenericDialog(const std::string& title, CL_Component* parent);

  void add_label(const std::string& text);
  void add_bool(const std::string& name, bool value);
  void add_int(const std::string& name, int value);
  void add_float(const std::string& name, float value);
  void add_string(const std::string& name, const std::string& value);
  void add_enum(const std::string& name,
                const std::vector<std::string>& types,
                const std::string& value);

  void on_ok();
  void on_cancel();

private:
  void update_layout();

private:
  GenericDialog(const GenericDialog&);
  GenericDialog& operator=(const GenericDialog&);
};

#endif

/* EOF */
