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
#include <functional>

#include "property_value.hpp"

class QWidget;
class QFormLayout;
class QDialog;
class QDialogButtonBox;
class QButtonGroup;

class GenericDialog
{
private:
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
    QWidget* label;
    QWidget* body;
    QButtonGroup* group;
    Item(Type type_, QWidget* label_, QWidget* body_, QButtonGroup* group_ = nullptr) :
      type(type_), label(label_), body(body_), group(group_)
    {}
  };

private:
  QDialog* m_dialog;
  QDialogButtonBox* m_buttonbox;
  QFormLayout* m_layout;
  std::vector<Item> m_items;

  std::function<void ()> m_ok_callback;

public:
#ifndef SWIG
  GenericDialog(const std::string& title, QWidget* parent);
  ~GenericDialog();
#endif

  void add_label(const std::string& text);
  void add_bool(const std::string& name, bool value);
  void add_int(const std::string& name, int value);
  void add_float(const std::string& name, float value);
  void add_string(const std::string& name, const std::string& value);
  void add_enum(const std::string& name,
                const std::vector<std::string>& types,
                const std::string& value);

  void set_ok_callback(std::function<void ()> callback);
  std::vector<PropertyValue> get_values() const;

private:
  GenericDialog(const GenericDialog&);
  GenericDialog& operator=(const GenericDialog&);
};

#endif

/* EOF */
