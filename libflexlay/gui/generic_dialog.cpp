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

#include <QButtonGroup>
#include <QCheckBox>
#include <QDialog>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>
#include <QRadioButton>
#include <QVBoxLayout>

#include "math/rect.hpp"

GenericDialog::GenericDialog(const std::string& title, QWidget* parent) :
  m_dialog(new QDialog(parent)),
  m_buttonbox(),
  m_layout(),
  m_items(),
  m_ok_callback()
{
  m_dialog->setModal(true);
  m_dialog->setWindowTitle(QString::fromStdString(title));
  m_buttonbox = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);

  QVBoxLayout* vbox = new QVBoxLayout;
  m_layout = new QFormLayout;
  vbox->addLayout(m_layout);
  vbox->addWidget(m_buttonbox);

  m_dialog->setLayout(vbox);
  m_dialog->setMinimumWidth(300);
  m_dialog->show();
}

GenericDialog::~GenericDialog()
{
}

void
GenericDialog::add_label(const std::string& text)
{
  QLabel* label = new QLabel(QString::fromStdString(text));
  m_layout->addRow(label);
  m_items.emplace_back(TYPE_LABEL, label, nullptr, nullptr);
}

void
GenericDialog::add_bool(const std::string& name, bool value)
{
  QLabel* label = new QLabel(QString::fromStdString(name));
  QCheckBox* checkbox = new QCheckBox();
  m_layout->addRow(label, checkbox);

  if (value)
  {
    checkbox->setCheckState(Qt::Checked);
  }

  m_items.emplace_back(TYPE_BOOL, label, checkbox);
}

void
GenericDialog::add_int(const std::string& name, int value)
{
  QLabel* label = new QLabel(QString::fromStdString(name));
  QLineEdit* inputbox = new QLineEdit();
  m_layout->addRow(label, inputbox);

  inputbox->setText(QString::number(value));

  m_items.emplace_back(TYPE_INT, label, inputbox);
}

void
GenericDialog::add_float(const std::string& name, float value)
{
  QLabel* label = new QLabel(QString::fromStdString(name));
  QLineEdit* inputbox = new QLineEdit();
  m_layout->addRow(label, inputbox);

  inputbox->setText(QString::number(value));

  m_items.emplace_back(TYPE_FLOAT, label, inputbox);
}

void
GenericDialog::add_string(const std::string& name, const std::string& value)
{
  QLabel* label = new QLabel(QString::fromStdString(name));
  QLineEdit* inputbox = new QLineEdit();
  m_layout->addRow(label, inputbox);

  inputbox->setText(QString::fromStdString(value));

  m_items.emplace_back(TYPE_STRING, label, inputbox);
}

void
GenericDialog::add_enum(const std::string& name,
                        const std::vector<std::string>& types,
                        const std::string& value)
{
  QLabel* label = new QLabel(QString::fromStdString(name));
  QButtonGroup* group = new QButtonGroup();
  for(auto& type : types)
  {
    QRadioButton* radio = new QRadioButton(QString::fromStdString(type));
    radio->setChecked(type == value);
    group->addButton(radio);
  }

  m_items.emplace_back(TYPE_ENUM, label, nullptr, group);
}

void
GenericDialog::set_ok_callback(std::function<void ()> callback)
{
  m_ok_callback = callback;
  QObject::connect(m_buttonbox, &QDialogButtonBox::accepted, [this]{
      m_ok_callback(); 
      m_dialog->hide(); 
    });
  QObject::connect(m_buttonbox, &QDialogButtonBox::rejected, [this]{ 
      m_dialog->hide(); 
    });
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
          for(auto& button : item.group->buttons())
          {
            if (button == item.group->checkedButton())
            {
              result.emplace_back(idx);
              break;
            }
            idx += 1;
          }
        }
        break;

      case TYPE_BOOL:
        result.emplace_back(static_cast<QCheckBox*>(item.body)->checkState() == Qt::Checked);
        break;

      case TYPE_INT:
        result.emplace_back(static_cast<QLineEdit*>(item.body)->text().toInt());
        break;

      case TYPE_FLOAT:
        result.emplace_back(static_cast<QLineEdit*>(item.body)->text().toFloat());
        break;

      case TYPE_STRING:
        result.emplace_back(static_cast<QLineEdit*>(item.body)->text().toStdString());
        break;
    }
  }

  return result;
}

/* EOF */
