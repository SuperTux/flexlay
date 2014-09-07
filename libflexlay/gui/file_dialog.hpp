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

#ifndef HEADER_FILE_DIALOG_HPP
#define HEADER_FILE_DIALOG_HPP

#include <string>
#include <functional>

class QFileDialog;

class FileDialog
{
private:
  std::function<void (std::string)> m_callback;
  QFileDialog* m_file_dialog;

protected:
  virtual ~FileDialog();
public:
  FileDialog(const std::string& titel,
             const std::string& ok_label, const std::string& cancel_label);

  void set_filename(const std::string& filename);
  std::string get_filename() const;
  void run(std::function<void(std::string)> func);
  void on_ok();
  void on_cancel();

private:
  FileDialog(const FileDialog&);
  FileDialog& operator=(const FileDialog&);
};

#endif

/* EOF */
