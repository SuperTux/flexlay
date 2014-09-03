// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmx.de>
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

class FileDialog
{
private:
public:
  FileDialog(const std::string& titel,
             const std::string& ok_label, const std::string& cancel_label);

private:
  FileDialog(const FileDialog&);
  FileDialog& operator=(const FileDialog&);
};

#endif

/* EOF */