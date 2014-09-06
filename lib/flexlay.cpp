// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
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

#include "flexlay.hpp"

#include "globals.hpp"
#include "fonts.hpp"

Flexlay* Flexlay::current_ = 0;

Flexlay::Flexlay() :
  m_app()
{
  current_ = this;

  int* argc = new int(1);
  char** argv = new char*[2];
  argv[0] = new char[10]{ 'a', '\0' };
  argv[1] = nullptr;
  m_app.reset(new QApplication(*argc, argv));
}

boost::signals2::signal<void (int, int)>&
Flexlay::sig_resize()
{
  return m_sig_resize;
}

void
Flexlay::init(const std::string& title)
{
  std::cout << "Flexlay::init()" << std::endl;
}

void
Flexlay::deinit()
{
  std::cout << "Flexlay::deinit()" << std::endl;
}

void
Flexlay::set_datadir(const std::string& datadir_)
{
  datadir = datadir_;
}

/* EOF */
