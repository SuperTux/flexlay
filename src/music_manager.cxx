//  $Id: music_manager.cxx,v 1.2 2003/11/06 09:53:43 grumbel Exp $
//
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#include <iostream>
#include <ClanLib/Core/System/system.h>
#include "music_manager.hxx"

MusicManager* MusicManager::current_ = 0; 

void
MusicManager::init()
{
  current_ = new MusicManager();
}

void
MusicManager::deinit()
{
  delete current_;
}

MusicManager::MusicManager()
{
  last_time = CL_System::get_time();
  state = STOPPED;
  waiting = false;
}

void
MusicManager::play(const std::string& filename, bool loop)
{
  if (state == STOPPED)
    {
      state = PLAYING;
      background_music = CL_SoundBuffer(filename);
      background_music_session = background_music.prepare();
      background_music_session.play();
      background_music_session.set_looping(loop);
      background_music_session.set_volume(1.0f);
      volume = 1.0f;
    }
  else
    {
      waiting = true;
      next_filename = filename;
      next_loop = loop;
      state = FADEOUT;
    }
}

void
MusicManager::stop()
{
  if (state == PLAYING)
    state = FADEOUT;
}

void
MusicManager::keep_alive()
{
  unsigned int cur_time = CL_System::get_time();
  float delta = (cur_time - last_time)/1000.0f;
  last_time = cur_time;

  if (state == FADEOUT)
    {
      volume -= delta;
      if (volume < 0)
        {
          background_music_session.stop();
          state = STOPPED;
          if (waiting)
            {
              waiting = false;
              play(next_filename, next_loop);
            }
        }
      else
        {
          background_music_session.set_volume(volume);
        }
    }
}

/* EOF */
