//  $Id: music_manager.hxx,v 1.1 2003/11/06 09:24:17 grumbel Exp $
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

#ifndef HEADER_MUSIC_MANAGER_HXX
#define HEADER_MUSIC_MANAGER_HXX

#include <ClanLib/Core/System/keep_alive.h>
#include <ClanLib/Sound/soundbuffer.h>
#include <ClanLib/Sound/soundbuffer_session.h>

/** */
class MusicManager
  : public CL_KeepAlive
{
private:
  unsigned int last_time;

  CL_SoundBuffer background_music;
  CL_SoundBuffer_Session background_music_session;

  enum State { FADEOUT, PLAYING, STOPPED } state;
  float volume;

  bool waiting;
  std::string next_filename;
  bool next_loop;

  static MusicManager* current_; 
public:
  static MusicManager* current() { return current_; }
  static void init();
  static void deinit();

  MusicManager();

  void play(const std::string& filename, bool loop);
  void stop();

  void keep_alive();
};

#endif

/* EOF */
