//  $Id: windstille_level.hxx,v 1.5 2003/08/18 08:50:22 grumbel Exp $
// 
//  Windstille - A Jump'n Shoot Game
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef WINDSTILLELEVEL_HXX
#define WINDSTILLELEVEL_HXX

#include <guile/gh.h>
#include <string>
#include "field.hxx"

class WindstilleLevel
{
private:
  Field<int>* tilemap;
  Field<int>* background_tilemap;

public:
  WindstilleLevel (const std::string& filename);

  Field<int>* get_tilemap() const { return tilemap; }
  Field<int>* get_background_tilemap() const { return background_tilemap; }
private:
  void parse_file (const std::string& filename);
  void parse_properties (SCM cur);

  Field<int>* parse_tilemap (SCM cur);
  void parse_foreground_tilemap (SCM cur);
  void parse_background_tilemap (SCM cur);

  void parse_backgound_tilemap (SCM cur);
  void parse_gameobjects (SCM cur);
};

#endif

/* EOF */
