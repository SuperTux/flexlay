//  $Id: globals.hxx,v 1.9 2003/11/04 22:48:51 grumbel Exp $
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

#ifndef GLOBALS_HXX
#define GLOBALS_HXX

#include <string>
#include <ClanLib/Core/Resources/resource_manager.h>

typedef enum { WEST, EAST} Direction;

#define TILE_SIZE    128
#define SUBTILE_SIZE  16
#define SUBTILE_NUM  (TILE_SIZE/SUBTILE_SIZE)

/** datadir => /usr/local/share/games/windstille/ */
extern std::string datadir;

/** libdir => /usr/local/lib/windstille/ */
extern std::string libdir;

/** bindir => /usr/local/games/ */
extern std::string bindir;

/** homedir => /home/juser/.windstille/ */
extern std::string homedir;

extern CL_ResourceManager* resources;

extern float game_speed;

extern int debug;

#endif

/* EOF */
