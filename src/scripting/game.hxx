//  $Id: game.hxx,v 1.16 2003/11/05 11:08:31 grumbel Exp $
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

#ifndef HEADER_SCRIPTING_GAME_HXX
#define HEADER_SCRIPTING_GAME_HXX

#include <guile/gh.h>

class GameWorld;

/** Set the size of the tiles and there subsize */
void game_set_tilesize(int size, int subsize);

/** Load a tile definition file into the TileFactory */
void game_load_tiles(const char* resourcefile);

/** Add a ClanLib resource file to the resource manager */
void game_load_resources(const char* resourcefile);

void game_add_water(int x, int y, int w, int h);
void game_add_bomb(int x, int y);
void game_add_igel(int x, int y);
void game_add_brush_light(int x, int y);
void game_add_brush_exit(int x, int y);

void player_set_pos(float x, float y);
float player_get_pos_x();
float player_get_pos_y();

void player_set_direction(const char* direction);
int  player_get_x();
int  player_get_y();

GameWorld* make_game_world(int w, int h);
void start_game(GameWorld*);

void add_region_trigger(int x, int y, int w, int h, SCM func);
void remove_trigger();

void game_set_pause(bool p);
bool game_get_pause();
void game_quit();

void dialog_add(const char* portrait, const char* text);
void dialog_add_answer(const char* text, SCM func);
void dialog_show();
void dialog_hide();
void dialog_clear();

/** @return number of seconds since game start, time doesn't progress
    in pause mode */
float game_get_time();

int game_get_diamonds();
int game_get_max_diamonds();

float get_game_speed();
void  set_game_speed(float s);

#endif

/* EOF */
