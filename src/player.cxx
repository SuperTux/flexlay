//  $Id: player.cxx,v 1.8 2003/08/12 14:37:03 grumbel Exp $
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

#include "display.hxx"
#include "game_world.hxx"
#include "default_shoot.hxx"
#include "laser_shoot.hxx"
#include "controller.hxx"
#include "player.hxx"
#include "globals.hxx"

Player::Player (Controller* c) :
  controller (c),
  pos (320, 500),
  velocity (0, 0),
  
  walk     ("hero/run",     resources),
  jump     ("hero/jump",     resources),
  stand    ("hero/stand",    resources),
  shild    ("turrican/shild",    resources),
  sit      ("turrican/sit",      resources),
  roll     ("turrican/roll",     resources),
  surround ("turrican/surround", resources),

  state (WALKING),
  gun_state (GUN_READY),
  ground_state (IN_AIR),
  shild_time (0)
{  
  walk.set_alignment(origin_bottom_center, 0, 3);
  jump.set_alignment(origin_bottom_center, 0, 3);
  stand.set_alignment(origin_bottom_center, 0, 3);
  shild.set_alignment(origin_bottom_center);
  roll.set_alignment(origin_bottom_center);
  surround.set_alignment(origin_bottom_center);

  direction = WEST;
}

void
Player::draw ()
{
  //std::cout << "onground: " << ground_state << std::endl;
  CL_Sprite* sprite = 0;

  switch (ground_state)
    {
    case ON_GROUND:
      switch (state)
	{
	case  WALKING:
	  sprite = &walk;
	  break;

	case STANDING:
	  sprite = &stand;
	  break;

	case SITTING:
	  sprite = &sit;
	  break;

	case ROLLING:
	  sprite = &roll;
	  break;

	case SURROUND:
	  sprite = &surround;
	  break;
	}
      break;
    default:
      sprite = &jump;
      break;
    }

  if (sprite)
    {
      if (direction == WEST)
	sprite->set_scale (1.0, 1.0);
      else
	sprite->set_scale (-1.0, 1.0);

      sprite->draw (int(pos.x), int(pos.y));

      if (shild_time > 0)
	{
	  shild.draw (int(pos.x), int(pos.y));
	}
    }

  if (0)
    {
      CL_Display::fill_rect(CL_Rect(int(pos.x - 5), int(pos.y - 5),
                                    int(pos.x + 5), int(pos.y + 5)),
                            CL_Color(255, 255, 255));

      SubTilePos new_subtile_pos = get_subtile_pos();
 
      CL_Color color;

      if (get_world()->get_tilemap()->get_pixel(new_subtile_pos.x, new_subtile_pos.y))
        color = CL_Color(255, 255, 0, 128);
      else
        color = CL_Color(0,  0, 255, 128);

      CL_Display::fill_rect(CL_Rect(new_subtile_pos.x*SUBTILE_SIZE,
                                    new_subtile_pos.y*SUBTILE_SIZE,
                                    new_subtile_pos.x*SUBTILE_SIZE + SUBTILE_SIZE,
                                    new_subtile_pos.y*SUBTILE_SIZE + SUBTILE_SIZE),
                            color);
    }
}

SubTilePos
Player::get_subtile_pos()
{
  return SubTilePos(int(pos.x/SUBTILE_SIZE), int(pos.y/SUBTILE_SIZE));
}

void 
Player::update (float delta)
{
  walk.update(delta);
  
  if (controller->is_left ())
    direction = WEST;
  else if  (controller->is_right ())
    direction = EAST;

  switch(ground_state)
    {
    case ON_GROUND:
      update_ground (delta);
      if (!on_ground ())
        ground_state = IN_AIR;
      break;
    case IN_AIR:
      update_air (delta);
      break;
    }

  SubTilePos new_subtile_pos = get_subtile_pos();
  if (!(subtile_pos == new_subtile_pos))
    {
      if (get_world()->get_tilemap()->get_pixel(new_subtile_pos.x, new_subtile_pos.y))
        {
          pos.x = subtile_pos.x * SUBTILE_SIZE;
          pos.y = subtile_pos.y * SUBTILE_SIZE;
        }
      else
        {
          //std::cout << "Position changed: " << subtile_pos.x << ", " << subtile_pos.y << std::endl;
          subtile_pos = new_subtile_pos;
        }
    }
}

void 
Player::update_shooting (float delta)
{
  if (state != ROLLING)
    {
      switch (gun_state)
	{
	case GUN_READY:
	  if (controller->fire_pressed ()) 
            {
              //get_world ()->add (new DefaultShoot (pos, (DefaultShoot::DirectionState) direction));
              get_world ()->add (new LaserShoot (pos, direction, 5));
              gun_state = GUN_RELOADING;
              reload_time = 0;
            }
	  break;
	case GUN_RELOADING:
	  if (reload_time > 1)
	    gun_state = GUN_READY;
	  reload_time += 20 * delta;
	  break;
	}
    }
}

void 
Player::update_ground (float delta)
{
  velocity = CL_Vector();

  if (controller->jump_pressed ()) 
    {
      velocity.y = -750;
      ground_state = IN_AIR;
    } 
  else
    {
      float tmp_x_pos = pos.x;

      if (controller->is_left ())
        {
          pos.x -= 200 * delta;
          state = WALKING;
        }
      else if (controller->is_right ())
        {
          pos.x += 200 * delta;
          state = WALKING;
        }
      else
        {
          state = STANDING;
        }

      if (stuck ()) 
        {
          // FIXME: Calculate nearest position to colliding object here
          pos.x = tmp_x_pos;
        }
    }
}

void 
Player::update_air (float delta)
{
  if (!controller->jump_pressed () && velocity.y < 0) {
    velocity.y = velocity.y/2;
    //ground_state = IN_AIR;
  }

  float tmp_x_pos = pos.x;
  if (controller->is_left ())
    pos.x -= 200 * delta;
  else if (controller->is_right ())
    pos.x += 200 * delta;
  if (stuck ())
    pos.x = tmp_x_pos;

  float tmp_y_pos = pos.y;
  pos += velocity * delta;
  velocity.y += 1500 * delta;

  if (on_ground () && velocity.y > 0) 
    {
      ground_state = ON_GROUND;
      // Cut the position to the tile size 
      pos.y = int(pos.y / SUBTILE_SIZE) * SUBTILE_SIZE + SUBTILE_SIZE - 1;
    } 
}
  
void 
Player::set_position (const CL_Vector& arg_pos)
{
  pos = arg_pos;
}

void 
Player::set_direction (Direction dir)
{
  direction = dir;
}

bool
Player::on_ground ()
{
  return get_world ()->get_tilemap()->is_ground(pos.x, pos.y+16);
}

bool 
Player::stuck ()
{
  return get_world ()->get_tilemap()->is_ground(pos.x, pos.y);
}

void
Player::activate_shild ()
{
  shild_time = 25;
}

/* EOF */
