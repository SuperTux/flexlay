//  $Id: player.cxx,v 1.18 2003/11/05 12:41:37 grumbel Exp $
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

#include "tile_map.hxx"
#include "display.hxx"
#include "game_world.hxx"
#include "default_shoot.hxx"
#include "laser_shoot.hxx"
#include "controller.hxx"
#include "player.hxx"
#include "bomb.hxx"
#include "globals.hxx"

#define MAX_ENERGIE 16

Player* Player::current_ = 0;

Player::Player (Controller* c) :
  controller (c),
  pos (320, 500),
  velocity (0, 0),
  
  walk     ("hero/run",   resources),
  sit      ("hero/sit",   resources),
  jump     ("hero/jump",  resources),
  stand    ("hero/stand", resources),
  killed   ("hero/kill", resources),

  state (WALKING),
  gun_state (GUN_READY),
  ground_state (IN_AIR)
{
  jumping = false;
  energie = 1; //MAX_ENERGIE;
  current_ = this;

  walk.set_alignment(origin_bottom_center, 0, 3);
  jump.set_alignment(origin_bottom_center, 0, 3);
  stand.set_alignment(origin_bottom_center, 0, 3);

  direction = WEST;
  hit_count = 0.0f;
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
        case KILLED:
          sprite = &killed;
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

      if (hit_count > 0)
        {
          if (rand()%2)
            sprite->set_alpha(1.0f);
          else
            sprite->set_alpha(1.0f - hit_count);
        }
      else
        sprite->set_alpha(1.0f);

      sprite->draw (int(pos.x), int(pos.y));
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
  if (state == KILLED)
    {
      killed.update(delta);
    }
  else
    {
      if (hit_count > 0)
        hit_count -= delta;

      walk.update(delta);
  
      if (controller->get_state(InputEvent::LEFT))
        direction = WEST;
      else if  (controller->get_state(InputEvent::RIGHT))
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
              subtile_pos = new_subtile_pos;
            }
        }
    }
}

void 
Player::update_shooting (float delta)
{
  switch (gun_state)
    {
    case GUN_READY:
      if (controller->get_state(InputEvent::FIRE))
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

void 
Player::update_ground (float delta)
{
  velocity = CL_Vector();

  if (controller->get_state(InputEvent::JUMP) && !jumping)
    {
      jumping = true;
      velocity.y = -750;
      ground_state = IN_AIR;
    } 
  else
    {
      if (!controller->get_state(InputEvent::JUMP))
        jumping = false;

      float tmp_x_pos = pos.x;

      if (controller->get_state(InputEvent::DOWN))
        {
          state = SITTING;
          if (controller->get_state(InputEvent::FIRE) && !bomb_placed)
            {
              GameWorld::current()->add(new Bomb(int(pos.x), int(pos.y)));
              bomb_placed = true;
            }
        }
      else
        {
          bomb_placed = false;
          if (controller->get_state(InputEvent::LEFT))
            {
              pos.x -= 300 * delta;
              state = WALKING;
            }
          else if (controller->get_state(InputEvent::RIGHT))
            {
              pos.x += 300 * delta;
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
}

void 
Player::update_air (float delta)
{
  if (!controller->get_state(InputEvent::JUMP) && velocity.y < 0) 
    {
      velocity.y = velocity.y/2;
      //ground_state = IN_AIR;
    }

  float tmp_x_pos = pos.x;
  if (controller->get_state(InputEvent::LEFT))
    pos.x -= 300 * delta;
  else if (controller->get_state(InputEvent::RIGHT))
    pos.x += 300 * delta;
  if (stuck ())
    pos.x = tmp_x_pos;

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

int
Player::get_energie()
{
  return energie;
}

int
Player::get_max_energie()
{
  return MAX_ENERGIE;
}

void
Player::hit(int points)
{
  if (energie > 0 && hit_count <= 0)
    {
      energie -= points;
      hit_count = 1.0f;
    }

  if (energie <= 0)
    {
      state = KILLED;
      hit_count = 0;
    }
}

/* EOF */
