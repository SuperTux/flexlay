//  $Id: trigger.cxx,v 1.2 2003/09/20 21:53:38 grumbel Exp $
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

#include <list>
#include "game_world.hxx"
#include "trigger.hxx"
#include "player.hxx"

RegionTriggerCondition::RegionTriggerCondition(CL_Rectf rect)
  : rect(rect)
{
}

bool
RegionTriggerCondition::check()
{
  GameWorld* world = GameWorld::current();

  Player* player = *(world->get_players()->begin());

  return rect.is_inside(CL_Pointf(player->get_pos().x,
                                  player->get_pos().y));
}

Trigger::Trigger(TriggerCondition* condition, SCMFunctor func)
  : condition(condition),
    func(func),
    triggered(false)
{
}

Trigger::~Trigger()
{
  delete condition;
}

void
Trigger::draw ()
{
  
}

void
Trigger::update (float delta)
{
  condition->update(delta);

  if (!triggered && condition->check())
    {
      triggered = true;
      func();
    }
  else if (!condition->check())
    {
      triggered = false;
    }
}

/* EOF */
