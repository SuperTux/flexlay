//  $Id: trigger.hxx,v 1.2 2003/09/21 17:34:00 grumbel Exp $
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

#ifndef HEADER_TRIGGER_HXX
#define HEADER_TRIGGER_HXX

#include <vector>
#include <ClanLib/Core/Math/rect.h>
#include "gameobj.hxx"
#include "scm_functor.hxx"

class TriggerCondition
{
private:
public:
  TriggerCondition() {}
  virtual ~TriggerCondition() {}
  virtual bool check() =0;
  virtual void update(float delta) {}
};

class RegionTriggerCondition : public TriggerCondition
{
private:
  CL_Rectf rect;
public:
  RegionTriggerCondition(CL_Rectf rect);
  bool check(); 
};

/** */
class Trigger : public GameObj
{
private:
  TriggerCondition* condition;
  SCMFunctor func;
  bool triggered;
  
  static Trigger* current_;
public:
  static Trigger* get_current() { return current_; }

  Trigger(TriggerCondition*, SCMFunctor func);
  virtual ~Trigger();

  void draw ();
  void update (float delta);
private:
  Trigger (const Trigger&);
  Trigger& operator= (const Trigger&);
};

#endif

/* EOF */
