//  $Id: guile_gameobj.hxx,v 1.2 2003/08/12 08:24:41 grumbel Exp $
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

#ifndef GUILEGAMEOBJ_HXX
#define GUILEGAMEOBJ_HXX

#include <guile/gh.h>
#include "gameobj.hxx"

class GuileGameObj : public GameObj
{
private:
  SCM smob;
  SCM obj;
  SCM scm_update;
  SCM scm_draw;
public:
  GuileGameObj (SCM arg_obj, SCM arg_update, SCM arg_draw);
  virtual ~GuileGameObj ();
  
  void draw ();
  void update (float);
  SCM get_data () { return obj; }
  SCM set_data (SCM arg_obj);
};

#endif

/* EOF */
