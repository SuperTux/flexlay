//  $Id: editor.cxx,v 1.7 2003/10/11 08:11:59 grumbel Exp $
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

#include <ClanLib/Display/sprite_description.h>
#include <ClanLib/Display/Providers/provider_factory.h>
#include <ClanLib/Signals/signal_v0.h>
#include "../python_functor.hxx"
#include "editor.hxx"

void connect(CL_Signal_v0& sig, PyObject* obj)
{
  //std::cout << "Connecting functor: " << std::endl;
  new CL_Slot(sig.connect_functor(PythonFunctor(obj)));
}

CL_Sprite
make_sprite(const std::string& filename)
{
  CL_SpriteDescription desc;
  desc.add_frame(CL_ProviderFactory::load(filename), true);
  return CL_Sprite(desc);
}

/* EOF */
