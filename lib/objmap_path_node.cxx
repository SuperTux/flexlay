//  $Id$
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

#include <ClanLib/Display/display.h>
#include "objmap_object_impl.hxx"
#include "objmap_path_node.hxx"

class ObjMapPathNodeImpl : public ObjMapObjectImpl
{
public:
  ObjMapPathNodeImpl* prev;
  ObjMapPathNodeImpl* next;
  
  ObjMapPathNodeImpl();

  void draw(CL_GraphicContext* gc);
  CL_Rectf get_bound_rect() const;
};

ObjMapPathNodeImpl::ObjMapPathNodeImpl()
{
  next = 0;
  prev = 0;
}

void
ObjMapPathNodeImpl::draw(CL_GraphicContext* gc)
{
  gc->fill_rect(CL_Rect(CL_Point(pos) - CL_Point(16,16), CL_Size(32, 32)), 
                CL_Color(200, 255, 200));
  if (next)
    {
      gc->draw_line(static_cast<int>(pos.x), static_cast<int>(pos.y),
                    static_cast<int>((pos.x + next->pos.x)/2),
                    static_cast<int>((pos.y+next->pos.y)/2),
                    CL_Color(255, 255, 0));

      gc->draw_line(static_cast<int>((pos.x + next->pos.x)/2), 
                    static_cast<int>((pos.y+next->pos.y)/2),
                    static_cast<int>(next->pos.x),
                    static_cast<int>(next->pos.y), 
                    CL_Color(255, 0, 0));
    }
}

CL_Rectf
ObjMapPathNodeImpl::get_bound_rect() const
{
  return CL_Rectf(pos - CL_Pointf(16,16), CL_Sizef(32, 32));
}

ObjMapPathNode::ObjMapPathNode(const CL_Pointf& pos_, 
                               const MetaData& data_)
  : impl(new ObjMapPathNodeImpl())
{  
  impl->pos  = pos_;
  impl->data = data_;
}

void
ObjMapPathNode::disconnect()
{
  impl->next = 0;
  impl->prev = 0;

  impl->next->prev = 0;
  impl->prev->next = 0;
}

void
ObjMapPathNode::connect(ObjMapPathNode next)
{
  if (next.impl->next != impl.get()) // avoid circular link between two nodes
    {
      if (next.impl->prev) // ensure that each node links exactly to one prev and one next node 
        {
          next.impl->prev->next = 0;
          next.impl->prev = 0;
        }

      impl->next = next.impl.get();
      next.impl->prev = impl.get();
    }
}

ObjMapObject
ObjMapPathNode::to_object()
{
  return ObjMapObject(impl);
}

/* EOF */
