//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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

#include <iostream>
#include <ClanLib/Core/core_iostream.h>
#include <ClanLib/Display/display.h>
#include <ClanLib/Display/sprite.h>
#include "object_layer.hxx"
#include "objmap_control_point.hxx"
#include "objmap_rect_object.hxx"
#include "flexlay.hxx"

class ObjMapRectObjectImpl : public ObjMapObjectImpl
{
public:
  std::vector<CL_Slot> slots;
  CL_Sizef size;
  CL_Color color;

  ObjMapControlPoint cp_top_left;
  ObjMapControlPoint cp_top_right;
  ObjMapControlPoint cp_bottom_left;
  ObjMapControlPoint cp_bottom_right;
  ObjMapControlPoint cp_top_middle;
  ObjMapControlPoint cp_bottom_middle;  
  ObjMapControlPoint cp_middle_left;
  ObjMapControlPoint cp_middle_right;  

  void set_rect(CL_Rect rect) {
    pos  = CL_Pointf(rect.left, rect.top);
    size = CL_Sizef(rect.get_width(), rect.get_height());
  }

  void cp_top_left_move(CL_Pointf pos_) {
    size.width  += pos.x - pos_.x;
    size.height += pos.y - pos_.y;
    pos = pos_;

    normalize_rect();
    update_control_points();
  }

  void cp_top_right_move(CL_Pointf pos_) {
    size.width  += pos_.x - (pos.x + size.width);
    size.height += pos.y - pos_.y;
    
    pos.y = pos_.y;

    normalize_rect();
    update_control_points();
  }

  void cp_bottom_left_move(CL_Pointf pos_) {
    size.width  += pos.x - pos_.x;
    size.height += pos_.y - (pos.y + size.height);
    pos.x = pos_.x;

    normalize_rect();
    update_control_points();
  }
  void cp_bottom_right_move(CL_Pointf pos_) {
    size.width  += pos_.x - (pos.x + size.width);
    size.height += pos_.y - (pos.y + size.height);

    normalize_rect();
    update_control_points();
  }

  void cp_top_middle_move(CL_Pointf pos_) {
    size.height += pos.y - pos_.y;  
    pos.y = pos_.y;

    normalize_rect();
    update_control_points();
  }
  void cp_bottom_middle_move(CL_Pointf pos_) {
    size.height += pos_.y - (pos.y + size.height);

    normalize_rect();
    update_control_points();
  }
  void cp_middle_left_move(CL_Pointf pos_) {
    size.width  += pos.x - pos_.x;
    pos.x = pos_.x;

    normalize_rect();
    update_control_points();
  }
  void cp_middle_right_move(CL_Pointf pos_) {
    size.width  += pos_.x - (pos.x + size.width);

    normalize_rect();
    update_control_points();
  }

  void normalize_rect() {
    if (size.width < 0) {
      pos.x     += size.width;
      size.width = -size.width;
    }

    if (size.height < 0) {
      pos.y      += size.height;
      size.height = -size.height;
    }
  }

  void draw(CL_GraphicContext* gc);
  CL_Rectf get_bound_rect() const;
  void add_control_points();
  void update_control_points();
};

CL_Rectf
ObjMapRectObject::get_rect() const
{
  return impl->get_bound_rect();
}

void
ObjMapRectObject::set_color(const CL_Color& color)
{
  impl->color = color;
}

void
ObjMapRectObject::set_rect(const CL_Rect& rect)
{
  impl->pos  = CL_Pointf(rect.left, rect.top);
  impl->size = CL_Sizef(rect.get_width(), rect.get_height());
}

void
ObjMapRectObjectImpl::update_control_points()
{
  cp_top_left.set_pos_raw(pos);
  cp_top_right.set_pos_raw(pos + CL_Pointf(size.width, 0));
  cp_bottom_left.set_pos_raw(pos + CL_Pointf(0, size.height));
  cp_bottom_right.set_pos_raw(pos + CL_Pointf(size.width, size.height));
  cp_top_middle.set_pos_raw(pos + CL_Pointf(size.width/2, 0));
  cp_bottom_middle.set_pos_raw(pos + CL_Pointf(size.width/2, size.height));
  cp_middle_left.set_pos_raw(pos + CL_Pointf(0, size.height/2));
  cp_middle_right.set_pos_raw(pos + CL_Pointf(size.width, size.height/2));
}

ObjMapRectObject::ObjMapRectObject(const CL_Rect&  rect_,
                                   const CL_Color& color_,
                                   const MetaData& data_)  
  : impl(new ObjMapRectObjectImpl)
{
  impl->pos   = CL_Pointf(rect_.left, rect_.top);
  impl->size  = CL_Sizef(rect_.get_width(), rect_.get_height());
  impl->color = color_;
  impl->data  = data_;

  impl->cp_top_left = ObjMapControlPoint(CL_Sprite("resize1", &(Flexlay::current()->resources)), 
                                         CL_Pointf(),
                                         MetaData());
  
  impl->cp_bottom_right = ObjMapControlPoint(CL_Sprite("resize1", &(Flexlay::current()->resources)), 
                                             CL_Pointf(),
                                             MetaData());
 
  impl->cp_top_right = ObjMapControlPoint(CL_Sprite("resize2", &(Flexlay::current()->resources)), 
                                             CL_Pointf(),
                                          MetaData());
  
  impl->cp_bottom_left = ObjMapControlPoint(CL_Sprite("resize2", &(Flexlay::current()->resources)), 
                                                   CL_Pointf(),
                                                   MetaData());
  
  impl->cp_middle_left = ObjMapControlPoint(CL_Sprite("resize_horz", &(Flexlay::current()->resources)), 
                                            CL_Pointf(),
                                            MetaData());
  impl->cp_middle_right  = ObjMapControlPoint(CL_Sprite("resize_horz", &(Flexlay::current()->resources)), 
                                              CL_Pointf(),
                                              MetaData());
  impl->cp_top_middle = ObjMapControlPoint(CL_Sprite("resize_vert", &(Flexlay::current()->resources)), 
                                           CL_Pointf(),
                                           MetaData());
  
  impl->cp_bottom_middle = ObjMapControlPoint(CL_Sprite("resize_vert", &(Flexlay::current()->resources)), 
                                              CL_Pointf(),
                                              MetaData());


  impl->slots.push_back(impl->cp_top_right.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_top_right_move));
  impl->slots.push_back(impl->cp_bottom_right.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_bottom_right_move));

  impl->slots.push_back(impl->cp_top_left.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_top_left_move));
  impl->slots.push_back(impl->cp_bottom_left.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_bottom_left_move));

  impl->slots.push_back(impl->cp_middle_left.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_middle_left_move));
  impl->slots.push_back(impl->cp_middle_right.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_middle_right_move));

  impl->slots.push_back(impl->cp_top_middle.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_top_middle_move));
  impl->slots.push_back(impl->cp_bottom_middle.sig_set_pos().connect(impl.get(), &ObjMapRectObjectImpl::cp_bottom_middle_move));
}

void
ObjMapRectObjectImpl::draw(CL_GraphicContext* gc)
{
  gc->fill_rect(get_bound_rect(), color);
}

CL_Rectf
ObjMapRectObjectImpl::get_bound_rect() const
{
  return CL_Rectf(pos, size);
}

ObjMapObject
ObjMapRectObject::to_object()
{
  return ObjMapObject(SharedPtr<ObjMapObjectImpl>(impl));
}

void
ObjMapRectObjectImpl::add_control_points()
{
  update_control_points();
  //std::cout << "Adding control poinst..." << std::endl;
  ObjectLayer objmap = ObjectLayer::current();

  objmap.add_control_point(cp_top_left);
  objmap.add_control_point(cp_top_right);
  objmap.add_control_point(cp_bottom_left);
  objmap.add_control_point(cp_bottom_right);
  objmap.add_control_point(cp_top_middle);
  objmap.add_control_point(cp_bottom_middle);
  objmap.add_control_point(cp_middle_left);
  objmap.add_control_point(cp_middle_right);
}

/* EOF */
