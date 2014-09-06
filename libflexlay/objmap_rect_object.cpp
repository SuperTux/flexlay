// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "objmap_rect_object.hpp"

#include <vector>
#include <functional>
#include <boost/signals2.hpp>

#include "color.hpp"
#include "flexlay.hpp"
#include "graphic_context.hpp"
#include "math/size.hpp"
#include "object_layer.hpp"

class ObjMapRectObjectImpl : public ObjMapObjectImpl
{
public:
  std::vector<boost::signals2::connection> m_slots;
  Sizef size;
  Color color;

  ObjMapControlPoint cp_top_left;
  ObjMapControlPoint cp_top_right;
  ObjMapControlPoint cp_bottom_left;
  ObjMapControlPoint cp_bottom_right;
  ObjMapControlPoint cp_top_middle;
  ObjMapControlPoint cp_bottom_middle;
  ObjMapControlPoint cp_middle_left;
  ObjMapControlPoint cp_middle_right;

  void set_rect(Rect rect) {
    pos  = Pointf(rect.left, rect.top);
    size = Sizef(rect.get_width(), rect.get_height());
  }

  void cp_top_left_move(Pointf pos_) {
    size.width  += pos.x - pos_.x;
    size.height += pos.y - pos_.y;
    pos = pos_;

    normalize_rect();
    update_control_points();
  }

  void cp_top_right_move(Pointf pos_) {
    size.width  += pos_.x - (pos.x + size.width);
    size.height += pos.y - pos_.y;

    pos.y = pos_.y;

    normalize_rect();
    update_control_points();
  }

  void cp_bottom_left_move(Pointf pos_) {
    size.width  += pos.x - pos_.x;
    size.height += pos_.y - (pos.y + size.height);
    pos.x = pos_.x;

    normalize_rect();
    update_control_points();
  }
  void cp_bottom_right_move(Pointf pos_) {
    size.width  += pos_.x - (pos.x + size.width);
    size.height += pos_.y - (pos.y + size.height);

    normalize_rect();
    update_control_points();
  }

  void cp_top_middle_move(Pointf pos_) {
    size.height += pos.y - pos_.y;
    pos.y = pos_.y;

    normalize_rect();
    update_control_points();
  }
  void cp_bottom_middle_move(Pointf pos_) {
    size.height += pos_.y - (pos.y + size.height);

    normalize_rect();
    update_control_points();
  }
  void cp_middle_left_move(Pointf pos_) {
    size.width  += pos.x - pos_.x;
    pos.x = pos_.x;

    normalize_rect();
    update_control_points();
  }
  void cp_middle_right_move(Pointf pos_) {
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

  void draw(GraphicContext& gc);
  Rectf get_bound_rect() const;
  void add_control_points();
  void update_control_points();
};

Rectf
ObjMapRectObject::get_rect() const
{
  return impl->get_bound_rect();
}

void
ObjMapRectObject::set_color(const Color& color)
{
  impl->color = color;
}

void
ObjMapRectObject::set_rect(const Rect& rect)
{
  impl->pos  = Pointf(rect.left, rect.top);
  impl->size = Sizef(rect.get_width(), rect.get_height());
}

void
ObjMapRectObjectImpl::update_control_points()
{
  cp_top_left.set_pos_raw(pos);
  cp_top_right.set_pos_raw(pos + Pointf(size.width, 0));
  cp_bottom_left.set_pos_raw(pos + Pointf(0, size.height));
  cp_bottom_right.set_pos_raw(pos + Pointf(size.width, size.height));
  cp_top_middle.set_pos_raw(pos + Pointf(size.width/2, 0));
  cp_bottom_middle.set_pos_raw(pos + Pointf(size.width/2, size.height));
  cp_middle_left.set_pos_raw(pos + Pointf(0, size.height/2));
  cp_middle_right.set_pos_raw(pos + Pointf(size.width, size.height/2));
}

ObjMapRectObject::ObjMapRectObject(const Rect&  rect_,
                                   const Color& color_,
                                   const MetaData& data_)
  : impl(new ObjMapRectObjectImpl)
{
  impl->pos   = Pointf(rect_.left, rect_.top);
  impl->size  = Sizef(rect_.get_width(), rect_.get_height());
  impl->color = color_;
  impl->data  = data_;

  impl->cp_top_left = ObjMapControlPoint(Sprite("resize1"),
                                         Pointf(),
                                         MetaData());

  impl->cp_bottom_right = ObjMapControlPoint(Sprite("resize1"),
                                             Pointf(),
                                             MetaData());

  impl->cp_top_right = ObjMapControlPoint(Sprite("resize2"),
                                          Pointf(),
                                          MetaData());

  impl->cp_bottom_left = ObjMapControlPoint(Sprite("resize2"),
                                            Pointf(),
                                            MetaData());

  impl->cp_middle_left = ObjMapControlPoint(Sprite("resize_horz"),
                                            Pointf(),
                                            MetaData());
  impl->cp_middle_right  = ObjMapControlPoint(Sprite("resize_horz"),
                                              Pointf(),
                                              MetaData());
  impl->cp_top_middle = ObjMapControlPoint(Sprite("resize_vert"),
                                           Pointf(),
                                           MetaData());

  impl->cp_bottom_middle = ObjMapControlPoint(Sprite("resize_vert"),
                                              Pointf(),
                                              MetaData());

#ifdef GRUMBEL
  impl->slots.push_back(impl->cp_top_right.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_top_right_move, impl.get(), std::placeholders::_1)));
  impl->slots.push_back(impl->cp_bottom_right.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_bottom_right_move, impl.get(), std::placeholders::_1)));

  impl->slots.push_back(impl->cp_top_left.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_top_left_move, impl.get(), std::placeholders::_1)));
  impl->slots.push_back(impl->cp_bottom_left.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_bottom_left_move, impl.get(), std::placeholders::_1)));

  impl->slots.push_back(impl->cp_middle_left.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_middle_left_move, impl.get(), std::placeholders::_1)));
  impl->slots.push_back(impl->cp_middle_right.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_middle_right_move, impl.get(), std::placeholders::_1)));

  impl->slots.push_back(impl->cp_top_middle.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_top_middle_move, impl.get(), std::placeholders::_1)));
  impl->slots.push_back(impl->cp_bottom_middle.sig_set_pos().connect(std::bind(&ObjMapRectObjectImpl::cp_bottom_middle_move, impl.get(), std::placeholders::_1)));
#endif
}

void
ObjMapRectObjectImpl::draw(GraphicContext& gc)
{
  gc.fill_rect(Rect(get_bound_rect()), color);
}

Rectf
ObjMapRectObjectImpl::get_bound_rect() const
{
  return Rectf(pos, size);
}

ObjMapObject
ObjMapRectObject::to_object()
{
  return ObjMapObject(std::shared_ptr<ObjMapObjectImpl>(impl));
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
