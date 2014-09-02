//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "ruby_meta_data.hpp"

#include <iostream>

#include "ruby_object.hpp"
#include "meta_data_impl.hpp"
#include "ruby_functor.hpp"

typedef MetaDataGeneric<RubyObject> RubyMetaData;

MetaData  make_metadata(VALUE obj)
{
  return MetaData(std::shared_ptr<MetaDataImpl>(new RubyMetaData(RubyObject(obj))));
}

VALUE get_ruby_object(const MetaData& data_obj)
{
  MetaDataImpl* data = data_obj.get_impl().get();

  if (data)
    {
      RubyMetaData* rbdata = dynamic_cast<RubyMetaData*>(data);
      if (rbdata)
        {
          return rbdata->data.ptr();
        }
    }
  return Qnil;
}

void connect(boost::signals2::signal<void ()>& sig, VALUE obj)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(obj));
}

void connect_cl(CL_Signal_v0& sig, VALUE obj)
{
  sig = CL_Signal_v0();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
  sig.connect_functor(RubyFunctor(obj));
}

void connect_v1_cl(CL_Signal_v1<int>& sig, VALUE obj)
{
  sig = CL_Signal_v1<int>();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
  sig.connect_functor(RubyFunctor(obj));
}

void connect_v1_float(boost::signals2::signal<void (float)>& sig, VALUE obj)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(obj));
}

void connect_v1(boost::signals2::signal<void (int)>& sig, VALUE obj)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(obj));
}

void connect_v2(boost::signals2::signal<void (int, int)>& sig, VALUE obj)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(obj));
}

void connect_v2_graceful(boost::signals2::signal<void (int, int)>& sig, VALUE obj)
{
  sig.connect(RubyFunctor(obj));
}

void connect_v1_Color(boost::signals2::signal<void (CL_Color)>& sig, VALUE func)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(func));
}

void connect_v1_ObjMapObject(boost::signals2::signal<void (ObjMapObject)>& sig, VALUE func)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(func));
}

void connect_v2_ObjectBrush_Point(boost::signals2::signal<void (ObjectBrush, CL_Point)>& sig, VALUE func)
{
  sig.disconnect_all_slots();
  sig.connect(RubyFunctor(func));
}

/* EOF */
