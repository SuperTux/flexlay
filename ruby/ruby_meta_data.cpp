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
#include <vector>
#include <ClanLib/Signals/signal_v0.h>
#include <ClanLib/Signals/slot.h>

#include "ruby_object.hpp"
#include "meta_data_impl.hpp"
#include "ruby_functor.hpp"

typedef MetaDataGeneric<RubyObject> RubyMetaData;

/** Retain CL_Slot instances for the process lifetime.
 *  Older code did `new CL_Slot(...)` and leaked the pointer; the Slot
 *  must stay alive or the connection is destroyed. */
static std::vector<CL_Slot>&
ruby_slot_store()
{
  static std::vector<CL_Slot> slots;
  return slots;
}

static void
keep_slot(const CL_Slot& slot)
{
  ruby_slot_store().push_back(slot);
}

MetaData
make_metadata(VALUE obj)
{
  return MetaData(std::shared_ptr<MetaDataImpl>(new RubyMetaData(RubyObject(obj))));
}

VALUE
get_ruby_object(const MetaData& data_obj)
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

/* Most connect* helpers replace the signal so a widget has a single
 * Ruby listener (historical behaviour).  Use connect_v2_graceful when
 * additional listeners must be preserved. */

void
connect(CL_Signal_v0& sig, VALUE obj)
{
  sig = CL_Signal_v0();
  keep_slot(sig.connect_functor(RubyFunctor(obj)));
}

void
connect_v1_float(CL_Signal_v1<float>& sig, VALUE obj)
{
  sig = CL_Signal_v1<float>();
  keep_slot(sig.connect_functor(RubyFunctor(obj)));
}

void
connect_v1(CL_Signal_v1<int>& sig, VALUE obj)
{
  sig = CL_Signal_v1<int>();
  keep_slot(sig.connect_functor(RubyFunctor(obj)));
}

void
connect_v2(CL_Signal_v2<int, int>& sig, VALUE obj)
{
  sig = CL_Signal_v2<int, int>();
  keep_slot(sig.connect_functor(RubyFunctor(obj)));
}

void
connect_v2_graceful(CL_Signal_v2<int, int>& sig, VALUE obj)
{
  keep_slot(sig.connect_functor(RubyFunctor(obj)));
}

void
connect_v1_Color(CL_Signal_v1<CL_Color>& sig, VALUE func)
{
  sig = CL_Signal_v1<CL_Color>();
  keep_slot(sig.connect_functor(RubyFunctor(func)));
}

void
connect_v1_ObjMapObject(CL_Signal_v1<ObjMapObject>& sig, VALUE func)
{
  sig = CL_Signal_v1<ObjMapObject>();
  keep_slot(sig.connect_functor(RubyFunctor(func)));
}

void
connect_v2_ObjectBrush_Point(CL_Signal_v2<ObjectBrush, CL_Point>& sig, VALUE func)
{
  sig = CL_Signal_v2<ObjectBrush, CL_Point>();
  keep_slot(sig.connect_functor(RubyFunctor(func)));
}

/* EOF */
