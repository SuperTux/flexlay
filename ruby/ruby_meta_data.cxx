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

#include <ClanLib/Signals/signal_v0.h>
#include "ruby_object.hxx"
#include "meta_data_impl.hxx"
#include "ruby_functor.hxx"
#include "ruby_meta_data.hxx"

typedef MetaDataGeneric<RubyObject> RubyMetaData;

MetaData  make_metadata(VALUE obj)
{
  return MetaData(SharedPtr<MetaDataImpl>(new RubyMetaData(RubyObject(obj))));
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

void connect(CL_Signal_v0& sig, VALUE obj)
{
  sig = CL_Signal_v0();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
}

void connect_v1_float(CL_Signal_v1<float>& sig, VALUE obj)
{
  sig = CL_Signal_v1<float>();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
}

void connect_v1(CL_Signal_v1<int>& sig, VALUE obj)
{
  sig = CL_Signal_v1<int>();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
}

void connect_v2(CL_Signal_v2<int, int>& sig, VALUE obj)
{
  sig = CL_Signal_v2<int, int>();
  new CL_Slot(sig.connect_functor(RubyFunctor(obj)));
}

void connect_v1_Color(CL_Signal_v1<CL_Color>& sig, VALUE func)
{
  sig = CL_Signal_v1<CL_Color>();
  new CL_Slot(sig.connect_functor(RubyFunctor(func)));
}

void connect_v1_ObjMapObject(CL_Signal_v1<ObjMapObject>& sig, VALUE func)
{
  sig = CL_Signal_v1<ObjMapObject>();
  new CL_Slot(sig.connect_functor(RubyFunctor(func)));
}

void connect_v2_ObjectBrush_Point(CL_Signal_v2<ObjectBrush, CL_Point>& sig, VALUE func)
{
  sig = CL_Signal_v2<ObjectBrush, CL_Point>();
  new CL_Slot(sig.connect_functor(RubyFunctor(func)));
}

/* EOF */
