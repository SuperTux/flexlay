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

#include "ruby_object.hxx"
#include "meta_data_impl.hxx"
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

/* EOF */
