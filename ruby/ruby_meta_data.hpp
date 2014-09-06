//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_RUBY_META_DATA_HXX
#define HEADER_RUBY_META_DATA_HXX

#include <ClanLib/signals.h>
#include <ClanLib/Display/color.h>
#include <ClanLib/Signals/signal_v0.h>
#include "ruby.h"
#include "../lib/meta_data.hpp"
#include "../lib/objmap_object.hpp"
#include "../lib/object_brush.hpp"

MetaData  make_metadata(VALUE obj);
VALUE get_ruby_object(const MetaData& data);

void connect(boost::signals2::signal<void ()>& sig, VALUE obj);
void connect_cl(CL_Signal_v0& sig, VALUE obj);
void connect_v1_cl(CL_Signal_v1<int>& sig, VALUE obj);
void connect_v1(boost::signals2::signal<void (int)>& sig, VALUE obj);
void connect_v1_float(boost::signals2::signal<void (float)>& sig, VALUE obj);
void connect_v2(boost::signals2::signal<void (int, int)>& sig, VALUE obj);
void connect_v2_graceful(boost::signals2::signal<void (int, int)>& sig, VALUE obj);

void connect_v1_Color(boost::signals2::signal<void (CL_Color)>& sig, VALUE func);
void connect_v1_ObjMapObject(boost::signals2::signal<void (ObjMapObject)>& sig, VALUE func);
void connect_v2_ObjectBrush_Point(boost::signals2::signal<void (ObjectBrush, Point)>& sig, VALUE func);

#endif

/* EOF */
