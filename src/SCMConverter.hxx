//  $Id: SCMConverter.hxx,v 1.2 2002/09/01 00:05:33 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
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

#ifndef SCMCONVERTER_HXX
#define SCMCONVERTER_HXX

#include <assert.h>
#include <iostream>
#include <typeinfo>
#include <guile/gh.h>

template<class T> struct SmobInfo {};

/* Type checking doesn't work correctly when downcasting from
   inherited types, so use smob_cast for that */
template<class T>
T* checked_smob_cast(long tag, SCM smob)
{
  assert (!gh_boolean_p(smob));
  if (SCM_NIMP (smob))
    {
      if (reinterpret_cast<long>(SCM_CAR (smob)) == tag)
	{
	  T* obj = reinterpret_cast<T*>(SCM_CDR (smob));
	  return obj;
	}
      else
	{
	  std::cout << "Error: checked_smob_cast: expected (" << typeid (T).name() << ") "
		    << tag
		    << " got " << SCM_CAR (smob) << std::endl;
	  assert (!"checked_smob_cast: Wrong cast");
	  return 0;
	}
    }
  else
    {
      std::cout << "Error: SCMConverter: cast error, not a smob" << std::endl;
      gh_display (smob); gh_newline ();
      return 0;
    }
}


template<class T>
T* checked_smob_cast(SCM smob)
{
  assert (!gh_boolean_p(smob));
  if (SCM_NIMP (smob))
    {
      if (reinterpret_cast<long>(SCM_CAR (smob)) == SmobInfo<T>::get_smob_tag ())
	{
	  T* obj = reinterpret_cast<T*>(SCM_CDR (smob));
	  return obj;
	}
      else
	{
	  std::cout << "Error: checked_smob_cast: expected (" << typeid (T).name() << ") "
		    << SmobInfo<T>::get_smob_tag ()
		    << " got " << SCM_CAR (smob) << std::endl;
	  assert (!"checked_smob_cast: Wrong cast");
	  return 0;
	}
    }
  else
    {
      std::cout << "Error: SCMConverter: cast error, not a smob" << std::endl;
      gh_display (smob); gh_newline ();
      return 0;
    }
}

template<class T>
SCM create_smob (T* const data)
{
  SCM_RETURN_NEWSMOB(SmobInfo<T>::get_smob_tag (), data);
}

#endif

/* EOF */
