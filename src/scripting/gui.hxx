//  $Id: gui.hxx,v 1.3 2003/10/12 11:58:09 grumbel Exp $
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

#ifndef HEADER_SCRIPTING_GUI_HXX
#define HEADER_SCRIPTING_GUI_HXX

#ifdef SWIGGUILE
#include <guile/gh.h>
#endif

class CL_MenuNode;
class CL_Component;

/** Add comp to the component stack and thus make comp the currently
    active component, all newly created components get added to this
    one. (add a window, push it to add buttons to it) */
CL_Component* gui_push_component(CL_Component* comp);

/** Remove the last component and make the former component the active
    one again. (switch from a window back to the gui manager to add
    another window) */
void gui_pop_component();

/** Create a window at the given position \a x, \a y and with the
    given dimensions \w and \h, all units in pixel */
CL_Component* gui_create_window(int x, int y, 
                                int w, int h, 
                                const char* title);

/** Create a button without attaching a callback directly to it,
    attaching a callback can later be done with a signal function. */
CL_Component* gui_create_button(int x, int y, int w, int h, const char* text);

/** Create a simple text label at the position x y with text \a text */
CL_Component* gui_create_label(int x, int y, const char* text);

/** Create an inputbox for text, text is used as default value,
    entered text can be accessed via an accessor function */
CL_Component* gui_create_inputbox(int x, int y, int w, int h, const char* text);

/** Create a menu */
CL_Component* gui_create_menu();

/** Remove the component \a comp from its parent 
    FIXME: Who deallocates?! */
void gui_remove_component(CL_Component* comp);

int gui_component_get_width(CL_Component* comp);
int gui_component_get_height(CL_Component* comp);

/** Return the client area of a window, i.e. the area to which buttons
    get added */
CL_Component* gui_window_get_client_area(CL_Component* comp);

void gui_window_close(CL_Component* comp);

/** Show a formally hidden component */
void gui_show_component(CL_Component* comp);

/** Hide a component temporary, without removing it */
void gui_hide_component(CL_Component* comp);

void gui_component_set_position(CL_Component* comp, int x, int y);
void gui_component_set_rect(CL_Component* comp, int x, int y, int w, int h);

bool gui_component_is_visible(CL_Component* comp);

/** Return the text that is currently in an input box */
std::string gui_inputbox_get_text(CL_Component* comp);
void        gui_inputbox_set_text(CL_Component* comp, std::string str);


CL_Component* gui_listbox_create(int x, int y, int w, int h);
int           gui_listbox_add(CL_Component* box, const char* str);

/** Quit the GUI manager, works only if the GUIManager is in a busy loop */
void gui_quit();

void gui_hide();
void gui_show();
bool gui_is_visible();

#ifdef SWIGGUILE
/** Create a button which launches func on click */
CL_Component* gui_create_button_func(int x, int y, int w, int h, const char* text, 
                                     SCM func);
/** Add an item to a menu */
void gui_add_menu_item(CL_Component* menu, const char* name, SCM func);
CL_MenuNode* gui_add_menu_toggle_item(CL_Component* c_menu, const char* name, SCM func);

/** Connect a function to an on_click event */
void gui_component_on_click(CL_Component* comp, SCM func);

/** Connect a function to an on_close event, mainly used to react on
    window closing */
void gui_component_on_close(CL_Component* comp, SCM func);

/** Start a file dialog, call func once ok is pressed.
    @param filename The directory where to start the file manager in */
void gui_file_dialog(const char* filename, SCM func);

void          gui_listbox_on_click(CL_Component* box, SCM func);

void gui_add_on_resize_callback(SCM func);
#endif

#endif

/* EOF */
