#include <string>
#include <ClanLib/display.h>
#include <ClanLib/core.h>
#include <ClanLib/gl.h>
#include <ClanLib/gui.h>
#include <Python.h>
#include <boost/python.hpp>
#include <iostream>

#include "scripting/editor.hxx"
#include "tile.hxx"
#include "editor.hxx"
#include "editor_map.hxx"
#include "workspace.hxx"
#include "tileset.hxx"
#include "editor_map_component.hxx"
#include "flexlay.hxx"
#include "globals.hxx"
#include "python_functor.hxx"
#include "gui_manager.hxx"

void
clerror_translator(CL_Error const& err)
{
  PyErr_SetString(PyExc_UserWarning, ("CL_Error: " + err.message).c_str());
}

void flexlay_init()   { flexlay.init(); }
void flexlay_deinit() { flexlay.deinit(); }

void sig_connect(CL_Signal_v0& sig, boost::python::object obj)
{
  std::cout << "Connecting functor: " << std::endl;
  new CL_Slot(sig.connect_functor(PythonFunctor(boost::python::object(obj))));
}

void foobar(CL_Menu* menu) 
{
  std::cout << "Do foobar" << menu << std::endl;
}

BOOST_PYTHON_MODULE(flexlay)
{
  using namespace boost::python;

  def("flexlay_init",   &flexlay_init);
  def("flexlay_deinit", &flexlay_deinit);
  def("editor_set_brush_tile", editor_set_brush_tile);
  
  register_exception_translator<CL_Error>(&clerror_translator);

  class_<GUIManager>("GUIManager")
    .def("run",            &GUIManager::run)
    .def("quit",           &GUIManager::quit)
    .def("push_component", &GUIManager::push_component)
    .def("pop_component",  &GUIManager::pop_component)
    .def("get_component",  &GUIManager::get_component,
         return_value_policy<reference_existing_object>());

  class_<CL_Size>("Size", init<int, int>())
    .def_readwrite("width",  &CL_Size::width)
    .def_readwrite("height", &CL_Size::height);

  class_<CL_Point>("Point", init<int, int>())
    .def_readwrite("x", &CL_Point::x)
    .def_readwrite("y", &CL_Point::y);

  class_<CL_Rect>("Rect", init<int, int, int, int>())
    .def(init<CL_Point, CL_Size>())
    .def_readwrite("left",   &CL_Rect::left)
    .def_readwrite("right",  &CL_Rect::right)
    .def_readwrite("top",    &CL_Rect::top)
    .def_readwrite("bottom", &CL_Rect::bottom);

  class_<CL_Signal_v0, boost::noncopyable>("Signal_v0", no_init);

  class_<CL_Component, boost::noncopyable>("Component", no_init);

  def("connect", &sig_connect);

  class_<CL_Window, bases<CL_Component>, CL_Window, boost::noncopyable>
    ("Window", init<CL_Rect, std::string, CL_Component*>());
  
  class_<CL_Button, boost::noncopyable, bases<CL_Component> >
    ("Button",  init<CL_Rect, std::string, CL_Component*>())
    .def("sig_clicked", &CL_Button::sig_clicked,
         return_value_policy<reference_existing_object>());

  class_<CL_MenuNode, boost::noncopyable>("MenuNode", no_init);

  class_<CL_Menu, bases<CL_Component>, CL_Menu, boost::noncopyable>
    ("Menu", init<CL_Component*>())
    .def("add_item", &CL_Menu::create_item,
         return_value_policy<reference_existing_object>())
    .def("foobar", &foobar);

  class_<Workspace, bases<>, Workspace, boost::noncopyable>
    ("Workspace", init<int, int>())
    .def("set_map", &Workspace::set_current_map);

  class_<Editor, bases<>, Editor, boost::noncopyable>
    ("Editor", init<>())
    .def("get_gui", &Editor::get_gui_manager, 
         return_value_policy<reference_existing_object>());
  
  class_<EditorMap>
    ("EditorMap", init<std::string>())
    .def("add", &EditorMap::add_layer);

  class_<EditorMapLayer, bases<>, EditorMapLayer, boost::noncopyable>
    ("EditorMapLayer", no_init);

  class_<EditorMapComponent, bases<CL_Component>, EditorMapComponent, boost::noncopyable>
    ("EditorMapComponent", init<CL_Rect, CL_Component*>())
    .def("set_zoom",      &EditorMapComponent::set_zoom)
    .def("set_workspace", &EditorMapComponent::set_workspace);

  class_<CL_Color>("Color", init<int, int, int, int>())
    .add_property("red",   &CL_Color::set_red,   &CL_Color::set_red)
    .add_property("green", &CL_Color::set_green, &CL_Color::set_green)
    .add_property("blue",  &CL_Color::set_blue,  &CL_Color::set_blue)
    .add_property("alpha", &CL_Color::set_alpha, &CL_Color::set_alpha);

  class_<Tile>("Tile", init<std::string, CL_Color, CL_Color>());

  class_<Tileset>("Tileset", init<int>())
    .def("get_tilesize", &Tileset::get_tile_size)
    .def("add_tile",     &Tileset::add_tile);

  class_<EditorTileMap, bases<EditorMapLayer>, EditorTileMap, boost::noncopyable>
    ("TileMap", init<Tileset*, int, int>())
    .def("get_tile", &EditorTileMap::get_tile)
    .def("resize",   &EditorTileMap::resize);

  def("tilemap_set_current", &EditorTileMap::set_current);
  def("tilemap_paint_tool_set_tilemap", &tilemap_paint_tool_set_tilemap);
}

/* EOF */
