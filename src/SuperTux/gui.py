class SuperTuxGUI:
    quit_button = None
    menu        = None
    
    selector_window = None
    tileselector    = None
    objectselector  = None

    def __init__(self, tileset, gui):
        self.selector_window = Panel(CL_Rect(CL_Point(800-134, 23+33), CL_Size(128 + 6, 558)),
                                         gui.get_component())
        self.tileselector = TileSelector(CL_Rect(CL_Point(3, 3), CL_Size(128, 552)), self.selector_window)
        self.tileselector.set_tileset(tileset)
        self.tileselector.set_tiles(range(1,100))
        self.tileselector.show(False)

        self.objectselector = ObjectSelector(CL_Rect(0, 0, 128, 256), 42, 42, self.selector_window)
        self.objectselector.show(True)
        for object in game_objects:
            self.objectselector.add_brush(ObjectBrush(make_sprite(config.datadir + object[1]),
                                                      make_metadata(BadGuy(object[0]))))

    def show_objects(self):
        self.tileselector.show(False)        
        self.objectselector.show(True)

    def show_tiles(self):
        self.tileselector.show(True)        
        self.objectselector.show(False)

    def show_none(self):
        self.tileselector.show(False)        
        self.objectselector.show(False)

def gui_level_save_as():
    save_dialog.set_filename(os.path.dirname(save_dialog.get_filename()) + "/")
    save_dialog.run(supertux_save_level)

def gui_level_save():
    if workspace.get_map().get_metadata().filename:
        save_dialog.set_filename(workspace.get_map().get_metadata().filename)
    else:
        save_dialog.set_filename(os.path.dirname(save_dialog.get_filename())  + "/")
        
    save_dialog.run(supertux_save_level)
   
def gui_level_load():
    load_dialog.run(supertux_load_level)

def gui_toggle_minimap():
    if minimap.is_visible():
        minimap.show(False)
        minimap_icon.set_up()
    else:
        minimap.show(True)
        minimap_icon.set_down()

def gui_toggle_grid():
    tilemap = workspace.get_map().get_metadata().foreground;
    tilemap.set_draw_grid(not(tilemap.get_draw_grid()))
    if tilemap.get_draw_grid():
        grid_icon.set_down()
    else:
        grid_icon.set_up()

def on_map_change():
    if (workspace.get_map().undo_stack_size() > 0):
        undo_icon.enable()
    else:
        undo_icon.disable()

    if (workspace.get_map().redo_stack_size() > 0):
        redo_icon.enable()
    else:
        redo_icon.disable()        

def set_tilemap_paint_tool():
    workspace.set_tool(tilemap_paint_tool.to_tool())
    paint.set_down()
    select.set_up()
    zoom.set_up()
    object.set_up()
    supertux.show_tiles()

def set_tilemap_select_tool():
    workspace.set_tool(tilemap_select_tool.to_tool())
    paint.set_up()
    select.set_down()
    zoom.set_up()
    object.set_up()
    supertux.show_none()
    
def set_zoom_tool():
    workspace.set_tool(zoom_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_down()
    object.set_up()
    supertux.show_none()
    
def set_objmap_select_tool():
    workspace.set_tool(objmap_select_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_up()
    object.set_down()
    supertux.show_objects()

def gui_show_foreground():
    display_properties.layer = Level.FOREGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().foreground)
    foreground_icon.set_down()
    interactive_icon.set_up()
    background_icon.set_up()
    minimap.update_minimap()

def gui_show_background():
    display_properties.layer = Level.BACKGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().background)
    foreground_icon.set_up()
    interactive_icon.set_up()
    background_icon.set_down()
    minimap.update_minimap()

def gui_show_interactive():
    display_properties.layer = Level.INTERACTIVE
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().interactive)
    foreground_icon.set_up()
    interactive_icon.set_down()
    background_icon.set_up()
    minimap.update_minimap()

def gui_show_all():
    display_properties.show_all = True
    display_properties.current_only = False
    display_properties.set(workspace.get_map().get_metadata())

def gui_show_current():
    display_properties.show_all = False
    display_properties.current_only = False
    display_properties.set(workspace.get_map().get_metadata())

def gui_show_only_current():
    display_properties.show_all = False
    display_properties.current_only = True
    display_properties.set(workspace.get_map().get_metadata())

def gui_toggle_display_props():
    if display_properties.show_all:
        display_properties.show_all = False
    elif not(display_properties.current_only):
        display_properties.current_only = True
    else:
         display_properties.show_all = True
         display_properties.current_only = False
        
    display_properties.set(workspace.get_map().get_metadata())    

def gui_resize_level():
    level = workspace.get_map().get_data()
    dialog = GenericDialog("Resize Level", gui.get_component())
    dialog.add_int("Width: ", level.width)
    dialog.add_int("Height: ", level.height)
    dialog.add_int("X: ", 0)
    dialog.add_int("Y: ", 0)
    def resize_callback(w, h, x, y):
        level.resize(CL_Size(w, h), CL_Point(x, y))
    dialog.set_callback(resize_callback)

def gui_resize_level_to_selection():
    level = workspace.get_map().get_data()
    rect  = tilemap_select_tool.get_selection_rect()
    if (rect.get_width() > 2 and rect.get_height() > 2):
        level.resize(rect.get_size(), CL_Point(-rect.left, -rect.top))

def supertux_load_level(filename):   
    def has_element(lst, el):
        for i in lst:
            if i == el:
                return True
        return False

    print "Loading: ", filename
    level = Level(filename)
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)
    
    if not(has_element(config.recent_files, filename)):
        config.recent_files.append(filename)
        recent_files_menu.add_item(mysprite, filename, lambda: supertux_load_level(filename))

    minimap.update_minimap()

def gui_set_zoom(zoom):
    gc = editor_map.get_workspace().get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)

def menu_file_open():
    print "File/Open"
    level = Level('/home/ingo/cvs/supertux/supertux/data/levels/world1/level2.stl')
    print "Loading done"
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)
    print "Activation done"

def supertux_save_level(filename):
    workspace.get_map().get_metadata().save(filename)

# EOF #
