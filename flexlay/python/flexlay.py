from flexlay_wrap import *

# Automatic convert of metadata to native Python 
EditorMap.__get_metadata = EditorMap.get_metadata
def EditorMap_get_metadata(self):
    return get_python_object(self.__get_metadata())
EditorMap.get_metadata = EditorMap_get_metadata
EditorMap.get_data = EditorMap_get_metadata
del EditorMap_get_metadata

# Helper function for callbacks
def Icon_set_callback(self, func):
    connect(self.sig_clicked(), func)
Icon.set_callback = Icon_set_callback
del Icon_set_callback

def Menu_add_item(self, sprite, text, func):
    i = self.__add_item(sprite, text)
    if func != None:
        connect(self.sig_clicked(i), func)
Menu.__add_item = Menu.add_item
Menu.add_item = Menu_add_item
del Menu_add_item

def CL_Menu_add_item(self, name, func):
    item = self.create_item(name)
    connect(item.sig_clicked(), func)
CL_Menu.add_item = CL_Menu_add_item
del CL_Menu_add_item

class SimpleFileDialog:
    """Very simple FileDialog, mainly a placeholder until the real thing gets ready."""
    window   = None
    inputbox = None
    ok_button     = None
    cancel_button = None
    callback = None

    def __init__(self, title, ok, cancel, g):
        self.window   = Window(CL_Rect(CL_Point(120, 200), CL_Size(560, 100)), title, g)
        self.inputbox = CL_InputBox(CL_Rect(CL_Point(10, 10), CL_Size(530, 25)),
                                    self.window.get_client_area())
        self.ok_button     = CL_Button(CL_Rect(CL_Point(490, 35), CL_Size(50, 25)), ok,
                                       self.window.get_client_area())
        self.cancel_button = CL_Button(CL_Rect(CL_Point(430, 35), CL_Size(50, 25)), cancel,
                                       self.window.get_client_area())
        self.window.hide()

    def set_filename(self, filename):
        self.inputbox.set_text(filename)

    def get_filename(self):
        return self.inputbox.get_text()
        
    def run(self, func):
        connect(self.ok_button.sig_clicked(), self.on_ok)
        connect(self.cancel_button.sig_clicked(), self.on_cancel)
        self.callback = func
        self.window.show()
        
    def on_ok(self):
        self.window.hide();
        self.callback(self.inputbox.get_text())

    def on_cancel(self):
        self.window.hide();

class GenericDialog:
    window = None
    items  = None
    ok     = None
    cancel = None
    callback = None
    
    def __init__(self, title, gui):
        self.items = []
        self.window = Window(CL_Rect(CL_Point(100, 100), CL_Size(400, 100)), title, gui)
        self.ok = CL_Button(CL_Rect(CL_Point(290, 35), CL_Size(50, 25)), "Ok",
                       self.window.get_client_area())
        self.cancel = CL_Button(CL_Rect(CL_Point(230, 35), CL_Size(50, 25)), "Cancel",
                           self.window.get_client_area())
        connect(self.cancel.sig_clicked(), self.on_cancel)
        connect(self.ok.sig_clicked(), self.on_ok)

    def on_cancel(self):
        self.window.hide()
        
    def on_ok(self):
        self.window.hide()
        if self.callback != None:
            apply(self.callback, self.get_values())

    def set_callback(self, c):
        self.callback = c

    def get_values(self):
        def get_value(item):
            (type, label, comp) = item
            if type == "int":
                return int(comp.get_text())
            elif type == "string":
                return comp.get_text()
            else:
                return None

        return map(get_value, self.items)
        
    def add_int(self, name, value = 0):
        self.items.append(("int",
                           CL_Label(CL_Point(10, 10), name,
                                    self.window.get_client_area()),
                           CL_InputBox(CL_Rect(CL_Point(110, 10), CL_Size(200, 25)),
                                       self.window.get_client_area())))
        self.items[-1][2].set_text(str(value))
        self.update()

    def add_string(self, name, value = ""):
        self.items.append(("string",
                           CL_Label(CL_Point(10, 10), name,
                                    self.window.get_client_area()),
                           CL_InputBox(CL_Rect(CL_Point(110, 10), CL_Size(200, 25)),
                                       self.window.get_client_area())))
        self.items[-1][2].set_text(value)
        self.update()    

    def update(self):
        y = 10
        for (type, label, comp) in self.items:
            if type == "int" or type == "string":
                label.set_position(10, y)
                comp.set_position(110, y)
                y += 25
        self.cancel.set_position(200, y)
        self.ok.set_position(260, y)
        self.window.set_size(330, y + 60)
#        def add_string(self, name):
        

# EOF #
