class DisplayProperties:
    layer = Level.INTERACTIVE
    show_all = False
    current_only = False
    
    def set(self, map):
        if self.current_only:
            active   = CL_Color(255, 255, 255)
            deactive = CL_Color(0, 0, 0, 10)
        else:
            active   = CL_Color(255, 255, 255)
            deactive = CL_Color(150, 150, 250, 150)

        if (self.show_all):
            map.foreground.set_foreground_color(active)
            map.interactive.set_foreground_color(active)
            map.background.set_foreground_color(active)
        else:
            if (self.layer == Level.FOREGROUND):
                map.foreground.set_foreground_color(active)
            else:
                map.foreground.set_foreground_color(deactive)

            if (self.layer == Level.INTERACTIVE):
                map.interactive.set_foreground_color(active)
            else:
                map.interactive.set_foreground_color(deactive)

            if (self.layer == Level.BACKGROUND):
                map.background.set_foreground_color(active)
            else:
                map.background.set_foreground_color(deactive)

# EOF #
