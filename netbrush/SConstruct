# netbrush_env = Environment()
# netbrush_env.ParseConfig('sdl-config --cflags --libs')
# netbrush_env['LIBS'] += ['SDL_net']
# netbrush_env['CXXFLAGS'] += ['-O2', '-Wall', '-g']
# netbrush_env.Program('netbrush', ['net.cpp', 'vector.cpp'])

common_env = Environment()
common_env['CXXFLAGS'] += ['-O0', '-Wall', '-g']
libcommon = common_env.StaticLibrary('common', [
        'src/command_line.cpp',
        'src/command_line_generic.cpp',
])

server_env = Environment()
server_env.ParseConfig('sdl-config --cflags --libs')
server_env['CXXFLAGS'] += ['-O0', '-Wall', '-g']
server_env['LIBS'] += ['SDL_net'] + libcommon
server_env['LIBPATH'] += ['.']
server_env.Program('netbrush-server', [
        'src/server.cpp',
        'src/client_connection.cpp',
])

client_env = Environment()
client_env.ParseConfig('sdl-config --cflags --libs')
client_env['CXXFLAGS'] += ['-O0', '-Wall', '-g']
client_env['CPPPATH'] += ['src/']
client_env['LIBPATH'] += ['.']
client_env['LIBS'] += ['SDL_image', 'SDL_net'] + libcommon
client_env.Program('netbrush-client', [
        'src/alpha_picker.cpp',
        'src/brush_widget.cpp',
        'src/brushmask.cpp',
        'src/client.cpp', 
        'src/client_state.cpp',
        'src/debug.cpp',
        'src/drawing_context.cpp',
        'src/drawing_parameter.cpp',
        'src/generic_brush.cpp',
        'src/globals.cpp',
        'src/grayscale_buffer.cpp', 
        'src/hue_picker.cpp',
        'src/math/matrix.cpp',
        'src/math/origin.cpp',
        'src/math/quaternion.cpp',
        'src/math/rect.cpp',
        'src/math/vector.cpp',
        'src/saturation_value_picker.cpp',
        'src/screen_buffer.cpp',
        'src/server_connection.cpp',
        'src/stroke.cpp',
        'src/stroke_buffer.cpp',
        'src/video.cpp',
        'src/widget/button.cpp',
        'src/widget/scrollbar.cpp',
        'src/widget/slider_widget.cpp',
        'src/widget/widget.cpp',
        'src/widget/widget_manager.cpp',
        'src/navigation.cpp',
        'src/graphic_context_state.cpp',
        'src/controller.cpp',
        'src/airbrush_tool.cpp',
        'src/paintbrush_tool.cpp',
        'src/scroll_tool.cpp',
        'src/colorpicker_tool.cpp',
        'src/region_tool.cpp',
        'src/color_display.cpp',
        'src/color.cpp',
        'src/SDL_tty.c',
        'src/text_view.cpp',
        'src/rect_tool.cpp',
        'src/SDL_gfx/SDL_gfxPrimitives.c',
#        'src/widget/events.cpp',
])

# EOF #

