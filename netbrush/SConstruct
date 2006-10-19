# netbrush_env = Environment()
# netbrush_env.ParseConfig('sdl-config --cflags --libs')
# netbrush_env['LIBS'] += ['SDL_net']
# netbrush_env['CXXFLAGS'] += ['-O2', '-Wall', '-g']
# netbrush_env.Program('netbrush', ['net.cpp', 'vector.cpp'])

server_env = Environment()
server_env.ParseConfig('sdl-config --cflags --libs')
server_env['CXXFLAGS'] += ['-O2', '-Wall', '-g']
server_env['LIBS'] += ['SDL_net']
server_env.Program('server', [
        'server.cpp'
])


client_env = Environment()
client_env.ParseConfig('sdl-config --cflags --libs')
client_env['CXXFLAGS'] += ['-O2', '-Wall', '-g']
client_env['CPPPATH'] += ['.']
client_env['LIBS'] += ['SDL_image', 'SDL_net']
client_env.Program('client', [
        'alpha_picker.cpp',
        'brush_widget.cpp',
        'brushmask.cpp',
        'client.cpp', 
        'client_state.cpp',
        'debug.cpp',
        'drawing_context.cpp',
        'drawing_parameter.cpp',
        'generic_brush.cpp',
        'globals.cpp',
        'grayscale_buffer.cpp', 
        'hue_picker.cpp',
        'math/matrix.cpp',
        'math/origin.cpp',
        'math/quaternion.cpp',
        'math/rect.cpp',
        'math/vector.cpp',
        'saturation_value_picker.cpp',
        'screen_buffer.cpp',
        'server_connection.cpp',
        'stroke.cpp',
        'stroke_buffer.cpp',
        'video.cpp',
        'widget/button.cpp',
        'widget/scrollbar.cpp',
        'widget/slider_widget.cpp',
        'widget/widget.cpp',
        'widget/widget_manager.cpp',
#        'widget/events.cpp',
])

# EOF #

