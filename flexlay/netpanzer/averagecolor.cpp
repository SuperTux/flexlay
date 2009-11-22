#include <ClanLib/display.h>
#include <ClanLib/core.h>

CL_Color
calc_average_color(const char* filename)
{
  // FIXME: Works only from indexed images
  CL_PNGProvider buffer(filename);
  int len = buffer.get_pitch() * buffer.get_height();
  unsigned char* buf = static_cast<unsigned char*>(buffer.get_data());
  CL_Palette palette = buffer.get_palette();
  float red   = 0;
  float green = 0;
  float blue  = 0;

  for(int i = 0; i < len; ++i)
    {
      red   += palette.colors[buf[i]].get_red();
      green += palette.colors[buf[i]].get_green();
      blue  += palette.colors[buf[i]].get_blue();
    }

  return CL_Color(int(red/len),
                  int(green/len),
                  int(blue/len));
}

int main(int argc, char** argv)
{
  CL_SetupCore::init();
  CL_SetupDisplay::init(true);

  for(int i = 1; i < argc; ++i)
    {
      CL_Color color = calc_average_color(argv[i]);
      std::cout << argv[i] 
                << " " << color.get_red()
                << " " << color.get_green()
                << " " << color.get_blue()
                << std::endl;
    }

  CL_SetupDisplay::deinit();
  CL_SetupCore::deinit();
}

/* EOF */
