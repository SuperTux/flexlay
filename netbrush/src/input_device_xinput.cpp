/*  $Id$
**            _   ___              _
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software; you can redistribute it and/or
**  modify it under the terms of the GNU General Public License
**  as published by the Free Software Foundation; either version 2
**  of the License, or (at your option) any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**
**  You should have received a copy of the GNU General Public License
**  along with this program; if not, write to the Free Software
**  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
**  02111-1307, USA.
*/

#include <iostream>
#include "input_device_xinput.hpp"
#include "SDL.h"
#include "SDL_syswm.h"
#include "widget/events.hpp"
#include "widget/widget_manager.hpp"
#include "globals.hpp"

/*
  static void print_bits(unsigned int n)
  {
  int i;
  for(i = 31; i >= 0; --i)
  {
  if (i == 29)
  putchar(' ');

  if (i == 21)
  putchar(' ');

  if (n & (1 << i))
  putchar('1');
  else
  putchar('0');
  }
  putchar(' ');
  }
*/

#define INVALID_EVENT_TYPE	-1
#define verbose false

InputDevice_XInput::InputDevice_XInput(Display* dpy, Window w, const std::string& name_)
  : name(name_),
    absolute(false),
    num_keys   (0),
    proximity(false),
    motion_type        (INVALID_EVENT_TYPE),
    button_press_type  (INVALID_EVENT_TYPE),
    button_release_type(INVALID_EVENT_TYPE),
    key_press_type     (INVALID_EVENT_TYPE),
    key_release_type   (INVALID_EVENT_TYPE),
    proximity_in_type  (INVALID_EVENT_TYPE),
    proximity_out_type (INVALID_EVENT_TYPE)

{
  window_rect = get_window_rect(dpy, w);

  display_width  = DisplayWidth(dpy, DefaultScreen(dpy));
  display_height = DisplayHeight(dpy, DefaultScreen(dpy));

  //std::cout << "WindowRect: " << window_rect << std::endl;

  XDeviceInfo* info = find_device_info(dpy, name.c_str(), True);
  if (!info)
    std::cout << "InputDeviceXInput Error: Couldn't find device: " << name << std::endl;
  get_info(info);

  if (!register_events(dpy, info, name.c_str(), True))
    {
      std::cout << "debug" << "InputDeviceXInput: Couldn't find device: " << name << std::endl;
    }
  else
    {
      //slot_xevent = owner->sig_unknown_xevent.connect(this, &InputDevice_XInput::on_xevent);
    }
}

InputDevice_XInput::~InputDevice_XInput()
{
	
}

Rect
InputDevice_XInput::get_window_rect(Display* dpy, Window w)
{ // Calculate the exact positon of the window
  Window root_win = RootWindow(dpy, DefaultScreen(dpy)); 
  Window current_window = w;

  int w_x, w_y, w_w, w_h;

  int x, y;
  unsigned int width, height;
  unsigned int border, depth;
  Window root;
  XGetGeometry(dpy, w,
               &root, &x, &y, &width, &height, &border, &depth);
  
  window_x = x;
  window_y = y;
  w_x = x;
  w_y = y;
  w_w = width;
  w_h = height;

  while(current_window != root_win)
    {
      Window  parent;
      Window* children;
      unsigned int num_children;
      XQueryTree(dpy, current_window, &root, &parent, &children, &num_children);

      XGetGeometry(dpy, parent,
                   &root, &x, &y, &width, &height, &border, &depth);

      w_x += x;
      w_y += y;

      if (children)
        XFree(children);
      
      current_window = parent;
    }

  return Rect(Point(w_x, w_y),
              Size(w_w, w_h));
}

void
InputDevice_XInput::get_info(XDeviceInfo* info)
{
  printf("\"%s\"\tid=%ld\t[%s]\n", info->name, info->id,
         (info->use == IsXExtensionDevice) ? "XExtensionDevice" :
         ((info->use == IsXPointer) ? "XPointer" : "XKeyboard"));

  if (info->num_classes > 0)
    {
      XAnyClassPtr any = (XAnyClassPtr) (info->inputclassinfo);
      for (int i = 0; i < info->num_classes; ++i) 
        {
          switch (any->c_class) {
          case KeyClass:
            {
              XKeyInfoPtr k = (XKeyInfoPtr) any;
              printf("\tNum_keys is %d\n",    k->num_keys);
              printf("\tMin_keycode is %d\n", k->min_keycode);
              printf("\tMax_keycode is %d\n", k->max_keycode);

              num_keys = k->num_keys;
            }
            break;

          case ButtonClass:
            {
              XButtonInfoPtr   b = (XButtonInfoPtr) any;
              printf("\tNum_buttons is %d\n", b->num_buttons);

              buttons.resize(b->num_buttons, false);
            }
            break;

          case ValuatorClass:
            {
              XValuatorInfoPtr v = (XValuatorInfoPtr) any;
              XAxisInfoPtr     a = (XAxisInfoPtr) ((char *) v +
                                                   sizeof (XValuatorInfo));

              printf("\tNum_axes is %d\n", v->num_axes);
              printf("\tMode is %s\n", (v->mode == Absolute) ? "Absolute" : "Relative");
              printf("\tMotion_buffer is %ld\n", v->motion_buffer);

              absolute = (v->mode == Absolute);

              for (int j = 0; j < v->num_axes; j++, a++)
                {
                  printf("\tAxis %d :\n", j);
                  printf("\t\tMin_value is %d\n",   a->min_value);
                  printf("\t\tMax_value is %d\n",   a->max_value);
                  printf ("\t\tResolution is %d\n", a->resolution);
					
                  axis.push_back(AxisInfo(a->min_value, a->max_value, a->resolution));
                }
            }
            break;
		
          default:
            printf ("unknown class\n");
          }
          any = (XAnyClassPtr) ((char *) any + any->length);
        }
    }
}

XDeviceInfo*
InputDevice_XInput::find_device_info(Display	*display,
                                     const char	*name,
                                     Bool	only_extended)
{
  // FIXME: Not really needed could simply pass XDeviceInfo to the
  // constructor, might however make a nicer interface
  XDeviceInfo	*devices;
  int		loop;
  int		num_devices;
  int		len = strlen(name);
  Bool     is_id = True;
  XID		id = 0;

  for(loop=0; loop<len; loop++) {
    if (!isdigit(name[loop])) {
      is_id = False;
      break;
    }
  }

  if (is_id) {
    id = atoi(name);
  }

  devices = XListInputDevices(display, &num_devices);

  for(loop=0; loop<num_devices; loop++) {
    if ((!only_extended || (devices[loop].use == IsXExtensionDevice)) &&
        ((!is_id && strcmp(devices[loop].name, name) == 0) ||
         (is_id && devices[loop].id == id))) {
      return &devices[loop];
    }
  }
  return NULL;
}

int
InputDevice_XInput::register_events(Display		*dpy,
                                    XDeviceInfo	*info,
                                    const char		*dev_name,
                                    Bool		handle_proximity)
{
  int             number = 0;	/* number of events registered */
  XEventClass     event_list[7];
  int             i;
  XDevice         *device;
  Window          root_win;
  unsigned long   screen;
  XInputClassInfo *ip;

  screen   = DefaultScreen(dpy);
  root_win = RootWindow(dpy, screen);

  device = XOpenDevice(dpy, info->id);

  if (!device) {
    fprintf(stderr, "unable to open device %s\n", dev_name);
    return 0;
  }

  if (device->num_classes > 0)
    {
      for (ip = device->classes, i = 0; i<info->num_classes; ip++, ++i)
        {
          switch (ip->input_class) {
          case KeyClass:
            DeviceKeyPress  (device, key_press_type,   event_list[number]); number++;
            DeviceKeyRelease(device, key_release_type, event_list[number]); number++;
            break;

          case ButtonClass:
            DeviceButtonPress  (device, button_press_type,   event_list[number]); number++;
            DeviceButtonRelease(device, button_release_type, event_list[number]); number++;
            break;

          case ValuatorClass:
            DeviceMotionNotify(device, motion_type, event_list[number]); number++;
            if (handle_proximity) {
              ProximityIn (device, proximity_in_type,  event_list[number]); number++;
              ProximityOut(device, proximity_out_type, event_list[number]); number++;
            }
            break;
		
          default:
            fprintf(stderr, "unknown class\n");
            break;
          }
        }

      if (XSelectExtensionEvent(dpy, root_win, event_list, number))
        {
          fprintf(stderr, "error selecting extended events\n");
          return 0;
        }
    }

  //std::cout << "### Registered events: " << number << std::endl;
  return number;
}

void
InputDevice_XInput::on_xevent(Display* dpy, Window w, XEvent &event)
{
  if (0)
    std::cout << this << " event: "
              << event.type << " Defs: "
              << motion_type << " "
              << button_press_type << " "
              << button_release_type << " "
              << key_press_type << " "
              << key_release_type << " "
              << proximity_out_type << " "
              << proximity_in_type << " "
              << std::endl;

  if (event.type == ConfigureNotify) //Resize or Move
    { // FIXME: Due to the way SDL works, this one is never ever reached
      //window_x = x;
      //window_y = y;
      std::cout 
        << event.xconfigure.x     << " " << event.xconfigure.y << " " 
        << event.xconfigure.width << " " << event.xconfigure.height << std::endl;
    }
  if (event.type == motion_type)
    {
      on_device_motion_event((XDeviceMotionEvent *)&event);
    }
  else if ((event.type == button_press_type) ||
           (event.type == button_release_type))
    {
      on_device_button_event((XDeviceButtonEvent *)&event);
    }
  else if ((event.type == key_press_type) ||
           (event.type == key_release_type))
    {
      on_device_key_event((XDeviceKeyEvent*)&event);
    }
  else if ((event.type == proximity_out_type) ||
           (event.type == proximity_in_type))
    {
      // FIXME: This shouldn't be here
      window_rect = get_window_rect(dpy, w);

      on_proximity_notify_event((XProximityNotifyEvent*)&event);
    }
  else
    {  // Events that aren't XInput events lang here (focus and stuff)
      printf("InputDevice_XInput: what's that %d\n", event.type);
    }
}

void
InputDevice_XInput::on_device_button_event(XDeviceButtonEvent *button)
{
  if (verbose) printf("button %s %d ", (button->type == button_release_type) ? "release" : "press  ", button->button);

  buttons[button->button] = (button->type == button_press_type);
	
  for(int i = 0; i < button->axes_count; ++i)
    {
      if (verbose)  printf("a[%d]=%d ", button->first_axis + i, button->axis_data[i]);
      axis[i + button->first_axis].pos = button->axis_data[i];
    }
  if (verbose) printf("\n");
}

void
InputDevice_XInput::on_device_key_event(XDeviceKeyEvent* key)
{
  if (verbose) printf("key %s %d ", (key->type == key_release_type) ? "release" : "press  ", key->keycode);
	
  for(int i = 0; i < key->axes_count; ++i)
    {
      if (verbose) printf("a[%d]=%d ", key->first_axis + i, key->axis_data[i]);
      axis[i + key->first_axis].pos = key->axis_data[i];
    }
  if (verbose) printf("\n");
}

void
InputDevice_XInput::on_device_motion_event(XDeviceMotionEvent* motion)
{
  if (verbose) printf("motion ");
  float x;
  float y;
  float pressure;
  float x_tilt;
  float y_tilt;

  for(int i = 0; i<motion->axes_count; ++i) 
    {
      if (i == 0)
        {
          x = float(motion->axis_data[i])/(axis[i].max_value - axis[i].min_value);
        }
      else if (i == 1)
        {
          y = float(motion->axis_data[i])/(axis[i].max_value - axis[i].min_value);
        }
      else if (i == 2)
        {
          pressure = float(motion->axis_data[i])/(axis[i].max_value - axis[i].min_value);
        }
      else if (i == 3)
        {
          x_tilt = float(*((short*)&(motion->axis_data[i])))/(axis[i].max_value - axis[i].min_value);
        }
      else if (i == 4)
        {
          y_tilt = float(*((short*)&(motion->axis_data[i])))/(axis[i].max_value - axis[i].min_value);
        }

      if (i >= 3)
        {
          //printf("a[%d] = ", motion->first_axis + i);
          //print_bits(*((unsigned int*)&(motion->axis_data[i])));

          if (verbose) printf("a[%d] = %5d ", motion->first_axis + i,
                              *((short*)&(motion->axis_data[i]))); // workaround for buggy driver
        }
      else
        {
          if (verbose) printf("a[%d] = %5d ", motion->first_axis + i, motion->axis_data[i]);
        }
      axis[i + motion->first_axis].pos = motion->axis_data[i];
    }
  if (verbose) printf("\n");

  if (0) 
    printf("x: %1.5f y: %1.5f pressure: %1.5f x_tilt: %2.5f y_tilt: %2.5f\n", x, y, pressure, x_tilt, y_tilt);

  // translate the pen coordinates into window space and send them to
  // the WidgetManager
  widget_manager->on_pen_motion(PenEvent(x * display_width  - window_rect.left,
                                         y * display_height - window_rect.top, 
                                         pressure, 
                                         x_tilt, y_tilt));
}

void
InputDevice_XInput::on_proximity_notify_event(XProximityNotifyEvent* prox)
{
  if (verbose) printf("proximity %s ", (prox->type == proximity_in_type) ? "in " : "out");
		
  if (prox->type == proximity_in_type)
    proximity = true;
  else 
    proximity = false;

  for(int i = 0; i < prox->axes_count; ++i)
    {
      if (verbose) printf("a[%d]=%d ", prox->first_axis + i, prox->axis_data[i]);
      axis[i + prox->first_axis].pos = prox->axis_data[i];
    }
  if (verbose) printf("\n");
}

bool
InputDevice_XInput::get_keycode(int keycode) const
{
  if (keycode >=0 && keycode < int(buttons.size()))
    {
      return buttons[keycode];
    }
  else
    {
      return false;
    }
}

float
InputDevice_XInput::get_axis(int index) const
{
  if (index >= 0 && index < get_axis_count())
    {
      return float(axis[index].pos)/(axis[index].max_value - axis[index].min_value);
    }
  else
    {
      return 0.0f;
    }
}

int
InputDevice_XInput::get_axis_count() const
{
  return axis.size();
}

std::string
InputDevice_XInput::get_name() const
{
  return name;
}

std::string
InputDevice_XInput::get_device_name() const
{
  return "xinput:" + name;
}

int
InputDevice_XInput::get_button_count() const
{
  return buttons.size();
}

/* EOF */
