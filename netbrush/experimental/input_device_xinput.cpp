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
#define verbose true

InputDevice_XInput::InputDevice_XInput(Display* dpy, const std::string& name_)
  : name(name_),
    absolute(false),
    num_keys   (0),
    motion_type        (INVALID_EVENT_TYPE),
    button_press_type  (INVALID_EVENT_TYPE),
    button_release_type(INVALID_EVENT_TYPE),
    key_press_type     (INVALID_EVENT_TYPE),
    key_release_type   (INVALID_EVENT_TYPE),
    proximity_in_type  (INVALID_EVENT_TYPE),
    proximity_out_type (INVALID_EVENT_TYPE)

{
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

void
InputDevice_XInput::get_info(XDeviceInfo	*info)
{
  int			i,j;
  XAnyClassPtr	any;
  XKeyInfoPtr		k;
  XButtonInfoPtr	b;
  XValuatorInfoPtr	v;
  XAxisInfoPtr	a;

  printf("\"%s\"\tid=%ld\t[%s]\n", info->name, info->id,
         (info->use == IsXExtensionDevice) ? "XExtensionDevice" :
         ((info->use == IsXPointer) ? "XPointer" : "XKeyboard"));

  if (info->num_classes > 0) {
    any = (XAnyClassPtr) (info->inputclassinfo);
    for (i=0; i<info->num_classes; i++) {
      switch (any->c_class) {
      case KeyClass:
        k = (XKeyInfoPtr) any;
        printf("\tNum_keys is %d\n",    k->num_keys);
        printf("\tMin_keycode is %d\n", k->min_keycode);
        printf("\tMax_keycode is %d\n", k->max_keycode);

        num_keys = k->num_keys;
        break;

      case ButtonClass:
        b = (XButtonInfoPtr) any;
        printf("\tNum_buttons is %d\n", b->num_buttons);

        buttons.resize(b->num_buttons, false);
        break;

      case ValuatorClass:
        v = (XValuatorInfoPtr) any;
        a = (XAxisInfoPtr) ((char *) v + 
                            sizeof (XValuatorInfo));

        printf("\tNum_axes is %d\n", v->num_axes);
        printf("\tMode is %s\n", (v->mode == Absolute) ? "Absolute" : "Relative");
        printf("\tMotion_buffer is %ld\n", v->motion_buffer);

        absolute = (v->mode == Absolute);

        for (j=0; j<v->num_axes; j++, a++) 
          {
            printf("\tAxis %d :\n", j);
            printf("\t\tMin_value is %d\n", a->min_value);
            printf("\t\tMax_value is %d\n", a->max_value);
            printf ("\t\tResolution is %d\n", a->resolution);
					
            axis.push_back(AxisInfo(a->min_value, a->max_value, a->resolution));
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
      for (ip = device->classes, i=0; i<info->num_classes; ip++, i++) 
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
InputDevice_XInput::on_xevent(XEvent &event)
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

  std::vector<AxisInfo> old_axis    = axis;
  std::vector<bool>     old_buttons = buttons;

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
      on_proximity_notify_event((XProximityNotifyEvent*)&event);
    }
  else 
    {  // Events that aren't XInput events lang here (focus and stuff)
      if (verbose) printf("InputDevice_XInput: what's that %d\n", event.type);
    }

  for (std::vector<bool>::size_type i = 0; i < buttons.size(); ++i)
    {
      if (buttons[i] != old_buttons[i])
        {
          //InputEvent ie;
			
          //ie.id           = i;
          //ie.type         = InputEvent::pressed;
          //ie.device       = InputDevice(this);
          //ie.mouse_pos    = Point(0, 0);
          //ie.axis_pos     = 0;
          //ie.repeat_count = false;

          //sig_axis_move(ie);
          //std::cout << "Wacom: Button" << std::endl;
        }
    }

  for (std::vector<AxisInfo>::size_type i = 0; i < axis.size(); ++i)
    {
      if (axis[i].pos != old_axis[i].pos)
        {
          //			InputEvent ie;
          //		
          //			ie.id           = i;
          //			ie.type         = InputEvent::axis_moved;
          //			ie.device       = InputDevice(this);
          //			ie.mouse_pos    = Point(0, 0);
          //			ie.axis_pos     = get_axis(i);
          //ie.repeat_count = false;

          //sig_axis_move(ie);

          //std::cout << "Wacom: Axis moved " << std::endl;
        }
    }
}

void
InputDevice_XInput::on_device_button_event(XDeviceButtonEvent *button)
{
  if (verbose) printf("button %s %d ", (button->type == button_release_type) ? "release" : "press  ", button->button);

  buttons[button->button] = (button->type == button_press_type);
	    
  for(int loop = 0; loop < button->axes_count; loop++) {
    if (verbose)  printf("a[%d]=%d ", button->first_axis + loop, button->axis_data[loop]);
    axis[loop + button->first_axis].pos = button->axis_data[loop];
  }
  if (verbose) printf("\n");
}

void
InputDevice_XInput::on_device_key_event(XDeviceKeyEvent* key)
{   
  if (verbose) printf("key %s %d ", (key->type == key_release_type) ? "release" : "press  ", key->keycode);
	    
  for(int loop = 0; loop < key->axes_count; loop++) 
    {
      if (verbose) printf("a[%d]=%d ", key->first_axis + loop, key->axis_data[loop]);
      axis[loop + key->first_axis].pos = key->axis_data[loop];
    }
  if (verbose) printf("\n");
}

void
InputDevice_XInput::on_device_motion_event(XDeviceMotionEvent* motion)
{
        if (verbose) printf("motion ");
	    
      for(int loop=0; loop<motion->axes_count; loop++) {
        if (loop >= 3)
          {
            //printf("a[%d] = ", motion->first_axis + loop);
            //print_bits(*((unsigned int*)&(motion->axis_data[loop])));

            if (verbose) printf("a[%d] = %8d ", motion->first_axis + loop,
                                *((short*)&(motion->axis_data[loop]))); // workaround for buggy driver
          }
        else
          {
            if (verbose) printf("a[%d] = %8d ", motion->first_axis + loop, motion->axis_data[loop]);
          }
        axis[loop + motion->first_axis].pos = motion->axis_data[loop];
      }
      if (verbose) printf("\n");

}

void
InputDevice_XInput::on_proximity_notify_event(XProximityNotifyEvent* prox)
{
  if (verbose) printf("proximity %s ", (prox->type == proximity_in_type) ? "in " : "out");
		
  for(int loop=0; loop < prox->axes_count; loop++)
    {
      if (verbose) printf("a[%d]=%d ", prox->first_axis + loop, prox->axis_data[loop]);
      axis[loop + prox->first_axis].pos = prox->axis_data[loop];
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
