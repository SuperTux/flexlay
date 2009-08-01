/*  $Id$
**            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef HEADER_INPUT_DEVICE_XINPUT_HPP
#define HEADER_INPUT_DEVICE_XINPUT_HPP

#include <vector>
#include "../src/math/point.hpp"
#include "../src/math/rect.hpp"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/XInput.h>

class InputDevice_XInput
{
  //!Construction:
public:
  InputDevice_XInput(Display* dpy, Window w, const std::string& name);

  virtual ~InputDevice_XInput();

  //!Attributes:
public:
  //: Returns true if the passed key code is down for this device.
  virtual bool get_keycode(int keycode) const;

  //: Returns the the current position of a joystick axis.
  virtual float get_axis(int index) const;

  //: Returns the number of axes available on this device.
  virtual int get_axis_count() const;

  //: Returns the name of the device (i.e. 'Microsoft Sidewinder 3D').
  virtual std::string get_name() const;

  //: Return the hardware id/device for this device (i.e. '/dev/input/js0')
  virtual std::string get_device_name() const;

  //: Returns the number of buttons available on this device.
  //- <p>If used on a keyboard, this function returns -1.</p>
  virtual int get_button_count() const;

  //!Operations:
public:

  //!Implementation:
private:
  int register_events(Display		*dpy,
                      XDeviceInfo	*info,
                      const char		*dev_name,
                      Bool		handle_proximity);

  XDeviceInfo* find_device_info(Display	*display,
                                const char		*name,
                                Bool		only_extended);

  void get_info(XDeviceInfo	*info);
public:
  void on_xevent(Display* dpy, Window w, XEvent &event);
  bool in_proximity() const { return proximity; }
private:
  void on_device_button_event(XDeviceButtonEvent *button);
  void on_device_key_event(XDeviceKeyEvent* key);
  void on_device_motion_event(Display* dpy, Window w, XDeviceMotionEvent* motion);
  void on_proximity_notify_event(XProximityNotifyEvent* prox);

private:
  void received_mouse_input(XEvent &event);
  void received_mouse_move(XEvent &event);
	
  std::string name;

  struct AxisInfo
  {
    int min_value;
    int max_value;
    int resolution;
    int pos;

    AxisInfo(int min_value_, int max_value_, int resolution_)
      : min_value(min_value_), max_value(max_value_), resolution(resolution_), pos(0)
    {}
  };

  std::vector<bool> buttons;

  std::vector<AxisInfo> axis;

  bool absolute;
  int num_keys;	

  bool proximity;
	
  Point mouse_pos;
  Time time_at_last_press;
  int last_press_id;

  bool key_states[5];

  int           motion_type;
  int           button_press_type;
  int           button_release_type;
  int           key_press_type;
  int           key_release_type;
  int           proximity_in_type;
  int           proximity_out_type;
};

#endif

/* EOF */
