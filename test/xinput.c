#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define INVALID_EVENT_TYPE	-1

static int           motion_type         = INVALID_EVENT_TYPE;
static int           button_press_type   = INVALID_EVENT_TYPE;
static int           button_release_type = INVALID_EVENT_TYPE;
static int           key_press_type      = INVALID_EVENT_TYPE;
static int           key_release_type    = INVALID_EVENT_TYPE;
static int           proximity_in_type   = INVALID_EVENT_TYPE;
static int           proximity_out_type  = INVALID_EVENT_TYPE;

static void
print_events(Display	*dpy) 
{
    XEvent        Event;
    
    while(1) {
	XNextEvent(dpy, &Event);
	
	if (Event.type == motion_type) {
	    int	loop;    
	    XDeviceMotionEvent *motion = (XDeviceMotionEvent *) &Event;

	    printf("motion ");
	    
	    for(loop=0; loop<motion->axes_count; loop++) {
		printf("a[%d]=%d ", motion->first_axis + loop, motion->axis_data[loop]);
	    }
	    printf("\n");
	} else if ((Event.type == button_press_type) ||
		   (Event.type == button_release_type)) {
	    int	loop;
	    XDeviceButtonEvent *button = (XDeviceButtonEvent *) &Event;
	    
	    printf("button %s %d ", (Event.type == button_release_type) ? "release" : "press  ",
		   button->button);
	    
	    for(loop=0; loop<button->axes_count; loop++) {
		printf("a[%d]=%d ", button->first_axis + loop, button->axis_data[loop]);
	    }
	    printf("\n");
	} else if ((Event.type == key_press_type) ||
		   (Event.type == key_release_type)) {
	    int	loop;
	    XDeviceKeyEvent *key = (XDeviceKeyEvent *) &Event;
	    
	    printf("key %s %d ", (Event.type == key_release_type) ? "release" : "press  ",
		   key->keycode);
	    
	    for(loop=0; loop<key->axes_count; loop++) {
		printf("a[%d]=%d ", key->first_axis + loop, key->axis_data[loop]);
	    }
	    printf("\n");
	} else if ((Event.type == proximity_out_type) ||
		   (Event.type == proximity_in_type)) {
	    int	loop;
	    XProximityNotifyEvent *prox = (XProximityNotifyEvent *) &Event;
	    
	    printf("proximity %s ", (Event.type == proximity_in_type) ? "in " : "out");
	    
	    for(loop=0; loop<prox->axes_count; loop++) {
		printf("a[%d]=%d ", prox->first_axis + loop, prox->axis_data[loop]);
	    }
	    printf("\n");
	}
	else {
	    printf("what's that %d\n", Event.type);
	}
    }
}

XDeviceInfo*
find_device_info(Display	*display,
		 char		*name,
		 Bool		only_extended)
{
    XDeviceInfo	*devices;
    int		loop;
    int		num_devices;
    int		len = strlen(name);
    Bool	is_id = True;
    XID		id;
    
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

static Bool
is_xinput_present(Display	*display)
{
    XExtensionVersion	*version;
    Bool		present;
    
    version = XGetExtensionVersion(display, INAME);

    if (version && (version != (XExtensionVersion*) NoSuchExtension)) {
	present = version->present;
	XFree(version);
	return present;
    } else {
	return False;
    }
}

static int
register_events(Display		*dpy,
		XDeviceInfo	*info,
		char		*dev_name,
		Bool		handle_proximity)
{
    int			number = 0;	/* number of events registered */
    XEventClass		event_list[7];
    int			i;
    XDevice		*device;
    Window		root_win;
    unsigned long	screen;
    XInputClassInfo	*ip;

    screen = DefaultScreen(dpy);
    root_win = RootWindow(dpy, screen);

    device = XOpenDevice(dpy, info->id);

    if (!device) {
	fprintf(stderr, "unable to open device %s\n", dev_name);
	return 0;
    }
    
    if (device->num_classes > 0) {
	for (ip = device->classes, i=0; i<info->num_classes; ip++, i++) {
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

	if (XSelectExtensionEvent(dpy, root_win, event_list, number)) {
	    fprintf(stderr, "error selecting extended events\n");
	    return 0;
	}
    }
    return number;
}


static void
print_info(XDeviceInfo	*info)
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
	    switch (any->class) {
	    case KeyClass:
		k = (XKeyInfoPtr) any;
		printf("\tNum_keys is %d\n", k->num_keys);
		printf("\tMin_keycode is %d\n", k->min_keycode);
		printf("\tMax_keycode is %d\n", k->max_keycode);
		break;

	    case ButtonClass:
		b = (XButtonInfoPtr) any;
		printf("\tNum_buttons is %d\n", b->num_buttons);
		break;

	    case ValuatorClass:
		v = (XValuatorInfoPtr) any;
		a = (XAxisInfoPtr) ((char *) v + 
				    sizeof (XValuatorInfo));
		printf("\tNum_axes is %d\n", v->num_axes);
		printf("\tMode is %s\n", (v->mode == Absolute) ? "Absolute" : "Relative");
		printf("\tMotion_buffer is %ld\n", v->motion_buffer);
		for (j=0; j<v->num_axes; j++, a++) {
		    printf("\tAxis %d :\n", j);
		    printf("\t\tMin_value is %d\n", a->min_value);
		    printf("\t\tMax_value is %d\n", a->max_value);
		    printf ("\t\tResolution is %d\n", a->resolution);
		}
		break;
		
	    default:
		printf ("unknown class\n");
	    }
	    any = (XAnyClassPtr) ((char *) any + any->length);
	}
    }
}

/* Simply Xinput test application */
int main()
{
  Display	*display;

  display = XOpenDisplay(NULL);

  if (!is_xinput_present(display)) {
    fprintf(stderr, "%s extension not available\n", INAME);
    return EXIT_FAILURE;
  }

  {
    char* mydev = "gstylus";
    //char* mydev = "USBMouse0";

    XDeviceInfo		*info;
    info = find_device_info(display, mydev, True);

    print_info(info);


    /*    printf("id:   %d\n", (int)info->id);
    printf("name: %s\n", info->name);
    printf("numclasses: %d\n\n", info->num_classes);

    for (int i=0; i < info->num_classes; ++i) {
      printf("i=%d\n", i);
      printf("beast: %d\n", (int)(info->inputclassinfo[i].class));
      switch (info->inputclassinfo[i].class)
        {
        case KeyClass:
          puts("\nKeyclass");
          XKeyInfo* k = (XKeyInfo*)(info->inputclassinfo+i);
          printf("minkey: %d\n", k->min_keycode);
          printf("maxkey: %d\n", k->max_keycode);
          printf("numkey: %d\n", k->num_keys);
          break;

        case ButtonClass:
          puts("\nButtonclass");
          XButtonInfo* b = (XButtonInfo*)(info->inputclassinfo+i);
          printf("numbuttons: %d\n", b->num_buttons);
          break;
        
        case ValuatorClass:
          puts("\nValuatorClass");
          XValuatorInfo* v = (XValuatorInfo*)(info->inputclassinfo+i);
          for (int j=0; j < v->num_axes; ++j)
            {
              printf("res: %d\n", v->axes[j].resolution);
              printf("min: %d\n", v->axes[j].min_value);
              printf("max: %d\n", v->axes[j].max_value);
            }
          break;
        default:
          printf("Unknown beast: %d\n", (int)(info->inputclassinfo[i].class));
        }
        }*/

    if (!info) {
      fprintf(stderr, "unable to find device %s\n", mydev);
      return EXIT_FAILURE;
    } else {
      if (register_events(display, info, mydev, False)) {
        print_events(display);
      }
      else {
        fprintf(stderr, "no event registered...\n");
        return EXIT_FAILURE;
      }
    }
  }
  return 0;
}

/* EOF */
