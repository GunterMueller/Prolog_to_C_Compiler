#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <assert.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <poll.h>
#include <unistd.h>


static Display *display;
static Window top;
static XFontStruct *fontst;
static GC gc;
static Colormap cmap;
static XColor rgb;
static Atom wm_protocols, wm_delete_window;
static int size_x, size_y;
static char *fontname = "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1";
static int screen;
static XColor colortable[ 256 ];
static int line_height;
static int char_width;


int xt_init(int w, int h, int lh, char *window_name)
{
  char  *server;
  XEvent ev;
  XSetWindowAttributes atr;
  server = (char *)getenv("DISPLAY");

  if(server == NULL) server = "localhost:0.0";

  display = XOpenDisplay(server);

  if(display == NULL) return 0;

  line_height = lh;
  size_x = w;
  size_y = h;
  screen = DefaultScreen(display);
  cmap = DefaultColormap(display, screen);
  XAllocNamedColor(display, cmap, "#34312c", &(colortable[ 0 ]), &rgb);
  XAllocNamedColor(display, cmap, "#d1f8c7", &(colortable[ 1 ]), &rgb);
  top = XCreateSimpleWindow(display, DefaultRootWindow(display), 0,
			    0, size_x, size_y, 2,
			    BlackPixel(display, screen),
			    colortable[ 0 ].pixel);

  if(window_name != NULL)
    XStoreName(display, top, window_name);

  gc = XCreateGC(display, top, 0, 0);
  fontst = XLoadQueryFont(display, fontname);

  if(fontst == NULL) return 0;

  char_width = fontst->max_bounds.rbearing - fontst->min_bounds.lbearing + 1;
  XSetFont(display, gc, fontst->fid);
  wm_protocols = XInternAtom(display, "WM_PROTOCOLS", True);
  wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", True);
  XSetWMProtocols(display, top, &wm_delete_window, 1);
  atr.backing_store = WhenMapped;
  XChangeWindowAttributes(display, top, CWBackingStore, &atr);
  XSelectInput(display, top,
	       ExposureMask | KeyPressMask | KeyReleaseMask | ButtonMotionMask |
	       OwnerGrabButtonMask | ButtonPressMask | ButtonReleaseMask |
	       StructureNotifyMask);
  XResizeWindow(display, top, size_x, size_y);
  XMapWindow(display, top);
  XFlush(display);
  XSetForeground(display, gc, colortable[ 1 ].pixel);  
  XSetBackground(display, gc, colortable[ 0 ].pixel);  

  do {
    XNextEvent(display, &ev);
  } while (ev.type != Expose);

  return 1;
}


static XEvent xevent;


int xt_event()
{
  XNextEvent(display, &xevent);
  return xevent.type;
}


char xt_keyevent(int *keysym, int *state)
{
  char c;
  KeySym sym;
  
  if((XLookupString((XKeyEvent *)&xevent, &c, 1, &sym, NULL)) == 1 ||
      (XK_Home <= sym && sym <= XK_Down)) {
    *keysym = sym;
    *state = xevent.xkey.state;
    return 1;
  }

  return 0;
}


int xt_buttonevent(int *x, int *y, int *state, int *button)
{
  if(xevent.type == ButtonPress || xevent.type == ButtonRelease) {
    *x = xevent.xbutton.x;
    *y = xevent.xbutton.x;
    *state = xevent.xbutton.state;
    *button = xevent.xbutton.button;
    return 1;
  }

  return 0;
}


int xt_client_event()
{
  if(xevent.type == ClientMessage) {
    if(xevent.xclient.message_type == wm_protocols &&
	xevent.xclient.data.l[0] == wm_delete_window) {
      return 1;
    }
  }

  return 0;
}


int xt_resize_event(int *w, int *h, int *rows, int *columns)
{
  if(xevent.type == ConfigureNotify) {
    int sw = xevent.xconfigure.width;
    int sh = xevent.xconfigure.height;

    if(sw != size_x || sh != size_y) {
      size_x = *w = sw;
      size_y = *h = sh;
      *rows = sh / line_height;
      *columns = sw / char_width;
      return 1;
    }
  }

  return 0;
}


void xt_color(int n, char *name)
{
  XAllocNamedColor(display, cmap, name, &(colortable[ n ]), &rgb);
}


void xt_text(int row, int left, char *str)
{
  static char line[ 1024 ];
  int p = left;
  char *lptr, *sptr = str;

  for(lptr = line; *sptr; ++sptr) {
    char c = *sptr;

    if(c == '\t') {
      int d = (p / 8 + 1) * 8;

      while(d--) {
	if(p >= 1023) goto done;
	else if(p >= left) *(lptr++) = ' ';

	++p;
      }
    }
    else {
      if(p >= 1023) goto done;
      else if(p >= left) *(lptr++) = c;

      ++p;
    }
  }

 done:
  XDrawImageString(display, top, gc, 0, row * line_height, line, p - left);
}


void xt_box(int x, int y, int w, int h)
{
  XFillRectangle(display, top, gc, (x - 1) * char_width - x, (y - 1) * line_height, 
		 w * char_width - 1, h * line_height);
}


void xt_foreground(int n) 
{
  XSetForeground(display, gc, colortable[ n ].pixel);
}


void xt_background(int n)
{
  XSetBackground(display, gc, colortable[ n ].pixel);  
}


void xt_size(int *r, int *c)
{
  *r = size_y / line_height;
  *c = size_x / char_width;
}
