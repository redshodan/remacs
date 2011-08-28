/*      common/idle.c
 *
 * Gajim Team:
 *      - Yann Le Boulanger <asterix@lagaule.org>
 *
 * Copyright (C) 2003-2004 Yann Le Boulanger <asterix@lagaule.org>
 *                         Vincent Hanquez <tab@snarc.org>
 * Copyright (C) 2005-2006 Yann Le Boulanger <asterix@lagaule.org>
 *                    Nikos Kouremenos <nkour@jabber.org>
 *
 * This file is part of Gajim.
 *
 * Gajim is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; version 3 only.
 *
 * Gajim is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public Licens
 * along with Gajim.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <Python.h>

#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/extensions/scrnsaver.h>
#include <X11/extensions/XTest.h>


Display *display;
Window xscreensaver;
Window win_sink;
Atom atomScreenSaver;
Atom atomActivate;
Atom atomDeactivate;


static PyObject * idle_init(PyObject *self, PyObject *args)
{
    display = XOpenDisplay(NULL);

    win_sink = XCreateWindow(display, DefaultRootWindow(display), 0, 0, 1, 1,
                             0, 0, InputOnly, CopyFromParent, 0, NULL);
    if (win_sink == None)
    {
        printf("Failed to create window\n");
    }
    
    atomScreenSaver = XInternAtom(display, "SCREENSAVER", False);
    atomActivate = XInternAtom(display, "ACTIVATE", False);
    atomDeactivate = XInternAtom(display, "DEACTIVATE", False);
    
    // find top-level window xscreensaver window
    Atom ssVer = XInternAtom(display, "_SCREENSAVER_VERSION", False);
    Window root = DefaultRootWindow(display);
    Window rw, pw, *cw;
    unsigned int nc;
    if (XQueryTree(display, root, &rw, &pw, &cw, &nc))
    {
        unsigned int i;
        for (i = 0; i < nc; ++i)
        {
            Atom type;
            unsigned long bytesLeft = 1;
            unsigned long numItems;
            unsigned char* rawData;
            int size;
            if ((XGetWindowProperty(display, cw[i], ssVer, 0, 0, False,
                                    type, &type, &size, &numItems,
                                    &bytesLeft, &rawData) == Success) &&
                (type == XA_STRING))
            {
                xscreensaver = cw[i];
                break;
            }
        }
        XFree(cw);
    }
        
    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject * idle_getIdleSec(PyObject *self, PyObject *args)
{
    static XScreenSaverInfo *mit_info = NULL;
    int idle_time, event_base, error_base;
    
    if (XScreenSaverQueryExtension(display, &event_base, &error_base))
    {
        if (mit_info == NULL)
        {
            mit_info = XScreenSaverAllocInfo();
        }
        XScreenSaverQueryInfo(display, RootWindow(display, 0), mit_info);
        idle_time = (mit_info->idle) / 1000;
    }
    else
    {
        idle_time = 0;
    }
    
    return Py_BuildValue("i", idle_time);
}

static PyObject * idle_unIdle(PyObject *self, PyObject *args)
{
    // Try a couple different ways of reseting the idle timer, ugh.

    // Send a client message towards the screen saver. This works
    // with xscreensaver.
    {
        XEvent event;
        event.xclient.type         = ClientMessage;
        event.xclient.display      = display;
        event.xclient.window       = win_sink;
        event.xclient.message_type = atomScreenSaver;
        event.xclient.format       = 32;
        event.xclient.data.l[0]    = (long)atomDeactivate;
        event.xclient.data.l[1]    = 0;
        event.xclient.data.l[2]    = 0;
        event.xclient.data.l[3]    = 0;
        event.xclient.data.l[4]    = 0;
    
        XSendEvent(display, xscreensaver, False, 0, &event);
    }

    // Fake up a mouse motion event.
    static int pos = 0;
    {
        XEvent event;
        event.xmotion.type         = MotionNotify;
        event.xmotion.display      = display;
        event.xmotion.window       = xscreensaver;
        event.xmotion.root         = DefaultRootWindow(display);
        event.xmotion.subwindow    = None;
        event.xmotion.time         = CurrentTime;
        event.xmotion.x            = pos;
        event.xmotion.y            = 0;
        event.xmotion.x_root       = pos;
        event.xmotion.y_root       = 0;
        event.xmotion.state        = 0;
        event.xmotion.is_hint      = NotifyNormal;
        event.xmotion.same_screen  = True;

        pos = 20 - pos;

        XSendEvent(display, xscreensaver, True, 0, &event);
    }

    // Xidle (I think?)
    XResetScreenSaver(display);

    // MIT screensaver
    XScreenSaverSuspend(display, True);
    XScreenSaverSuspend(display, False);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyObject * idle_close(PyObject *self, PyObject *args)
{
    XCloseDisplay(display);

    Py_INCREF(Py_None);
    return Py_None;
}

static PyMethodDef idleMethods[] =
{
    {"init",  idle_init, METH_NOARGS, "init idle"},
    {"getIdleSec",  idle_getIdleSec, METH_NOARGS,
     "Give the idle time in secondes"},
    {"unIdle",  idle_unIdle, METH_NOARGS,
     "Fake event to unidle the X session"},
    {"close",  idle_close, METH_NOARGS, "close idle"},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initidle(void)
{
    (void) Py_InitModule("idle", idleMethods);
}
