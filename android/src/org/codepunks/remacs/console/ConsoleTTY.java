// Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
//
// This file is part of remacs.
//
// remacs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// remacs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with remacs.  If not, see <http://www.gnu.org/licenses/>.

//
// Author: Chris Newton <redshodan@gmail.com>
// $Revision$
//

package org.codepunks.remacs.console;


import android.util.Log;
import android.view.KeyCharacterMap;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnKeyListener;

import de.mud.terminal.VDUBuffer;
import de.mud.terminal.VDUDisplay;
import de.mud.terminal.VDUInput;
import de.mud.terminal.vt320;

import java.io.IOException;

import org.codepunks.remacs.Connection;
import org.codepunks.remacs.RemacsCfg;


public class ConsoleTTY implements VDUDisplay, OnKeyListener
{
    protected static final String TAG = "Remacs";

	public final static int MOD_CTRL_ON = 0x01;
	public final static int MOD_CTRL_LOCK = 0x02;
	public final static int MOD_ALT_ON = 0x04;
	public final static int MOD_ALT_LOCK = 0x08;
	public final static int MOD_SHIFT_ON = 0x10;
	public final static int MOD_SHIFT_LOCK = 0x20;
	public final static int MOD_SLASH = 0x40;
	public final static int MOD_TAB = 0x80;
	public final static int MOD_CTRL_MASK = MOD_CTRL_ON | MOD_CTRL_LOCK;
	public final static int MOD_ALT_MASK = MOD_ALT_ON | MOD_ALT_LOCK;
	public final static int MOD_SHIFT_MASK = MOD_SHIFT_ON | MOD_SHIFT_LOCK;
	public final static int MOD_TRANSIENT_MASK =
        MOD_CTRL_ON | MOD_ALT_ON | MOD_SHIFT_ON;
    public final static int MOD_FAKE_MASK = MOD_SLASH | MOD_TAB;

    protected ConsoleView mView;
    protected Connection mConn;
	protected ConsoleBuffer mBuffer;
    protected int mModifiers;
    protected Integer[] mColors;
    
    public ConsoleTTY(ConsoleView view, Connection conn)
    {
        mView = view;
        mConn = conn;
        mBuffer = new ConsoleBuffer(mConn);
        mBuffer.setDisplay(this);
        mBuffer.setBufferSize(mConn.getConfig().term_scrollback);
        mBuffer.setTerminalID(mConn.getConfig().term);
        mBuffer.setAnswerBack(mConn.getConfig().term);
        mBuffer.setBackspace(vt320.DELETE_IS_DEL);

        resetColors();
    }

    public void putString(String str)
    {
        mBuffer.putString(str);
    }

    public void putString(char[] s, byte[] wattrs, int length)
    {
        mBuffer.putString(s, wattrs, 0, length);
        redraw();
    }

    public ConsoleBuffer getBuffer()
    {
        return mBuffer;
    }
    
    public Integer[] getColors()
    {
        return mColors;
    }

    public boolean isShifted()
    {
        return (mModifiers & MOD_SHIFT_ON) != 0;
    }

    public boolean isShiftLocked()
    {
        return (mModifiers & MOD_SHIFT_LOCK) != 0;
    }

    public boolean isCtrled()
    {
        return (mModifiers & MOD_CTRL_ON) != 0;
    }

    public boolean isCtrlLocked()
    {
        return (mModifiers & MOD_CTRL_LOCK) != 0;
    }

    public boolean isAlted()
    {
        return (mModifiers & MOD_ALT_ON) != 0;
    }

    public boolean isAltLocked()
    {
        return (mModifiers & MOD_ALT_LOCK) != 0;
    }

    public int vduModifiers()
    {
        int ret = 0;
        
        if ((mModifiers & VDUInput.KEY_CONTROL) != 0)
            ret |= VDUInput.KEY_CONTROL;
        if ((mModifiers & VDUInput.KEY_SHIFT) != 0)
            ret |= VDUInput.KEY_SHIFT;
        if ((mModifiers & VDUInput.KEY_ALT) != 0)
            ret |= VDUInput.KEY_ALT;
        mModifiers &= ~MOD_TRANSIENT_MASK;
        
        return ret;
    }
    
    /*
     * OnKeyListener interface
     */
    public boolean onKey(View v, int keycode, KeyEvent event)
    {
        // int keychar = event.getUnicodeChar();
        int modchar = 0;
        int mods = 0;
        boolean shifted = false;
        // boolean alted = false;
        boolean ctrled = false;
        if ((mModifiers & MOD_SHIFT_MASK) != 0)
        {
            mods |= KeyEvent.META_SHIFT_ON;
            shifted = true;
        }
        if ((mModifiers & MOD_ALT_MASK) != 0)
        {
            mods |= KeyEvent.META_ALT_ON;
            // alted = true;
        }
        if ((mModifiers & MOD_CTRL_ON) != 0)
        {
            ctrled = true;
        }
        modchar = event.getUnicodeChar(mods);
        
        // Log.d(TAG, String.format(
        //           "onKey: %s: %d/%d prints=%d %d='%c'/%d='%c' mods=%d/%d",
        //           event.toString(), keycode, event.getKeyCode(),
        //           (event.isPrintingKey() ? 1 : 0),
        //           keychar, keychar, modchar, modchar, mModifiers, mods));
        
        // Ignore all up events
        if (event.getAction() == KeyEvent.ACTION_UP)
        {
            return false;
        }
        
        try
        {
            switch (keycode)
            {
            case KeyEvent.KEYCODE_CAMERA:
                break;
            case KeyEvent.KEYCODE_DEL:
                mBuffer.keyPressed(vt320.KEY_BACK_SPACE, ' ', vduModifiers());
                mModifiers &= ~MOD_TRANSIENT_MASK;
                return true;
            case KeyEvent.KEYCODE_ENTER:
                mBuffer.keyTyped(vt320.KEY_ENTER, ' ', 0);
                mModifiers &= ~MOD_TRANSIENT_MASK;
                return true;
            case KeyEvent.KEYCODE_DPAD_LEFT:
                mBuffer.keyPressed(vt320.KEY_LEFT, ' ', vduModifiers());
                mView.vibrate();
                return true;
            case KeyEvent.KEYCODE_DPAD_UP:
                mBuffer.keyPressed(vt320.KEY_UP, ' ', vduModifiers());
                mView.vibrate();
                return true;
            case KeyEvent.KEYCODE_DPAD_DOWN:
                mBuffer.keyPressed(vt320.KEY_DOWN, ' ', vduModifiers());
                mView.vibrate();
                return true;
            case KeyEvent.KEYCODE_DPAD_RIGHT:
                mBuffer.keyPressed(vt320.KEY_RIGHT, ' ', vduModifiers());
                mView.vibrate();
                return true;
            case KeyEvent.KEYCODE_DPAD_CENTER:
                if (ctrled)
                {
                    mBuffer.keyTyped(vt320.KEY_ESCAPE, ' ', 0);
                    mModifiers &= ~MOD_CTRL_ON;
                    mView.vibrate();
                }
                else
                {
                    mModifiers |= MOD_CTRL_ON;
                }
                redraw();
                return true;
            case KeyEvent.KEYCODE_ALT_LEFT:
                modPress(MOD_ALT_ON);
                return true;
            case KeyEvent.KEYCODE_ALT_RIGHT:
                mConn.sendData(0x9); // TAB
                return true;
            case KeyEvent.KEYCODE_SHIFT_LEFT:
                modPress(MOD_SHIFT_ON);
                return true;
            case KeyEvent.KEYCODE_SHIFT_RIGHT:
                mConn.sendData("/");
                return true;
            case KeyEvent.KEYCODE_1:
            case KeyEvent.KEYCODE_2:
            case KeyEvent.KEYCODE_3:
            case KeyEvent.KEYCODE_4:
            case KeyEvent.KEYCODE_5:
            case KeyEvent.KEYCODE_6:
            case KeyEvent.KEYCODE_7:
            case KeyEvent.KEYCODE_8:
            case KeyEvent.KEYCODE_9:
                if (shifted)
                {
                    mBuffer.keyPressed(
                        vt320.KEY_F1 + keycode - KeyEvent.KEYCODE_1, ' ', 0);
                    return true;
                }
                break;
            case KeyEvent.KEYCODE_0:
                if (shifted)
                {
                    mBuffer.keyPressed(vt320.KEY_F10, ' ', 0);
                    return true;
                }
                break;
            }

            if (event.isPrintingKey() || (keycode == KeyEvent.KEYCODE_SPACE))
            {
                mModifiers &= ~MOD_TRANSIENT_MASK;

                if (ctrled)
                {
                    // CTRL-a through CTRL-z
                    if ((modchar >= 0x61) && (modchar <= 0x7A))
                    {
                        modchar -= 0x60;
                    }
                    // CTRL-A through CTRL-_
                    else if ((modchar >= 0x41) && (modchar <= 0x5F))
                    {
                        modchar -= 0x40;
                    }
                    else if (modchar == 0x20)
                    {
                        modchar = 0x00;
                    }
                    else if (modchar == 0x3F)
                    {
                        modchar = 0x7F;
                    }
                }
                                
                if (modchar < 0x80)
                {
                    mConn.sendData(modchar);
                }
                else
                {
                    mConn.sendData(new String(Character.toChars(modchar))
                                        .getBytes(mConn.getConfig().charset));
                }
                return true;
            }
            else if ((keycode == KeyEvent.KEYCODE_UNKNOWN) &&
                     (event.getAction() == KeyEvent.ACTION_MULTIPLE))
            {
                mConn.sendData(
                    event.getCharacters().getBytes(mConn.getConfig().charset));
                return true;
            }
        }
        catch (IOException ex)
        {
            Log.w(TAG, "Failure writing to transport");
            Log.w(TAG, ex);
        }
        
        return false;
    }

	/**
	 * Handle meta key presses where the key can be locked on.
	 * <p>
	 * 1st press: next key to have meta state<br />
	 * 2nd press: meta state is locked on<br />
	 * 3rd press: disable meta state
	 *
	 * @param code
	 */
	private void modPress(int code)
    {
		if ((mModifiers & (code << 1)) != 0)
        {
			mModifiers &= ~(code << 1);
		}
        else if ((mModifiers & code) != 0)
        {
			mModifiers &= ~code;
			mModifiers |= code << 1;
		}
        else
        {
			mModifiers |= code;
        }
        redraw();
	}

    /*
     * VDUDisplay interface
     */
    public void redraw()
    {
		if (mView != null)
        {
			mView.postInvalidate();
        }
	}

    public void updateScrollBar()
    {
	}

    public void setVDUBuffer(VDUBuffer buffer)
    {
	}

    public VDUBuffer getVDUBuffer()
    {
        return mBuffer;
	}

    public void setColor(int index, int red, int green, int blue)
    {
        Log.d(TAG, String.format("ConsoleTTY.Buffer.setColor: %d: (%d,%d,%d)",
                                 index, red, green, blue));
        if (index < mColors.length && index >= 16)
        {
            mColors[index] = 0xff000000 | red << 16 | green << 8 | blue;
        }
	}

    public void resetColors()
    {
        mColors = Colors.DEFAULT_COLORS.clone();
	}
}
