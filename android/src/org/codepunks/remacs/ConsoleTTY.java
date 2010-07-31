package org.codepunks.remacs;


import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.graphics.Bitmap.Config;
import android.graphics.Paint.FontMetrics;
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

	public final static int DEFAULT_FG_COLOR = 7;
	public final static int DEFAULT_BG_COLOR = 0;
	public final static Integer[] DEFAULT_COLORS = new Integer[]
    {
    0xff000000, // black
    0xffcc0000, // red
    0xff00cc00, // green
    0xffcccc00, // brown
    0xff0000cc, // blue
    0xffcc00cc, // purple
    0xff00cccc, // cyan
    0xffcccccc, // light grey
    0xff444444, // dark grey
    0xffff4444, // light red
    0xff44ff44, // light green
    0xffffff44, // yellow
    0xff4444ff, // light blue
    0xffff44ff, // light purple
    0xff44ffff, // light cyan
    0xffffffff, // white
    0xff000000, 0xff00005f, 0xff000087, 0xff0000af, 0xff0000d7,
    0xff0000ff, 0xff005f00, 0xff005f5f, 0xff005f87, 0xff005faf,
    0xff005fd7, 0xff005fff, 0xff008700, 0xff00875f, 0xff008787,
    0xff0087af, 0xff0087d7, 0xff0087ff, 0xff00af00, 0xff00af5f,
    0xff00af87, 0xff00afaf, 0xff00afd7, 0xff00afff, 0xff00d700,
    0xff00d75f, 0xff00d787, 0xff00d7af, 0xff00d7d7, 0xff00d7ff,
    0xff00ff00, 0xff00ff5f, 0xff00ff87, 0xff00ffaf, 0xff00ffd7,
    0xff00ffff, 0xff5f0000, 0xff5f005f, 0xff5f0087, 0xff5f00af,
    0xff5f00d7, 0xff5f00ff, 0xff5f5f00, 0xff5f5f5f, 0xff5f5f87,
    0xff5f5faf, 0xff5f5fd7, 0xff5f5fff, 0xff5f8700, 0xff5f875f,
    0xff5f8787, 0xff5f87af, 0xff5f87d7, 0xff5f87ff, 0xff5faf00,
    0xff5faf5f, 0xff5faf87, 0xff5fafaf, 0xff5fafd7, 0xff5fafff,
    0xff5fd700, 0xff5fd75f, 0xff5fd787, 0xff5fd7af, 0xff5fd7d7,
    0xff5fd7ff, 0xff5fff00, 0xff5fff5f, 0xff5fff87, 0xff5fffaf,
    0xff5fffd7, 0xff5fffff, 0xff870000, 0xff87005f, 0xff870087,
    0xff8700af, 0xff8700d7, 0xff8700ff, 0xff875f00, 0xff875f5f,
    0xff875f87, 0xff875faf, 0xff875fd7, 0xff875fff, 0xff878700,
    0xff87875f, 0xff878787, 0xff8787af, 0xff8787d7, 0xff8787ff,
    0xff87af00, 0xff87af5f, 0xff87af87, 0xff87afaf, 0xff87afd7,
    0xff87afff, 0xff87d700, 0xff87d75f, 0xff87d787, 0xff87d7af,
    0xff87d7d7, 0xff87d7ff, 0xff87ff00, 0xff87ff5f, 0xff87ff87,
    0xff87ffaf, 0xff87ffd7, 0xff87ffff, 0xffaf0000, 0xffaf005f,
    0xffaf0087, 0xffaf00af, 0xffaf00d7, 0xffaf00ff, 0xffaf5f00,
    0xffaf5f5f, 0xffaf5f87, 0xffaf5faf, 0xffaf5fd7, 0xffaf5fff,
    0xffaf8700, 0xffaf875f, 0xffaf8787, 0xffaf87af, 0xffaf87d7,
    0xffaf87ff, 0xffafaf00, 0xffafaf5f, 0xffafaf87, 0xffafafaf,
    0xffafafd7, 0xffafafff, 0xffafd700, 0xffafd75f, 0xffafd787,
    0xffafd7af, 0xffafd7d7, 0xffafd7ff, 0xffafff00, 0xffafff5f,
    0xffafff87, 0xffafffaf, 0xffafffd7, 0xffafffff, 0xffd70000,
    0xffd7005f, 0xffd70087, 0xffd700af, 0xffd700d7, 0xffd700ff,
    0xffd75f00, 0xffd75f5f, 0xffd75f87, 0xffd75faf, 0xffd75fd7,
    0xffd75fff, 0xffd78700, 0xffd7875f, 0xffd78787, 0xffd787af,
    0xffd787d7, 0xffd787ff, 0xffd7af00, 0xffd7af5f, 0xffd7af87,
    0xffd7afaf, 0xffd7afd7, 0xffd7afff, 0xffd7d700, 0xffd7d75f,
    0xffd7d787, 0xffd7d7af, 0xffd7d7d7, 0xffd7d7ff, 0xffd7ff00,
    0xffd7ff5f, 0xffd7ff87, 0xffd7ffaf, 0xffd7ffd7, 0xffd7ffff,
    0xffff0000, 0xffff005f, 0xffff0087, 0xffff00af, 0xffff00d7,
    0xffff00ff, 0xffff5f00, 0xffff5f5f, 0xffff5f87, 0xffff5faf,
    0xffff5fd7, 0xffff5fff, 0xffff8700, 0xffff875f, 0xffff8787,
    0xffff87af, 0xffff87d7, 0xffff87ff, 0xffffaf00, 0xffffaf5f,
    0xffffaf87, 0xffffafaf, 0xffffafd7, 0xffffafff, 0xffffd700,
    0xffffd75f, 0xffffd787, 0xffffd7af, 0xffffd7d7, 0xffffd7ff,
    0xffffff00, 0xffffff5f, 0xffffff87, 0xffffffaf, 0xffffffd7,
    0xffffffff, 0xff080808, 0xff121212, 0xff1c1c1c, 0xff262626,
    0xff303030, 0xff3a3a3a, 0xff444444, 0xff4e4e4e, 0xff585858,
    0xff626262, 0xff6c6c6c, 0xff767676, 0xff808080, 0xff8a8a8a,
    0xff949494, 0xff9e9e9e, 0xffa8a8a8, 0xffb2b2b2, 0xffbcbcbc,
    0xffc6c6c6, 0xffd0d0d0, 0xffdadada, 0xffe4e4e4, 0xffeeeeee,
	};

    protected class Buffer extends vt320
    {
        @Override public void write(byte[] b)
        {
        }
        @Override public void write(int b)
        {
        }
        @Override public void sendTelnetCommand(byte cmd)
        {
        }
        @Override public void setWindowSize(int c, int r)
        {
        }
        @Override public void debug(String s)
        {
        }
    };

    protected ConsoleView mView;
    protected ConnectionCfg mCfg;
    protected Transport mTransport;
	protected Buffer mBuffer;
	protected Bitmap mBitmap;
    protected Canvas mCanvas;
    protected boolean mFullRedraw;
    protected Paint mPaint;
	protected int mCharWidth;
	protected int mCharHeight;
	protected int mCharTop;
    protected Integer[] mColors;
    protected int mModifiers;
	protected KeyCharacterMap mKeymap;
    
    public ConsoleTTY(ConsoleView view, ConnectionCfg cfg)
    {
        mView = view;
        mCfg = cfg;
        mCanvas = new Canvas();
        mBuffer = new Buffer();
        mBuffer.setDisplay(this);
        mBuffer.setBufferSize(mCfg.term_scrollback);
        mBuffer.setTerminalID(mCfg.term);
        mBuffer.setAnswerBack(mCfg.term);

        mPaint = new Paint();
		mPaint.setAntiAlias(true);
		mPaint.setTypeface(Typeface.MONOSPACE);
		mPaint.setFakeBoldText(true);

        mModifiers = 0;
        mKeymap = KeyCharacterMap.load(KeyCharacterMap.BUILT_IN_KEYBOARD);
        resetColors();
    }

    public void setTransport(Transport transport)
    {
        mTransport = transport;
    }

    public void putString(String str)
    {
        mBuffer.putString(str);
    }

    public void putString(char[] s, byte[] wattrs, int start, int len)
    {
        mBuffer.putString(s, wattrs, start, len);
    }

    public void getTextWidths(char[] chars, int offset, float[] widths)
    {
        mPaint.getTextWidths(chars, 0, offset, widths);
    }

    public int getCharWidth()
    {
        return mCharWidth;
    }
    
    public void onSizeChanged(ConsoleView view)
    {
        Log.d(TAG, "ConsoleTTY.Buffer.onSizeChanged");

        mView = view;

        int width = view.getWidth();
        int height = view.getHeight();
        if ((mBitmap == null) ||
            ((mBitmap.getWidth() != width) || (mBitmap.getHeight() != height)))
        {
            if (mBitmap != null)
            {
                mBitmap.recycle();
            }
			mBitmap = Bitmap.createBitmap(width, height, Config.ARGB_8888);
			mCanvas.setBitmap(mBitmap);
		}

		FontMetrics fm = mPaint.getFontMetrics();
		mCharTop = (int)Math.ceil(fm.top);
		float[] widths = new float[1];
		mPaint.getTextWidths("X", widths);
		mCharWidth = (int)Math.ceil(widths[0]);
		mCharHeight = (int)Math.ceil(fm.descent - fm.top);
        mCfg.term_width = width / mCharWidth;
        mCfg.term_height = height / mCharHeight;

        Log.d(TAG, String.format("Setting term size (%d,%d) (%d,%d)",
                                 width, height,
                                 mCfg.term_width, mCfg.term_height));
        mBuffer.setScreenSize(mCfg.term_width, mCfg.term_height, false);
        
        mFullRedraw = true;
    }

    public Bitmap onDraw(Canvas pcanvas)
    {
        boolean redraw = false;
        if (mBuffer.update[0] || mFullRedraw)
        {
            redraw = true;
        }

        mCanvas.drawPaint(mPaint);

        for (int l = 0; l < mBuffer.height; ++l)
        {
            if (!redraw && !mBuffer.update[l + 1])
            {
                continue;
            }

            for (int c = 0; c < mBuffer.width; ++c)
            {
                int fg;
                int bg;
                int addr = 0;
                int currAttr = mBuffer.charAttributes[mBuffer.windowBase + l][c];
                int fgcolor = DEFAULT_FG_COLOR;

                // // check if foreground color attribute is set
                if ((currAttr & VDUBuffer.COLOR_FG) != 0)
                {
                    fgcolor = ((currAttr & VDUBuffer.COLOR_FG) >>
                               VDUBuffer.COLOR_FG_SHIFT) - 1;
                }

                if (fgcolor < 8 && (currAttr & VDUBuffer.BOLD) != 0)
                {
                    fg = mColors[fgcolor + 8];
                }
                else
                {
                    fg = mColors[fgcolor];
                }

                // check if background color attribute is set
                if ((currAttr & VDUBuffer.COLOR_BG) != 0)
                {
                    bg = mColors[((currAttr & VDUBuffer.COLOR_BG) >>
                                VDUBuffer.COLOR_BG_SHIFT) - 1];
                }
                else
                {
                    bg = mColors[DEFAULT_BG_COLOR];
                }

                // support character inversion by swapping background and
                // foreground color
                if ((currAttr & VDUBuffer.INVERT) != 0)
                {
                    int swapc = bg;
                    bg = fg;
                    fg = swapc;
                }

                // set underlined attributes if requested
                mPaint.setUnderlineText((currAttr & VDUBuffer.UNDERLINE) != 0);

                boolean isWideCharacter = (currAttr & VDUBuffer.FULLWIDTH) != 0;
                if (isWideCharacter)
                {
                    addr++;
                }
                else
                {
                    // determine the amount of continuous characters with the
                    // same settings and print them all at once
                    while ((c + addr < mBuffer.width) &&
                       (mBuffer.charAttributes[mBuffer.windowBase + l][c + addr]
                        == currAttr))
                    {
                        addr++;
                    }
                }

                // Save the current clip region
                mCanvas.save(Canvas.CLIP_SAVE_FLAG);

                // clear this dirty area with background color
                mPaint.setColor(bg);
                if (isWideCharacter)
                {
                    mCanvas.clipRect(c * mCharWidth,
                                     l * mCharHeight,
                                     (c + 2) * mCharWidth,
                                     (l + 1) * mCharHeight);
                } else {
                    mCanvas.clipRect(c * mCharWidth,
                                     l * mCharHeight,
                                     (c + addr) * mCharWidth,
                                     (l + 1) * mCharHeight);
                }
                mCanvas.drawPaint(mPaint);

                // write the text string starting at 'c' for 'addr' number of
                // characters
                mPaint.setColor(fg);
                if ((currAttr & VDUBuffer.INVISIBLE) == 0)
                {
                    mCanvas.drawText(mBuffer.charArray[mBuffer.windowBase + l],
                                     c, addr, c * mCharWidth,
                                     (l * mCharHeight) - mCharTop, mPaint);
                }

                // Restore the previous clip region
                mCanvas.restore();

                // advance to the next text block with different characteristics
                c += addr - 1;
                if (isWideCharacter)
                {
                    c++;
                }
            }

            mBuffer.update[l] = false;
        }

        mBuffer.update[0] = false;

        return mBitmap;
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
    @Override public boolean onKey(View v, int keycode, KeyEvent event)
    {
        Log.d(TAG, String.format("onKey: %d %s", keycode, event.toString()));
        
        // hard buttons. Only pay attention to keyups for hard buttons.
        if (event.getAction() == KeyEvent.ACTION_UP)
        {
            return false;
        }
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
            return false;
        }

        if (mKeymap.isPrintingKey(keycode) ||
            (keycode == KeyEvent.KEYCODE_SPACE))
        {
            int mods = 0;
            if ((mModifiers & MOD_SHIFT_MASK) != 0)
            {
                mods |= KeyEvent.META_SHIFT_ON;
                mModifiers &= ~MOD_SHIFT_ON;
            }

            if ((mModifiers & MOD_ALT_MASK) != 0)
            {
                mods |= KeyEvent.META_ALT_ON;
                mModifiers &= ~MOD_ALT_ON;
            }

            int key = mKeymap.get(keycode, mods);
            if ((mModifiers & MOD_CTRL_MASK) != 0)
            {
                mModifiers &= ~MOD_CTRL_ON;

                // CTRL-a through CTRL-z
                if (key >= 0x61 && key <= 0x7A)
                {
                    key -= 0x60;
                }
                // CTRL-A through CTRL-_
                else if (key >= 0x41 && key <= 0x5F)
                {
                    key -= 0x40;
                }
                else if (key == 0x20)
                {
                    key = 0x00;
                }
                else if (key == 0x3F)
                {
                    key = 0x7F;
                }
            }

            if (((mods & KeyEvent.META_SHIFT_ON) != 0) &&
                sendFunctionKey(keycode))
            {
                return true;
            }

            try
            {
                if (key < 0x80)
                {
                    mTransport.write(key);
                }
                else
                {
                    mTransport.write(new String(Character.toChars(key))
                                     .getBytes(mCfg.charset));
                }
            }
            catch (IOException ex)
            {
                Log.w(TAG, "Failure writing to transport");
                Log.w(TAG, ex);
            }

            return true;
        }

        return false;
    }

    private boolean sendFunctionKey(int keycode) {
        switch (keycode) {
        case KeyEvent.KEYCODE_1:
            mBuffer.keyPressed(vt320.KEY_F1, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_2:
            mBuffer.keyPressed(vt320.KEY_F2, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_3:
            mBuffer.keyPressed(vt320.KEY_F3, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_4:
            mBuffer.keyPressed(vt320.KEY_F4, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_5:
            mBuffer.keyPressed(vt320.KEY_F5, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_6:
            mBuffer.keyPressed(vt320.KEY_F6, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_7:
            mBuffer.keyPressed(vt320.KEY_F7, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_8:
            mBuffer.keyPressed(vt320.KEY_F8, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_9:
            mBuffer.keyPressed(vt320.KEY_F9, ' ', 0);
            return true;
        case KeyEvent.KEYCODE_0:
            mBuffer.keyPressed(vt320.KEY_F10, ' ', 0);
            return true;
        default:
            return false;
        }
    }

    /*
     * VDUDisplay interface
     */
    @Override public void redraw()
    {
		if (mView != null)
        {
			mView.postInvalidate();
        }
	}

    @Override public void updateScrollBar()
    {
	}

    @Override public void setVDUBuffer(VDUBuffer buffer)
    {
	}

    @Override public VDUBuffer getVDUBuffer()
    {
        return mBuffer;
	}

    @Override public void setColor(int index, int red, int green, int blue)
    {
        Log.d(TAG, String.format("ConsoleTTY.Buffer.setColor: %d: (%d,%d,%d)",
                                 index, red, green, blue));
        if (index < mColors.length && index >= 16)
        {
            mColors[index] = 0xff000000 | red << 16 | green << 8 | blue;
        }
	}

    @Override public void resetColors()
    {
        mColors = DEFAULT_COLORS.clone();
	}
}
