package org.codepunks.remacs;


import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Typeface;
import android.graphics.Bitmap.Config;
import android.graphics.Paint.FontMetrics;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnKeyListener;

import de.mud.terminal.VDUBuffer;
import de.mud.terminal.VDUDisplay;
import de.mud.terminal.vt320;


public class TTY implements VDUDisplay, OnKeyListener
{
    protected static final String TAG = "Remacs";

    protected class Buffer extends vt320
    {
        @Override public void write(byte[] b)
        {
            Log.d(TAG, "TTY.Buffer.write");
        }
        
        @Override public void write(int b)
        {
            Log.d(TAG, "TTY.Buffer.write");
        }
        
        @Override public void sendTelnetCommand(byte cmd)
        {
            Log.d(TAG, "TTY.Buffer.sendTelnetCommand");
        }

        @Override public void setWindowSize(int c, int r)
        {
            Log.d(TAG, "TTY.Buffer.setWindowSize");
        }
        
        @Override public void debug(String s)
        {
            Log.d(TAG, "TTY.Buffer.debug: " + s);
        }
    };
    
    ConsoleView mView;
	protected Buffer mBuffer;
	protected Bitmap mBitmap;
    protected Canvas mCanvas;
    protected boolean mFullRedraw;
    protected int[] mColors;
    protected Paint mPaint;
	protected int mCharWidth;
	protected int mCharHeight;
	protected int mCharTop;
    
    public TTY()
    {
        mCanvas = new Canvas();
        mBuffer = new Buffer();
        mBuffer.setDisplay(this);
        mBuffer.setBufferSize(0);

        mPaint = new Paint();
		mPaint.setAntiAlias(true);
		mPaint.setTypeface(Typeface.MONOSPACE);
		mPaint.setFakeBoldText(true);

        mBuffer.putString("fu!");
    }

    public void onSizeChanged(ConsoleView view)
    {
        Log.d(TAG, "TTY.Buffer.onSizeChanged");

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
                int addr = 0;
                int currAttr = mBuffer.charAttributes[mBuffer.windowBase + l][c];

                // int fgcolor = defaultFg;

                // // check if foreground color attribute is set
                // if ((currAttr & VDUBuffer.COLOR_FG) != 0)
                //     fgcolor = ((currAttr & VDUBuffer.COLOR_FG) >> VDUBuffer.COLOR_FG_SHIFT) - 1;

                // if (fgcolor < 8 && (currAttr & VDUBuffer.BOLD) != 0)
                //     fg = color[fgcolor + 8];
                // else
                //     fg = color[fgcolor];

                // // check if background color attribute is set
                // if ((currAttr & VDUBuffer.COLOR_BG) != 0)
                //     bg = color[((currAttr & VDUBuffer.COLOR_BG) >> VDUBuffer.COLOR_BG_SHIFT) - 1];
                // else
                //     bg = color[defaultBg];

                // // support character inversion by swapping background and foreground color
                // if ((currAttr & VDUBuffer.INVERT) != 0) {
                //     int swapc = bg;
                //     bg = fg;
                //     fg = swapc;
                // }

                // // set underlined attributes if requested
                // mPaint.setUnderlineText((currAttr & VDUBuffer.UNDERLINE) != 0);

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
                //mPaint.setColor(bg);
                mPaint.setColor(Color.BLACK);
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
                // mCanvas.drawPaint(mPaint);

                // write the text string starting at 'c' for 'addr' number of
                // characters
                //mPaint.setColor(fg);
                mPaint.setColor(Color.GRAY);
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
    
    public boolean onKey(View v, int keyCode, KeyEvent event)
    {
        return false;
    }

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
        Log.d(TAG, String.format("TTY.Buffer.setColor: %d: (%d,%d,%d)",
                                 index, red, green, blue));
        // if (index < color.length && index >= 16)
        // {
        //     color[index] = 0xff000000 | red << 16 | green << 8 | blue;
        // }
	}

    public void resetColors()
    {
	}
}
