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

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Matrix;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.PixelXorXfermode;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.graphics.Bitmap.Config;
import android.graphics.Paint.FontMetrics;
import android.os.Vibrator;
import android.os.Debug;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnKeyListener;

import de.mud.terminal.VDUBuffer;

import org.codepunks.remacs.Connection;
import org.codepunks.remacs.RemacsCfg;
import org.codepunks.remacs.transport.Transport;


public class ConsoleView extends View
{
    protected static final String TAG = "Remacs";
	public static final long VIBRATE_DURATION = 30;

    protected ConsoleTTY mTty;
    protected Connection mConn;
    protected Vibrator mVibrator;
	protected Bitmap mBitmap;
    protected Canvas mCanvas;
    protected boolean mFullRedraw;
    protected Paint mPaint;
	protected int mCharWidth;
	protected int mCharHeight;
	protected int mCharTop;
    protected byte[] mWAttrs;
    protected float[] mWidths;
	// Cursor paints to distinguish modes
	protected Paint mCursorPaint;
	protected Paint mCursorStrokePaint;
	private Path mCtrlCursor, mAltCursor, mShiftCursor;
	private RectF mScaleSrc, mScaleDst;
	private Matrix mScaleMatrix;
    
    public ConsoleView(Context context, AttributeSet attrs)
    {
        super(context, attrs);

        setFocusable(true);
        setFocusableInTouchMode(true);
        
        mVibrator = (Vibrator)context.getSystemService(Context.VIBRATOR_SERVICE);

        mCanvas = new Canvas();
        mPaint = new Paint();
		mPaint.setAntiAlias(true);
		mPaint.setTypeface(Typeface.MONOSPACE);
		mPaint.setFakeBoldText(true);

        mCursorPaint = new Paint();
		mCursorPaint.setAntiAlias(true);

		mCursorStrokePaint = new Paint(mCursorPaint);
		mCursorStrokePaint.setStrokeWidth(0.1f);
		mCursorStrokePaint.setStyle(Paint.Style.STROKE);

		/*
		 * Set up our cursor indicators on a 1x1 Path object which we can later
		 * transform to our character width and height
		 */
		mShiftCursor = new Path();
		mShiftCursor.moveTo(0.0f, 1.0f);
		mShiftCursor.lineTo(0.5f, 0.66f);
		mShiftCursor.lineTo(1.0f, 1.0f);

        mAltCursor = new Path();
		mAltCursor.moveTo(1.0f, 0.25f);
		mAltCursor.lineTo(.33f, 0.50f);
		mAltCursor.lineTo(1.0f, 0.75f);

		mCtrlCursor = new Path();
        mCtrlCursor.moveTo(0.0f, 0.33f);
		mCtrlCursor.lineTo(0.5f, 0.0f);
		mCtrlCursor.lineTo(1.0f, 0.33f);

		// For creating the transform when the terminal resizes
		mScaleSrc = new RectF();
		mScaleSrc.set(0.0f, 0.0f, 1.0f, 1.0f);
		mScaleDst = new RectF();
		mScaleMatrix = new Matrix();

        mWAttrs = new byte[Transport.BUFFER_SIZE];
        mWidths = new float[Transport.BUFFER_SIZE];
    }

    public void setup(Connection conn)
    {
        mConn = conn;
        mTty = new ConsoleTTY(this, mConn);
        setOnKeyListener(mTty);

        mCursorPaint.setColor(mTty.getColors()[Colors.WHITE]);
		mCursorPaint.setXfermode(
            new PixelXorXfermode(mTty.getColors()[Colors.DEFAULT_BG_COLOR]));
    }

    public void putString(String str)
    {
        mTty.putString(str);
    }

    public void putString(char[] s, int length)
    {
        mPaint.getTextWidths(s, 0, length, mWidths);
        for (int i = 0; i < length; ++i)
        {
            if ((int)mWidths[i] != mCharWidth)
            {
                mWAttrs[i] = (byte)1;
            }
            else
            {
                mWAttrs[i] = (byte)0;
            }
        }
        mTty.putString(s, mWAttrs, length);
    }

	@Override public void onDraw(Canvas pcanvas)
    {
        ConsoleBuffer buffer = mTty.getBuffer();
        boolean redraw = false;
        
        if (buffer.update[0] || mFullRedraw)
        {
            redraw = true;
        }

        mCanvas.drawPaint(mPaint);

        Integer[] colors = mTty.getColors();

        if (redraw)
        {
            mCanvas.drawColor(colors[Colors.DEFAULT_BG_COLOR]);
        }
        
        for (int l = 0; l < buffer.height; ++l)
        {
            if (!redraw && !buffer.update[l + 1])
            {
                continue;
            }

            for (int c = 0; c < buffer.width; ++c)
            {
                int fg;
                int bg;
                int addr = 0;
                int currAttr = buffer.charAttributes[buffer.windowBase + l][c];
                int fgcolor = Colors.DEFAULT_FG_COLOR;

                // // check if foreground color attribute is set
                if ((currAttr & VDUBuffer.COLOR_FG) != 0)
                {
                    fgcolor = ((currAttr & VDUBuffer.COLOR_FG) >>
                               VDUBuffer.COLOR_FG_SHIFT) - 1;
                }

                if (fgcolor < 8 && (currAttr & VDUBuffer.BOLD) != 0)
                {
                    fg = colors[fgcolor + 8];
                }
                else
                {
                    fg = colors[fgcolor];
                }

                // check if background color attribute is set
                if ((currAttr & VDUBuffer.COLOR_BG) != 0)
                {
                    bg = colors[((currAttr & VDUBuffer.COLOR_BG) >>
                                  VDUBuffer.COLOR_BG_SHIFT) - 1];
                }
                else
                {
                    bg = colors[Colors.DEFAULT_BG_COLOR];
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
                    while (
                        (c + addr < buffer.width) &&
                        (buffer.charAttributes[buffer.windowBase + l][c + addr]
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
                    mCanvas.drawText(buffer.charArray[buffer.windowBase + l],
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

            buffer.update[l] = false;
        }

        buffer.update[0] = false;

        pcanvas.drawBitmap(mBitmap, 0, 0, mPaint);

        if (buffer.isCursorVisible())
        {
            int col = buffer.getCursorColumn();
            int row = buffer.getCursorRow();
            int cols = buffer.getColumns();

            if (col == cols)
            {
                col = cols - 1;
            }

            int attr = buffer.getAttributes(col, row);
            int x = col * mCharWidth;
            int y = ((row + buffer.screenBase - buffer.windowBase) *
                     mCharHeight);
            int w = ((attr & VDUBuffer.FULLWIDTH) != 0 ? 2 : 1);

            // Save the current clip and translation
            pcanvas.save();
            pcanvas.translate(x, y);
            pcanvas.clipRect(0, 0, mCharWidth * w , mCharHeight);
            pcanvas.drawPaint(mCursorPaint);

            // Make sure we scale our decorations to the correct size.
            pcanvas.concat(mScaleMatrix);

            if (mTty.isShifted())
            {
                pcanvas.drawPath(mShiftCursor, mCursorStrokePaint);
            }
            else if (mTty.isShiftLocked())
            {
                pcanvas.drawPath(mShiftCursor, mCursorPaint);
            }

            if (mTty.isAlted())
            {
                pcanvas.drawPath(mAltCursor, mCursorStrokePaint);
            }
            else if (mTty.isAltLocked())
            {
                pcanvas.drawPath(mAltCursor, mCursorPaint);
            }

            if (mTty.isCtrled())
            {
                pcanvas.drawPath(mCtrlCursor, mCursorStrokePaint);
            }
            else if (mTty.isCtrlLocked())
            {
                pcanvas.drawPath(mCtrlCursor, mCursorPaint);
            }

            // Restore previous clip region
            pcanvas.restore();
        }
    }

    @Override protected void onSizeChanged(int w, int h, int oldw, int oldh)
    {
		super.onSizeChanged(w, h, oldw, oldh);

        Log.d(TAG, "ConsoleView.onSizeChanged");

        int width = getWidth();
        int height = getHeight();
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
        int tw = width / mCharWidth;
        int th = height / mCharHeight;

        Log.d(TAG, String.format("Setting term size (%d,%d) (%d,%d)",
                                 width, height, tw, th));
        mConn.setTTY(tw, th, null);
        mTty.getBuffer().setScreenSize(tw, th, false);
        
        // Create a scale matrix to scale our 1x1 representation of the cursor
        mScaleDst.set(0.0f, 0.0f, mCharWidth, mCharHeight);
        mScaleMatrix.setRectToRect(mScaleSrc, mScaleDst,
                                   Matrix.ScaleToFit.FILL);
        
        mFullRedraw = true;
    }

    public void vibrate()
    {
        mVibrator.vibrate(VIBRATE_DURATION);
    }

    public void finish()
    {
        Log.d(TAG, "ConsoleView.finish()");
        mConn.stop();
    }
}
