package org.codepunks.remacs;


import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Bitmap.Config;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnKeyListener;

import de.mud.terminal.VDUBuffer;
import de.mud.terminal.VDUDisplay;
import de.mud.terminal.vt320;


public class TTY implements VDUDisplay, OnKeyListener
{
    protected final String TAG = "Remacs:TTY";

    protected class Buffer extends vt320
    {
        @Override public void write(byte[] b)
        {
            Log.d(TAG, "write");
        }
        
        @Override public void write(int b)
        {
            Log.d(TAG, "write");
        }
        
        @Override public void sendTelnetCommand(byte cmd)
        {
            Log.d(TAG, "sendTelnetCommand");
        }

        @Override public void setWindowSize(int c, int r)
        {
            Log.d(TAG, "setWindowSize");
        }
        
        @Override public void debug(String s)
        {
            Log.d(TAG, "debug: " + s);
        }
    };
    
    ConsoleView mView;
	protected Buffer mBuffer;
	protected Bitmap mBitmap;
    protected Canvas mCanvas;

    public TTY(ConsoleView view)
    {
        mCanvas = new Canvas();
        mBuffer = new Buffer();
        mBuffer.setDisplay(this);

        mBuffer.putString("fu!");

        setView(view);
    }

    public void setView(ConsoleView view)
    {
        mView = view;

        int width = view.getWidth();
        int height = view.getHeight();
        if ((mBitmap != null) &&
            ((mBitmap.getWidth() != width) || (mBitmap.getHeight() != height)))
        {
			mBitmap.recycle();
			mBitmap = Bitmap.createBitmap(width, height, Config.ARGB_8888);
			mCanvas.setBitmap(mBitmap);
		}
    }
    
    public boolean onKey(View v, int keyCode, KeyEvent event)
    {
        return false;
    }

    public void redraw()
    {
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
	}

    public void resetColors()
    {
	}
}
