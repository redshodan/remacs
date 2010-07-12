package org.codepunks.remacs;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;


public class ConsoleView extends View
{
    protected static final String TAG = "Remacs";

    protected ConsoleTTY mTty;
    protected Paint mPaint;
    protected Transport mTransport;
    protected Thread mTransportThread;
    protected ConnectionCfg mCfg;
    
    public ConsoleView(Context context, AttributeSet attrs)
    {
        super(context, attrs);

		mPaint = new Paint();
        mCfg = new ConnectionCfg("10.0.2.2", 22, "test", "test", "UTF-8",
                                 "screen");
        mTty = new ConsoleTTY(mCfg);
        mTransport = new TransportSSH(mTty, mCfg);
        mTransportThread = new Thread(mTransport);
        mTransportThread.setName("Transport");
		mTransportThread.setDaemon(true);
        mTransportThread.start();
    }

    public void putString(String str)
    {
        mTty.putString(str);
    }
    
	@Override public void onDraw(Canvas canvas)
    {
        Bitmap bitmap = mTty.onDraw(canvas);
        if (bitmap == null)
        {
            return;
        }

        canvas.drawBitmap(bitmap, 0, 0, mPaint);
    }

    @Override protected void onSizeChanged(int w, int h, int oldw, int oldh)
    {
		super.onSizeChanged(w, h, oldw, oldh);

		mTty.onSizeChanged(this);
		//scaleCursors();
	}
}
