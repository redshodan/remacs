package org.codepunks.remacs;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.os.Vibrator;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnKeyListener;


public class ConsoleView extends View
{
    protected static final String TAG = "Remacs";
	public static final long VIBRATE_DURATION = 30;

    protected ConsoleTTY mTty;
    protected Paint mPaint;
    protected Transport mTransport;
    protected ConnectionCfg mCfg;
    protected Vibrator mVibrator;
    
    public ConsoleView(Context context, AttributeSet attrs)
    {
        super(context, attrs);

		mPaint = new Paint();
        // mCfg = new ConnectionCfg("10.0.2.2", 22, "remacs", "remacs", "UTF-8",
        //                          "screen");
        mCfg = new ConnectionCfg("10.1.1.20", 22, "remacs", "remacs", "UTF-8",
                                 "screen");
        mCfg.term_scrollback = 100;
        mTty = new ConsoleTTY(this, mCfg);
        setOnKeyListener(mTty);
        mTransport = new TransportSSH(mTty, mCfg);
        mTty.setTransport(mTransport);
        mTransport.start();

        setFocusable(true);
        setFocusableInTouchMode(true);
        
        mVibrator = (Vibrator)context.getSystemService(Context.VIBRATOR_SERVICE);
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
	}

    public void vibrate()
    {
        mVibrator.vibrate(VIBRATE_DURATION);
    }

    public void finish()
    {
        Log.d(TAG, "ConsoleView.finish()");
        mTransport.stop();
    }
}
