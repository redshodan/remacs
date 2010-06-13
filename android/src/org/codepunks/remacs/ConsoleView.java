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
    
    public ConsoleView(Context context, AttributeSet attrs)
    {
        super(context, attrs);

		mPaint = new Paint();
        mTty = new ConsoleTTY();
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
