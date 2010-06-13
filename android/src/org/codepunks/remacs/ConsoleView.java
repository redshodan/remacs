package org.codepunks.remacs;

import android.content.Context;
import android.view.View;
import android.util.AttributeSet;


public class ConsoleView extends View
{
    protected TTY mTty;
    
    public ConsoleView(Context context, AttributeSet attrs)
    {
        super(context, attrs);

        mTty = new TTY(this);
    }

}
