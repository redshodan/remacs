package org.codepunks.remacs;

import android.app.Activity;
import android.os.Bundle;

import org.codepunks.remacs.TerminalView;

public class REmacs extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        // setContentView(R.layout.main);

        TerminalView view = new TerminalView(REmacs.this, null);
        setContentView(view);
    }
}
