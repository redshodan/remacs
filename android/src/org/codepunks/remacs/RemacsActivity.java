package org.codepunks.remacs;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

public class RemacsActivity extends Activity
{
    protected static final String TAG = "Remacs";
    protected static final int MENU_QUIT = Menu.FIRST;
    
    private ConsoleView mView;
    
    public RemacsActivity()
    {
        Log.d(TAG, "RemacsActivity.()");
    }

    @Override public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        Log.d(TAG, "RemacsActivity.onCreate");

        setContentView(R.layout.console_view);
        mView = (ConsoleView) findViewById(R.id.console);
    }

    @Override protected void onResume()
    {
        super.onResume();
    }

    @Override public boolean onCreateOptionsMenu(Menu menu)
    {
        super.onCreateOptionsMenu(menu);

        menu.add(0, MENU_QUIT, 0, R.string.quit);

        return true;
    }

    @Override public boolean onPrepareOptionsMenu(Menu menu)
    {
        super.onPrepareOptionsMenu(menu);

        return true;
    }

    @Override public boolean onOptionsItemSelected(MenuItem item)
    {
        switch (item.getItemId())
        {
        case MENU_QUIT:
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    @Override public void finish()
    {
        Log.d(TAG, "RemacsActivity.finish()");
        mView.finish();
        super.finish();
    }
}
