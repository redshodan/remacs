package org.codepunks.remacs;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Debug;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

import org.codepunks.remacs.console.ConsoleView;


public class RemacsActivity extends Activity
{
    protected static final String TAG = "Remacs";
    protected static final int MENU_QUIT = Menu.FIRST;

    protected RemacsCfg mRcfg;
    protected ConnectionCfg mCfg;
    protected ConsoleView mView;
    
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

    @Override protected void onStart()
    {
        super.onStart();
        loadPrefs();
        mView.setup(mRcfg, mCfg);
    }
        
    @Override protected void onResume()
    {
        super.onResume();
    }

    @Override public boolean onCreateOptionsMenu(Menu menu)
    {
        super.onCreateOptionsMenu(menu);

        menu.add(0, MENU_QUIT, 0, R.string.quit);

		MenuItem settings = menu.add(R.string.menu_settings);
		settings.setIcon(android.R.drawable.ic_menu_preferences);
		settings.setIntent(new Intent(RemacsActivity.this,
                                      RemacsSettings.class));

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

    protected void defaultPrefs()
    {
        mRcfg = new RemacsCfg();
        mRcfg.def_port = 22;
        mRcfg.def_term_scrollback = 100;
    }
    
    protected void loadPrefs()
    {
        SharedPreferences sp =
            PreferenceManager.getDefaultSharedPreferences(getBaseContext());

        mCfg = new ConnectionCfg();
        mCfg.set("10.0.2.2", 22, "remacs", "remacs", "UTF-8", "screen", 100);

        try
        {
            // mKeyboardName = sp.getString("keyboard", DEF_KEYBOARD_NAME);
            // mLongPressEnabled = sp.getBoolean("longpress", true);
            // mTouchSlop = Integer.parseInt(sp.getString("touchSlop", "10"));
            // mDoubleTapSlop =
            //     Integer.parseInt(sp.getString("doubleTapSlop", "100"));
            // mMinFlingVelocity =
            //     Integer.parseInt(sp.getString("minFlingVelocity", "5"));
        }
        catch (ClassCastException e)
        {
            Log.d(TAG, "loadPrefs: failed");
            defaultPrefs();
        }
        // Log.d(TAG, String.format("config: ts=%d dts=%d mfs=%d lp=%d", mTouchSlop,
        //                          mDoubleTapSlop, mMinFlingVelocity, ilp));
    }
}
