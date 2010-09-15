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
        super();
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

        String host = "10.0.2.2";
        int port = 22;
        String user = "remacs";
        String pass = "remacs";
        String encoding = "UTF-8";
        String term = "screen";
        int scrollback = 100;

        try
        {
            host = sp.getString("hostname", "10.0.2.2");
            port = Integer.parseInt(sp.getString("port", "22"));
            user = sp.getString("username", "remacs");
            pass = sp.getString("password", "remacs");
            encoding = sp.getString("encoding", "UTF-8");
            term = sp.getString("term", "screen");
        }
        catch (ClassCastException e)
        {
            Log.d(TAG, "loadPrefs: failed");
            defaultPrefs();
        }

        mCfg = new ConnectionCfg();
        mCfg.set(host, port, user, pass, encoding, term, scrollback);

        Log.d(TAG, String.format("config: host=%s port=%d user=%s pass=XXX " +
                                 "encoding=%s term=%s scroll=%d",
                                 host, port, user, encoding, term, scrollback));
    }
}
