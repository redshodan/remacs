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

package org.codepunks.remacs;

import android.app.Activity;
import android.app.NotificationManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
// import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.os.Debug;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.TextView;

// import org.codepunks.remacs.RemacsService;
import org.codepunks.remacs.console.ConsoleView;


public class RemacsActivity extends Activity
{
    protected static final String TAG = "Remacs";

    // private class RemacsServiceConnection implements ServiceConnection
    // {
    //     public void onServiceConnected(ComponentName className, IBinder service)
    //     {
    //         Log.d(TAG, "RemacsServiceConnection.onServiceConnected");
    //         mService = ((RemacsService.RemacsBinder)service).getService();
    //     }
        
    //     public void onServiceDisconnected(ComponentName className)
    //     {
    //         Log.d(TAG, "RemacsServiceConnection.onServiceDisconnected");
    //         mService = null;
    //     }
    // };

    protected static final int MENU_QUIT = Menu.FIRST;

    protected RemacsCfg mRcfg;
    protected Connection mConn;
    protected RemacsNotify mNotify;
    // protected RemacsService mService;
    // protected ServiceConnection mServiceConnection;
    // protected boolean mIsBound = false;
    
    public RemacsActivity()
    {
        super();
        Log.d(TAG, "RemacsActivity()");
    }

    @Override public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        Log.d(TAG, String.format("RemacsActivity.onCreate: %s - %s",
                                 getIntent().getAction(),
                                 getIntent().getDataString()));

        NotificationManager nm =
            (NotificationManager)getSystemService(NOTIFICATION_SERVICE);
        mNotify = new RemacsNotify(this, nm);
        // mServiceConnection = new RemacsServiceConnection();
        setContentView(R.layout.console_view);
        loadPrefs();
        // doBindService();
    }

    @Override protected void onStart()
    {
        super.onStart();
        Log.d(TAG, String.format("RemacsActivity.onStart: %s - %s",
                                 getIntent().getAction(),
                                 getIntent().getDataString()));
        if (!mConn.started())
        {
            ConsoleView view = (ConsoleView) findViewById(R.id.console);
            mConn.start(view, mNotify);
        }
    }
        
    @Override protected void onResume()
    {
        super.onResume();
        Log.d(TAG,
              String.format("RemacsActivity.onResume: %s - %s", 
                            getIntent().getAction(),
                            getIntent().getDataString()));
    }

    @Override protected void onPause()
    {
        super.onPause();
        Log.d(TAG, String.format("RemacsActivity.onPause: %s - %s",
                                 getIntent().getAction(),
                                 getIntent().getDataString()));
    }

    @Override protected void onStop()
    {
        super.onStop();
        Log.d(TAG, String.format("RemacsActivity.onStop: %s - %s",
                                 getIntent().getAction(),
                                 getIntent().getDataString()));
        mConn.stop();
        mNotify.stop();
    }

    @Override protected void onNewIntent(Intent intent)
    {
        super.onNewIntent(intent);
        Log.d(TAG, String.format("RemacsActivity.onNewIntent: %s - %s",
                                 intent.getAction(),
                                 intent.getDataString()));
        String action = intent.getAction();
        if (action.equals(Intent.ACTION_VIEW))
        {
            Uri uri = intent.getData();
            boolean notify = uri.getPath().equals("/notify");
            boolean clear = uri.getPath().equals("/notify-clear");
            if (notify || clear)
            {
                Integer id = -1;
                try
                {
                    id = Integer.valueOf(uri.getFragment());
                }
                catch (NumberFormatException e)
                {
                    Log.e(TAG, "Invalid notify URI", e);
                    return;
                }
                mNotify.respondNotify(id, notify, mConn);
            }
            else if (uri.getPath().equals("/minimize"))
            {
                suspend();
            }
        }
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
        mConn.finish();
        super.finish();
    }

    // protected void doBindService()
    // {
    //     Log.d(TAG, "RemacsActivity.doBindService");
    //     bindService(new Intent(this, RemacsService.class),
    //                 mServiceConnection, Context.BIND_AUTO_CREATE);
    //     mIsBound = true;
    // }

    // protected void doUnbindService()
    // {
    //     Log.d(TAG, "RemacsActivity.doUnbindService");
    //     if (mServiceConnection != null)
    //     {
    //         unbindService(mServiceConnection);
    //         mIsBound = false;
    //     }
    // }

    @Override protected void onDestroy()
    {
        super.onDestroy();
        // doUnbindService();
    }

    public void suspend()
    {
        Log.d(TAG, "Suspending Activity");
        moveTaskToBack(true);
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

        ConnectionCfg cfg = new ConnectionCfg();
        cfg.set(host, port, user, pass, encoding, term, scrollback);
        mConn = new Connection(this, mRcfg, cfg);

        Log.d(TAG, String.format("config: host=%s port=%d user=%s pass=XXX " +
                                 "encoding=%s term=%s scroll=%d",
                                 host, port, user, encoding, term, scrollback));
    }
}
