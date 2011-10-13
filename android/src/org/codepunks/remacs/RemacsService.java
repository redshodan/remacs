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


import android.app.Service;
import android.content.Intent;
import android.net.Uri;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;


public class RemacsService extends Service
{
    protected static final String TAG = "Remacs";

    public class RemacsBinder extends Binder
    {
        RemacsService getService()
        {
            return RemacsService.this;
        }
    }

    protected final IBinder mBinder = new RemacsBinder();

    public RemacsService()
    {
        super();
    }
    
    @Override public void onCreate()
    {
        Log.d(TAG, "RemacsService.onCreate");
    }

    @Override public void onStart(Intent intent, int startId)
    {
        Log.d(TAG, "RemacsService.onStart: Received start id " +
              startId + ": " + intent);
    }

    // Android 2.0
    @Override public int onStartCommand(Intent intent, int flags, int startId)
    {
        Log.d(TAG, "Received start id " + startId + ": " + intent);
        return START_STICKY;
    }

    @Override public void onDestroy()
    {
    }

    @Override public IBinder onBind(Intent intent)
    {
        Log.d(TAG, "RemacsService.onBind");
        startService(new Intent(this, RemacsService.class));
        return mBinder;
    }

    @Override public boolean onUnbind(Intent intent)
    {
        Log.d(TAG, "RemacsService.onUnbind");
        stopSelf();
        return true;
    }
}
