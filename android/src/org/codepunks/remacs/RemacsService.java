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
