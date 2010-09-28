package org.codepunks.remacs;


import android.app.Service;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;

import java.util.HashMap;
import java.util.Map;

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

    public class RemacsNotif
    {
        public RemacsNotif(int id, String title, String body)
        {
            this.id = id;
            this.title = title;
            this.body = body;
        }
        
        public int id;
        public String title;
        public String body;
    }

    protected NotificationManager mNM;
    protected final IBinder mBinder = new RemacsBinder();
    protected Map mNotifMap;

    public RemacsService()
    {
        super();
        mNotifMap = new HashMap<Integer, RemacsNotif>();
    }
    
    @Override public void onCreate()
    {
        Log.d(TAG, "RemacsService.onCreate");
        mNM = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);
    }

    @Override public void onStart(Intent intent, int startId)
    {
        Log.d(TAG, "RemacsService.onStart: Received start id " +
              startId + ": " + intent);
        handleNotification(1, "title", "body");
    }

    // Android 2.0
    // @Override public int onStartCommand(Intent intent, int flags, int startId)
    // {
    //     Log.d(TAG, "Received start id " + startId + ": " + intent);
    //     return START_STICKY;
    // }

    @Override public void onDestroy()
    {
        mNM.cancel(1);
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

    public void handleNotification(int id, String title, String body)
    {
        mNotifMap.put(id, new RemacsNotif(id, title, body));
        showNotification(id, title, body);
    }
    
    protected void showNotification(int id, String title, String body)
    {
        Notification notification =
            new Notification(R.drawable.emacs, title,
                             System.currentTimeMillis());

        Intent intent = new Intent(this, RemacsActivity.class);
        PendingIntent pending = PendingIntent.getActivity(this, 0, intent, 0);
        notification.setLatestEventInfo(this, title, body, pending);
        mNM.notify(id, notification);
    }
}
