package org.codepunks.remacs;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;

import java.util.HashMap;
import java.util.Map;

import org.codepunks.remacs.ConnectionCfg;
import org.codepunks.remacs.RemacsCfg;
import org.codepunks.remacs.console.ConsoleView;
import org.codepunks.remacs.transport.Transport;
import org.codepunks.remacs.transport.TransportSSH;

public class RemacsNotify
{
    protected static final String TAG = "Remacs";

    public class Notif
    {
        public Notif(int id, String title, String body)
        {
            this.id = id;
            this.title = title;
            this.body = body;
        }
        
        public int id;
        public String title;
        public String body;
    }

    protected RemacsActivity mAct;
    protected NotificationManager mNM;
    protected Map<Integer, Notif> mNotifMap;
    protected int mNotifCounter = 0;

    public RemacsNotify(RemacsActivity act, NotificationManager nm)
    {
        mAct = act;
        mNM = nm;
        mNotifMap = new HashMap<Integer, Notif>();
    }

    public void stop()
    {
        mNM.cancelAll();
        mNotifMap.clear();
    }

    protected void showNotify(int id, String title, String body)
    {
        Intent intent =
            new Intent(Intent.ACTION_VIEW,
                       Uri.parse(String.format("remacs://%s/notify#%d",
                                               "host", mNotifCounter)));
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent pending =
            PendingIntent.getActivity(mAct, 0, intent,
                                      PendingIntent.FLAG_UPDATE_CURRENT);
        Intent cintent =
            new Intent(Intent.ACTION_VIEW,
                       Uri.parse(String.format("remacs://%s/notify-clear#%d",
                                               "host", mNotifCounter)));
        cintent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent cpending =
            PendingIntent.getActivity(mAct, 0, cintent,
                                      PendingIntent.FLAG_UPDATE_CURRENT);
        Notification notif = new Notification(R.drawable.emacs, title,
                                              System.currentTimeMillis());
        notif.setLatestEventInfo(mAct, title, body, pending);
        notif.flags = Notification.FLAG_AUTO_CANCEL;
        notif.contentIntent = pending;
        notif.deleteIntent = cpending;
        notif.icon = R.drawable.emacs;

        mNotifMap.put(mNotifCounter, new Notif(id, title, body));
        mNM.notify(mNotifCounter, notif);
        mNotifCounter++;
    }

    protected void respondNotify(Integer id, boolean invoke, Connection conn)
    {
        Log.d(TAG, String.format("respondNotify: id=%d", id));
        Notif n = mNotifMap.remove(id);
        conn.respondNotify(n.id, invoke);
    }
}
