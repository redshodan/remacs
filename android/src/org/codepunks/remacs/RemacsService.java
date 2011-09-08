package org.codepunks.remacs;


import android.app.Service;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import javax.xml.parsers.*;

import org.w3c.dom.*;
import org.xml.sax.*;

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
    protected Map<Integer, RemacsNotif> mNotifMap;
    protected int mNotifCounter = 0;
    protected DocumentBuilder mParser;

    public RemacsService()
    {
        super();
        mNotifMap = new HashMap<Integer, RemacsNotif>();

        try
        {
            DocumentBuilderFactory factory =
                DocumentBuilderFactory.newInstance();
            mParser = factory.newDocumentBuilder();
        }
        catch (Exception e)
        {
            Log.e(TAG, "Failed to create XML parser: " + e.toString());
        }
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

    // @SuppressWarnings("deprecation")
    public void handleCmd(String data)
    {
        try
        {
            Document document = mParser.parse(
                new StringBufferInputStream(data), null);
            Node cmd = document.getFirstChild();
            Log.d(TAG, String.format("nodename: %s", cmd.getNodeName()));
            if (cmd.getNodeName().compareTo("query") == 0)
            {
                Node child = document.getFirstChild();
                if (child.getNodeName().compareTo("error") == 0)
                {
                    handleError(cmd, child, data);
                }
                else if (child.getNodeName().compareTo("notify") == 0)
                {
                    handleNotify(cmd, child, data);
                }
                else if (child.getNodeName().compareTo("suspend") == 0)
                {
                    handleSuspend(cmd, child, data);
                }
                else
                {
                    Log.w(TAG, "Unknown command: " + data);
                }
            }
            else
            {
                Log.w(TAG, "Unknown command: " + data);
            }
        }
        catch (Exception e)
        {
            Log.e(TAG, "Failed handling command: " + e.toString());
        }
        // mParser.reset();
    }
    
    public void handleNotify(Node cmd, Node child, String data)
    {
        NamedNodeMap attrs = child.getAttributes();
        int id = Integer.parseInt(
            attrs.getNamedItem("id").getNodeValue());
        String title = null;
        String body = "";
        NodeList children = child.getChildNodes();
        for (int i = 0; i < children.getLength(); ++i)
        {
            Node citer = children.item(i);
            if (citer.getNodeName().compareTo("title") == 0)
            {
                title = citer.getFirstChild().getNodeValue();
            }
            else if (citer.getNodeName().compareTo("body") == 0)
            {
                body = citer.getFirstChild().getNodeValue();
            }
        }
        if (title == null)
        {
            Log.w(TAG, "Invalid notify command: " + data);
        }
        else
        {
            showNotification(id, title, body);
        }
    }
    
    public void handleError(Node cmd, Node child, String data)
    {
        String error = child.getNodeValue();
        showNotification(-1, "Error", error);
    }

    protected void showNotification(int id, String title, String body)
    {
        Notification notification =
            new Notification(R.drawable.emacs, title,
                             System.currentTimeMillis());

        Intent intent = new Intent(this, RemacsActivity.class);
        PendingIntent pending = PendingIntent.getActivity(this, 0, intent, 0);
        notification.setLatestEventInfo(this, title, body, pending);
        mNotifMap.put(mNotifCounter, new RemacsNotif(id, title, body));
        mNotifCounter++;
        mNM.notify(id, notification);
    }

    public void handleSuspend(Node cmd, Node child, String data)
    {
        Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse("minimize://"));
        startActivity(intent);
    }
}
