package org.codepunks.remacs;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import javax.xml.parsers.*;

import org.w3c.dom.*;
import org.xml.sax.*;

import org.codepunks.remacs.ConnectionCfg;
import org.codepunks.remacs.RemacsCfg;
import org.codepunks.remacs.console.ConsoleView;
import org.codepunks.remacs.transport.Transport;
import org.codepunks.remacs.transport.TransportSSH;

public class Connection
{
    protected static final String TAG = "Remacs";
    
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

    protected RemacsActivity mAct;
    protected RemacsCfg mRcfg;
    protected ConnectionCfg mCfg;
    protected Transport mTransport;
    protected ConsoleView mView;

    protected NotificationManager mNM;
    protected Map<Integer, RemacsNotif> mNotifMap;
    protected int mNotifCounter = 0;
    protected DocumentBuilder mParser;
    protected byte[] mCharBuff = new byte[1];

    public Connection(RemacsActivity act, RemacsCfg rcfg, ConnectionCfg cfg)
    {
        super();

        mAct = act;
        mRcfg = rcfg;
        mCfg = cfg;
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

    public void start(ConsoleView view, NotificationManager nm)
    {
        mNM = nm;
        mView = view;
        mView.setup(this);
        mTransport = new TransportSSH(this);
        mTransport.start();
    }

    public void stop()
    {
        mTransport.stop();
        mNM.cancel(1);
    }

    public void finish()
    {
        mView.finish();
    }

    public boolean started()
    {
        return mView != null;
    }
    
    public ConnectionCfg getConfig()
    {
        return mCfg;
    }

    public void putString(String str)
    {
        mView.putString(str);
    }

    public void putString(char[] s, int length)
    {
        mView.putString(s, length);
    }

    public void setTTY(int width, int height, String term)
    {
        if (term != null)
        {
            mCfg.term = term;
        }
        mCfg.term_width = width;
        mCfg.term_height = height;
        sendTTY();
    }

    public void sendTTY()
    {
        if (!mTransport.isConnected())
        {
            return;
        }
        
        String data =
            String.format("<query><setup><tty term='%s' row='%d' col='%d'/>" +
                          "</setup></query>", mCfg.term, mCfg.term_height,
                          mCfg.term_width);
        mTransport.sendCmd(Transport.CMD_CMD, data);
    }

    public void sendData(String data)
    {
        mTransport.sendCmd(Transport.CMD_TTY, data);
    }
    
    public void sendData(byte[] data)
    {
        mTransport.sendCmd(Transport.CMD_TTY, data);
    }

    public void sendData(int data)
    {
        mCharBuff[0] = (byte)(data & 0xFF);
        mTransport.sendCmd(Transport.CMD_TTY, mCharBuff);
    }

    public void handleCmd(String data)
    {
        try
        {
            Document document = mParser.parse(
                new StringBufferInputStream(data), null);
            Node cmd = document.getFirstChild();
            if (cmd.getNodeName().compareTo("query") == 0)
            {
                Node child = cmd.getFirstChild();
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

        Intent intent =
            new Intent(Intent.ACTION_VIEW,
                       Uri.parse(String.format("remacs://%s/notify#%d",
                                               "host", id)));
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent pending =
            PendingIntent.getActivity(mAct, 0, intent,
                                      PendingIntent.FLAG_UPDATE_CURRENT);
        notification.setLatestEventInfo(mAct, title, body, pending);
        mNotifMap.put(mNotifCounter, new RemacsNotif(id, title, body));
        mNotifCounter++;
        mNM.notify(id, notification);
    }

    public void handleSuspend(Node cmd, Node child, String data)
    {
        Intent intent =
            new Intent(Intent.ACTION_VIEW,
                       Uri.parse(String.format("remacs://%s/minimize", "host")));
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        mAct.startActivity(intent);
    }
}
