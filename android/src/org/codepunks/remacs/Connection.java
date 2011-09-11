package org.codepunks.remacs;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;

import java.io.*;
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
    
    protected RemacsActivity mAct;
    protected RemacsCfg mRcfg;
    protected ConnectionCfg mCfg;
    protected Transport mTransport;
    protected ConsoleView mView;
    protected RemacsNotify mNotify;

    protected DocumentBuilder mParser;
    protected byte[] mCharBuff = new byte[1];

    public Connection(RemacsActivity act, RemacsCfg rcfg, ConnectionCfg cfg)
    {
        super();

        mAct = act;
        mRcfg = rcfg;
        mCfg = cfg;

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

    public void start(ConsoleView view, RemacsNotify notify)
    {
        mView = view;
        mView.setup(this);
        mNotify = notify;
        mTransport = new TransportSSH(this);
        mTransport.start();
    }

    public void stop()
    {
        mTransport.stop();
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

    public void respondNotify(Integer id, boolean invoke)
    {
        String child = "";
        if (invoke)
        {
            child = "<invoke/>";
        }
        String data =
            String.format("<query><notify id='%d' type='result'>%s</notify>" +
                          "</query>", id, child);
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
            Log.e(TAG, "Failed handling command", e);
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
            mNotify.showNotify(id, title, body);
        }
    }
    
    public void handleError(Node cmd, Node child, String data)
    {
        String error = child.getNodeValue();
        mNotify.showNotify(-1, "Error", error);
    }

    public void handleSuspend(Node cmd, Node child, String data)
    {
        // Intent intent =
        //     new Intent(Intent.ACTION_VIEW,
        //                Uri.parse(String.format("remacs://%s/minimize", "host")));
        // intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        // mAct.startActivity(intent);
        mAct.suspend();
    }
}
