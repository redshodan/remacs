package org.codepunks.remacs;

import android.util.Log;

public class TransportCfg
{
    public String host;
    public int port;
    public int default_port;
    public String user;
    public String pass;
    public String charset;

    public TransportCfg(String ahost, int aport, String auser, String apass,
                        String acharset)
    {
        host = ahost;
        port = aport;
        user = auser;
        pass = apass;
        charset = acharset;
    }

    public int getPort()
    {
        if (port == 0)
        {
            return default_port;
        }
        else
        {
            return port;
        }
    }
}
