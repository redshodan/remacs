package org.codepunks.remacs;

import android.util.Log;

public class ConnectionCfg
{
    public String host;
    public int port;
    public int default_port;
    public String user;
    public String pass;
    public String charset;
    public String term;

    public ConnectionCfg(String host, int port, String user, String pass,
                         String charset, String term)
    {
        this.host = host;
        this.port = port;
        this.user = user;
        this.pass = pass;
        this.charset = charset;
        this.term = term;
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
