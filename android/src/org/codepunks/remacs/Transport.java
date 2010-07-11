package org.codepunks.remacs;

import android.util.Log;
import java.io.IOException;

public abstract class Transport implements Runnable
{
    protected static final String TAG = "Remacs";

    protected ConsoleTTY mTty;
    protected String mHost;
    protected int mPort;

    public Transport(ConsoleTTY tty, String host, int port)
    {
        mTty = tty;
        mHost = host;
        mPort = port;
    }

    public void putString(String str)
    {
        mTty.putString(str);
    }

    public void run()
    {
        connect();
        
        try
        {
            String buff = null;
            while ((buff = read()) != null)
            {
                mTty.putString(buff);
            }
        }
        catch (IOException e)
        {
            Log.w(TAG, e);
        }
    }
        
    public abstract void connect();
    public abstract String read() throws IOException;
    public abstract int read(byte[] buffer, int offset) throws IOException;
    public abstract void write(byte[] buffer) throws IOException;
    public abstract void write(int c) throws IOException;
    public abstract void flush() throws IOException;
	public abstract void close();
}
