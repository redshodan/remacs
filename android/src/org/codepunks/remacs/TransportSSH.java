package org.codepunks.remacs;

import android.util.Log;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.ConnectionMonitor;
import ch.ethz.ssh2.InteractiveCallback;
import ch.ethz.ssh2.Session;
import ch.ethz.ssh2.StreamGobbler;


public class TransportSSH
    extends Transport
    implements ConnectionMonitor, InteractiveCallback
{
    protected static final String TAG = "Remacs";
    static public final int DEFAULT_PORT = 22;

    protected Connection mConn;
    protected Session mSess;
    protected InputStreamReader mStdout;
    
    public TransportSSH(ConsoleTTY tty, String host, int port)
    {
        super(tty, host, port);
    }

    public TransportSSH(ConsoleTTY tty, String host)
    {
        super(tty, host, DEFAULT_PORT);
    }

    public void connect()
    {
        try
        {
            mConn = new Connection(mHost, mPort);
            mConn.addConnectionMonitor(this);
            mConn.connect();
            mConn.authenticateWithPassword("test", "test");

            mSess = mConn.openSession();
			mSess.execCommand("uname -a && date && uptime && who");
            mStdout =
                new InputStreamReader(new StreamGobbler(mSess.getStdout()));
        }
        catch (IOException e)
        {
            Log.e(TAG, "Failed to connect", e);
            putString("Failed to connect: " + e.getMessage());
        }
    }
    
    public String read() throws IOException
    {
        char[] buff = new char[1024];
        
        buff[0] = '\0';
        if (mStdout.read(buff) < 0)
        {
            return null;
        }
        else
        {
            return new String(buff);
        }
    }

    public int read(byte[] buffer, int offset) throws IOException
    {
        return 0;
    }
    
    public void write(byte[] buffer) throws IOException
    {
    }
    
    public void write(int c) throws IOException
    {
    }
    
    public void flush() throws IOException
    {
    }
    
	public void close()
    {
    }

    /*
     * ConnectionMonitor interface
     */
	public void connectionLost(Throwable reason)
    {
        Log.i(TAG, "Connection lost");
	}

	public String[] replyToChallenge(String name, String instruction,
                                     int numPrompts, String[] prompt,
                                     boolean[] echo)
    {
        Log.d(TAG, String.format("prompt: %s : %s", name, instruction));
        return new String[numPrompts];
	}
}
