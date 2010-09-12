package org.codepunks.remacs.transport;

import android.util.Log;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.ConnectionMonitor;
import ch.ethz.ssh2.InteractiveCallback;
import ch.ethz.ssh2.Session;
import ch.ethz.ssh2.StreamGobbler;

import org.codepunks.remacs.ConnectionCfg;
import org.codepunks.remacs.RemacsCfg;
import org.codepunks.remacs.console.ConsoleTTY;


public class TransportSSH
    extends Transport
    implements ConnectionMonitor, InteractiveCallback
{
    protected static final String TAG = "Remacs";
    static public final int DEFAULT_PORT = 22;

    protected Connection mConn;
    protected Session mSess;
    protected InputStream mStdout;
    protected OutputStream mStdin;
    protected boolean mConnected;
    
    public TransportSSH(ConsoleTTY tty, ConnectionCfg cfg)
    {
        super(tty, cfg, DEFAULT_PORT);
        mConnected = false;
    }

    @Override public void stop()
    {
        mConn.close();
        super.stop();
    }

    @Override public void connect()
    {
        Log.d(TAG, "Connecting...");
        try
        {
            mConn = new Connection(mCfg.host, mCfg.getPort());
            mConn.addConnectionMonitor(this);
            mConn.connect();
            mConn.authenticateWithPassword(mCfg.user, mCfg.pass);

            mSess = mConn.openSession();
			mSess.execCommand("remacs --server");
            // mSess.requestPTY(mCfg.term, mCfg.term_width, mCfg.term_height, 0, 0,
            //                  null);
            // mSess.startShell();
            mStdout = new StreamGobbler(mSess.getStdout());
            mStdin = mSess.getStdin();
            mConnected = true;
        }
        catch (IOException e)
        {
            Log.e(TAG, "Failed to connect", e);
            putString("Failed to connect: " + e.getMessage());
        }
    }
    
    @Override public int read(byte[] buffer, int offset, int length)
        throws IOException
    {
        int count = mStdout.read(buffer, offset, length);
        
        if (count <= 0)
        {
            stop();
        }

        return count;
    }

    @Override public void write(byte[] buffer) throws IOException
    {
        mStdin.write(buffer);
    }
    
    @Override public void write(int c) throws IOException
    {
        mStdin.write(c);
    }
    
    @Override public void flush() throws IOException
    {
    }
    
	@Override public void close()
    {
    }

    public boolean isConnected()
    {
        return mConnected;
    }
    
    /*
     * ConnectionMonitor interface
     */
	public void connectionLost(Throwable reason)
    {
        Log.i(TAG, "Connection lost");
        mConnected = false;
        // super.stop();
	}

	public String[] replyToChallenge(String name, String instruction,
                                     int numPrompts, String[] prompt,
                                     boolean[] echo)
    {
        Log.d(TAG, String.format("prompt: %s : %s", name, instruction));
        return new String[numPrompts];
	}
}
