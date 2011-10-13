// Copyright (C) 2009 Chris Newton <redshodan@gmail.com>
//
// This file is part of remacs.
//
// remacs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// remacs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with remacs.  If not, see <http://www.gnu.org/licenses/>.

//
// Author: Chris Newton <redshodan@gmail.com>
// $Revision$
//

package org.codepunks.remacs.transport;

import android.util.Log;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import org.codepunks.remacs.Connection;
import org.codepunks.remacs.RemacsCfg;


public class TransportSSL
    extends Transport
{
    protected static final String TAG = "Remacs";
    static public final int DEFAULT_PORT = 4747;

    protected SSLSocket mSock;
    protected Session mSess;
    protected InputStream mStdout;
    protected OutputStream mStdin;
    
    public TransportSSH(Connection conn)
    {
        super(conn, DEFAULT_PORT);
    }

    @Override public void stop()
    {
        mSshConn.close();
        super.stop();
    }

    @Override public void connect()
    {
        Log.d(TAG, "Connecting...");
        try
        {
            mSshConn = new ch.ethz.ssh2.Connection(mConn.getConfig().host,
                                                   mConn.getConfig().getPort());
            mSshConn.addConnectionMonitor(this);
            mSshConn.connect();
            mSshConn.authenticateWithPassword(mConn.getConfig().user,
                                           mConn.getConfig().pass);

            mSess = mSshConn.openSession();
			mSess.execCommand("remacs --server");
            // mSess.requestPTY(mCfg.term, mCfg.term_width, mCfg.term_height, 0, 0,
            //                  null);
            // mSess.startShell();
            mStdout = new StreamGobbler(mSess.getStdout());
            mStdin = mSess.getStdin();
            mConnected = true;
            Log.d(TAG, "...Connected");
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

    /*
     * ConnectionMonitor interface
     */
	public void connectionLost(Throwable reason)
    {
        Log.i(TAG, "Connection lost");
        mConnected = false;
        stop();
	}

	public String[] replyToChallenge(String name, String instruction,
                                     int numPrompts, String[] prompt,
                                     boolean[] echo)
    {
        Log.d(TAG, String.format("prompt: %s : %s", name, instruction));
        return new String[numPrompts];
	}
}
