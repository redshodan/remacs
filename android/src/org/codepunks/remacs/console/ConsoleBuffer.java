package org.codepunks.remacs.console;


import android.util.Log;

import de.mud.terminal.VDUBuffer;
import de.mud.terminal.VDUDisplay;
import de.mud.terminal.VDUInput;
import de.mud.terminal.vt320;

import java.io.IOException;

import org.codepunks.remacs.Connection;


public class ConsoleBuffer extends vt320
{
    protected static final String TAG = "Remacs";

    protected Connection mConn;

    public ConsoleBuffer(Connection conn)
    {
        super();
        mConn = conn;
    }
        
    @Override public void write(byte[] b)
    {
        if ((b != null) && (mConn != null))
        {
            mConn.sendData(b);
        }
    }
        
    @Override public void write(int b)
    {
        if (mConn != null)
        {
            mConn.sendData(b);
        }
    }
        
    @Override public void sendTelnetCommand(byte cmd)
    {
    }
        
    @Override public void setWindowSize(int c, int r)
    {
    }
        
    @Override public void debug(String s)
    {
    }
        
    @Override public void beep()
    {
        // if (mView.isShown())
        // {
        //     m.playBeep();
        // else
        //     manager.sendActivityNotification(host);
    }
};
