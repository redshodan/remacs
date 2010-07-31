package org.codepunks.remacs;


import android.util.Log;

import de.mud.terminal.VDUBuffer;
import de.mud.terminal.VDUDisplay;
import de.mud.terminal.VDUInput;
import de.mud.terminal.vt320;

import java.io.IOException;


public class ConsoleBuffer extends vt320
{
    protected static final String TAG = "Remacs";

    public Transport mTransport;
        
    @Override public void write(byte[] b)
    {
        try
        {
            if ((b != null) && (mTransport != null))
            {
                mTransport.write(b);
            }
        }
        catch (IOException e)
        {
            Log.e(TAG, "Fail", e);
        }
    }
        
    @Override public void write(int b)
    {
        try
        {
            if (mTransport != null)
            {
                mTransport.write(b);
            }
        }
        catch (IOException e)
        {
            Log.e(TAG, "Fail", e);
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
