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
