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

package org.codepunks.remacs;

import android.util.Log;

public class ConnectionCfg
{
    public RemacsCfg cfg;
    public String host;
    public int port;
    public String user;
    public String pass;
    public String charset;
    public String term;
    public int term_scrollback;
    public int term_width;
    public int term_height;

    public void set(String host, int port, String user, String pass,
                    String charset, String term, int term_scrollback)
    {
        this.host = host;
        this.port = port;
        this.user = user;
        this.pass = pass;
        this.charset = charset;
        this.term = term;
        this.term_scrollback = term_scrollback;
        term_width = 0;
        term_height = 0;
    }

    public int getPort()
    {
        if (port == 0)
        {
            return cfg.def_port;
        }
        else
        {
            return port;
        }
    }
}
