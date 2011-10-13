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

import android.content.SharedPreferences;
import android.content.res.Resources;
import android.os.Bundle;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;

public class RemacsSettings extends PreferenceActivity
{
    static final String TAG = "Remacs";

	@Override protected void onCreate(Bundle savedInstanceState)
    {
		super.onCreate(savedInstanceState);
        
        try
        {
            addPreferencesFromResource(R.xml.preferences);
        }
        catch (ClassCastException e)
        {
            Log.d(TAG, "Failed to load preferences, setting to defaults");
            SharedPreferences sp =
                PreferenceManager.getDefaultSharedPreferences(getBaseContext());
            SharedPreferences.Editor editor = sp.edit();
            editor.clear();
            editor.commit();

            Resources res = getResources();

            editor = sp.edit();
            editor.putString("hostname",
                             res.getString(R.string.pref_hostname_def));
            editor.putString("username",
                             res.getString(R.string.pref_username_def));
            editor.putString("password",
                             res.getString(R.string.pref_password_def));
            editor.putString("encoding",
                             res.getString(R.string.pref_encoding_def));
            editor.putString("term",
                             res.getString(R.string.pref_term_def));
            editor.commit();

            addPreferencesFromResource(R.xml.preferences);
        }
	}
}
