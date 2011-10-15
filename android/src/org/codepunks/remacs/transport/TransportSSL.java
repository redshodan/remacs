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

import android.content.Context;
import android.util.Log;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.security.KeyStore;
import java.security.KeyFactory;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateFactory;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Collection;
import java.util.Iterator;
import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSessionContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
// import android.net.SSLSessionCache;

import org.codepunks.remacs.Connection;
import org.codepunks.remacs.RemacsCfg;


public class TransportSSL
    extends Transport
{
    protected static final String TAG = "Remacs";
    static public final int DEFAULT_PORT = 4747;
    static public final int DEFAULT_TIMEOUT = 600000;

    // protected SSLSessionCache mSessCache;
    protected SSLSocket mSock;
    protected InputStream mStdout;
    protected OutputStream mStdin;
    protected String mClientKey;
    protected String mCAKey;
    protected char[] mStorePass;
    
    public TransportSSL(Connection conn)
    {
        super(conn, DEFAULT_PORT);
        mStorePass = new String("remacs").toCharArray();
        mClientKey = "remacs";
        mCAKey = "remacs-ca";
    }

    @Override public void stop()
    {
        if (mSock != null)
        {
            try
            {
                mSock.close();
            }
            catch (IOException e)
            {
            }
        }
        super.stop();
    }

    public static byte[] fullStream(InputStream fis)
        throws IOException
    {
        DataInputStream dis = new DataInputStream(fis);
        byte[] bytes = new byte[dis.available()];
        dis.readFully(bytes);
        return bytes;
    }

    public Certificate[] loadCert(String path)
        throws Exception
    {
        FileInputStream certf = new FileInputStream(path);
        CertificateFactory cf = CertificateFactory.getInstance("X.509");
        Collection c = cf.generateCertificates(certf) ;
        Certificate[] certs = new Certificate[c.toArray().length];

        if (c.size() == 1) {
            certf = new FileInputStream("/sdcard/remacs.cert");
            Certificate cert = cf.generateCertificate(certf) ;
            certs[0] = cert;
        } else {
            System.out.println("Certificate chain length: "+c.size());
            certs = (Certificate[])c.toArray();
        }

        return certs;
    }
    
    public KeyStore loadKeystore()
    {
        try
        {
            // Private key
            FileInputStream keyf = new FileInputStream("/sdcard/remacs.key");
            byte[] keyb = fullStream(keyf);
            KeyFactory kf = KeyFactory.getInstance("RSA");
            PKCS8EncodedKeySpec keysp = new PKCS8EncodedKeySpec(keyb);
            PrivateKey pkey = kf.generatePrivate(keysp);

            // Certificate
            Certificate[] cert = loadCert("/sdcard/remacs.cert");
            
            // CA cert
            Certificate[] cacert = loadCert("/sdcard/remacs.cacert");

            // Keystore
            KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
            ks.load(null, mStorePass);
            ks.setKeyEntry(mClientKey, pkey, mStorePass, cert);
            ks.setCertificateEntry(mCAKey, cacert[0]);
            FileOutputStream kso =
                mConn.getContext().openFileOutput("remacs.ks",
                                                  Context.MODE_PRIVATE);
            ks.store(kso, mStorePass);
            
            return ks;
        }
        catch (Exception e)
        {
            Log.e(TAG, "Failed to connect", e);
            putString("Failed to connect: " + e.getMessage());
        }
        return null;
    }

    public KeyStore loadCAKeystore()
    {
        try
        {
            // CA cert
            Certificate[] cacert = loadCert("/sdcard/remacs.cacert");

            // Keystore
            KeyStore ks = KeyStore.getInstance(KeyStore.getDefaultType());
            ks.load(null, mStorePass);
            ks.setCertificateEntry(mCAKey, cacert[0]);
            FileOutputStream kso =
                mConn.getContext().openFileOutput("remacs-ca.ks",
                                                  Context.MODE_PRIVATE);
            ks.store(kso, null);
            
            return ks;
        }
        catch (Exception e)
        {
            Log.e(TAG, "Failed to connect", e);
            putString("Failed to connect: " + e.getMessage());
        }
        return null;
    }
    
    @Override public boolean connect()
    {
        Log.d(TAG, "Connecting...");
        try
        {
            KeyStore ks = loadKeystore();
            KeyStore ks_ca = loadCAKeystore();
            if ((ks == null) || (ks_ca == null))
            {
                return false;
            }
            
            KeyManagerFactory keyfty = KeyManagerFactory.getInstance(
                KeyManagerFactory.getDefaultAlgorithm());
            keyfty.init(ks, mStorePass);
            KeyManager keys[] = keyfty.getKeyManagers();
            Log.d(TAG, String.format("keys: %d", keys.length));
            int i;
            for (i = 0; i < keys.length; ++i)
            {
                Log.d(TAG, String.format("key: %s", keys[i].toString()));
            }
            TrustManagerFactory trustfty = TrustManagerFactory.getInstance(
                TrustManagerFactory.getDefaultAlgorithm());
            trustfty.init(ks_ca);
            TrustManager[] trusts = trustfty.getTrustManagers();
            SSLContext ctx = SSLContext.getInstance("tlsv1");
            ctx.init(keys, trusts, null);
            SSLSocketFactory sockfty = ctx.getSocketFactory();

            // mSock = sockfty.createSocket(
            //     mConn.getConfig().host, mConn.getConfig().getPort());
            mSock = (SSLSocket)sockfty.createSocket(mConn.getConfig().host,
                                                    DEFAULT_PORT);
            mSock.setUseClientMode(true);
            mSock.setWantClientAuth(true);
            mStdout = mSock.getInputStream();
            mStdin = mSock.getOutputStream();
            mConnected = true;
            Log.d(TAG, "...Connected");
            return true;
        }
        catch (Exception e)
        {
            Log.e(TAG, "Failed to connect", e);
            putString("Failed to connect: " + e.getMessage());
        }
        return false;
    }
    
    @Override public int read(byte[] buffer, int offset, int length)
        throws IOException
    {
        if (mStdout != null)
        {
            int count = mStdout.read(buffer, offset, length);
            
            if (count <= 0)
            {
                stop();
            }

            return count;
        }
        else
        {
            return 0;
        }
    }

    @Override public void write(byte[] buffer) throws IOException
    {
        if (mStdin != null)
        {
            mStdin.write(buffer);
        }
    }
    
    @Override public void write(int c) throws IOException
    {
        if (mStdin != null)
        {
            mStdin.write(c);
        }
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
