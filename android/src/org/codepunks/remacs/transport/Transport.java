package org.codepunks.remacs.transport;

import android.util.Log;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

import org.codepunks.remacs.Connection;
import org.codepunks.remacs.RemacsCfg;
import org.codepunks.remacs.console.ConsoleTTY;


public abstract class Transport implements Runnable
{
    protected static final String TAG = "Remacs";
    public static final int BUFFER_SIZE = 4096;
 
    // Command byte:
    //  - least significant 3 bits are command.
    //  - middle 4 bits are size of data.
    //  - most significant bit indicates size is number of size bytes.
    public static final long CMD_NONE = -1;
    public static final long CMD_ACK = 0;
    public static final long CMD_TTY = 1;
    public static final long CMD_CMD = 2;
    public static final long CMD_BLOCK = 3;
    public static final long CMD_MAX = 3;
    public static final long CMD_BITS = 2;
    public static final long CMD_SIZE_MAX = 62;
    public static final long CMD_SIZE_MAXED = 252;

    protected Connection mConn;
    protected Thread mThread;
    protected boolean mConnected;
    // String decoding
    protected CharsetDecoder mDecoder;
    protected ByteBuffer mBBuff;
    protected CharBuffer mCBuff;
    protected byte[] mBytes;
    protected char[] mChars;
    // Remacs Protocol handling
    protected long mCmd = CMD_NONE;
    protected long mCmdLength = 0;
    
    public Transport(Connection conn, int def_port)
    {
        mConn = conn;
        // mConn.getConfig().setDefaultPort(def_port);
        Charset cs = Charset.forName(mConn.getConfig().charset);
        mDecoder = cs.newDecoder();
        mDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
        mDecoder.onMalformedInput(CodingErrorAction.REPLACE);
        mConnected = false;
    }

    public void putString(String str)
    {
        mConn.putString(str);
    }

    public void sendCmd(long cmd, String data)
    {
        try
        {
            if (cmd == CMD_CMD)
            {
                Log.d(TAG, String.format("Sending cmd=%s", data));
            }
            sendCmd(cmd, data.getBytes(mConn.getConfig().charset));
        }
        catch (UnsupportedEncodingException ex)
        {
            Log.w(TAG, "Failed to write", ex);
        }
    }

    public void sendCmd(long cmd, byte[] data)
    {
        try
        {
            long length = data.length;
            // Log.d(TAG, String.format("Sending cmd=%d len=%d", cmd, length));

            if ((data != null) && (length > 0))
            {
                byte[] cbuff;
                long offset;
                if (length <= CMD_SIZE_MAX)
                {
                    cmd = cmd + (length << CMD_BITS);
                    offset = 1;
                }
                else
                {
                    cmd = cmd + CMD_SIZE_MAXED;
                    offset = 5;
                }
                cbuff = new byte[(int)(length + offset)];
                cbuff[0] = (byte)(cmd & 0xFF);
                if (offset == 5)
                {
                    cbuff[1] = (byte)((length & 0xFF000000L) >> 24);
                    cbuff[2] = (byte)((length & 0x00FF0000L) >> 16);
                    cbuff[3] = (byte)((length & 0x0000FF00L) >> 8);
                    cbuff[4] = (byte)(length & 0x000000FFL);
                }
                for (int i = 0; i < length; ++i)
                {
                    cbuff[(int)(offset + i)] = data[i];
                }
                write(cbuff);
            }
            else
            {
                write((byte)(cmd & 0xFF));
            }
        }
        catch (IOException ex)
        {
            Log.w(TAG, "Failed to write", ex);
        }
    }

    public void run()
    {
        connect();
        mConn.sendTTY();
        
        try
        {
            int count;
            mBBuff = ByteBuffer.allocate(BUFFER_SIZE);
            mCBuff = CharBuffer.allocate(BUFFER_SIZE);
            mBytes = mBBuff.array();
            mChars = mCBuff.array();

            mBBuff.limit(0);
            do
            {
                if ((mCmd == CMD_NONE) && (mBBuff.remaining() > 0))
                {
                    count = mBBuff.remaining();
                }
                else
                {
                    count = read(mBytes, mBBuff.arrayOffset() + mBBuff.limit(),
                                 mBBuff.capacity() - mBBuff.limit());
                    if (count > 0)
                    {
                        mBBuff.limit(mBBuff.limit() + count);
                    }
                }
                if (count > 0)
                {
                    try
                    {
                        if (mCmd == CMD_NONE)
                        {
                            mCmd = mBBuff.get();
                            mCmd = (mCmd & 0xFF);
                            // Log.d(TAG, String.format("unpacked cmd=%d", mCmd));
                        }
                        if (mCmdLength == 0)
                        {
                            if ((mCmd & CMD_SIZE_MAXED) == CMD_SIZE_MAXED)
                            {
                                // Log.d(TAG, "size maxed, getting next 4 bytes");
                                try
                                {
                                    mCmdLength = mBBuff.getInt();
                                }
                                catch (java.nio.BufferUnderflowException e)
                                {
                                    continue;
                                }
                            }
                            else
                            {
                                mCmdLength = mCmd >> CMD_BITS;
                            }
                            mCmd = mCmd & CMD_MAX;
                            // Log.d(TAG,
                            //       String.format("Decoded mCmd=%d mCmdLength=%d",
                            //                     mCmd, mCmdLength));
                        }
                        int blen = mBBuff.remaining();
                        if (blen < mCmdLength)
                        {
                            // Log.d(TAG,
                            //       String.format(
                            //           "need more: blen=%d mCmdLength=%d",
                            //           blen, mCmdLength));
                            continue;
                        }
                        else 
                        {
                            decodeStringData(blen);
                            mCmd = CMD_NONE;
                            mCmdLength = 0;
                        }
                    }
                    catch (IndexOutOfBoundsException e)
                    {
                        Log.w(TAG, e);
                    }
                }
            } while (!Thread.interrupted() && (count > -1));
        }
        catch (IOException e)
        {
            Log.w(TAG, e);
        }
    }

    protected void decodeStringData(int count)
    {
        int length = (int)mCmdLength;
        int oldlim = -1;

        if (mBBuff.remaining() > length)
        {
            // Log.d(TAG, "Extra data, limiting buffer");
            oldlim = mBBuff.limit();
            mBBuff.limit(mBBuff.position() + length);
        }
        
        CoderResult result = mDecoder.decode(mBBuff, mCBuff, false);
        if (result.isUnderflow() &&
            (mBBuff.limit() == mBBuff.capacity()))
        {
            mBBuff.compact();
            mBBuff.limit(mBBuff.position());
            mBBuff.position(0);
        }

        // Log.d(TAG, String.format("decodeStringData: length=%d size=%d", length,
        //                          mCBuff.position()));
        if (mCmd == CMD_TTY)
        {
            // Log.d(TAG, "TTY DATA");
            mConn.putString(mChars, length);
        }
        else if (mCmd == CMD_CMD)
        {
            String data = new String(mChars, 0, length);
            Log.d(TAG, String.format("CMD: %s", data));
            mConn.handleCmd(data);
        }
        
        mCBuff.clear();
        if (oldlim > -1)
        {
            mBBuff.limit(oldlim);
        }
    }
    
    public void start()
    {
        mThread = new Thread(this);
        mThread.setName("Transport");
        mThread.setDaemon(true);
        mThread.start();
    }

    public void stop()
    {
        mThread.interrupt();
    }

    public boolean isConnected()
    {
        return mConnected;
    }
    
    public abstract void connect();
    public abstract int read(byte[] buffer, int offset, int length)
        throws IOException;
    public abstract void write(byte[] buffer) throws IOException;
    public abstract void write(int c) throws IOException;
    public abstract void flush() throws IOException;
    public abstract void close();
}
