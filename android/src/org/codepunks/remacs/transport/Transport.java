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

import org.codepunks.remacs.ConnectionCfg;
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
    public final long CMD_NONE = -1;
    public final long CMD_DATA = 0;
    public final long CMD_TTY = 1;
    public final long CMD_NOTIFY = 2;
    public final long CMD_MAX = 7;
    public final long CMD_CMDS = 1 | 2 | 4;
    public final long CMD_SIZE_MAX = 15;
    public final long CMD_SIZE_MAXED = 128;

    protected ConsoleTTY mTty;
    protected ConnectionCfg mCfg;
    protected Thread mThread;
    // String decoding
    protected CharsetDecoder mDecoder;
    protected ByteBuffer mBBuff;
    protected CharBuffer mCBuff;
    protected byte[] mBytes;
    protected char[] mChars;
    protected byte[] mWAttrs;
    protected float[] mWidths;
    protected int mCWidth;
    // Remacs Protocol handling
    protected long mCmd = CMD_NONE;
    protected long mCmdLength = 0;
    protected byte[] mCharBuff = new byte[1];
    
    public Transport(ConsoleTTY tty, ConnectionCfg cfg, int def_port)
    {
        mTty = tty;
        mCfg = cfg;
        //mCfg.def_port = def_port;
        Charset cs = Charset.forName(mCfg.charset);
        mDecoder = cs.newDecoder();
        mDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
        mDecoder.onMalformedInput(CodingErrorAction.REPLACE);
    }

    public void putString(String str)
    {
        mTty.putString(str);
    }

    public void sendCmd(long cmd, String data)
    {
        Log.d(TAG, String.format("Sending cmd=%d: %s", cmd, data));
        try
        {
            sendCmd(cmd, data.getBytes(mCfg.charset));
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
            Log.d(TAG, String.format("Sending cmd=%d len=%d", cmd, length));

            if ((data != null) && (length > 0))
            {
                byte[] cbuff;
                long offset;
                if (length <= CMD_SIZE_MAX + 1)
                {
                    cmd = cmd + (length << 3);
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

    public void sendTTY(boolean initial)
    {
        String data;

        if (initial)
        {
            data = String.format("term=%s;row=%d;col=%d", mCfg.term,
                                 mCfg.term_height, mCfg.term_width);
        }
        else
        {
            data = String.format("row=%d;col=%d", mCfg.term_width,
                                 mCfg.term_height);
        }
        sendCmd(CMD_TTY, data);
    }

    public void sendData(String data)
    {
        sendCmd(CMD_DATA, data);
    }
    
    public void sendData(byte[] data)
    {
        sendCmd(CMD_DATA, data);
    }

    public void sendData(int data)
    {
        mCharBuff[0] = (byte)(data & 0xFF);
        sendCmd(CMD_DATA, mCharBuff);
    }
    
    public void run()
    {
        connect();
        sendTTY(true);
        
        try
        {
            int count;
            mBBuff = ByteBuffer.allocate(BUFFER_SIZE);
            mCBuff = CharBuffer.allocate(BUFFER_SIZE);
            mBytes = mBBuff.array();
            mChars = mCBuff.array();
            mWAttrs = new byte[BUFFER_SIZE];
            mWidths = new float[BUFFER_SIZE];
            mCWidth = mTty.getCharWidth();

            mBBuff.limit(0);
            do
            {
                count = read(mBytes, mBBuff.arrayOffset() + mBBuff.limit(),
                             mBBuff.capacity() - mBBuff.limit());
                if (count > 0)
                {
                    mBBuff.limit(mBBuff.limit() + count);
                    try
                    {
                        if (mCmd == CMD_NONE)
                        {
                            mCmd = mBBuff.get();
                            mCmd = (mCmd & 0xFF);
                            Log.d(TAG, String.format("unpacked cmd=%d", mCmd));
                            if ((mCmd & CMD_SIZE_MAXED) != 0)
                            {
                                Log.d(TAG, "size maxed, getting next byte");
                                mCmdLength = mBBuff.getInt();
                            }
                            else
                            {
                                mCmdLength = mCmd >> 3;
                            }
                            mCmd = mCmd & CMD_CMDS;
                            Log.d(TAG,
                                  String.format("Decoded mCmd=%d mCmdLength=%d",
                                                mCmd, mCmdLength));
                        }
                        int blen = mBBuff.remaining();
                        if (blen < mCmdLength)
                        {
                            Log.d(TAG,
                                  String.format(
                                      "need more: blen=%d mCmdLength=%d",
                                      blen, mCmdLength));
                            continue;
                        }
                        else 
                        {
                            decodeStringData(blen);
                            mCmd = CMD_NONE;
                        }
                    }
                    catch (IndexOutOfBoundsException ex)
                    {
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

        CoderResult result = mDecoder.decode(mBBuff, mCBuff, false);
        if (result.isUnderflow() &&
            (mBBuff.limit() == mBBuff.capacity()))
        {
            mBBuff.compact();
            mBBuff.limit(mBBuff.position());
            mBBuff.position(0);
        }

        if (mCmd == CMD_DATA)
        {
            Log.d(TAG, String.format("DATA: length=%d size=%d", length,
                                     mCBuff.position()));
            mTty.getTextWidths(mChars, length, mWidths);
            for (int i = 0; i < length; ++i)
            {
                if ((int)mWidths[i] != mCWidth)
                {
                    mWAttrs[i] = (byte)1;
                }
                else
                {
                    mWAttrs[i] = (byte)0;
                }
            }
            mTty.putString(mChars, mWAttrs, 0, length);
        }
        else if (mCmd == CMD_NOTIFY)
        {
            Log.d(TAG, String.format("NOTIFY: %s",
                                     new String(mChars, 0, length)));
            mTty.putString(mChars, mWAttrs, 0, length);
        }
        
        mCBuff.clear();
        mTty.redraw();
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
    
    public abstract void connect();
    public abstract int read(byte[] buffer, int offset, int length)
        throws IOException;
    public abstract void write(byte[] buffer) throws IOException;
    public abstract void write(int c) throws IOException;
    public abstract void flush() throws IOException;
    public abstract void close();
}
