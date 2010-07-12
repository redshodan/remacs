package org.codepunks.remacs;

import android.util.Log;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;

public abstract class Transport implements Runnable
{
    protected static final String TAG = "Remacs";
    public static final int BUFFER_SIZE = 4096;

    protected ConsoleTTY mTty;
    protected TransportCfg mCfg;
    protected CharsetDecoder mDecoder;
    
    public Transport(ConsoleTTY tty, TransportCfg cfg, int default_port)
    {
        mTty = tty;
        mCfg = cfg;
        mCfg.default_port = default_port;
        Charset cs = Charset.forName(mCfg.charset);
        mDecoder = cs.newDecoder();
        mDecoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
        mDecoder.onMalformedInput(CodingErrorAction.REPLACE);
    }

    public void putString(String str)
    {
        mTty.putString(str);
    }

    public void run()
    {
        connect();
        
        try
        {
            int length;
            int count;
            ByteBuffer bbuff = ByteBuffer.allocate(BUFFER_SIZE);
            CharBuffer cbuff = CharBuffer.allocate(BUFFER_SIZE);
            byte[] bytes = bbuff.array();
            char[] chars = cbuff.array();
            byte[] wattrs = new byte[BUFFER_SIZE];
            float[] widths = new float[BUFFER_SIZE];
            int cwidth = mTty.getCharWidth();

            bbuff.limit(0);
            do
            {
                count = read(bytes, bbuff.arrayOffset() + bbuff.limit(),
                             bbuff.capacity() - bbuff.limit());
                if (count > 0)
                {
                    bbuff.limit(bbuff.limit() + count);
                    CoderResult result = mDecoder.decode(bbuff, cbuff, false);
					if (result.isUnderflow() &&
                        (bbuff.limit() == bbuff.capacity()))
                    {
						bbuff.compact();
						bbuff.limit(bbuff.position());
						bbuff.position(0);
					}
                    length = cbuff.position();
                    mTty.getTextWidths(chars, length, widths);
                    for (int i = 0; i < length; ++i)
                    {
                        if ((int)widths[i] != cwidth)
                        {
                            wattrs[i] = (byte)1;
                        }
                        else
                        {
                            wattrs[i] = (byte)0;
                        }
                    }
                    mTty.putString(chars, wattrs, 0, cbuff.position());
                    cbuff.clear();
                    mTty.redraw();
                }
            } while (count > -1);
        }
        catch (IOException e)
        {
            Log.w(TAG, e);
        }
    }
        
    public abstract void connect();
    public abstract int read(byte[] buffer, int offset, int length)
        throws IOException;
    public abstract void write(byte[] buffer) throws IOException;
    public abstract void write(int c) throws IOException;
    public abstract void flush() throws IOException;
	public abstract void close();
}
