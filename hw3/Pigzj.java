import java.util.zip.*;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.nio.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

import java.io.*;

public class Pigzj {

    private static final int BLOCK_SIZE = 128*1024;  // 128 KiB
    private static final int DICT_SIZE = 32*1024;  // 32 KiB
    private final static int GZIP_MAGIC = 0x8b1f;

    private static int numProcessors = Runtime.getRuntime().availableProcessors();

    private static AtomicReferenceArray<Boolean> threadsRunning;
    public static void main(String[] args) {
        if(args.length != 0 && args.length != 2) {
            // error
            return;
        }
        if(args.length == 2) {
            if(args[0].equals("-p")) {
                numProcessors = Integer.parseInt(args[1]);
            } else {
                // error
                return;
            }
        }
        
        // new Thread(new Pigzj()).start();

        ByteArrayOutputStream out = new ByteArrayOutputStream(BLOCK_SIZE);
        
        InputStream in = System.in;
        byte[] block = new byte[BLOCK_SIZE];
        
        try {
            PigzjOutputStream pigzjOut = new PigzjOutputStream(out);

            int len;
            while((len = in.read(block, 0, BLOCK_SIZE)) > 0) {
                pigzjOut.write(block, 0, len);
            }
            pigzjOut.finish();

            out.writeTo(System.out);
            out.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}

public class PigzjOutputStream extends DeflaterOutputStream {
    
    private CRC32 crc = new CRC32();
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;

    public PigzjOutputStream(OutputStream out, int size) throws IOException {
        super(out, new Deflater(Deflater.DEFAULT_COMPRESSION, true), size);
        writeHeader();
        crc.reset();
    }

    public PigzjOutputStream(OutputStream out) throws IOException {
        this(out, 512);
    }

    // public synchronized void write(byte[] b, int off, int len) throws IOException {
    //     super.write(b, off, len);
    //     crc.update(b, off, len);
    // }

    public synchronized void write(byte[] b, int off, int len, byte[] dict, int dictOff, int dictLen) throws IOException {
        def.setDictionary(dict, dictOff, dictLen);
        super.write(b, off, len);
        crc.update(b, off, len);
    }

    /*
     * Writes GZIP member header.
     */
    private final static byte[] header = {
        (byte) GZIP_MAGIC,                // Magic number (short)
        (byte)(GZIP_MAGIC >> 8),          // Magic number (short)
        Deflater.DEFLATED,                // Compression method (CM)
        0,                                // Flags (FLG)
        0,                                // Modification time MTIME (int)
        0,                                // Modification time MTIME (int)
        0,                                // Modification time MTIME (int)
        0,                                // Modification time MTIME (int)
        0,                                // Extra flags (XFLG)
        0                                 // Operating system (OS)
    };

    private void writeHeader() throws IOException {
        out.write(header);
    }

    /*
     * Writes GZIP member trailer to a byte array, starting at a given
     * offset.
     */
    private void writeTrailer(byte[] buf, int offset) throws IOException {
        writeInt((int)crc.getValue(), buf, offset); // CRC-32 of uncompr. data
        writeInt(def.getTotalIn(), buf, offset + 4); // Number of uncompr. bytes
    }

    /*
     * Writes integer in Intel byte order to a byte array, starting at a
     * given offset.
     */
    private void writeInt(int i, byte[] buf, int offset) throws IOException {
        writeShort(i & 0xffff, buf, offset);
        writeShort((i >> 16) & 0xffff, buf, offset + 2);
    }

    /*
     * Writes short integer in Intel byte order to a byte array, starting
     * at a given offset
     */
    private void writeShort(int s, byte[] buf, int offset) throws IOException {
        buf[offset] = (byte)(s & 0xff);
        buf[offset + 1] = (byte)((s >> 8) & 0xff);
    }

    /**
     * Finishes writing compressed data to the output stream without closing
     * the underlying stream. Use this method when applying multiple filters
     * in succession to the same output stream.
     * @exception IOException if an I/O error has occurred
     */
    public void finish() throws IOException {
        if (!def.finished()) {
            def.finish();
            while (!def.finished()) {
                int len = def.deflate(buf, 0, buf.length);
                if (len > 0)
                    out.write(buf, 0, len);
            }
            byte[] trailer = new byte[TRAILER_SIZE];
            writeTrailer(trailer, 0);
            out.write(trailer);
        }
    }

}
