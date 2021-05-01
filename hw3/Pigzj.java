import java.util.zip.*;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.nio.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;

import java.io.*;

public class Pigzj {

    private static int numProcessors = Runtime.getRuntime().availableProcessors();
    public static void main(String[] args) throws IOException {
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
        
        InputStream in = System.in;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        PigzjOutputStream pigzjOut = new PigzjOutputStream(in, out, numProcessors);

        pigzjOut.write();
        pigzjOut.flush();
        
        out.writeTo(System.out);
        out.flush();

        in.close();
        pigzjOut.close();
        out.close();
    }

}

public class PigzjOutputStream extends DeflaterOutputStream {
    
    private AtomicInteger numBlocks;
    private AtomicInteger threadsRunning;
    private AtomicInteger totalIn;

    private CRC32 crc = new CRC32();

    private static final int BLOCK_SIZE = 128*1024;  // 128 KiB
    private static final int DICT_SIZE = 32*1024;  // 32 KiB
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;
    
    private final int NTHREADS;
    private InputStream in;

    private class PigzjThread implements Runnable {

        private int tid;

        private byte[] b;
        private int off;
        private int len;
        private byte[] dictionary;
        private boolean isLast;

        private byte[] buf;
        private Deflater def;
    
        public void run() {
            if(dictionary != null)
                def.setDictionary(dictionary);

            
            def.setInput(b, off, len);

            while(!def.needsInput()) {
                int dlen = def.deflate(buf, 0, buf.length, Deflater.SYNC_FLUSH);
                if (dlen > 0) {
                    while(numBlocks.get() != tid) ;
                    
                    try {
                        writeOut(buf, 0, dlen);
                    } catch(Exception e) {
                        e.printStackTrace();
                    }
                }
            }

            if(isLast) {
                def.finish();
                while (!def.finished()) {
                    int len = def.deflate(buf, 0, buf.length);
                    if (len > 0) {
                        try {
                            writeOut(buf, 0, len);
                        } catch(Exception e) {
                            e.printStackTrace();
                        }
                    }
                }
            }

            totalIn.addAndGet(def.getTotalIn());
            numBlocks.incrementAndGet();
            threadsRunning.decrementAndGet();
        }
    
        public PigzjThread(int tid, byte[] b, int off, int len, byte[] dictionary, boolean isLast) {
            this.tid = tid;

            this.b = b;
            this.off = off;
            this.len = len;
            this.dictionary = dictionary;

            this.buf = new byte[len];
            this.def = new Deflater(Deflater.DEFAULT_COMPRESSION, true);

            this.isLast = isLast;
        }
    
    }

    public PigzjOutputStream(InputStream in, OutputStream out, int nThreads) throws IOException {
        super(out);
        this.numBlocks = new AtomicInteger(0);
        this.threadsRunning = new AtomicInteger(0);
        this.totalIn = new AtomicInteger(0);
        this.in = in;
        this.NTHREADS = nThreads;

        writeHeader();
        crc.reset();
    }

    public void write() throws IOException {
        int currBlock = 0;
        byte[] block = new byte[BLOCK_SIZE];
        byte[] dictionary = null;

        try {
            int len;
            while((len = in.read(block, 0, BLOCK_SIZE)) > 0) {
                int total = len;
                while((len = in.read(block, total, BLOCK_SIZE-total)) > 0) {
                    total += len;
                }

                while(threadsRunning.get() == NTHREADS) ;

                boolean isLast = (total < BLOCK_SIZE);

                if(dictionary == null) {
                    new Thread(new PigzjThread(currBlock, Arrays.copyOf(block, total), 0, total, null, isLast)).start();
                } else {
                    new Thread(new PigzjThread(currBlock, Arrays.copyOf(block, total), 0, total, Arrays.copyOf(dictionary, dictionary.length), isLast)).start();
                }

                currBlock++;
                updateCRC(block, 0, total);
                threadsRunning.incrementAndGet();
                dictionary = Arrays.copyOfRange(block, BLOCK_SIZE-DICT_SIZE, BLOCK_SIZE);
            }

            while(threadsRunning.get() > 0) {
                Thread.sleep(1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        byte[] trailer = new byte[TRAILER_SIZE];
        writeTrailer(trailer, 0);
        out.write(trailer);
    }

    private synchronized void updateCRC(byte[] b, int off, int len) {
        crc.update(b, off, len);
    }

    private synchronized void writeOut(byte[] b, int off, int len) throws IOException {
        out.write(b, off, len);
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
        writeInt(totalIn.get(), buf, offset + 4); // Number of uncompr. bytes
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

}
