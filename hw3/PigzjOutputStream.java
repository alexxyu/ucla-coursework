import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class PigzjOutputStream {
    
    private CRC32 crc = new CRC32();

    private static final int BLOCK_SIZE = 131072;  // 128 KiB
    private static final int DICT_SIZE = 32768;  // 32 KiB
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;

    private InputStream in; 
    private ByteArrayOutputStream out;

    private AtomicInteger nextBlock;
    private List<PigzjThread> runnables = new ArrayList<>();
    private BlockingQueue<Runnable> taskQueue = null;
    private ConcurrentHashMap<Integer, byte[]> compressedBlocks;

    private int totalBytesIn;

    private class PigzjTask implements Runnable {

        private int tid;

        private byte[] b;
        private int off;
        private int len;
        private byte[] dictionary;
        private boolean isLast;

        private byte[] buf;
        private Deflater def;

        /*
         * Constructor for PigzjTask class.
         */
        public PigzjTask(int tid, byte[] b, int off, int len, byte[] dictionary, boolean isLast) {
            this.tid = tid;

            this.b = b;
            this.off = off;
            this.len = len;
            this.dictionary = dictionary;

            this.buf = new byte[BLOCK_SIZE];
            this.def = new Deflater(Deflater.DEFAULT_COMPRESSION, true);

            this.isLast = isLast;
        }

        /*
         * Compresses a single block using given member variables as parameters.
         */
        public void run() {
            if(dictionary != null)
                def.setDictionary(dictionary);

            def.setInput(b, off, len);

            int totalCompressed = 0;
            while(!def.needsInput()) {
                int dlen = def.deflate(buf, totalCompressed, buf.length-totalCompressed, Deflater.SYNC_FLUSH);
                if (dlen > 0) {
                    totalCompressed += dlen;
                }
            }

            // Ensure that all contents are written out
            if(isLast) {
                def.finish();
                while (!def.finished()) {
                    int dlen = def.deflate(buf, totalCompressed, buf.length-totalCompressed, Deflater.SYNC_FLUSH);
                    if (dlen > 0) {
                        totalCompressed += dlen;
                    }
                }
            }

            compressedBlocks.putIfAbsent(tid, Arrays.copyOf(buf, totalCompressed));
            nextBlock.incrementAndGet();
        }

    }

    private class PigzjThread implements Runnable {

        private Thread thread;
        private boolean isStopped;
        private BlockingQueue<Runnable> taskQueue;

        /*
         * Constructor for PigzjThread class.
         */
        public PigzjThread(BlockingQueue<Runnable> taskQueue) {
            this.isStopped = false;
            this.taskQueue = taskQueue;
        }
    
        /*
         * Continuously waits for available task and runs it.
         */
        public void run() {
            this.thread = Thread.currentThread();
            while(!isStopped()) {
                try {
                    // Wait until a task is available, take it, and run it
                    PigzjTask task = (PigzjTask) taskQueue.take();
                    task.run();
                } catch(InterruptedException e) {}
            }
        }

        /*
         * Stops and interrupts this thread.
         */
        public synchronized void finish() {
            isStopped = true;
            this.thread.interrupt();
        }

        /*
         * Returns whether this thread has been stopped.
         */
        public synchronized boolean isStopped() {
            return isStopped;
        }
    
    }

    /*
     * Constructor for PigzjOutputStream class.
     */
    public PigzjOutputStream(InputStream in, ByteArrayOutputStream out, int nThreads) {
        this.nextBlock = new AtomicInteger(0);
        this.totalBytesIn = 0;
        this.in = in;
        this.out = out;
        this.compressedBlocks = new ConcurrentHashMap<>();

        initializePoolThread(nThreads);
        writeHeader();
        crc.reset();
    }

    /*
     * Initializes member variables used for Pigzj's thread pool. 
     */
    private void initializePoolThread(int nThreads) {
        this.taskQueue = new ArrayBlockingQueue<>(nThreads);
        for(int i=0; i<nThreads; i++) {
            runnables.add( new PigzjThread(this.taskQueue) );
        }
        for(PigzjThread t: runnables) {
            new Thread(t).start();
        }
    }

    /*
     * Manages compression of input through task delegation to other threads
     * and outputting compressed blocks.
     */
    public synchronized void compress() {
        int currBlock = 0, blockToOutput = 0;
        byte[] block = new byte[BLOCK_SIZE];
        byte[] dictionary = null;

        try {
            int len;
            while((len = in.read(block, 0, BLOCK_SIZE)) > 0) {
                // Read until block buffer is full
                int total = len;
                while((len = in.read(block, total, BLOCK_SIZE-total)) > 0) {
                    total += len;
                }

                // Create task and add it to the task queue
                boolean isLast = (total < BLOCK_SIZE);
                byte[] copyOfBlock = Arrays.copyOf(block, total);
                byte[] copyOfDictionary = (dictionary == null) ? null : Arrays.copyOf(dictionary, dictionary.length);

                while(!taskQueue.offer(new PigzjTask(currBlock, copyOfBlock, 0, total, copyOfDictionary, isLast))) {
                    // If all threads are busy, output any newly compressed blocks
                    if(compressedBlocks.containsKey(blockToOutput)) {
                        byte[] b = compressedBlocks.get(blockToOutput++);
                        if(b == null) {
                            System.err.println("Error while compressing data in order");
                        }

                        out.write(b);
                        out.writeTo(System.out);
                        out.reset();
                    } else {
                        Thread.sleep(1);
                    }
                }

                currBlock++;
                totalBytesIn += total;
                crc.update(block, 0, total);
                dictionary = Arrays.copyOfRange(block, BLOCK_SIZE-DICT_SIZE, BLOCK_SIZE);
            }

            // Blocking until all threads have finished compressing and outputting
            while(nextBlock.get() != currBlock) {
                Thread.sleep(1);
            }
        } catch (IOException e) {
            System.err.printf("I/O error: %s\n", e.getMessage());
            System.exit(1);
        } catch(InterruptedException e) {}

        // Output any remaining compressed blocks
        while(blockToOutput < currBlock) {
            byte[] b = compressedBlocks.get(blockToOutput++);
            if(b == null) {
                System.err.println("Error while compressing data in order");
            }

            try {
                out.write(b);
                out.writeTo(System.out);
                out.reset();
            } catch(IOException e) {
                System.err.printf("Write error: %s\n", e.getMessage());
                System.exit(1);
            }
        }

        // Stop created threads
        for(PigzjThread runnable: runnables){
            runnable.finish();
        }
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

    /*
     * Writes GZIP member header out.
     */
    private void writeHeader() {
        try {
            out.write(header);
            out.writeTo(System.out);
            out.reset();
        } catch(IOException e) {
            System.err.printf("Write error: %s\n", e.getMessage());
            System.exit(1);
        }
    }

    /*
     * Writes GZIP member trailer to a byte array, starting at a given
     * offset.
     */
    private void writeTrailer(byte[] buf, int offset) {
        writeInt((int)crc.getValue(), buf, offset); // CRC-32 of uncompr. data
        writeInt(totalBytesIn, buf, offset + 4); // Number of uncompr. bytes
    }

    /*
     * Writes integer in Intel byte order to a byte array, starting at a
     * given offset.
     */
    private void writeInt(int i, byte[] buf, int offset) {
        writeShort(i & 0xffff, buf, offset);
        writeShort((i >> 16) & 0xffff, buf, offset + 2);
    }

    /*
     * Writes short integer in Intel byte order to a byte array, starting
     * at a given offset
     */
    private void writeShort(int s, byte[] buf, int offset) {
        buf[offset] = (byte)(s & 0xff);
        buf[offset + 1] = (byte)((s >> 8) & 0xff);
    }

    /*
     * Finishes compression by writing GZIP member trailer out.
     */
    public synchronized void finish() {
        byte[] trailer = new byte[TRAILER_SIZE];
        try {
            writeTrailer(trailer, 0);
    
            out.write(trailer);
            out.writeTo(System.out);
            out.reset();
        } catch(IOException e) {
            System.err.printf("Write error: %s\n", e.getMessage());
            System.exit(1);
        }
    }
}