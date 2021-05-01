import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.zip.CRC32;
import java.util.zip.Deflater;
import java.util.zip.DeflaterOutputStream;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;

// TODO: Split classes into separate files
// TODO: Exception and error handling
// TODO: Improve efficiency (e.g. have main thread stitch together compressed blocks in order)

public class Pigzj {

    private static int availableProcessors = Runtime.getRuntime().availableProcessors();
    public static void main(String[] args) throws IOException {
        if(args.length != 0 && args.length != 2) {
            System.err.println("Unrecognized arguments");
            return;
        }

        int numProcessors = availableProcessors;
        if(args.length == 2) {
            if(args[0].equals("-p")) {
                try {
                    numProcessors = Integer.parseInt(args[1]);
                    if(numProcessors < 0 || numProcessors >= availableProcessors) {
                        System.err.println("Invalid number of processes");
                        return;
                    }
                } catch(NumberFormatException e) {
                    System.err.println("Invalid number of processes");
                    return;
                }
            } else {
                System.err.println("Unrecognized option");
                return;
            }
        }
        
        InputStream in = System.in;
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        PigzjOutputStream pigzjOut = new PigzjOutputStream(in, out, numProcessors);

        pigzjOut.write();
        pigzjOut.finish();
        pigzjOut.flush();
        
        out.writeTo(System.out);
        out.flush();

        in.close();
        pigzjOut.close();
        out.close();
    }

}

public class PigzjOutputStream extends DeflaterOutputStream {
    
    private CRC32 crc = new CRC32();

    private static final int BLOCK_SIZE = 128*1024;  // 128 KiB
    private static final int DICT_SIZE = 32*1024;  // 32 KiB
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;

    private InputStream in; 

    private AtomicInteger totalIn;
    private AtomicInteger nextBlock;
    private List<PigzjThread> runnables = new ArrayList<>();
    private BlockingQueue<Runnable> taskQueue = null;

    private class PigzjTask implements Runnable {

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
                    // Blocking until previous data block has been written
                    while(nextBlock.get() != tid) ;
                    
                    try {
                        synchronized(out) {
                            out.write(this.buf, 0, dlen);
                        }
                    } catch(IOException e) {
                        e.printStackTrace();
                    }
                }
            }

            // Ensure that all contents are written out
            if(isLast) {
                def.finish();
                while (!def.finished()) {
                    int dlen = def.deflate(buf, 0, buf.length);
                    if (dlen > 0) {
                        try {
                            synchronized(out) {
                                out.write(this.buf, 0, dlen);
                            }
                        } catch(IOException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }

            totalIn.addAndGet(def.getTotalIn());
            nextBlock.incrementAndGet();
        }

        public PigzjTask(int tid, byte[] b, int off, int len, byte[] dictionary, boolean isLast) {
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

    private class PigzjThread implements Runnable {

        private Thread thread;
        private boolean isStopped;
        private BlockingQueue<Runnable> taskQueue;

        public PigzjThread(BlockingQueue<Runnable> taskQueue) {
            this.isStopped = false;
            this.taskQueue = taskQueue;
        }
    
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

        public synchronized void finish() {
            isStopped = true;
            this.thread.interrupt();
        }

        public synchronized boolean isStopped() {
            return isStopped;
        }
    
    }

    public PigzjOutputStream(InputStream in, OutputStream out, int nThreads) throws IOException {
        super(out);
        this.nextBlock = new AtomicInteger(0);
        this.totalIn = new AtomicInteger(0);
        this.in = in;

        initializePoolThread(nThreads);
        writeHeader();
        crc.reset();
    }

    private void initializePoolThread(int nThreads) {
        this.taskQueue = new ArrayBlockingQueue<>(nThreads);
        for(int i=0; i<nThreads; i++) {
            runnables.add( new PigzjThread(this.taskQueue) );
        }
        for(PigzjThread t: runnables) {
            new Thread(t).start();
        }
    }

    public synchronized void write() throws IOException {
        int currBlock = 0;
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

                // Create task
                boolean isLast = (total < BLOCK_SIZE);
                byte[] copyOfBlock = Arrays.copyOf(block, total);
                byte[] copyOfDictionary = (dictionary == null) ? null : Arrays.copyOf(dictionary, dictionary.length);
                taskQueue.put(new PigzjTask(currBlock, copyOfBlock, 0, total, copyOfDictionary, isLast));

                currBlock++;
                crc.update(block, 0, total);
                dictionary = Arrays.copyOfRange(block, BLOCK_SIZE-DICT_SIZE, BLOCK_SIZE);
            }

            // Blocking until all threads have finished compressing and outputting
            while(nextBlock.get() != currBlock) {
                Thread.sleep(1);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch(InterruptedException e) {

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

    public void finish() throws IOException {
        byte[] trailer = new byte[TRAILER_SIZE];
        writeTrailer(trailer, 0);
        out.write(trailer);
    }
}
