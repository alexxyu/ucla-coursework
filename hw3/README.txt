I ran tests comparing gzip, pigz, and Pigzj on SEASnet server 12, using the following commands, 
which were provided by the spec.

    input=/usr/local/cs/jdk-16.0.1/lib/modules
    time gzip <$input >gzip.gz
    time pigz <$input >pigz.gz
    time java Pigzj <$input >Pigzj.gz
    ls -l gzip.gz pigz.gz Pigzj.gz

These tests used version 2.6 of pigz, version 1.10 of gzip, and OpenJDK version 16.0.1. Each 
compression program was run 3 times, and 4 threads were used for pigz and Pigzj (the number
of available processors on the SEASnet server). 

The performance data compiled from these runs are summarized below.

Original file size = 125942959 B

gzip compressed size = 43261332 B (compression ratio = 2.91)
        run 1       run 2       run 3       average
real	0m7.719s    0m7.937s    0m7.842s    0m7.833s
user	0m7.456s    0m7.466s    0m7.453s    0m7.458s
sys     0m0.047s    0m0.066s    0m0.051s    0m0.057s

pigz compressed size = 43134815 B (compression ratio = 2.92)
        run 1       run 2       run 3       average
real	0m2.262s    0m2.271s    0m2.279s    0m2.271s
user	0m7.358s    0m7.356s    0m7.373s    0m7.362s
sys	    0m0.041s    0m0.038s    0m0.032s    0m0.037s

Pigzj compressed size = 43136282 B (compression ratio = 2.92)
        run 1       run 2       run 3       average
real	0m2.538s    0m2.515s    0m2.460s    0m2.504s
user	0m7.558s    0m7.538s    0m7.535s    0m7.544s
sys	    0m0.195s    0m0.187s    0m0.171s    0m0.184s

Defintion:
real - actual elapsed time for the call
user - amount of CPU time in user-mode within process
sys - amount of CPU time in kernel within process

From these tests, my implementation of Pigzj clearly performs better than gzip and is competitive
with pigz in terms of wall clock time. 

strace observations:
Along with the aforementioned runs analyzing their performance, I used 'strace' to generate traces
of system calls used by each of the compression programs on the same input file as above. There 
were 6526 system calls executed by gzip, 1550 system calls executed by pigz, and 181 system calls
executed by Pigzj. Clearly, the number of system calls distinguishes pigz and Pigzj, which run 
faster, from gzip, which runs slower. However, system calls are not enough on their own to explain 
the difference in performance between the three programs, as Pigzj runs slower than pigz despite
the fact that it uses fewer system calls.

In terms of types of system calls invoked, gzip primarily executed 'read' and 'write' while pigz
primarily executed 'read' and 'futex'. Pigzj used many different calls that mostly consisted of
'mmap' (and related system calls) and calls relating to opening JDK files.

Concerns about scalability:
As the number of threads scales up, I expect more time to be spent blocking on synchronized and
atomic variables since there may be more contention for resources. In other words, there will be
greater overhead maintaining safe thread accesses. Thus, at a certain point, I anticipate that 
Pigzj will stop scaling as the drawbacks of synchronization outweigh begin to overtake the benefits 
of concurrency.

Discrepencies between Pigzj and pigz's compression results:
My implementation of Pigzj does not produce output that is byte-for-byte identical with that of 
pigz. From the performance test noted above, there is roughly a 1400 B difference between Pigzj's
compressed output and pigz's compressed output. I am not entirely certain what are the sources of 
this discrepency, but I have a few guesses backed by evidence. For one, I found that the headers 
are slightly different. Using 'xxd' to dump the .gz files to hex allowed me to find that the 
header of Pigzj's output is two bytes shorter. 

Furthermore, the pigz manual includes the following information:
    Each partial raw deflate stream is terminated by an empty stored block (using the
    Z_SYNC_FLUSH option of zlib), in order to end that partial bit stream at a byte boundary. That
    allows the partial streams to be concatenated simply as sequences of bytes. This adds a very
    small four to five byte overhead to the output for each input chunk.
It is possible that the concatenation of the empty stored block differs between Pigzj and pigz.
From the hex dump of the .gz files, I found the first difference after the headers was the 
following sequence of bytes (spaces added for clarity):

    pigz:   f5ff 0820 8000   94bd
    Pigzj:  f5ff 0000 00ff ff94

Pigzj appears to have a 5 byte chunk in a location where pigz has a 4 byte chunk, and this may
be due to different overheads provided by the "sync flush" option between Pigzj and pigz. I cannot
confirm what are the differences past this one, so my best guess is that the header and the 
"sync flush" option account for the differences between Pigzj's and pigz's compressed outputs.

References:
Oracle's Java documentation - https://docs.oracle.com/en/java/javase/16/docs/api/index.html
Tutorial on Java thread pools - http://tutorials.jenkov.com/java-concurrency/thread-pools.html
OpenJDK implementation of java.zip classes - 
    http://hg.openjdk.java.net/jdk7/jdk7/jdk/file/00cd9dc3c2b5/src/share/classes/java/util/zip/
pigz manual - https://zlib.net/pigz/pigz.pdf