Tests using the following commands, provided by the spec:

    input=/usr/local/cs/jdk-16.0.1/lib/modules
    time gzip <$input >gzip.gz
    time pigz <$input >pigz.gz
    time java Pigzj <$input >Pigzj.gz
    ls -l gzip.gz pigz.gz Pigzj.gz

Original file size = 125942959 B

gzip compressed size = 43261332 B
compression ratio = 2.91
real	0m7.719s    0m7.937s    0m7.842s
user	0m7.456s    0m7.466s    0m7.453s
sys     0m0.047s    0m0.066s    0m0.051s

pigz compressed size = 43134815 B
compression ratio = 2.92
real	0m2.262s    0m2.271s    0m2.279s
user	0m7.358s    0m7.356s    0m7.373s
sys	    0m0.041s    0m0.038s    0m0.032s

Pigzj compressed size = 43136282 B
compression ratio = 2.92
real	0m3.214s    0m3.233s    0m3.411s
user	0m9.506s    0m9.433s    0m10.033s
sys	    0m0.227s    0m0.264s    0m0.275s

Concerns:
As the number of threads scales up, there will be ...

References:
Oracle's Java documentation - https://docs.oracle.com/en/java/javase/16/docs/api/index.html
Tutorial on Java thread pools - http://tutorials.jenkov.com/java-concurrency/thread-pools.html
OpenJDK implementation of java.zip classes - 
    http://hg.openjdk.java.net/jdk7/jdk7/jdk/file/00cd9dc3c2b5/src/share/classes/java/util/zip/
