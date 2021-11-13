from pyspark import SparkContext

sc = SparkContext("local", "BookPairs")
lines = sc.textFile("/home/cs143/data/goodreads.user.books")
lines = lines.map(lambda x: x[x.find(':')+1:])

bookPairs = lines.flatMap(lambda line: [(int(b1), int(b2)) for b2 in line.split(',') for b1 in line.split(',') if b1 != b2])
bookPairsUniq = bookPairs.filter(lambda pair: pair[0] < pair[1])
bookPairs1s = bookPairsUniq.map(lambda pair: (pair, 1))
pairCounts = bookPairs1s.reduceByKey(lambda a, b: a+b)
pairCountsFiltered = pairCounts.filter(lambda a: a[1] > 20)
pairCountsFiltered.saveAsTextFile("output")
