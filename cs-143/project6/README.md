# Useful Unix Commands for Data Processing

Note: By default, all of the commands below that accept a flag specifying the field delimiter assume that the delimiter is the tab character.

## Looking at a file: `cat` and `less`
`cat` prints out the contents of a text file.

`less` allows you to view the content of a text file one page at a time, which is useful when the file is large.

## Subset a file: `head`, `tail`, and `shuf`
`head -n <line_count>` prints out the first `line_count` lines in a file. `tail` is similar in behavior, except that it prints out the last `line_count` lines in a file. Both commands also allow you to use `-<line_count>` as shorthand for `-n <line_count>`. 

`shuf` outputs a random permutation of the lines in a text file.

## Counting with `wc`
`wc` prints out the number of lines, words, and characters in a text file.

## Projection: `cut`
`cut` is equivalent to the project operator in relational algebra; it prints out the specified columns in a text file. The `-d` flag is used to specify the column delimiter, and the `-f` flag is used to specify the subset of (comma-separated) columns that you want to output.

For example, `cut -d , -f 1,3 adult.data` outputs the 1st and 3rd columns of a comma-separated file.

## Selection: `grep` and `awk`
`grep` outputs only lines that match a given regular expression. The `-i` flag performs case-insensitive matches, the `-c` flag prints the count of matching lines, and the `-v` flag inverts the output and prints only non-matching lines. 

`awk` allows you to perform column-based conditionals, similar to the select operator in relational algebra. The `-F` flag is used to sepcify the column delimiter. 

For example, `awk -F , '($3 > 10) && ($7 = "Female")' adult.data` will prints rows where the 3rd column value is greater than 10, and the 7th column value is Female.

## Sorting data using `sort`
As the name suggests, `sort` sorts the rows of a file and allows you to sort on one or more fields. The `-t` flag is the field delimiter, and the `-k <sort_field_start>,<sort_field_end>` flag specifies the field(s) to sort on. 

By default, `sort` sorts lexicographically and in ascending order. To sort numerically, you can add `n` to the end of the `-k` flag, and to sort in descending order, you can add `r` to the end of the `-k` flag. You can also sort on multiple columns by adding more `-k` flags.

For example, `sort -t , -k1,1 -k3,3rn adult.data` first sorts on the 1st column (lexicographically and in ascending order) and then sorts on the 3rd column (numerically and in descending order).

## Finding/removing duplicates with `uniq`
`uniq` collapses consecutive identical lines into a single copy. The `-c` flag prepends the collapsed count before each line. You can also use `-d` to output only lines that appear multiple times, or the `-u` flag to output only lines that appear once.

## Aggregation functions: `datamash`
The `datamash` commands allows you to apply aggregation functions on the data and takes the form `datamash [options] operation [field_num] [operation field_num ...]`. The `-t` flag specifies the field delimiter. `operation` refers to the aggregation function to be applied (e.g. `groupby`, `sum`, `count`).

Like `uniq`, if you want to group by some field(s), `datamash` assumes that the data is sorted by those fields already, but you can specify the `--sort` option, which will automatically sort on the specified groupby fields.

For example, `datamash --sort -t, groupby 3,2 count 1 max 1 mean 3 < adult.data` groups the data on the 3rd and 2nd fields. It prints the row count, max value of the 1st column, and mean of the 3rd column within each group.

By default, `datamash` prints only the `groupby` columns and the results from any other applied operations. To print all input columns, you can use the `--full` flag. You can also use the `cut` operator: for example, `cut 2` will add the 2nd column to the output.

## Updating a file with `sed`
`sed` is another command that allows you to perform text transformation. The general syntax is `sed 's/<string_to_replace>/<string_to_replace_it_with>/g' <source_file> > <target_file>.`

For example, `sed 's/?/NULL/g' adult.data > adult.null` replaces all instances of `NULL` to `?` in the input text file.
