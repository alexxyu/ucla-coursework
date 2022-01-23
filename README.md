# CS118 Project 1

This is the repo for winter22 cs118 project 1.

## Makefile

This provides a couple make targets for things.
By default (all target), it makes the `server` executables.

It provides a `clean` target, and `tarball` target to create the submission file as well.

You will need to modify the `Makefile` to add your userid for the `.tar.gz` turn-in at the top of the file.

## Academic Integrity Note

You are encouraged to host your code in private repositories on [GitHub](https://github.com/), [GitLab](https://gitlab.com), or other places.  At the same time, you are PROHIBITED to make your code for the class project public during the class or any time after the class.  If you do so, you will be violating academic honestly policy that you have signed, as well as the student code of conduct and be subject to serious sanctions.

## Provided Files

`server.c` is the entry points for the server part of the project.

## Testing

You can test your HTTP server directly using a browser, and/or a utility like `telnet` or `nc`. Your code will be graded using a script similar to the provided `test.sh`, and you should ensure that your code passes the provided tests. The final grading will use additional hidden tests to make sure your code follows the specification.

The output of `test.sh` would indicate which tests passed and failed along with the total number of passing tests. You can use this information to debug your code and make sure all the tests pass.

```
Checking HTTP status code ... pass
Checking content length ... pass
Checking if content is correct ... pass
Checking case insensitivity
Checking HTTP status code ... pass
Checking if content is correct ... pass
Checking GET without extension
Checking HTTP status code ... pass

Passed 6 tests, 0 failed
```

## Project Report

**Name: Alex Yu**

**UID: 105295708**

### High Level Design

My server follows a typical program using BSD sockets. It creates a socket to listen for new
connections, and every time it accepts a new connection, it forks into two processes. The parent 
process continues to listen for other connections while the child processes the incoming HTTP 
request.

The child handles the request by first parsing the filename from the request message and replacing
any instances of "%20" with spaces. Then, it looks at each file in the same current working
directory and compares its name with the requested filename. This comparison is case-insensitive
and also takes into account the possibility that the request file has no extension.

If the requested file is found, it figures out the content type by looking at its extension. Then,
it calculates the file size, and sends the header of the response message with `Content-Type` and 
`Content-Length` fields. Then, it reads and sends to the client some number of bytes from the file 
at a time until the entire file is read. Finally, the connection to the client is closed.

### Problems Encountered

One tricky problem that I encountered was parsing URL encoded spaces "%20" back into normal spaces.
I had trouble implementing this behavior because there could be multiple spaces in the filename, 
meaning tokenizing the string by space via `strtok` would not suffice since it ignores consecutive
spaces. Instead, I chose to compare every current substring with "%20". If it matched "%20", then I
used `strcat` to add a space to the new filename string; otherwise, I used `strcat` to add the 
current character.

One other problem that I had was handling cases where the requested file was without an extension.
One issue that I ran into was that upon sending the file's contents over to the client, my browser
would download the file instead of displaying it. I remedied this problem by parsing the extension
from the file matching the request, and setting the Content-Type field in the response header.

### Additional Libraries Used

* `dirent.h` was used to look through directory entries for files matching the request
* `errno.h` was used to retrieve error conditions
* `string.h` was used for various tasks like pre-processing filename strings and converting `errno`
to a human-readable string
* `ctype.h` was used to convert characters to lowercase (useful for case-insenstive comparison)
* `unistd.h` was used for various system call wrapper functions, like `fork` and `close`
* `signal.h` was used to catch `SIGINT` and handle graceful shutdown of the server
* `sys/socket.h` and `netinet/in.h`was used for socket programming

### Other Resources Used
* https://man7.org/linux/man-pages/index.html
* https://www.cplusplus.com/reference/cstring/
* https://www.geeksforgeeks.org/http-headers-content-type/
* https://www.tutorialspoint.com/cprogramming/c_file_io.htm
* https://stackoverflow.com/questions/238603/how-can-i-get-a-files-size-in-c
* https://stackoverflow.com/questions/4204666/how-to-list-files-in-a-directory-in-a-c-program
