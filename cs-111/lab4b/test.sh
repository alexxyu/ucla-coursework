#!/bin/bash

echo "Testing program..."

let errors=0
(echo "STOP"; sleep 1; echo "START"; sleep 3; echo "LOG Hello, World!"; echo "OFF") | ./lab4b --log=LOGFILE --scale=C >> /dev/null
if [ $? -ne 0 ]; then
    echo "Test unsucessful: program did not return with exit code 0"
    let errors+=1
    rm -rf LOGFILE
    exit 1
fi

if [ `grep -c "^LOG Hello, World\!$" LOGFILE` -ne 1 ]; then
    echo "Test unsuccessful: program did not LOG message"
    let errors+=1
fi

if [ `cat LOGFILE | grep -c "^"` -lt 6 ]; then
    echo "Test unsuccessful: program did not appear to log all expected messages"
    echo "Output of program is below:"
    cat LOGFILE
    let errors+=1
fi

if [ `grep -c "SHUTDOWN" LOGFILE` -ne 1 ]; then
    echo "Test unsuccessful: program did not log shutdown message"
    let errors+=1
fi

if [ $errors -eq 0 ]; then
    echo "Program ran successfully with expected behavior"
fi

rm -rf LOGFILE
exit 0
