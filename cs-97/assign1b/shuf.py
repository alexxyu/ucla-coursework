#!/usr/bin/python

import string
import random, sys
import argparse

class shuf:
    def __init__(self, input):
        self.lines = input

    def shuffle(self, repeat, num):
        temp = self.lines.copy()
        if repeat:
            if num is None:
                while True:
                    print(random.choice(temp))
            else:	
                num = num[0]
                for i in range(int(num)):
                    print(random.choice(temp))
        else:
            random.shuffle(temp)
            [print(line) for line in temp]

def print_help():
    print("Try \'python " + sys.argv[0] + "  -- help\' for more information")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-n", "--head-count", action="store", nargs=1,
                        metavar="COUNT", help="output at most COUNT lines")
    parser.add_argument("-r", "--repeat", action="store_true", 
                        help="output lines can be repeated")
    parser.add_argument("file", nargs='?', metavar="FILE")    
    parser.add_argument("-e", "--echo", nargs='*', 
                        metavar="ARG", help="treat each ARG as an input line")

    args, extra = parser.parse_known_args()

    # Check if valid head count
    if args.head_count is not None and not args.head_count[0].isdigit():
        print(sys.argv[0]+": invalid line count: \'" + args.head_count[0] + "\'")
        exit(1)

    if args.file == '-' or (args.file is None and args.echo is None):
        # Read from standard input
        input = []
        for line in sys.stdin:
            input.append(line.rstrip())
    elif args.echo is not None:
        # Read each ARG as input line
        input = args.echo
        input.extend(extra)
        if args.file is not None:
            input.append(args.file)
        if len(input) == 0 and args.repeat:
            print(sys.argv[0]+": no lines to repeat")
            exit(1)
    elif args.file is not None:
        # Read from file
        if len(extra) > 0:
            print(sys.argv[0]+": extra operand \'" + extra[0] + "\'")
            exit(1)
        filename = args.file
        try:
            with open(filename, 'r') as file:
                input = file.read().splitlines()
        except FileNotFoundError:
            print(sys.argv[0]+": " + filename + ": No such file or directory")
            exit(1)

    shuffler = shuf(input)
    shuffler.shuffle(args.repeat, args.head_count)

if __name__ == "__main__":
    main()
