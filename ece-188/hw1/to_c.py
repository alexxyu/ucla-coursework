import sys
with open(sys.argv[1], 'rb') as fh:
    data = fh.read()
    i = 1
    sys.stdout.write('unsigned char data[] = {\n');
    for c in data:
        sys.stdout.write('0x{:02x}'.format(c))
        if i % 15 == 0:
            sys.stdout.write(',\n')
        else:
            sys.stdout.write(', ')
        i = i + 1
    sys.stdout.write('\n};\n')
