import sys
import time
import asyncio

async def main():
    if len(sys.argv) != 2:
        print("Need to provide port", file=sys.stderr)
        exit(1)
    port = int(sys.argv[1])

    # Test IAMAT command
    reader, writer = await asyncio.open_connection('127.0.0.1', port)
    t = time.time_ns()
    writer.write(f'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 {time.time_ns()/1e9}\n'.encode())
    data = await reader.readline()
    print(f'Received: {data.decode()}')
    writer.close()

    # Test WHATSAT command
    reader, writer = await asyncio.open_connection('127.0.0.1', port)
    writer.write(f'WHATSAT kiwi.cs.ucla.edu 10 1\n'.encode())
    data = b''
    while not reader.at_eof():
        data += await reader.readline()
    print(f'Received: {data.decode()}')

    writer.close()
    await writer.wait_closed()

if __name__ == '__main__':
    asyncio.run(main())