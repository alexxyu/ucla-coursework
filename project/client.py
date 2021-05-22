import sys
import time
import asyncio

PORT_MAPPING={
    'Riley': 12115,
    'Bernard': 12116,
    'Juzang': 12117,
    'Jaquez': 12118,
    'Campbell': 12119
}

async def main():
    if len(sys.argv) != 3:
        print(f"Usage: python3 {sys.argv[1]} client_name server_name", file=sys.stderr)
        exit(1)

    client, server = sys.argv[1:]
    if server not in PORT_MAPPING.keys():
        print("Invalid server name", file=sys.stderr)
        exit(1)
    port = PORT_MAPPING[server]

    # Test IAMAT command
    reader, writer = await asyncio.open_connection('127.0.0.1', port)
    writer.write(f'IAMAT {client} +34.068930-118.445127 {time.time_ns()/1e9}\n'.encode())
    await writer.drain()
    writer.write_eof()
    
    data = await reader.readline()
    print(f'Received: {data.decode()}')
    writer.close()

    # Test WHATSAT command
    reader, writer = await asyncio.open_connection('127.0.0.1', port)
    writer.write(f'WHATSAT {client} 10 1\n'.encode())
    await writer.drain()
    writer.write_eof()

    data = await reader.read()
    print(f'Received: {data.decode()}')
    writer.close()

if __name__ == '__main__':
    asyncio.run(main())