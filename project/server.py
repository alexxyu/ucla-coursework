import sys
import time
import json
import aiohttp
import asyncio
import argparse

PORT=1234
API_KEY='<key_here>'
SERVER_LINKS={
    'Riley': ['Jaquez', 'Juzang'],
    'Bernard': ['Jaquez', 'Juzang', 'Campbell'],
    'Juzang': ['Campbell'],
    'Jaquez': ['Riley', 'Bernard'],
    'Campbell': ['Bernard', 'Juzang']
}

class Server:
    def __init__(self, name, host='127.0.0.1', port=1234):
        self.name = name
        self.host = host
        self.port = port

        self.msg_log = set()
        self.client_locations = dict()

    def get_formatted_time_diff(self, start, end):
        time_diff = end - start
        time_diff_sign = '+' if time_diff > 0 else ''
        time_diff = '{:.9f}'.format(round(time_diff, 9))
        return f"{time_diff_sign}{time_diff}"

    """
    Sample client message:
    IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503

    Sample server message:
    AT Riley +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1621464827.959498503
    """
    async def handle_connection(self, reader, writer):
        data = await reader.readline()
        data = data.decode()

        curr_time = time.time_ns() / 1e9
        fields = data.strip().split()
        
        if fields[0] == 'IAMAT' and len(fields) == 4:
            client, coords, msg_time = fields[1:]
            time_diff_str = self.get_formatted_time_diff(float(msg_time), curr_time)

            writer.write(f"AT {self.name} {time_diff_str} {client} {coords} {msg_time}".encode())

            if data not in self.msg_log:
                await self.propogate_message(data)

            self.msg_log.add(data)
            split_idx = max(coords.rfind('+'), coords.rfind('-'))
            self.client_locations[client] = (coords[:split_idx], coords[split_idx:])
        elif fields[0] == "WHATSAT" and len(fields) == 4:
            client, rad, limit = fields[1:]
            rad, limit = int(rad), int(limit)
            
            if client not in self.client_locations.keys() or rad > 50 or limit > 20:
                writer.write(f'? {data}'.encode())
            else:
                response = await self.nearby_search_request(client, rad, limit)
                writer.write(f"AT {self.name} [more info here]".encode())
                writer.write(f"{json.dumps(response)}".encode())
        elif fields[0] == "AT":
            pass
        else:
            writer.write(f'? {data}'.encode())

        writer.close()

    async def nearby_search_request(self, client, rad, limit):
        latitude, longitude = self.client_locations[client]
        location = f'{latitude},{longitude}'

        url_req = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={API_KEY}&location={location}&radius={rad}'
        async with aiohttp.ClientSession(
            connector=aiohttp.TCPConnector(
                ssl=False,
            ),
        ) as session:
            async with session.get(url) as resp:
                response = await resp.json()
                print(response)
                response = json.loads(response)
                response[results] = response[results][:limit]
                return response

    async def propogate_message(self, data):
        pass
        # _, writer = await asyncio.open_connection('127.0.0.1', port)
        # for s in SERVER_LINKS[self.name]:
        #     writer.write(data)
        # writer.close()

    async def run(self):
        server = await asyncio.start_server(self.handle_connection, host='127.0.0.1', port=1234)
        await server.serve_forever()

def main():
    args = sys.argv
    if len(args) != 2:
        print(f"Usage: python {args[0]} [SERVER-NAME]", file=sys.stderr)
        exit(1)

    server = Server(args[1], port=PORT)
    asyncio.run(server.run())

if __name__ == '__main__':
    main()