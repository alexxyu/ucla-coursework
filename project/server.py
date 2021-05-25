import re
import sys
import time
import json
import aiohttp
import asyncio
import logging

API_KEY='AIzaSyDIDfU2F1b1vEwQdhv1KFwdgUKft2KqCM8'
SERVER_LINKS={
    'Riley': ['Jaquez', 'Juzang'],
    'Bernard': ['Jaquez', 'Juzang', 'Campbell'],
    'Juzang': ['Campbell', 'Riley'],
    'Jaquez': ['Riley', 'Bernard'],
    'Campbell': ['Bernard', 'Juzang']
}
PORT_MAPPING={
    'Riley': 12115,
    'Bernard': 12116,
    'Juzang': 12117,
    'Jaquez': 12118,
    'Campbell': 12119
}

RADIUS_LIMIT = 50
RESULT_LIMIT = 20

class Server:
    def __init__(self, name, host='127.0.0.1', port=1234):
        self.name = name
        self.host = host
        self.port = port

        self.client_last_msg_time = dict()
        self.client_locations = dict()
        self.client_at_log = dict()
        self.connection_is_down = {s:False for s in SERVER_LINKS[name]}
        logging.info(f'Initialized server named {name}')

    def get_formatted_time_diff(self, start, end):
        time_diff = end - start
        time_diff_sign = '+' if time_diff > 0 else ''
        time_diff = '{:.9f}'.format(round(time_diff, 9))
        return f"{time_diff_sign}{time_diff}"

    async def write_error(self, writer, command, error):
        logging.error(f'{error}: {command}')
        writer.write(f'? {command}'.encode())
        await writer.drain()
        writer.write_eof()
        writer.close()

    async def handle_IAMAT(self, writer, command, client, coords, msg_time):
        # Validate command's parameters
        curr_time = time.time_ns() / 1e9
        try:
            msg_time = float(msg_time)
        except ValueError:
            await self.write_error(writer, command, 'Invalid timestamp')
            return
        time_diff_str = self.get_formatted_time_diff(msg_time, curr_time)

        split_idx = max(coords.rfind('+'), coords.rfind('-'))
        if split_idx == -1 or (coords[0] != '-' and coords[0] != '+'):
            await self.write_error(writer, command, 'Malformed coordinates')
            return

        # Send response back and update server's information about client
        msg = f"AT {self.name} {time_diff_str} {client} {coords} {msg_time}"
        writer.write(msg.encode())
        await writer.drain()
        logging.info(f'Sent IAMAT response to client {client}')

        self.client_locations[client] = (coords[:split_idx], coords[split_idx:])
        self.client_at_log[client] = msg
        self.client_last_msg_time[client] = msg_time

        await self.propogate_message(msg)

    async def handle_WHATSAT(self, writer, command, client, rad, limit):
        if client not in self.client_locations.keys() or rad > RADIUS_LIMIT or limit > RESULT_LIMIT:
            await self.write_error(writer, command, 'Invalid WHATSAT parameters')
        else:
            response = await self.nearby_search_request(client, rad, limit)

            writer.write(f"{self.client_at_log[client]}\n".encode())
            await writer.drain()

            response_data = re.sub(r'\n+', '\n', json.dumps(response, indent=2).rstrip())
            writer.write(response_data.encode())
            await writer.drain()
            logging.info(f'Sent WHATSAT response to client {client}')

    async def handle_connection(self, reader, writer):
        data = await reader.read()
        data = data.decode().strip()

        fields = data.split()
        
        if len(fields) == 0:
            await self.write_error(writer, data, 'No fields in command found')
            return
        elif fields[0] == 'IAMAT' and len(fields) == 4:
            # Handle IAMAT command from client
            client, coords, msg_time = fields[1:]
            logging.info(f'Received IAMAT command from client {client}: {data}')
            await self.handle_IAMAT(writer, data, client, coords, msg_time)
        elif fields[0] == "WHATSAT" and len(fields) == 4:
            # Handle WHATSAT command from client
            client, rad, limit = fields[1:]
            rad, limit = int(rad), int(limit)
            logging.info(f'Received WHATSAT command from client {client}: {data}')
            await self.handle_WHATSAT(writer, data, client, rad, limit)
        elif fields[0] == "AT" and len(fields) == 6:
            # Handle propogated messages from other servers
            server, time_diff_str, client, coords, msg_time = fields[1:]
            msg_time = float(msg_time)
            if client not in self.client_last_msg_time.keys() or msg_time > self.client_last_msg_time[client]:
                logging.info(f'Received new propogated message about client {client}')
                msg = data
                self.client_at_log[client] = msg
                self.client_last_msg_time[client] = msg_time
                split_idx = max(coords.rfind('+'), coords.rfind('-'))
                self.client_locations[client] = (coords[:split_idx], coords[split_idx:])

                await self.propogate_message(msg)
            else:
                logging.info(f'Received old propogated message about client {client}')
        else:
            await self.write_error(writer, data, 'Invalid command')
            return

        writer.write_eof()
        writer.close()

    async def nearby_search_request(self, client, rad, limit):
        rad *= 1000
        latitude, longitude = self.client_locations[client]
        location = f'{latitude},{longitude}'

        logging.info('Making Nearby Places request to Google Places')
        url_req = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={API_KEY}&location={location}&radius={rad}'
        async with aiohttp.ClientSession(
            connector=aiohttp.TCPConnector(
                ssl=False,
            ),
        ) as session:
            async with session.get(url_req) as resp:
                response = await resp.json()
                response["results"] = response["results"][:limit]
                return response

    async def propogate_message(self, msg):
        for other_server in SERVER_LINKS[self.name]:
            logging.info(f'Propogating message to server {other_server}')
            try:
                _, writer = await asyncio.open_connection(self.host, PORT_MAPPING[other_server])
                writer.write(msg.encode())
                await writer.drain()
                writer.write_eof()
                writer.close()

                if self.connection_is_down[other_server]:
                    logging.warning(f"Reopened connection to server {other_server}")
                    self.connection_is_down[other_server] = False
            except ConnectionRefusedError:
                logging.warning(f"Cannot connect to server {other_server}")
                self.connection_is_down[other_server] = True

    async def run(self):
        server = await asyncio.start_server(self.handle_connection, host=self.host, port=self.port)
        await server.serve_forever()

def main():
    args = sys.argv
    if len(args) != 2:
        print(f"Usage: python3 {args[0]} server_name", file=sys.stderr)
        exit(1)

    server_name = args[1]
    if server_name not in PORT_MAPPING.keys():
        print(f"Invalid server name provided", file=sys.stderr)
        exit(1)

    logging.basicConfig(filename=f'server_{server_name}.log', format='%(levelname)s:%(message)s', filemode='w+', level=logging.INFO)
    server = Server(server_name, port=PORT_MAPPING[server_name])

    try:
        asyncio.run(server.run())
    except KeyboardInterrupt:
        logging.info(f'Shutting down server {server_name}')

if __name__ == '__main__':
    main()