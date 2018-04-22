import numpy as np
from tiingo import TiingoClient
config = {}
config['session'] = True
config['api_key'] = 'ENTER API KEY HERE'
client = TiingoClient(config)


def sp(stock,SD,ED,FREQ):
	hp = client.get_ticker_price(stock,fmt='csv', startDate=SD, endDate=ED,frequency=FREQ)
	return hp;
