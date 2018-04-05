# This works 3.28.18

import numpy as np
from tiingo import TiingoClient
config = {}
config['session'] = True
config['api_key'] = '160f7f115de5c12affc723dfc8a8c1a4402e9a25'
client = TiingoClient(config)


def sp(stock,SD,ED,FREQ):
	hp = client.get_ticker_price(stock,fmt='csv', startDate=SD, endDate=ED,frequency=FREQ)
	return hp;
