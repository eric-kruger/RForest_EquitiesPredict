# RForest_EquitiesPredict
Random Forest Classification Equities Prediction Algorithm
------
This a random forest classification prediction algorithm which is designed to predict a stocks future increase in price (based on % of price increase in the future). This model approaches the future market as a classification problem. Rather than try to predict an exact stock price it predicts a category of percent change. The default is to make this prediction 20 days out.

 The demonstration predicts VTI ([Vanguard Total Stock Market Index Fund](https://finance.google.com/finance?q=VTI&ei=NlrpWeD3IYb3jAGCi5ewCg)) from technical indicators taken from OHLCV data from VTI, BND ([Vanguard Total US Bond Index Fund](https://finance.google.com/finance?q=BND&ei=c1rpWaH9NIvNjAGE2YjQBw)), SH ([ProShares Short S&P500](https://finance.google.com/finance?q=SH&ei=eFrpWenXD4PG2AbAqYfACw)) and GLD ([SPDR Gold Trust](https://finance.google.com/finance?q=GLD&ei=o1rpWZiGJYOVjAGS3pegCQ)). 
 + _Note:_ I am predicting VTI based on VTIs own technical indicators and those of other contra-indicators for the general stock market (i.e. price of gold, US bond market, an inverse traded fund)

 In the example VTI prediction (within the VTI folder), stocks are predicted to be in one of 9 categories based on percent change in 20 (financial) days time:
 + -99% to -0.09%
 + -0.09% to -0.06%
 + -0.06% to -0.03%
 + -0.03% to -0.01% 
 + -0.01% to  0.01%
 +  0.01% to  0.03% 
 +  0.03% to  0.06%
 +  0.06% to  0.09%
 +  0.09% to  99%

 In the VTI model, categorical probabilities are summed if they are below -0.01%, and this corresponds to the probability of SELL action. The probability of being between -0.01% and 0.01% corresponds to the STAY action. The summed categorical probability of being above 0.01% corresponds to the buy action.

 _Note:_ I have also added a google example prediction (See /GOOGLE folder)

There are a number of functions that are highly customizable in the /LIB folder. The main functions are 
+ stocks.pre.process()
	+ This does all the pre.processing of creating technical indicators based on OHLCV data and loading data into a data frame for analysis.
	+ This function is embedded within stock.RF.mod() and stock.RF.pred()
	+ Technical indicators used are: 
		+ exponential moving average (defaults to smoothing 0.14, N = 10 days), this is applied to all raw OHLCV data
		+ interday difference (the difference between Open and Close between days)
		+ intraday difference (the difference between Open and Close within day)
		+ Schaff Trading Cycle (EMA short = 9, EMA long = 30, N = 10)
		+ Aroon
		+ RSI
		+ MACD
		+ Stochastic Oscillator (fastK, slowK, slowD)
		+ Rate of Change
		+ OBV
		+ CCI
	+ (Click here for an overview of technical indicators)[https://www.linnsoft.com/indicators]

+ stock.RF.mod() 
	+ This trains a model based on preset parameters
	+ You can specify the model prediction stock by changing 'stock2model' argument.
	+ You can add or remove predictor assets.
+ stock.RF.pred()
	+ takes the output of stock.RF.mod() and predicts up to a specified date (today's date is the default)
+ stock.RF.performance()
	+ takes the predicted object and performs all the back-testing and reports a variety of performance (error) indicators
	+ This function takes various arguments that set thresholds of probabilities for buy, sell and stay actions
+ policy.test()
	+ takes the performance object and executes back-testing of trading policy. The goal of this trading policy is to move into cash when the market is predicted to go down, and to move into equities when it is predicted to go up (i.e. sell all stocks at the first sell ACTION, BUY all as much as possible at first buy ACTION, otherwise stay in the position current position until either of the first two options occur).
	+ You can specify various starting positions (money in bank, number of shares to start with, trading costs)

Rmarkdown reports are generated in markdown in the /VTI/Reports folder. 

A batch file 'AnalyzeMarket.bat' is used from calling the process from windows, generating a report and sending that report to your email.  This is set up for my personal computer.

Back-testing and optimization (via simulation) are done in the 'Model_Optimization&Backtessting.R' script. A new model based on new parameters is set in the 'Model_Design&Train.R' script.

This project was based on the work by: [Khaidem, L., Saha, S., & Dey, S. R. (2016). Predicting the direction of stock market prices using random forest. arXiv preprint arXiv:1605.00003.](https://arxiv.org/abs/1605.00003)

------
### _Note:_ Place all files in a folder labeled R in your documents directory. On a windows machine this would be '/Documents/R/RForest_EquitiesPredict'. on MACs I am not sure exactly but wherever the functional equivalent is that is where you place these files. That will allow you to use the files without having to change the directory paths in each file.


