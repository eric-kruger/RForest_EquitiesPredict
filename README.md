# RForest_EquitiesPredict
Random Forest Classification Equities Prediction Algorithm
------
This a random forest classification prediction algorithm which is designed to predict a stocks future increase in price (based on % of price increase in the future). This model approaches the future market as a classification problem. Rather than try to predict an exact stock price it predicts a category of percent change. The default is to make this prediction 20 days out.

There are several examples of implementation in the folder. Each prediction algroithim gets its own folder. There is a template folder for making new files. The core functionality is in the 'Model_Design&Train.R' file and the '/Reports/MarketAnalysis.Rmd' Here is an overview of what each fild does in the folder.  

The example discussed below predicts VTI ([Vanguard Total Stock Market Index Fund](https://finance.google.com/finance?q=VTI&ei=NlrpWeD3IYb3jAGCi5ewCg)) from technical indicators taken from OHLCV data from VTI, BND ([Vanguard Total US Bond Index Fund](https://finance.google.com/finance?q=BND&ei=c1rpWaH9NIvNjAGE2YjQBw)), SH ([ProShares Short S&P500](https://finance.google.com/finance?q=SH&ei=eFrpWenXD4PG2AbAqYfACw)) and GLD ([SPDR Gold Trust](https://finance.google.com/finance?q=GLD&ei=o1rpWZiGJYOVjAGS3pegCQ)). 
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

## Overview of Files
+ Main Folder Files
	+ 'AnalyzeMarket.bat': a file used for executiing batch scripts in order to autogenerate reports. Must place your 'Rscript.exe' in  your systems PATH to work. 
	+ 'ReturnsCalculator.xlsx': a simple excel spreadhseet used for calculating compounding interest at various intervals. Used for setting up breaks categegories for model.
	+ 'MAILR.personal': you must create this file yourself based on your own gmail credentials. The file consists of your the following each on sepearate lines without any labels.
		+ Your gmail email, e.g. spongebob@gmail.com
		+ Your gmail username, e.g. spongebob
		+ Your gamil specific app password, e.g. slksjfoiwru39kf

+ Files in Model subdirectory e.g. 'VTI'
	+ 'AnalyzeMarket.R': This is responsible for generating emails and BUY and SELL notifications via email. It does so by using the 'mailR' package (_Note:_ you must have JAVA installed to use). Also you must install pandoc into your R environment in order to do this. This file is accessed by the batch files. Also to use 'mailR' with gmail you must set up an app specific password. I am obviously not showing you this and this is information is stored in a file called MAILR.personal within the main folder.
	+ 'Model_Design&Train.R': setup model, train model, also sets up key configurations that are carried forward to other files in the directory.
	+ 'Model_Optimization&Backtesting.R': this file is used for testing, optimizing and backtesting a model with various configurations of decision rules for implementing a policy.
	+ 'MODEL.Rdata': Where the model is stored
	+ 'DAT.Rdata': Where some of the variables set up in 'Model_Design&Train.R' are saved and accessed from other scripts.
	+  SETUP.R: When this script is run it defines various paths and other variables necessary for the model to be implemented across a variety of scripts.
	+ '/Reports/MarketAnalysis.Rmd': Builds the report based on the days prediction from the model. The report contains various information including prediction, historical predictions, plots of position (based on trading policy), intraday trade plots, cycle metrics (which is the position and the swing that occured while in that position, the goal is to be in stocks and change = + and be in cash when change = -)
	+ 'VTI_pred_history.csv': A .csv with output of all the data used in the predicition including RAW OHLCV data

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

This project was based on the work by: [Khaidem, L., Saha, S., & Dey, S. R. (2016). Predicting the direction of stock market prices using random forest. arXiv preprint arXiv:1605.00003.](https://arxiv.org/abs/1605.00003)

------
### _Note:_ Place all files in a folder labeled R in your documents directory. On a windows machine this would be '/Documents/R/RForest_EquitiesPredict'. on MACs I am not sure exactly but wherever the functional equivalent is that is where you place these files. That will allow you to use the files without having to change the directory paths in each file.


