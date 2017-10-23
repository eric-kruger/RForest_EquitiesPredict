#---------------------------------------------------------------------------------------------------------------------#
# load libraries
library(scales)
library(quantmod)
library(randomForest)
library(tidyverse)

#---------------------------------------------------------------------------------------------------------------------#
# Error Prediction Functions

# Predict overall error
  pred.error <- function(mat) {
    error <- 1-sum(diag(mat))/sum(mat)
    return(error)
  }

# Predict inidividual class error
  pred.class.error <-  function(mat) {
    error <- 1-(diag(mat)/apply(mat,1,sum))
    return(error)
  }
#---------------------------------------------------------------------------------------------------------------------#
# Combine Data Frames in a list into a wide data frame

Combine.ls.by.date.2.df <- function(ls,BY="Date") {
  # Script Debugging
    # ls <- sd
    # BY <- "Date"
  out <- ls[[1]]
  if(length(ls)>1) {
  i <- 2
  for (i in 2:length(ls)) {
   out <- merge(out,ls[[i]],by=BY)
  }
  }
  return(out)
}
  
  
#---------------------------------------------------------------------------------------------------------------------#
# This calcualtes the schaff trading indicator
  
STC <- function(x,EMA.long=50,EMA.short=23,n.stoch=10) {
  #Script Debugging
    x <- as.numeric(GE$GE.Close)
    EMA.long=50
    EMA.short=23
    n.stoch=10
  EMA.s                <- EMA(x,n=EMA.short)
  EMA.l                <- EMA(x,n=EMA.long)
  MACD                 <- EMA.s-EMA.l
  S1                   <- as.data.frame(stoch(MACD,nFastK=n.stoch,nFastD=n.stoch))
  S1$fastD[S1$fastD<0] <- 0
  S1$fastD[S1$fastD>1] <- 1
  L                    <- runMin(S1$fastD,n=10)
  H                    <- runMax(S1$fastD,n=10)
  n                    <- H-L
  fastK                <- ifelse(n>0,(S1$fastD-L/H)*100,0)                 # This indicator is buggy here and needs more debugging
  S2                   <- EMA(fastK,n=10)
  
  
  
  S2                   <- rep(NA,nrow(S1))
  s.data               <- max(which(is.na(S1$fastD)==TRUE))+1
  e.data               <- nrow(S1)
  S1fastd              <- S1$fastD[s.data:e.data]*100
  S.temp               <- as.data.frame(stoch(S1fastd,nFastK=15,nFastD=10))  # This line tends to produce errors
  S2[s.data:e.data]    <- S.temp$fastD
  S2[S2<0]             <- 0
  S2[S2>1]             <- 1
  STC                  <- S2*100
  return(STC)
}
  
  
  

#---------------------------------------------------------------------------------------------------------------------#
# This function obtains data and adds all indicicators and ouputs all stocks to a list object

stocks.pre.process <- function(symbols,        # Stocks to download or a list of predownloaded stocks 
                               FROM,           # start date (will subset if data is larger)
                               TO,             # end date (will subset)
                               smooth.r) {     # smoothing ratio for expoential moving average
  # Function Debugging
    # symbols <- stock.data
    # TO <- "2017-08-29"
    # smooth.r <- 0.4
    # stock <- "GLD"
    # 
  if(is.list(symbols)==TRUE) {
    for (stock in names(symbols)) {
      symbols[[stock]] <- symbols[[stock]][which(index(symbols[[stock]])<=as.Date(TO)),]
      assign(stock,symbols[[stock]])
    }
    symbols <- names(symbols)
  } else {
    getSymbols(symbols,src="google",from = FROM,to = TO) # Don't use yahoo
    # Note for analysis predictions to be correct you must have start the prediction dataset must include all the data used
    # in the training dataset this is because all of the technical indicators are retrospective and thus you need to make
    # sure that they have the same starting point or their values will be too different from the fitted model and you will
    # get less accurate predictions
    # In this model prediciton begins at 8-1-17 because 20 days are removed from the forecast
  }
  stocks <- list()
  for (stock in symbols) {
    s.name <- stock
    
    # Rename Variables to remove specific stock names and create a stock specific dataframe
      df <- as.data.frame(eval(parse(text=stock)))
      df <- cbind.data.frame(Date=rownames(df),df)
      colnames(df) <- c("Date","Open.raw","High.raw","Low.raw","Close.raw","Volume.raw")
      df$Date <- as.Date(as.character(df$Date))
      df <- na.exclude(df) # some daily data has NAs so this must be removed
    
    # Smoothing
      df$Open.ema <- EMA(df$Open.raw,ratio=smooth.r)
      df$High.ema <- EMA(df$High.raw,ratio=smooth.r)
      df$Low.ema   <- EMA(df$Low.raw,ratio=smooth.r)
      df$Close.ema <- EMA(df$Close.raw,ratio=smooth.r)
      df$Volume.ema <- EMA(df$Volume.raw,ratio=smooth.r) 
    
     # Custom Indicators
        df$inter.day.dif <- NA
        df$intra.day.dif <- NA
        for(i in 2:nrow(df)) {
          if(is.na(df$Close.raw[i])==TRUE | is.na(df$Open.raw[i-1])==TRUE) {
            df$inter.day.dif[i] <-NA
          } else {
            df$inter.day.dif[i] <- diff(c(df$Open.raw[i],df$Close.raw[i-1]))
            df$intra.day.dif[i]    <- diff(c(df$Close.raw[i],df$Open.raw[i]))
          }
        }
        df.inter.day.dif <- EMA(df$inter.day.dif,ratio=smooth.r)
        df.intra.day.dif <- EMA(df$inter.day.dif,ratio=smooth.r)
        
        # Not currently using these indicators
        #########################################
        # Moving regression coefficient
        # df$r5 <- NA
        # df$r5e <- NA
        # df$r10 <- NA
        # df$r10e <- NA
        # df$r20 <- NA
        # df$r20e <- NA
        # 
        # for(i in 6:nrow(df)) {
        #   df.5 <- df[(i-5):i,"Close.raw"]
        #   day   <- 1:6
        #   r5.lm <- lm(df.5~day)
        #   df$r5[[i]] <- r5.lm$coefficients[2]
        #   df$r5e[[i]] <- sqrt(sum(r5.lm$residuals^2))
        # }
        # 
        # for(i in 11:nrow(df)) {
        #   df.10 <- df[(i-10):i,"Close.raw"]
        #   day   <- 1:11
        #   r10.lm <- lm(df.10~day)
        #   df$r10[[i]] <- r10.lm$coefficients[2]
        #   df$r10e[[i]] <- sqrt(sum(r10.lm$residuals^2))
        # }
        # 
        # for(i in 21:nrow(df)) {
        #   df.20 <- df[(i-20):i,"Close.raw"]
        #   day   <- 1:21
        #   r20.lm <- lm(df.20~day)
        #   df$r20[[i]] <- r20.lm$coefficients[2]
        #   df$r20e[[i]] <- sqrt(sum(r20.lm$residuals^2))
        # }
        ################################################
    
    # Remove Missing Data
      df <- na.exclude(df) #smoothing creates leading NAs so this must be removed
    
    # Add Technical Indicators
      # Schaff Trading Cycle
        HLC    <- c("High.ema","Low.ema","Close.ema")
        #df$STC <- STC(df$Close.raw,EMA.short = 9,EMA.long = 30)
      # Aroon 
        A.s    <- aroon(df$Close.ema,n=20)  # Short
        colnames(A.s) <- paste0(colnames(A.s),".s")
        df     <- cbind.data.frame(df,A.s[,1:2])
        A.l    <- aroon(df$Close.ema,n=50)  # Long
        colnames(A.l) <- paste0(colnames(A.l),".l")
        df     <- cbind.data.frame(df,A.l[,1:2])
      # RSI (Relative Strength Index)
        df$RSI <- RSI(df$Close.ema)
      # MACD
        df     <- cbind.data.frame(df,as.data.frame(MACD(df$Close.ema)))
      # WPR
        df$WPR <- as.vector(WPR(as.xts(df[,HLC])))             
      # stoch  (Stochastic Oscilator)
        df     <- cbind.data.frame(df,as.data.frame(stoch(as.xts(df[,HLC]))))
      # Rate of Change
        df$ROC <- ROC(df$Close.ema)
      # On Balance Volume
        df$OBV <- OBV(df[,"Close.ema"],df[,"Volume.ema"]) 
      # CCI
        df$CCI <- CCI(as.xts(df[,HLC]))
    
    colnames(df) <- c("Date",paste0(s.name,".",colnames(df)[-1])) # Rename Columns
    
    stocks[[s.name]] <- df # Output all stocks to a data.frame
  }
  return(stocks)
}

#---------------------------------------------------------------------------------------------------------------------#
# This function builds a random forest model based on stocks included

stock.RF.mod <- function(stock2model = "VTI",                                          # What stock are you predicting
                         symbols = c("VTI","GLD","SH","BND"),                          # Specify the stocks to use for prediction 
                         breaks=c(-99,-0.09,-0.06,-0.03,-0.01,0.01,0.03,0.06,0.09,99), # What are the categories you want to break
                         forecast.days = 20,                                           # How far out do you want to predict
                         smooth.r =  0.4,                                              # What smoothign ratio do you want to use on raw data
                         FROM = "2004-12-15",                                          # Start of training data
                         TO = "2017-08-29",                                             # End of training data
                         trees=1000,
                         ...) {
  # Function Debugging 
    # symbols <- stock.data
    # stock2model = "VTI"
    # symbols = c("VTI","GLD","SH","BND")
    # breaks=c(-99,-0.09,-0.06,-0.03,-0.01,0.01,0.03,0.06,0.09,99)
    # forecast.days = 20
    # smooth.r =  0.4
    # FROM = "2004-12-15"
    # TO = "2017-08-29"
  
  library(randomForest)
  library(quantmod)
  library(tidyverse)
  direction.categories <- breaks
  
  # Pre process stock data (see function above)
    stocks <- stocks.pre.process(symbols,FROM,TO,smooth.r)                                
    if(is.list(symbols)==TRUE) {symbols <- names(symbols)} # If you don't want to have the function download the data
                                                           # you must specify this variable
  
  # Combine list into wide data frame (needed structure for RF model)
    stocks.df  <- Combine.ls.by.date.2.df(stocks)
    #stocks.df <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),stocks) #Combine list into a single dataframe
  
  # Forecase percent increase 
    c.ema <- paste0(stock2model,".Close.ema")
    c.raw <- paste0(stock2model,".Close.raw")
    stocks.df <- na.exclude(stocks.df)
    stocks.df$forecast.ema <- NA
    stocks.df$forecast <- NA
    for(i in 1:(nrow(stocks.df)-forecast.days)) {
      stocks.df$forecast.ema[i] <- (as.numeric(stocks.df[i+forecast.days,c.ema])/as.numeric(stocks.df[i,c.ema]))-1
      stocks.df$forecast[i] <- (as.numeric(stocks.df[i+forecast.days,c.raw])/as.numeric(stocks.df[i,c.raw]))-1
    }
    stocks.df <- na.exclude(stocks.df)
  
  # Create categories for classification 
    # Based on smoothed data
    brks <- ordered(cut(stocks.df$forecast.ema,breaks=direction.categories))
    levels(brks) <- gsub(",","to",levels(brks),fixed=TRUE) 
    levels(brks) <- gsub("\\(|\\]","",levels(brks))
    stocks.df$Direction.ema <- brks
    xtabs(~stocks.df$Direction.ema)
  
    # Based on Raw data
    brks <- ordered(cut(stocks.df$forecast,breaks=direction.categories))
    levels(brks) <- gsub(",","to",levels(brks),fixed=TRUE) 
    levels(brks) <- gsub("\\(|\\]","",levels(brks))
    stocks.df$Direction <- brks
    xtabs(~stocks.df$Direction)
  
  # Add Julian day of year variable 
    stocks.df$yday <- lubridate::yday(stocks.df$Date)
  
  # Trim data to just predicted variables for model
    stocks.mod <- select(stocks.df,
                         -ends_with(".raw"),
                         -Date,
                         -starts_with("forecast"),
                         -Direction,
                         -contains("Open"),
                         -contains("Close"),
                         -contains("High"),
                         -contains("Low"))
  
  # Run Random Forest Model
    fit <- randomForest(Direction.ema ~ .,
                        data=stocks.mod, 
                        importance=TRUE, 
                        ntree=trees,...)
 # Create Output object (model)
  output <- list()
  output[["stock2model"]]   <- stock2model
  output[["symbols"]]       <- symbols
  output[["breaks"]]        <- breaks
  output[["forecast.days"]] <- forecast.days
  output[["smooth.r"]]      <- smooth.r
  output[["start.date.train"]]    <- FROM
  output[["end.date.train"]]      <- TO
  output[["model.data"]]          <- stocks.df
  output[["model"]]         <- fit
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------#
# This function uses new data to predict future categories of market direction

stock.RF.predict <- function(model.object,
                             newdata=NULL,
                             end.date=Sys.Date(),
                             ...) {     
  
  # Based on model object specified in the previous data 
    stock2predict = model.object[["stock2model"]]
    symbols = model.object[["symbols"]]
    breaks= model.object[["breaks"]]
    forecast.days = model.object[["forecast.days"]]
    smooth.r = model.object[["smooth.r"]]
    FROM = model.object[["start.date.train"]]
    TO = end.date                                 #<- model.object[["end.date"]]
    model <- model.object[["model"]]
  
  library(randomForest)
  library(quantmod)
  library(tidyverse)
  direction.categories <- breaks
  
  # Pre process stock data
    stocks <- stocks.pre.process(symbols,FROM,TO,smooth.r)
    if(is.list(symbols)==TRUE) {symbols <- names(symbols)}
  
  # Combine into a single data frame
    stocks.df  <- Combine.ls.by.date.2.df(stocks)
    #stocks.df <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),stocks) #Combine list into a single dataframe
  
  # Add Julian day of the year
    stocks.df$yday <- lubridate::yday(stocks.df$Date)
  
  stocks.df.new <- na.exclude(stocks.df)

  output <- model.object
  output[["end.date.predict"]]    <- TO
  output[["predict.data"]]  <- stocks.df.new
  output[["probs"]]         <- predict(model,newdata=stocks.df.new,type="prob")     # Predicted Probabilities
  output[["response"]]      <- predict(model,newdata=stocks.df.new,type="response") # Predicted Response CAtegory
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------#
# This function deterimnes model performance on trained data and predicted data

stock.RF.performance <- function(predict.obj,
                                 sell.max,
                                 stay.max,
                                 buy.max=99,
                                 sell.t=0.85,
                                 buy.t=0.85) {    
  
  # Function Debugging 
    # predict.obj   <- pred
    # s    <-  sell.max  <- -0.01  # Max Sell Value
    # st   <-  stay.max  <-  0.01  # Max Stay Value
    # b    <-  buy.max   <-  99    # Max Buy  Value
    # sell.t <- 0.6               # Threshold prob to sell
    # buy.t  <- 0.6               # Threshold prob to buy
  
  s    <-  sell.max  # Threshold max prob to sell
  st   <-  stay.max  # Threshold max prob to stay 
  b    <-  buy.max   # Threshold max prob to buy 
  stock2predict = predict.obj[["stock2model"]]
  breaks        = predict.obj[["breaks"]]
  forecast.days = predict.obj[["forecast.days"]]
  TO            = predict.obj[["end.date.predict"]]
  model         <- predict.obj[["model"]]
  p.data        <- predict.obj[["predict.data"]]
  probs         <- predict.obj[["probs"]]
  response      <- predict.obj[["response"]]
  direction.categories <- breaks
  library(randomForest)
  library(quantmod)
  library(tidyverse)
  pred.error <- function(mat) {
    error <- 1-sum(diag(mat))/sum(mat)
    return(error)
  }
  
  s.c.ema <- paste0(stock2predict,".Close.ema")
  s.c.raw <- paste0(stock2predict,".Close.raw")
  
 # Caclulate Actual Returns
  Actual.Return.ema <- NA
  Actual.Return <- NA
  i <- 1
  for(i in 1:(nrow(p.data)-forecast.days)) {
    Actual.Return.ema[i] <- (as.numeric(p.data[i+forecast.days, s.c.ema])/as.numeric(p.data[i, s.c.ema]))-1
    Actual.Return[i]     <- (as.numeric(p.data[i+forecast.days,s.c.raw]) /as.numeric(p.data[i,s.c.raw]))-1
  }
  Actual.Return.ema <- c(Actual.Return.ema,rep(NA,forecast.days))
  Actual.Return <- c(Actual.Return,rep(NA,forecast.days))
  
 # Calculate true Categories
  brks <- ordered(cut(Actual.Return.ema,breaks=direction.categories))
  levels(brks) <- gsub(",","to",levels(brks),fixed=TRUE) 
  levels(brks) <- gsub("\\(|\\]","",levels(brks))
  Actual.Direction.ema <- brks
  
  brks <- ordered(cut(Actual.Return,breaks=direction.categories))
  levels(brks) <- gsub(",","to",levels(brks),fixed=TRUE) 
  levels(brks) <- gsub("\\(|\\]","",levels(brks))
  Actual.Direction <- brks
  
 # Calculate Aggregated prob for Sell, Stay and Buy Decisions based on categorical membership
  SELL.c <- which(breaks<s)
  STAY.c <- which(breaks<st & breaks>=s)
  BUY.c  <- which(breaks<b  & breaks>=st)
  if(length(STAY.c)==1) {
    STAY=probs[,STAY.c]
  } else {
    STAY=apply(probs[,STAY.c],1,sum)
  }
  
  if(length(SELL.c)==1) {
    SELL=probs[,SELL.c]
  } else {
    SELL=apply(probs[,SELL.c],1,sum)
  }
  
  if(length(BUY.c)==1) {
    BUY=probs[,BUY.c]
  } else {
    BUY=apply(probs[,BUY.c],1,sum)
  }
  S.ST.B <- cbind.data.frame(SELL,STAY,BUY)
  S.ST.B$ACTION <- with(S.ST.B,ifelse(BUY>=buy.t,"BUY", ifelse(SELL>=sell.t,"SELL","STAY"))) # Determine action based on probabilies
  
  # Determine if True Direction smoothed matched predicted response
    MATCH <- ifelse(as.character(response)==as.character(Actual.Direction.ema),TRUE,FALSE)
  # Determine if Action (Sell, Stay, Buy) matched predicted action
    ACTUAL.ACTION <- ifelse(Actual.Return>=st,"BUY",ifelse(Actual.Return<=s,"SELL","STAY"))
    Action.df <- cbind.data.frame(ACTION=S.ST.B$ACTION,ACTUAL.ACTION)
  # Performance Data
    Performance <- cbind.data.frame(Actual.Return,Actual.Return.ema,Direction.predict=response,Actual.Direction.ema,Actual.Direction,MATCH,Pred.Action=S.ST.B$ACTION,ACTUAL.ACTION)
  
  # Only include predicted data (excluded predicted data)
    predict.days <- which(as.numeric(p.data$Date)>as.numeric(as.Date(predict.obj[["end.date.train"]])))
  
 # Error Matrices
    Prediction.Error.matrix             <- xtabs(~Direction.predict+Actual.Direction.ema,data=Performance[predict.days,])
    Actual.Direction.Error.Matrix       <- xtabs(~Actual.Direction+Actual.Direction.ema,data=Performance[predict.days,])
    Action.Error.Matrix                 <- xtabs(~ACTION+ACTUAL.ACTION,data=Action.df[predict.days,])
    TOTAL.Prediction.Error.matrix       <- xtabs(~Direction.predict+Actual.Direction.ema,data=Performance)
    TOTAL.Actual.Direction.Error.Matrix <- xtabs(~Actual.Direction+Actual.Direction.ema,data=Performance)
    TOTAL.Action.Error.Matrix           <- xtabs(~S.ST.B$ACTION+ACTUAL.ACTION)

  # Overal Error Values (Total includes trained and predicted data)
    Model.Fit.Error                     <-   pred.error(model$confusion[,-which(colnames(model$confusion)=="class.error")])
    Prediction.Error                    <-   pred.error(Prediction.Error.matrix)
    Actual.Direction.Error              <-   pred.error(Actual.Direction.Error.Matrix) 
    Action.Error                        <-   pred.error(Action.Error.Matrix)
    TOTAL.Prediction.Error              <-   pred.error(TOTAL.Prediction.Error.matrix)
    TOTAL.Actual.Direction.Error        <-   pred.error(TOTAL.Actual.Direction.Error.Matrix)
    TOTAL.Action.Error                  <-   pred.error(TOTAL.Action.Error.Matrix)
  
  # Overall error summary 
    Error <- cbind.data.frame(Model.Fit.Error,
                              Prediction.Error,
                              Actual.Direction.Error,
                              Action.Error,TOTAL.Prediction.Error,
                              TOTAL.Actual.Direction.Error,
                              TOTAL.Action.Error) 
  
  # Create output object
    output <- predict.obj
    output[["sell.max.threshold"]]              <- s
    output[["stay.max.threshold"]]              <- st
    output[["buy.max.threshold"]]               <- b
    output[["sell.prob.threshold"]]             <- sell.t
    output[["buy.prob.threshold"]]              <- buy.t
    output[["S.ST.B"]]                          <- S.ST.B
    output[["Performance"]]                     <- Performance 
    output[["Prediction.Error.matrix"]]         <- Prediction.Error.matrix  
    output[["Actual.Direction.Error.matrix"]]   <- Actual.Direction.Error.Matrix
    output[["Action.Error.matrix"]]             <- Action.Error.Matrix
    output[["TOTAL.Prediction.Error.matrix"]]          <- TOTAL.Prediction.Error.matrix
    output[["TOTAL.Actual.Direction.Error.Matrix"]]    <- TOTAL.Actual.Direction.Error.Matrix
    output[["TOTAL.Action.Error.Matrix"]]              <- TOTAL.Action.Error.Matrix
    output[["Error Rates"]]                     <- Error
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------#
# This function determines performance based on a specific investment policy end evaluates performance

policy.test <- function(performance.obj,        # Performance object specified above
                        Bank.start=10000,       # How much money start in the bank
                        Shares.start=0,         # How many shares to start with
                        trade.cost=6.95) {      # Trad cost
 # Function Debugging
   # performance.obj           <- perf
   # Bank   <- Bank.start      <- 10000
   # shares <- Shares.start    <- 0
  
  Bank   <- Bank.start      
  shares <- Shares.start   
  trade.cost <- 6.95
  
  
  
  # Policy Implemenation 
  o <- paste0(performance.obj$stock2model,".Open.raw")
  c <- paste0(performance.obj$stock2model,".Close.raw")
  df <- cbind.data.frame(performance.obj$predict.data,performance.obj$S.ST.B,
                         Mean.price=(performance.obj$predict.data[,c]+performance.obj$predict.data[,o])/2)
  shares.value <- shares*df$Mean.price[1]
  output           <- list()
  Bank.out         <- NULL
  Share.out        <- NULL
  Shares.value.out <- NULL
  Date             <- NULL
  Close.price      <- NULL
  Mean.price       <- NULL
  ACTION           <- NULL
  Can.buy.out      <- NULL
  t                <- NULL
  
  for (i in 1:nrow(df)) {
    can.buy <- as.integer((Bank-trade.cost)/df$Mean.price[i])
    if(can.buy<0) {can.buy <- 0}
    if(can.buy > 0 & shares > 0 ) {
      if(df$ACTION[i]=="BUY") {
        Bank <- Bank-can.buy*df$Mean.price[i]-trade.cost
        shares <- shares+can.buy
        shares.value <- shares*df[i,c]
        t[[i]] <- TRUE
      }
      if(df$ACTION[i]=="SELL") {
        Bank <- Bank+shares*df$Mean.price[i]-trade.cost
        shares <- 0
        shares.value <- 0 # shares*df[i,c]
        t[[i]] <- TRUE
      }
      if(df$ACTION[i]=="STAY") {
        shares.value <- shares*df[i,c]
        t[[i]] <- FALSE
      }
    }
    if(can.buy==0 & shares >0 ) {
      if(df$ACTION[i]=="BUY") {
        shares.value <- shares*df[i,c]
        t[[i]] <- FALSE
      }
      if(df$ACTION[i]=="SELL") {
        Bank <- Bank+shares*df$Mean.price[i]-trade.cost
        shares <- 0
        shares.value <- 0 #shares*df[i,c]
        t[[i]] <- TRUE
      }
      if(df$ACTION[i]=="STAY") {
        shares.value <- shares*df[i,c]
        t[[i]] <- FALSE
      }
    }
    if(can.buy > 0 & shares<=0) {
      if(df$ACTION[i]=="BUY") {
        Bank <- Bank-can.buy*df$Mean.price[i]-trade.cost
        shares <- shares+can.buy
        shares.value <- shares*df[i,c]
        t[[i]] <- TRUE
      }
      if(df$ACTION[i]=="SELL") {
        shares.value <- shares*df[i,c]
        t[[i]] <- FALSE
      }
      if(df$ACTION[i]=="STAY") {
        shares.value <- shares*df[i,c]
        t[[i]] <- FALSE
      }
    }
    Date[[i]]             <- as.character(df$Date[i])
    Close.price[[i]]      <- df[i,c]
    Mean.price[[i]]       <- df[i,"Mean.price"]
    ACTION[[i]]           <- df$ACTION[i]
    Bank.out[[i]]         <- Bank
    Share.out[[i]]        <- shares
    Shares.value.out[[i]] <- shares.value
    Can.buy.out[[i]]      <- can.buy
  }
  
  # Create an Output Object
    output[["Bank.start"]]  <- Bank.start
    output[["Shares.start"]]<- Shares.start
    output[["Date.start"]]  <- as.character(df$Date[1])
    output[["Date.end"]]    <- as.character(df$Date[nrow(df)])
    output[["End.Value"]]   <- sum(Bank,shares*df[i,c])
    output[["Total.Trades"]]<- sum(t)
    output[["Percent.Inc"]] <- output[["End.Value"]]/sum(Bank.start,Shares.start*Mean.price[1])*100
    output[["policy.predict"]] <- cbind.data.frame(Date,Close.price,
                                                   Mean.price,ACTION,
                                                   Trade=t,
                                                   Can.buy=Can.buy.out,
                                                   Bank=Bank.out,
                                                   Shares=Share.out,
                                                   Shares.value=Shares.value.out)
  # Append Policy Output to evaluate Policy specific error rates 
    p.df <- output[["policy.predict"]]
    p.df$Position <- ifelse(p.df$Shares==0,"CASH","STOCKS")
    trades <- which(p.df$Trade==TRUE)
    trades <- c(1,trades)
    trades <- c(trades,nrow(p.df))
    
    type        <- NULL
    Price.start <- NULL
    Price.end   <- NULL
    change      <- NULL
    start.date  <- NULL
    end.date    <- NULL
    w.start     <- NULL
    w.end       <- NULL
    run.length  <- NULL
    
    for(i in 1:(length(trades)-1)) {
      type[i]        <- p.df$Position[trades[i]]
      Price.start[i] <- p.df$Close.price[trades[i]]
      Price.end[i]   <- p.df$Close.price[trades[i+1]-1]
      change[i]      <- Price.end[i]-Price.start[i]
      start.date[i]  <- as.character(p.df$Date[trades[i]])
      end.date[i]    <- as.character(p.df$Date[trades[i+1]-1])
      w.start[i]     <- trades[i]
      w.end[i]       <- trades[i+1]-1
      run.length[i]     <- (trades[i+1]-1)-trades[i]
    }
    runc <- cbind.data.frame(Position=type,Price.start,Price.end,change,start.date,end.date,w.start,w.end,run.length)
    runc$MATCH <- with(runc,ifelse(Position=="CASH" & change < 0,TRUE,
                                   ifelse(Position=="STOCKS" & change >=0,TRUE,FALSE)))
    Cycle.Error <- 1-sum(runc$MATCH,na.rm = TRUE)/(nrow(runc)-sum(is.na(runc$MATCH)))
    output[["policy.predict"]]   <- p.df
    output[["Cycle.metrics"]]    <- runc
    output[["Cycle.Error"]]      <- Cycle.Error
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------#
# Position Plots by dayback window

position.plot <- function(p.df,days.back.window=40) {
  # p.df <- pol$policy.predict
  # days.back.window <- 40
  if(days.back.window=="ALL") {
  } else {
        p.df <- p.df[nrow(p.df):(nrow(p.df)-days.back.window),]
    } 
  p.df$COLOR <- ifelse(p.df$Position=="STOCKS","#00BFC4","#F8766D")
  p.df <- arrange(p.df,Date)
  p.df$Date <- as.Date(as.character(p.df$Date))
  
  p1 <- ggplot(data=p.df,aes(x=Date,y=Close.price)) + 
    geom_vline(mapping=aes(xintercept = Date,color=Position),size=2,color=p.df$COLOR) + 
    geom_line(size=1.5) + 
    theme_bw() +
    ggtitle(paste0(days.back.window," days back"))
  return(p1)
}



#---------------------------------------------------------------------------------------------------------------------#
# Download Intraday stock information (used currently only in plotting)

Intraday <- function(URL) {
  #URL <- "https://finance.google.com/finance/getprices?q=VTI&x=NYSEARCA&i=60&p=1d&f=d,c,h,l,o,v"
  #URL <- paste0("https://finance.google.com/finance/getprices?q=",SYMBOL,"&x=",market,"&i=60&p=1d&f=d,c,h,l,o,v")
  ID <- readLines(URL)
  ls <- list()
  ls[["INTERVAL"]]         <- as.numeric(strsplit(ID[4],"=")[[1]][2])
  ls[["TIMEZONE_OFFSET"]]  <- as.numeric(strsplit(ID[7],"=")[[1]][2])
  ls[["COLUMNS"]]          <- strsplit(strsplit(ID[5],"=")[[1]][2],",")
  ls[["DATA"]]             <- ID[8:length(ID)]
  ls[["DATA"]]             <- as.data.frame(t(data.frame(strsplit(ls$DATA, ","))),stringsAsFactors=FALSE)
  row.names(ls$DATA)       <- 1:nrow(ls$DATA)
  colnames(ls$DATA)        <- unlist(ls$COLUMNS)
  ls$DATA$DATE[1] <- gsub('a', '', ls$DATA$DATE[1])
  ls$DATA <- sapply(ls$DATA,as.numeric)
  ls$DATA[2:nrow(ls$DATA),1] <- (ls$DATA[2:nrow(ls$DATA),1]+ls$DATA[1,1]+ls$INTERVAL*ls$DATA[2:nrow(ls$DATA),1])
  ls$DATA[,1] <- anytime::anytime(ls$DATA[,1])
  
  a <- as.data.frame(ls$DATA)
  a$DATE <- anytime::anytime(a$DATE)
  a <- xts(a[,-1],order.by = a[,1])
  return(a)
}

