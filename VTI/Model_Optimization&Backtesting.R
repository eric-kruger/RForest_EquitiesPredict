source("~/R/RForest_EquitiesPredict/VTI/SETUP.R")
source(paths$path.lib)

# Load Data
  quantmod::getSymbols(c("VTI","GLD","SH","BND"),src="google",from ="2004-12-15",to = Sys.Date()) 
  stock.data <- list(VTI=VTI,GLD=GLD,SH=SH,BND=BND)

# Model
  mod <- stock.RF.mod(stock2model = "VTI",
                      symbols=stock.data,
                      forecast.days = 20,
                      FROM = "2004-12-15",
                      TO = "2017-08-29",
                      smooth.r=0.14, 
                      mtry=10,
                      trees=1500)
  pred <- stock.RF.predict(mod,newdata=stock.data)

# Simulate Number of Variables to Try at each step
  rs <- 1:20
  ER <- NULL
  for(i in 1:length(rs)) {
    mod <- stock.RF.mod(symbols=stock.data,forecast.days = 20,FROM = "2004-12-15",TO = "2017-08-29",smooth.r=0.14,mtry=rs[i]) 
    ER[i] <- pred.error(mod$model$confusion)
  }
  mtry <- cbind.data.frame(rs, ER)
  mtry
  
# Simulate Smoothing 
  sim.s <- seq(0.1,1,by=0.02)
  errors <- NULL
  ce     <- NULL
  EV     <- NULL
  for (i in 1:length(sim.s)) {
    mod <-   mod <- stock.RF.mod(stock2model = "VTI",                     # Need to fix this model 
                                 symbols=stock.data,
                                 forecast.days = 20,
                                 FROM = "2001-12-15",
                                 TO = "2017-08-29",
                                 smooth.r=sim.s[i], 
                                 mtry=10,
                                 trees=1500)
    pred <- stock.RF.predict(mod,newdata=stock.data)
    perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.85,buy.t=0.5)
    pol <- policy.test(perf)
    errors <- rbind.data.frame(errors,perf$`Error Rates`)
    ce[[i]] <- pol$Cycle.Error
    EV[[i]] <- pol$End.Value
    rm(mod,pred,perf,pol)
  }
  sim.sum <- cbind.data.frame(Smoothing=sim.s,errors,Cycle.Error=ce,End.Value=EV)
  sim.sum
  
  
# Simulation Policy
  sim <- expand.grid(sell=seq(0,1,by=0.02),buy=seq(0,1,by=0.02)) #buys and sell probabilities
  sim.out      <- NULL
  cycle.Error  <- NULL
  cycle.aggregate <- NULL
  EV <- NULL
  i <- 1
  for(i in 1:nrow(sim)) {
    # performance
    p <- stock.RF.performance(predict.obj=pred,
                              sell.max = -0.01,
                              stay.max = 0.01,
                              buy.max = 99,
                              sell.t=sim$sell[i],
                              buy.t=sim$buy[i])
    # Policy
    p2 <- policy.test(p)
    
    sim.out[i] <- p$`Error Rates`[7]
    cycle.Error[i] <- p2$Cycle.Error
    a <- ifelse(p2$Cycle.metrics$Position=="CASH",p2$Cycle.metrics$change*-1,p2$Cycle.metrics$change*1)
    cycle.aggregate[i] <-sum(a)
    EV[i]             <- p2$End.Value
  }
  
  # Policy Output
  sim$Cycle.Error <- cycle.Error
  sim$cycle.a <- cycle.aggregate
  sim$End.Value <- EV
  sim$T.ACTION.ERROR <- unlist(sim.out)
  min(sim$T.ACTION.ERROR)
  sim <- arrange(sim,desc(cycle.a))
  sim
  
  
#-----------------------------------------------------------------------------------------------------------------------#
# Examine the results of specific types
  perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.99,buy.t=0.99)
  pol  <- policy.test(perf)
  