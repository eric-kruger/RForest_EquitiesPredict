# Directory Setup
  source("~/R/RForest_EquitiesPredict/VTI/SETUP.R") # e.g. source("~/R/RForest_EquitiesPredict/VEU/SETUP.R")         
  FROM             <- "2001-12-15"
  TO.model         <- "2017-08-29"
  TO.predict       <- Sys.Date()
  stock2model      <- "VTI"              # e.g. "VEU"
  symbols          <- c("VTI","SH","BND","GLD")                    # e.g. c("VEU")
  Market           <- "NYSEARCA"   # e.g. "NYSEARCA"
  
# Load Training Data
  source(paths$path.lib)
  quantmod::getSymbols(symbols,src="yahoo",from = FROM,to = TO.predict) 
  stock.data <- list()
  for(stock.name in symbols) {
    stock.data[[stock.name]] <- eval(parse(text=stock.name))
  }
 
  dat <- list(stock2model=stock2model,
              FROM=FROM,
              TO.model=TO.model,
              TO.predict=TO.predict,
              symbols=symbols,
              stock.data=stock.data,
              Market=Market)
  
  save(dat,file=paste0(paths$path.dir,"/DAT.Rdata"))
  
# Train the Model
  mod <- stock.RF.mod(stock2model = stock2model,
                      symbols=stock.data,
                      forecast.days = 20,
                      FROM = FROM,
                      TO = TO.model,
                      smooth.r=0.14, 
                      mtry=10,
                      trees=1500)
  mod
# Save Data
  save(personal,paths,mod,file=paste0(paths$path.dir,"/MODEL.Rdata"))
  
# Evaluate Variable importance
  impt <- importance(mod$model,type=2)
  rn <- rownames(impt)
  impt <- data.frame(impt)
  impt$Variable <- rn
  impt <-arrange(impt,desc(MeanDecreaseGini))
  impt

# Prediction (Predict the data)
  pred <- stock.RF.predict(mod,newdata=stock.data)
  pred
  

# Performance (Evaluate performance of prediction)
  perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.84,buy.t=0.4)
  perf

# Policy (Evaluate performance of model based on a specific investment policy)
  pol <- policy.test(perf)
  pol
  pol$Cycle.metrics
 

