# Directory Setup
source("~/R/RForest_EquitiesPredict/GT/SETUP.R")

# Load Training Data
  source(paths$path.lib)
  quantmod::getSymbols(c("GT"),src="google",from ="1995-12-15",to = Sys.Date()) 
  stock.data <- list(GT=GT)

# Train the Model
  mod <- stock.RF.mod(stock2model = "GT",
                      symbols=stock.data,
                      forecast.days = 20,
                      FROM = "2001-12-15",
                      TO = "2017-08-29",
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
  perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.6,buy.t=0.6)
  perf

# Policy (Evaluate performance of model based on a specific investment policy)
  pol <- policy.test(perf)
  pol

 

