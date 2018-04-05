# Directory Setup
source("~/R/RForest_EquitiesPredict/VTI3/SETUP.R") # e.g. source("~/R/RForest_EquitiesPredict/VEU/SETUP.R")         
FROM             <- "2009-01-01"
TO.model         <- "2018-03-01"
TO.predict       <- Sys.Date()
stock2model      <- "VTI"              # e.g. "VEU"
symbols          <- c("VTI","VXX")                    # e.g. c("VEU")
Market           <- "NYSEARCA"   # e.g. "NYSEARCA"

# Load functions
source(paths$path.lib)

# Load Training Data
for (stock in symbols) {
  df.stock <- read.csv(text=sp(stock,FROM,TO.model,'daily'),stringsAsFactors = FALSE)
  df.stock %<>% select(date,open,high,low,close,volume)
  df.stock$date %<>% as.Date()
  assign(stock, df.stock)
}

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
                    forecast.days = 10,
                    breaks=c(-99,-0.03,-0.01,0.01,0.03,99),
                    FROM = FROM,
                    TO = TO.model,
                    smooth.r=0.2, 
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
pred <- stock.RF.predict(mod)
pred$end.date.predict
tail(pred$probs)

# Performance (Evaluate performance of prediction)
perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.5,buy.t=0.5)
perf

# Policy (Evaluate performance of model based on a specific investment policy)
pol <- policy.test(perf)
pol
tail(pol$policy.predict)
pol$Cycle.metrics
pol$Percent.Inc

tail(VIX)
tail(VTI)
