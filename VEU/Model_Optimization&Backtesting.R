source("~/R/RForest_EquitiesPredict/VEU/SETUP.R") # e.g. source("~/R/RForest_EquitiesPredict/VEU/SETUP.R") 
load(paste0(paths$path.dir,"/DAT.Rdata"))
source(paths$path.lib)

# Load Data
  quantmod::getSymbols(dat[["symbols"]],src="google",from = dat[["FROM"]],to = dat[["TO.predict"]]) 
    stock.data <- list()
    for(stock.name in dat$symbols) {
      stock.data[[stock.name]] <- eval(parse(text=stock.name))
    }
  
# Model
  mod <- stock.RF.mod(stock2model = dat$stock2model,
                      symbols=stock.data,
                      forecast.days = 20,
                      FROM = dat$FROM,
                      TO = dat$TO.model,
                      smooth.r=0.14, 
                      mtry=10,
                      trees=1500)
  pred <- stock.RF.predict(mod,newdata=stock.data)

# Simulate Number of Variables to Try at each step
  sim.mtry(stock2model = dat$stock2model,                     
           symbols=stock.data,
           forecast.days = 20,
           FROM = dat$FROM,
           TO = dat$TO.model,
           smooth.r=0.14, 
           mtry=10,
           trees=1500,
           SEQ = 1:3)               # Edit this line 

# Simulate Smoothing 
  sim.smooth(stock2model = dat$stock2model,                     
             symbols=stock.data,
             forecast.days = 20,
             FROM = dat$FROM,
             TO = dat$TO.model,
             mtry=10,
             trees=1500,
             sell.max=-0.01,
             stay.max=0.01,
             buy.max=99,
             sell.t=0.6,
             buy.t=0.6,
             BY=0.2)               # Edit this line

# Simulation Policy
  sim.policy(pred,
             BY=0.05,
             sell.max=-0.01,
             stay.max=0.01,
             buy.max=99)

#-----------------------------------------------------------------------------------------------------------------------#
# Examine the results of specific types
  perf <- stock.RF.performance(predict.obj=pred,sell.max = -0.01,stay.max = 0.01,buy.max = 99,sell.t=0.6,buy.t=0.6)
  pol  <- policy.test(perf)
