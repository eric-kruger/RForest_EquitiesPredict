source("~/R/RForest_EquitiesPredict/LIB/RF_FX_lib.R")
library(rmarkdown)

Today <- weekdays(Sys.Date())

if(Today %in% c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) {
  
  rmarkdown::render("~/R/RForest_EquitiesPredict/VTI/Reports/MarketAnalysis.Rmd")
  
  #Google Credentials
  EMAIL      <- "{insert full gmail here}"   # Your full email adddress to be sent to
  UserName   <- "{insert google user name}"  # User name is everything before @
  password   <- "{insert password}"          # Custom app password for gmail (needs to be set up in app passwords)
  
  mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                   to = EMAIL,
                   subject = "VTI Analysis",
                   body = tab.email.t,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE, 
                   attach.files = "~/R/RForest_EquitiesPredict/VTI/Reports/MarketAnalysis.html",
                   debug=FALSE)
  
  if(WARNING.SELL.VTI==TRUE) {
    mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                     to = EMAIL,
                     subject = "SELL WARNING VTI",
                     body = "WARNING SELL",
                     smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE, 
                     debug=FALSE)
    
  }
  
  if(WARNING.BUY.VTI==TRUE) {
    mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                     to = EMAIL,
                     subject = "BUY NOTIFICATION VTI",
                     body = "BUY NOTIFICATION VTI",
                     smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE, 
                     debug=FALSE)
    
  }
  
}
#Print the last analysis
print(tab.email.t)
