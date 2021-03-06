source("~/R/RForest_EquitiesPredict/{CHANGE DIRECTORY HERE}/SETUP.R") # e.g. source("~/R/RForest_EquitiesPredict/VEU/SETUP.R") 
source(paths$path.lib)
library(rmarkdown)

Today <- weekdays(Sys.Date())

if(Today %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) {
  
  rmarkdown::render(paste0(paths$path.report,"/MarketAnalysis.Rmd"))
  
  if(is.na(personal)==TRUE) {
    #Google Credentials
    EMAIL      <- "{insert full gmail here}"   # Your full email adddress to be sent to
    UserName   <- "{insert google user name}"  # User name is everything before @
    password   <- "{insert password}"          # Custom app password for gmail (needs to be set up in app passwords)
  } else {
    EMAIL    <- personal$V1[1]
    UserName <- personal$V1[2]
    password <- personal$V1[3]
  }
  
  mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                   to = EMAIL,
                   subject = paste0(directory," Analysis"),
                   body = tab.email.t,
                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                   authenticate = TRUE,
                   send = TRUE, 
                   attach.files = paste0(paths$path.report,"/MarketAnalysis.html"),
                   debug=FALSE)
  
  if(WARNING.SELL==TRUE) {
    mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                     to = EMAIL,
                     subject = paste0("SELL NOTIFICATION: ",directory),
                     body = paste0("SELL NOTIFICATION: ",directory),
                     smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE, 
                     debug=FALSE)
    
  }
  
  if(WARNING.BUY==TRUE) {
    mailR::send.mail(from = "Pirate.Analysis@ARRRRH.com",
                     to = EMAIL,
                     subject = paste0("BUY NOTIFICATION: ",directory),
                     body = paste0("BUY NOTIFICATION: ",directory),
                     smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = UserName, passwd = password, ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE, 
                     debug=FALSE)
    
  }
  
}
#Print the last analysis
  print(tab.email.t)
