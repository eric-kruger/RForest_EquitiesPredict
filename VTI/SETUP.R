# Directory Setup
  setup.dir             <- dirname(sys.frame(1)$ofile)                                # Pull the current directory of the script
  directory             <- tail(strsplit(setup.dir,"/")[[1]],1)
  paths <- list()
  paths["path.main"]     <- "~/R/RForest_EquitiesPredict"
  paths["path.dir"]      <-  paste(paths$path.main,directory,sep="/")
  paths["path.report"]   <-  paste(paths$path.main,directory,"Reports",sep="/")
  paths["path.history"]  <-  paste(paths$path.main,directory,"HistoricalData",sep="/")
  paths["path.lib"]      <-  paste(paths$path.main,"/LIB/","RF_FX_lib.R",sep="")
  paths["path.personal"] <-  paste(paths$path.main,"/MAILR.personal",sep="")
  
  if(file.exists(paths$path.personal)==TRUE) {
    personal   <- read.csv(paths$path.personal,header=FALSE,stringsAsFactors=FALSE)
  } else {
      personal <- NA
    }
  
  


