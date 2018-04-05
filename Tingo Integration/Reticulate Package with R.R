library(reticulate)
setwd("~/R/RForest_EquitiesPredict/Tingo Integration")

# Set up Virtual Environment
use_virtualenv("~/myenv")

# Run python code and make functions availble for R to use
source_python('TingoQuotes.py')

# Load the data from python
m <- read.csv(text=sp('VTI',as.character(Sys.Date()-1),as.character(Sys.Date()),'daily'))
m
