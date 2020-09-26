library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)

data <- read.csv("final_predictive_data.csv")
data <- data[,-1]
data$wl <- as.factor(data$wl)





