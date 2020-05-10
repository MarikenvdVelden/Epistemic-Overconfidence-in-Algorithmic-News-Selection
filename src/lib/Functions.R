library(tidyverse)
library(sjPlot)
library(sjmisc)
library(psy)
library(psych)
library(ggstatsplot)
library(broom)
library(tidybayes)


qualtrics <- function(df){
  tmp <- read_csv(df, skip = 3, col_names = F)
  colnames(tmp) <- read_csv(df, col_names = FALSE, skip = 0, n_max=1)
  colnames(tmp)<-make.names(colnames(tmp), unique = TRUE) #duplicate column names in original file
  return(tmp)
}
rename1 <- function(variable){
  variable <- ifelse(variable == "never", 0,
                     ifelse(variable == "every day", 7, variable))
  variable <- as.numeric(variable)
  return(variable)
}
rename2 <- function(variable){
  variable <- ifelse(variable == "completely disagree", 1,
                     ifelse(variable == "completely agree", 7, variable))
  variable <- as.numeric(variable)
  return(variable)
}
rename3 <- function(variable){
  variable <- ifelse(variable == 2, 1,
                     ifelse(variable == 3, 1, 0))
  variable <- as.numeric(variable)
  return(variable)
}
