library(dplyr)

data.dir <- "UCI HAR Dataset"
if(!file.exists(data.dir)) {
  message("downloading and extracting data...")
  source("retrieve_data.R")
}

#' extract match of regexp then cut it
#'
#' @param name Full measure name 
#' @param pattern regexp to extract 
#' @param start first character index to cut
#' @param end last charactrr index to cut
#'
#' @return character vector of length(name) with NAs in unmatched strings
extract.match <- function(name, pattern, start = 1L, end = 1000000L){
   match <- regexpr(pattern, name)
   string <- regmatches(name, match)
   result <- rep(NA_character_, length(name))
   matches <- grepl(pattern, name)
   result[matches] <- substring(string , start, end) 
   result
}


#' read features dataset
read.features <- function(){
  features.path <- file.path(data.dir,"features.txt")
  feat.table <- read.table(features.path, colClasses = c("integer","character"), header = FALSE)
  feat.table %>% 
    rename(Name = V2) %>%
    select(Name) %>% 
    mutate(Is.Mean = grepl("-mean()", Name, fixed = TRUE),
           Is.Std  = grepl("-std()", Name, fixed = TRUE),
           Is.FFT  = grepl("^f", Name),
           Measure = as.factor(extract.match(Name, "^[[:alpha:]]+", 2)),
           Axis    = as.factor(extract.match(Name, "-[[:alpha:]]$", 2)))
}

#' Read single dataset
#'
#' @param kind - kind of data (X, y, subject)
#' @param ... - additional parameters to read.table
#' @param subset (train,test)
read.part <- function(kind, subset,...){
  path <- file.path(data.dir,subset, paste0(kind,"_", subset,".txt"))
  read.table(path, ...)
}

#' reads union of test and train data in single dataset
#'
#' @param kind kind of data (X, y, subject)
#' @param ... additional parameters to read.table
read.entire<- function(kind, ...){
  train <- read.part(kind, "train", ...)
  test <- read.part(kind, "test", ...)
  rbind(train, test)
}

#' Reading factor vector of activities
read.activities <- function(){
  path <- file.path(data.dir, "activity_labels.txt")
  labels <- read.table(path, colClasses = "character")
  activities <- read.entire("y",  colClasses = "factor")$V1
  levels(activities) <- labels$V2
  activities
}

#' read full dataset
read.dataset <- function(){
  #reading
  feats <- read.features()
  col.classes = rep("numeric", nrow(feats))
  entire.X <- read.entire("X", col.names = feats$Name, colClasses = col.classes)
  
  #choose only needed columns
  need <- feats$Is.Mean | feats$Is.Std
  entire.X <- entire.X[, need]
  
  
  #assign new column names
  newfeats <- feats %>% 
    filter(Is.Mean | Is.Std) %>%
    mutate( New.Name = paste (
      Measure,
      ifelse(Is.Mean, "mean", "std"),
      ifelse(is.na(Axis), "", as.character(Axis)),
      ifelse(Is.FFT, "FFT", ""),
    sep = "."))
  
  colnames(entire.X) <- newfeats$New.Name
  
  entire.X %>%
    mutate(Activity = read.activities(),
           Subject = read.entire("subject", colClasses = "factor")$V1)
}



#' Calculate mean for each variable using Activity and Subject as key columns
#'
#' @param table dataframe containing key and variable columns
#'
#' @return aggregated dataframe
calc.means <- function(table){
  library(reshape2)
  #
  table %>% 
    melt(id.vars = c("Activity", "Subject")) %>%
    group_by(Activity, Subject, variable) %>%
    summarize(mean = mean(value)) %>%
    dcast(Activity + Subject ~ variable, mean, value.var = "mean")
}

ds <- read.dataset()
tidy <- calc.means(ds)


