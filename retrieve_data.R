#Downloading and unzipping data by course link
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url ,"dataset.zip", method = "curl")
unzip("dataset.zip")
