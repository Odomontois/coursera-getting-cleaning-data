# Creating tidy dataset for "Human Activity Recognition Using Smartphones dataset "

Project is using data from the Human Activity Recognition Using Smartphones Data Set available at 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Script provides two main sources:
  * [run_analysis.R](../master/run_analysis.R)
  * [retrieve_data.R](../master/retrieve_data.R)
  
[retrieve_data.R](../master/retrieve_data.R) provides small code for downloading file from original location in course, 
and unzipping in the current folder

[run_analysis.R](../master/run_analysis.R) defines two general functions for handling data:
  * [`read.dataset`](../master/run_analysis.R#L71) collecting original dataset 
  preserving only *mean* and *standard deviation* variables
  * [`calc.means`](../master/run_analysis.R#L106) calculating average value for each variable, 
  gained from [`read.dataset`](../master/run_analysis.R#L71) making pairs `Acitivity,Subject` unique
