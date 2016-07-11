load_all()
load_all("~/analysis/Rowens")
library(lubridate)
library(dplyr)
library(ggplot2)

start_date <- mdy("01-01-2016")
end_date <- start_date %m+% months(6) %m-% days(1)

mfile_df <- pull_mfile_data(start_date, end_date)
