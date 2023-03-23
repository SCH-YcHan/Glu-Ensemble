rm(list=ls())

#install.packages(c("openxlsx", "dplyr", "stringr", "lubridate"))

library(openxlsx)
library(dplyr)
library(stringr)
library(lubridate)

xlsx_folder_path <- "../data/BG data/"
xlsx_file_names <- list.files(xlsx_folder_path)

if(!file.exists("../data/BG EDA")){
  dir.create("../data/BG EDA")
}

#columns rename and cutting row
for(file_name in xlsx_file_names){
  f <- read.xlsx(paste0(xlsx_folder_path, file_name))
  names(f) <- c("index", "time_stamp", "event", "event_sub",
                "patient", "device", "source",
                "glucose", "insulin", "carbo",
                "time", "glu_v", "tm", "tm_id")

  f2 <- f %>%
    select(time_stamp, event, glucose, insulin, carbo) %>%
    filter(!is.na(time_stamp))

  write.csv(f2, paste0("../data/BG EDA/", strsplit(file_name, "\\.")[[1]][1], ".csv"), row.names = F)
}

csv_folder_path <- "/data/BG EDA/"
csv_names <- list.files(csv_folder_path)

not_bg_value <- data.frame()
for(csv_name in csv_names){
  f <- read.csv(paste0(csv_folder_path, csv_name))
  
  if(!is.numeric(f$glucose)){
    row <- table(f$glucose) %>% 
      data.frame %>% 
      mutate(Var1=ifelse(is.na(as.numeric(as.character(Var1))), as.character(Var1), NA)) %>% 
      filter(!is.na(Var1)) %>% 
      cbind(File=strsplit(csv_name, "\\.")[[1]][1],.) %>% 
      mutate(ratio = round(Freq/nrow(f), 3))
    
    not_bg_value <- rbind(not_bg_value, row)
  }
}
not_bg_value

range <- function(x){
  min_v <- min(x, na.rm=T)
  max_v <- max(x, na.rm=T)
  
  return(paste0(min_v, " ~ ", max_v))
}

time_interval <- data.frame()
for(csv_nan csv_names){
  f <- read.csv(paste0(csv_path,file_ csv_name))
  f$time_stamp <- ymd_hfolder_ms(f$time_stamp)
  
  row <- f %>% 
    filter(event=="EGV") %>% 
    mutate(time_lag = as.duration(interval(lag(time_stamp), time_stamp))) %>% 
    mutate(time_sec = time_lag/as.duration(seconds(1))) %>% 
    summarise_at(.vars=c("time_sec"), .funs=c("range")) %>% 
    cbind(File=strsplit(csv_name, "\\.")[[1]][1], .)
  
  time_interval <- rbind(time_interval, row)
}
time_interval

f <