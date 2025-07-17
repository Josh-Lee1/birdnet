library(tidyverse)
setwd("trial1")

key<-read_csv("key_log_2025-01-19_14-32-50.csv")

recording<-read_delim("2MM03792_20250119_141450.BirdNET.selection.table.txt")
recording2<-read_delim("2MM03792_20250119_151402.BirdNET.selection.table.txt")


recording_start_time<-ymd_hms("2025/1/19 14:14:50")
recording2_start_time<-ymd_hms("2025/1/19 15:14:02")
key$time<-ymd_hms(paste("2025/1/19",key$AEDT_Timestamp))

recording$recording_window_time<-recording_start_time+recording$`Begin Time (s)`
recording2$recording_window_time<-recording2_start_time+recording2$`Begin Time (s)`

recording_all<-bind_rows(recording,recording2)

ggplot(recording_all,aes(x=recording_window_time,y=`Common Name`))+geom_point(size=2)+
  geom_point(data=key,aes(x=time,y=`Translation`),col="red",alpha=0.1)+theme_bw()

key$AEDT_Timestamp


setwd("../trial3")

key<-read_csv("key_log_2025-01-20_10-49-06.csv")

recording<-read_delim("2MM03822_20250120_091202.BirdNET.selection.table.txt")
recording2<-read_delim("2MM03822_20250120_101202.BirdNET.selection.table.txt")

#hack
minute_second<-gsub(":00","",as.character(key$AEDT_Timestamp))

minute_second[1:63]<-paste0("09:",minute_second[1:63])
minute_second[64:664]<-paste0("10:",minute_second[64:664])

hour_minute_second<-ifelse(str_length(minute_second)<=5,paste0(minute_second,":00"),minute_second)

recording_start_time<-ymd_hms("2025/1/20 09:12:02")
recording2_start_time<-ymd_hms("2025/1/20 10:12:02")
key$time<-ymd_hms(paste("2025/1/20",hour_minute_second))

recording$recording_window_time<-recording_start_time+recording$`Begin Time (s)`
recording2$recording_window_time<-recording2_start_time+recording2$`Begin Time (s)`

recording_all<-bind_rows(recording,recording2)

ggplot(recording_all,aes(x=recording_window_time,y=`Common Name`))+geom_point(size=2)+
  geom_point(data=key,aes(x=time,y=`Translation`),col="red",alpha=0.1)+theme_bw()

key$AEDT_Timestamp

