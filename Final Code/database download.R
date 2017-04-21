install.packages("RCurl")
install.packages("data.table")
library(RCurl)
library(dplyr)
library(data.table)

setwd("~/berkeley/capstone")

URL      <- "http://funglab.berkeley.edu/pub2/records_0714.tsv"
destfile <- "fung lab records.tsv"
download.file(URL, destfile)

data <- read.delim("fung lab records.tsv")

data <- as.data.frame(data)

typeof(data)
class(data)

head(data, 5)

write.csv(data, file = "allrecords.RData")
load("allrecords.Rdata")

data$GrantDate <- as.Date(data$GrantDate) 

#filter(mydata, date >= "2014-12-02" & date <= "2014-12-05")
#data95to2000 <- filter(data, GrantDate < 2001-01-01, GrantDate > 1994-01-01)

#install.packages("lubridate")
library(lubridate)

data$y <- format(as.Date(data$GrantDate, format="%d/%m/%Y"),"%Y")


data95to2000 <- subset(data, y > 1994 & y< 2001)
table(data95to2000$y)

data03to05 <- subset(data, y > 2002 & y< 2006)

write.csv(data95to2000, "95to2000.RData")
load("95to2000.RData")

#install.packages("readr")
library(readr)

write.csv(data95to2000, file = "95to2000.csv")

cols.dont.want <- "y"
#cols.dont.want <- c("genome", "region") # if you want to remove multiple columns

datafinal <- data95to2000[, ! names(data95to2000) %in% cols.dont.want, drop = F]

write.csv(datafinal, file = "final95to2000.RData")
write.csv(datafinal, file = "final95to2000.csv")

finaldata<- data95to2000[complete.cases(data95to2000), ]
write.csv(finaldata, file = "complete95to2000.csv")
write.csv(finaldata, file = "complete95to2000.RData")

install.packages("tidyr")
library(tidyr)
#DF %>% drop_na(CPCClass)

data1995 <- filter(data95to2000, y == "1995")
data1996 <- filter(data95to2000, y == "1996")
data1997 <- filter(data95to2000, y == "1997")
data1998 <- filter(data95to2000, y == "1998")
data1999 <- filter(data95to2000, y == "1999")
data2000 <- filter(data95to2000, y == "2000")
data2003 <- filter(data03to05, y == "2003")
data2004 <- filter(data03to05, y == "2004")
data2005 <- filter(data03to05, y == "2005")

finaldata1995 <- data1995[!data1995$CPCClass %in% "NULL", ] 
finaldata1996 <- data1996[!data1996$CPCClass %in% "NULL", ] 
finaldata1997 <- data1997[!data1997$CPCClass %in% "NULL", ] 
finaldata1998 <- data1998[!data1998$CPCClass %in% "NULL", ] 
finaldata1999 <- data1999[!data1999$CPCClass %in% "NULL", ] 
finaldata2000 <- data2000[!data2000$CPCClass %in% "NULL", ] 
finaldata2003 <- data2003[!data2003$CPCClass %in% "NULL", ] 
finaldata2004 <- data2004[!data2004$CPCClass %in% "NULL", ] 
finaldata03to05<- data03to05[!data03to05$CPCClass %in% "NULL", ] 

cleandata1995 <- finaldata1995[!finaldata1995$AssigneeName %in% "NULL", ] 
cleandata1996 <- finaldata1996[!finaldata1996$AssigneeName %in% "NULL", ] 
cleandata1997 <- finaldata1997[!finaldata1997$AssigneeName %in% "NULL", ] 
cleandata1998 <- finaldata1998[!finaldata1998$AssigneeName %in% "NULL", ] 
cleandata1999 <- finaldata1999[!finaldata1999$AssigneeName %in% "NULL", ] 
cleandata2000 <- finaldata2000[!finaldata2000$AssigneeName %in% "NULL", ] 
cleandata2003 <- finaldata2003[!finaldata2003$AssigneeName %in% "NULL", ]
cleandata2004 <- finaldata2004[!finaldata2004$AssigneeName %in% "NULL", ]
cleandata03to05 <- finaldata03to05[!finaldata03to05$AssigneeName %in% "NULL", ]

write.csv(cleandata2003, "clean2003.csv")
write.csv(cleandata2004, "clean2004.csv")
write.csv(cleandata03to05, "clean03to05.csv")


test <- cleandata1995[1:50,]
test2 <- filter(cleandata1995, AssigneeName == "NEC Corporation")
test2b <- filter(test2, CPCClass == "G11B 5/5521 (20130101)" & AssigneeName == "NEC Corporation")
test2c <- test2b[2:3,]
test3 <- rbind(test,test2b)
write.csv(test3, "test.csv")
write.csv(test3, "testing.csv")

rm(test2a)

write.csv(cleandata1995, "clean1995.csv")
write.csv(cleandata1995, "clean1995.RData")

write.csv(cleandata1995, "clean1995.csv")
write.csv(cleandata1996, "clean1996.csv")
write.csv(cleandata1997, "clean1997.csv")
write.csv(cleandata1998, "clean1998.csv")
write.csv(cleandata1999, "clean1999.csv")
write.csv(cleandata2000, "clean2000.csv")

#inner join
# patent number, assignee name, CPCClass, Xi
kpss <- read.csv("patents.csv")
colnames(kpss)[1] <-"PatentNo"
cleankpss <- kpss[!kpss$xi %in% NA, ] 
write.csv(cleankpss, "cleankpss.csv")

head(kpss)

clean2004 <- read.csv("clean2004.csv")
clean2004$PatentNo = as.numeric(as.character(clean2004$PatentNo))

clean2003 <- read.csv("clean2003.csv")
clean2003$PatentNo = as.numeric(as.character(clean2003$PatentNo))

clean2005 <- read.csv("clean2005.csv")
cleandata03to05$PatentNo = as.numeric(as.character(cleandata03to05$PatentNo))


library(dplyr)

cleanmerge03to05 <- inner_join(cleandata03to05, cleankpss, by= "PatentNo")
write.csv(cleanmerge03to05, "cleanmerge03to05.csv")

cleanmerge04 <- inner_join(clean2004, cleankpss, by= "PatentNo")
write.csv(cleanmerge04, "cleanmerge2004.csv")

cleanmerge05 <- inner_join(clean2005, cleankpss, by= "PatentNo")
write.csv(cleanmerge05, "cleanmerge2005.csv")

disambig <- read.delim("disambiguation.tsv")
disambig <- as.data.frame(disambig)

View(head(disambig))

cleanmerge03 <- read.csv("cleanmerge2003.csv")
cleanmerge04 <- read.csv("cleanmerge2004.csv")
cleanmerge05 <- read.csv("cleanmerge2005.csv")

View(head(cleanmerge03))

disambig$Patent = as.numeric(as.character(disambig$Patent))

disambigkpss <- inner_join(cleankpss, disambig, by= c("PatentNo" = "Patent"))

disambig03to05 <- inner_join(cleanmerge03to05, disambig, by= c("PatentNo" = "Patent"))
disambig04 <- inner_join(cleanmerge04, disambig, by= c("PatentNo" = "Patent"))
disambig05 <- inner_join(cleanmerge05, disambig, by= c("PatentNo" = "Patent"))

write.csv(disambig03to05, "disambig03to05.csv")
write.csv(disambig04, "disambig04.csv")
write.csv(disambig05, "disambig05.csv")

