setwd("~/berkeley/capstone")
library(readr)

Result38 <- read_csv("~/berkeley/capstone/New folder/Cluster #38/Result38.csv")

data38 <- subset(Result38, select = Returns)
data38 <- data.frame(data38)
t.test(data38, mu= 0)

Result33 <- read_csv("~/berkeley/capstone/New folder/Cluster #33/Result33.csv")

data33 <- subset(Result33, select = Returns)
data33 <- data.frame(data33)
t.test(data33, mu= 0)

Result32 <- read_csv("~/berkeley/capstone/New folder/Cluster #32/Result32.csv")

data32 <- subset(Result32, select = Returns)
data32 <- data.frame(data32)
t.test(data32, mu= 0)

Result29 <- read_csv("~/berkeley/capstone/New folder/Cluster #29/Result29.csv")

data29 <- subset(Result29, select = Returns)
data29 <- data.frame(data29)
t.test(data29, mu= 0)

