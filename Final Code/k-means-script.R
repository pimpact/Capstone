library(data.table)
library(readr)
library(dplyr)
library(stringr)
setwd("~/berkeley/capstone")

# read csv file and replace NAs with 0
patent <- read.csv("disambig03to05Out2.csv")
patent[is.na(patent)] <- 0

View(head(patent,5))

# scale the data set
df <- scale(patent[-1])

# find the "rule of thumb" cluster size
sqrt(nrow(df)/2)

# code below find optimal cluster size
wssplot <- function(data, nc=200, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
# 
wssplot(df)

#fit.km <- kmeans(df,100,nstart=50)

fit.km <- kmeans(df,40)

fit.km$size
fit.km$cluster
fit.km$betweenss
fit.km$tot.withinss
fit.km$withinss
fit.km$betweenss/fit.km$tot.withinss

library(cluster) 
clusplot(df, fit.km$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(fpc)
plotcluster(df, fit.km$cluster)


head(fit.km$centers)

# append cluster size
patent$cluster <- fit.km$cluster

cluster_df <- select(patent, MatchAsgName, cluster)
df1 <- filter(cluster_df, cluster %in% c(6, 7, 9,13,14,16, 29,32, 33, 38))
df1 <- arrange(df1, cluster)
df1

df3 <- t(df1)
write.csv(df1, "companycluster03to05.csv")

df <- read.csv("Assignee&ClusterNum.csv", header=TRUE, stringsAsFactors=FALSE)
df <- read.csv("companycluster03to05.csv", header=TRUE, stringsAsFactors=FALSE)

df$Competitors <- mapply(function(x,y)
  toString(setdiff(x,y)),
  ave(df$MatchAsgName,df$cluster,FUN= list),df$MatchAsgName)  

write.csv(df, "companycompetitors03to05.csv")

#=============================================================
# cluster 33
data <- read.csv("disambig03to05.csv")
target1 <- "CHEVRON USA INC"
target2 <- "CONOCOPHILLIPS COMPANY"
target3 <- "ECOLAB INC"
target4 <- "EXXONMOBIL RESEARCH AND ENGINEERING COMPANY"


###chevron
data1 <- filter(data, MatchAsgName %in% target1)
head(data1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))
head(data2)

chev1<- data.frame(chevron, comp = "CONOCOPHILLIPS COMPANY")
chev2<- data.frame(chevron, comp = "ECOLAB INC")
chev3<- data.frame(chevron, comp = "EXXONMOBIL RESEARCH AND ENGINEERING COMPANY")
chevfinal<- rbind.data.frame(chev1,chev2,chev3)

####conocophillips
data1.1 <- filter(data, MatchAsgName %in% target2)
head(data1.1)
data2.1<- subset(data1.1, select=c(xi,GrantDate, PatentNo, MatchAsgName))
head(data2.1)

conoco1<- data.frame(data2.1, comp = "CHEVRON USA INC")
conoco2<- data.frame(data2.1, comp = "ECOLAB INC")
conoco3<- data.frame(data2.1, comp = "EXXONMOBIL RESEARCH AND ENGINEERING COMPANY")
conocofinal<- rbind.data.frame(conoco1,conoco2,conoco3)

#### ECOLAB
data1.2 <- filter(data, MatchAsgName %in% target3)
head(data1.2)
data2.2<- subset(data1.2, select=c(xi,GrantDate, PatentNo, MatchAsgName))
head(data2.2)

ecolab1<- data.frame(data2.2, comp = "CHEVRON USA INC")
ecolab2<- data.frame(data2.2, comp = target2)
ecolab3<- data.frame(data2.2, comp = "EXXONMOBIL RESEARCH AND ENGINEERING COMPANY")
ecolabfinal<- rbind.data.frame(ecolab1,ecolab2,ecolab3)

#### EXXON

data1.2 <- filter(data, MatchAsgName %in% target4)
head(data1.2)
data2.2<- subset(data1.2, select=c(xi,GrantDate, PatentNo, MatchAsgName))
head(data2.2)

exxon1<- data.frame(data2.2, comp = target1)
exxon2<- data.frame(data2.2, comp = target2)
exxon3<- data.frame(data2.2, comp = target3)
exxonfinal<- rbind.data.frame(exxon1,exxon2,exxon3)

finalfinal <- rbind(chevfinal, conocofinal, ecolabfinal, exxonfinal)
finalfinal2 <- finalfinal[1:5,]
write.csv(finalfinal, "finalfinal.csv")
write.csv(finalfinal2, "headfinalfinal.csv")



#=============================================================
# cluster 6
data <- read.csv("disambig03to05.csv")
target1 <- "BAXTER INTERNATIONAL INC"
target2 <- "BECTON DICKINSON AND COMPANY"
target3 <- "KENNAMETAL INC"


###target1
data1 <- filter(data, MatchAsgName %in% target1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))


t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t1final<- rbind.data.frame(t1,t2)
View(t1final)

####target2
data1 <- filter(data, MatchAsgName %in% target2)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))


t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target3)
t2final<- rbind.data.frame(t1,t2)


#### target3
data1 <- filter(data, MatchAsgName %in% target3)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))


t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3final<- rbind.data.frame(t1,t2)

finalfinal <- rbind(t1final,t2final,t3final)
finalfinal2 <- finalfinal[1:5,]
write.csv(finalfinal, "cluster6finalfinal.csv")
write.csv(finalfinal2, "cluster6headfinalfinal.csv")

###=====================================
#cluster29
target1 <- "ABBOTT LABORATORIES"
target2 <- "ARCHER DANIELS MIDLAND COMPANY"
target3 <- "ASTRAZENECA AB"
target4 <- "BRISTOL MYERS SQUIBB COMPANY"
target5 <- "DOW GLOBAL TECHNOLOGIES INC"
target6 <- "EASTMAN CHEMICAL COMPANY"
target7 <- "ELI LILLY AND COMPANY"
target8 <- "GERON CORPORATION"
target9 <- "MERCK & CO INC"
target10 <- "ISIS PHARMACEUTICALS INC"
target11 <- "MONSANTO TECHNOLOGY LLC"
target12 <- "NOVARTIS AG"
target13 <- "PFIZER INC"
target14 <- "UNILEVER HOME & PERSONAL CARE USA DIVISION OF CONOPCO INC"

###target1
data1 <- filter(data, MatchAsgName %in% target1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t1final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target2
data1 <- filter(data, MatchAsgName %in% target2)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t2final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target3
data1 <- filter(data, MatchAsgName %in% target3)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t3final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target4
data1 <- filter(data, MatchAsgName %in% target4)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t4final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)


###target5
data1 <- filter(data, MatchAsgName %in% target5)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target4)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t5final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target6
data1 <- filter(data, MatchAsgName %in% target6)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target3)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t6final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target7
data1 <- filter(data, MatchAsgName %in% target7)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target3)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t7final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target8
data1 <- filter(data, MatchAsgName %in% target8)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target3)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t8final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target9
data1 <- filter(data, MatchAsgName %in% target9)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target3)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t9final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target10
data1 <- filter(data, MatchAsgName %in% target10)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target3)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t10final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target11
data1 <- filter(data, MatchAsgName %in% target11)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target1)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t11final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target12
data1 <- filter(data, MatchAsgName %in% target12)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target1)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t12final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target13
data1 <- filter(data, MatchAsgName %in% target13)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target1)
t13<- data.frame(data2, comp = target14)

t13final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)


###target14
data1 <- filter(data, MatchAsgName %in% target14)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target1)

t14final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)
#final
finalfinal <- rbind(t1final,t2final,t3final,t4final,t5final, t6final,t7final,t8final,t9final,t10final,t11final, t12final, t13final,t14final)

write.csv(finalfinal, "cluster29finalfinal.csv")

#=============================================
#cluster13
target1 <- "BAKER HUGHES INCORPORATED"
target2 <- "EMERSON ELECTRIC CO"
target3 <- "HALLIBURTON ENERGY SERVICES INC"
target4 <- "SCHLUMBERGER TECHNOLOGY CORPORATION"
target5 <- "WHIRLPOOL CORPORATION"

###target1
data1 <- filter(data, MatchAsgName %in% target1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t1final<- rbind.data.frame(t1,t2,t3,t4)

####target2
data1 <- filter(data, MatchAsgName %in% target2)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))


t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t2final<- rbind.data.frame(t1,t2,t3,t4)

#### target3
data1 <- filter(data, MatchAsgName %in% target3)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t3final<- rbind.data.frame(t1,t2,t3,t4)

#### target4
data1 <- filter(data, MatchAsgName %in% target4)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target5)
t4final<- rbind.data.frame(t1,t2,t3,t4)

#### target5
data1 <- filter(data, MatchAsgName %in% target5)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target4)
t5final<- rbind.data.frame(t1,t2,t3,t4)

#final
finalfinal <- rbind(t1final,t2final,t3final,t4final,t5final)

write.csv(finalfinal, "cluster13finalfinal.csv")

#===========================================
#cluster32
target1 <- "ADVANCED MICRO DEVICES INC"
target2 <- "ALBANY INTERNATIONAL CORP"
target3 <- "CIRRUS LOGIC INC"
target4 <- "INFINEON TECHNOLOGIES AG"
target5 <- "KONINKLIJKE PHILIPS ELECTRONICS NV"
target6 <- "KYOCERA WIRELESS CORP"
target7 <- "NATIONAL SEMICONDUCTOR CORPORATION"
target8 <- "NCR CORPORATION"
target9 <- "NORTHROP GRUMMAN CORPORATION"
target10 <- "PITNEY BOWES INC"
target11 <- "SKYWORKS SOLUTIONS INC"
target12 <- "STMICROELECTRONICS SA"
target13 <- "XEROX CORPORATION"
target14 <- "XILINX INC"

###target1
data1 <- filter(data, MatchAsgName %in% target1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t1final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target2
data1 <- filter(data, MatchAsgName %in% target2)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t2final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target3
data1 <- filter(data, MatchAsgName %in% target3)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t3final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target4
data1 <- filter(data, MatchAsgName %in% target4)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t4final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)


###target5
data1 <- filter(data, MatchAsgName %in% target5)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target4)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t5final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target6
data1 <- filter(data, MatchAsgName %in% target6)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target3)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t6final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target7
data1 <- filter(data, MatchAsgName %in% target7)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target3)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t7final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target8
data1 <- filter(data, MatchAsgName %in% target8)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target3)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t8final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target9
data1 <- filter(data, MatchAsgName %in% target9)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target3)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t9final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target10
data1 <- filter(data, MatchAsgName %in% target10)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target3)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t10final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target11
data1 <- filter(data, MatchAsgName %in% target11)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target1)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t11final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target12
data1 <- filter(data, MatchAsgName %in% target12)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target1)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target14)

t12final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)

###target13
data1 <- filter(data, MatchAsgName %in% target13)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target1)
t13<- data.frame(data2, comp = target14)

t13final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)


###target14
data1 <- filter(data, MatchAsgName %in% target14)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)
t10<- data.frame(data2, comp = target11)
t11<- data.frame(data2, comp = target12)
t12<- data.frame(data2, comp = target13)
t13<- data.frame(data2, comp = target1)

t14final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13)
#final
finalfinal <- rbind(t1final,t2final,t3final,t4final,t5final, t6final,t7final,t8final,t9final,t10final,t11final, t12final, t13final,t14final)

write.csv(finalfinal, "cluster32finalfinal.csv")

#===========================================
#cluster38
target1 <- "ALCATEL"
target2 <- "AT&T BELL LABORATORIES"
target3 <- "CISCO TECHNOLOGY INC"
target4 <- "INTERDIGITAL TECHNOLOGY CORPORATION"
target5 <- "LUCENT TECHNOLOGIES INC"
target6 <- "MICROSOFT CORPORATION"
target7 <- "NOKIA CORPORATION"
target8 <- "NORTEL NETWORKS LIMITED"
target9 <- "QUALCOMM INCORPORATED"
target10 <- "TELEFONAKTIEBOLAGET LM ERICSSON (PUBL)"

###target1
data1 <- filter(data, MatchAsgName %in% target1)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target2)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t1final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target2
data1 <- filter(data, MatchAsgName %in% target2)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target3)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t2final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target3
data1 <- filter(data, MatchAsgName %in% target3)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t3final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target4
data1 <- filter(data, MatchAsgName %in% target4)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t4final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)


###target5
data1 <- filter(data, MatchAsgName %in% target5)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target3)
t4<- data.frame(data2, comp = target4)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t5final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target6
data1 <- filter(data, MatchAsgName %in% target6)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target3)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t6final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target7
data1 <- filter(data, MatchAsgName %in% target7)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target3)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t7final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target8
data1 <- filter(data, MatchAsgName %in% target8)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target3)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target10)

t8final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target9
data1 <- filter(data, MatchAsgName %in% target9)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target3)
t9<- data.frame(data2, comp = target10)

t9final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

###target10
data1 <- filter(data, MatchAsgName %in% target10)
data2<- subset(data1, select=c(xi,GrantDate, PatentNo, MatchAsgName))

t1<- data.frame(data2, comp = target1)
t2<- data.frame(data2, comp = target2)
t3<- data.frame(data2, comp = target4)
t4<- data.frame(data2, comp = target5)
t5<- data.frame(data2, comp = target6)
t6<- data.frame(data2, comp = target7)
t7<- data.frame(data2, comp = target8)
t8<- data.frame(data2, comp = target9)
t9<- data.frame(data2, comp = target3)

t10final<- rbind.data.frame(t1,t2,t3,t4,t5,t6,t7,t8,t9)

#final
finalfinal <- rbind(t1final,t2final,t3final,t4final,t5final, t6final,t7final,t8final,t9final,t10final)

write.csv(finalfinal, "cluster38finalfinal.csv")

