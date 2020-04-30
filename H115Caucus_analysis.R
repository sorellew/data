#SWG Caucus/ PAC Networks
##binding centrality and funding measurements with member covariates

setwd("~/Documents/Working papers/Caucus Paper/Data")
H115_degrees <- read.csv("H115_DegreeMeasures2.csv")
head(H115_degrees)
allnodes <-  read.csv("H115_membersparty.csv")
head(allnodes)
paccontributions <- read.csv("H115_pactotalcont.csv", header = T)
pacfundsrecieved <- read.csv("H115_pacfundsrecieved.csv", header = T)

library(dplyr)
allnodesdegree<-allnodes%>%left_join(H115_degrees)
allnodesdegree <-allnodesdegree%>%left_join(paccontributions)
allnodesdegree <-allnodesdegree%>%left_join(pacfundsrecieved)
head(allnodesdegree)
allnodesdegree$totalcont%>%is.na(allnodesdegree)]=0
head(allnodesdegree)
#reeegggression time

#creating factor variables
allnodesdegree$higheroffice <- factor(allnodesdegree$higheroffice)
allnodesdegree$defeatedgen <- factor(allnodesdegree$defeatedgen)
allnodesdegree$defeatedprim <- factor(allnodesdegree$defeatedprim)
allnodesdegree$pvi_tossup <- factor(allnodesdegree$pvi_tossup)
allnodesdegree$pvi_lean <- factor(allnodesdegree$pvi_lean)
allnodesdegree$pvi_likely <- factor(allnodesdegree$pvi_likely)
allnodesdegree$GOPleadership <- factor(allnodesdegree$GOPleadership)
allnodesdegree$GOPtopleader <- factor(allnodesdegree$GOPtopleader)
allnodesdegree$DEMleadership <- factor(allnodesdegree$DEMleadership)
allnodesdegree$DEMtopleader <- factor(allnodesdegree$DEMtopleader)
allnodesdegree$CommitteeChair <- factor(allnodesdegree$CommitteeChair)
allnodesdegree$TopCommittee <- factor(allnodesdegree$TopCommittee)
allnodesdegree$totalcont <-as.numeric(allnodesdegree$totalcont)
allnodesdegree$amtrecieved <-as.numeric(allnodesdegree$amtrecieved)

m1 <- lm(degreeout~DEMtopleader+GOPtopleader+CommitteeChair, data = allnodesdegree)
summary(m1)
plogis(coef(m1)[c(3)])

m2<- lm(degreein~pvi_tossup+pvi_lean+pvi_likely+higheroffice, data = allnodesdegree)
summary(m2)
library(stargazer)
stargazer(m1, m2, type = "text")
#these models are much more signficant. money AMOUNT doesn't answer our question anyways.

m3 <- lm(totalcont~GOPleadership+DEMleadership+CommitteeChair, data=allnodesdegree)
summary(m3)
#more likely for dem leaders to be the source of pac funding. negative relationship for gop leadership
#indicating money is coming from another source (non leadership)
plogis(coef(m3)[c(2, 3)])

m4 <- lm(amtrecieved~pvi_lean, data = allnodesdegree)
summary(m3)
stargazer(m3, m4, type = "text")
