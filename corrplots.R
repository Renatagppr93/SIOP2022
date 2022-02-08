
data <- read.csv("SIOPcleaned.csv")      ## from "datacleaning.R" script

## Study 2 DVs

data$item387      <- 7- data$item387           ## only reflected engagement item

data$absorption   <- rowMeans(data[c(380:386)], na.rm=TRUE)
data$vigor        <- rowMeans(data[c(387:392)], na.rm=TRUE) 
data$dedication   <- rowMeans(data[c(393:399)], na.rm=TRUE) 

data$cognitive    <- rowMeans(data[c(380:382, 387:388, 393:395)], na.rm=TRUE)
data$affective    <- rowMeans(data[c(383:384, 389:390, 396:397)], na.rm=TRUE) 
data$behavioral   <- rowMeans(data[c(385:386, 391:392, 398:399)], na.rm=TRUE) 

######################################################

data$burnout      <- rowMeans(data[c(373:376)], na.rm=TRUE)
data$stress       <- rowMeans(data[c(377:379)], na.rm=TRUE)
data$engagement   <- rowMeans(data[c(380:399)], na.rm=TRUE)        ## added 10/12 because analyses too wonky with 3 subscales

alpha.abs <- psych::alpha(data[c(380:386)])
alpha.vig <- psych::alpha(data[c(387:392)])
alpha.ded <- psych::alpha(data[c(393:399)])
alpha.eng <- psych::alpha(data[c(380:399)])

alpha.bur <- psych::alpha(data[c(373:376)])
alpha.str <- psych::alpha(data[c(377:379)])


## O*NET categories

data$onet.resource.ii <- rowMeans(data[c(162:166)], na.rm=TRUE)
data$onet.resource.mp <- rowMeans(data[c(167:176)], na.rm=TRUE)
data$onet.resource.wo <- rowMeans(data[c(177:185)], na.rm=TRUE)
data$onet.resource.io <- rowMeans(data[c(186:202)], na.rm=TRUE)
data$onet.resource.ir <- rowMeans(data[c(149:161)], na.rm=TRUE)
data$onet.resource.pc <- rowMeans(data[c(128:144,146:148)], na.rm=TRUE)
data$onet.resource.sc <- rowMeans(data[c(118:127)], na.rm=TRUE)

overall.resource <- rowMeans(data[c(118:144,147:202)], na.rm=TRUE)

data$onet.hindrance.ii <- rowMeans(data[c(246:247,249:252)], na.rm=TRUE)
data$onet.hindrance.mp <- rowMeans(data[c(252:261)], na.rm=TRUE)
data$onet.hindrance.wo <- rowMeans(data[c(262:270)], na.rm=TRUE)
data$onet.hindrance.io <- rowMeans(data[c(271:287)], na.rm=TRUE)
data$onet.hindrance.ir <- rowMeans(data[c(233:245)], na.rm=TRUE)
data$onet.hindrance.pc <- rowMeans(data[c(213:232)], na.rm=TRUE)
data$onet.hindrance.sc <- rowMeans(data[c(203:212)], na.rm=TRUE)

overall.hindrance <- rowMeans(data[c(203:247,249:287)], na.rm=TRUE)

data$onet.challenge.ii <- rowMeans(data[c(332:336)], na.rm=TRUE)
data$onet.challenge.mp <- rowMeans(data[c(337:346)], na.rm=TRUE)
data$onet.challenge.wo <- rowMeans(data[c(347:355)], na.rm=TRUE)
data$onet.challenge.io <- rowMeans(data[c(356:372)], na.rm=TRUE)
data$onet.challenge.ir <- rowMeans(data[c(319:331)], na.rm=TRUE)
data$onet.challenge.pc <- rowMeans(data[c(298:307,309:318)], na.rm=TRUE)
data$onet.challenge.sc <- rowMeans(data[c(288:297)], na.rm=TRUE)

overall.challenge <- rowMeans(data[c(288:307,309:372)], na.rm=TRUE)



library(tidyverse)  
library(corrr)

## Resource:

mydata <- data[,c(413, 411:412, 414:420)]

res.cor <- correlate(mydata)

res.cor %>% fashion()

res.cor %>% rearrange(method="MDS", absolute=FALSE) %>% shave(upper=FALSE) %>% rplot()

res.cor %>% network_plot(min_cor = .4)

library(corrplot)

new <- cor(data[,c(413, 411:412, 414:420)], use="complete.obs")
corrplot(new, type="upper", order="hclust", tl.col="black", tl.srt=45, diag=TRUE)

## Challenge:


mydata <- data[,c(413, 411:412, 428:434)]

res.cor <- correlate(mydata)

res.cor %>% rearrange(method="MDS", absolute=FALSE) %>% shave(upper=FALSE) %>% rplot()

res.cor %>% network_plot(min_cor = .4)

new2 <- cor(data[,c(413, 411:412, 428:434)], use="complete.obs")
corrplot(new2, type="upper", order="hclust", tl.col="black", tl.srt=45, diag=TRUE)

## Hindrance:


mydata <- data[,c(413, 411:412, 421:427)]

res.cor <- correlate(mydata)

res.cor %>% rearrange(method="MDS", absolute=FALSE) %>% shave(upper=FALSE) %>% rplot()

res.cor %>% network_plot(min_cor = .4)

new3 <- cor(data[,c(413, 411:412, 421:427)], use="complete.obs")
corrplot(new3, type="upper", order="hclust", tl.col="black", tl.srt=45, diag=TRUE)

