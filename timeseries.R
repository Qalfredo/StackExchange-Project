library(lubridate)
library(stackr)
library(dplyr)
library(ggplot2)

load(file = "proba.Rdata")
load(file = "analisis.Rdata")
load(file = "algebra.Rdata")
load(file = "calculus.Rdata")
load(file = "equations.Rdata")



proba <- select(proba, answer_count, creation_date)
analisis <- select(analisis, answer_count, creation_date)
algebra <- select(algebra, answer_count, creation_date)
calculus <- select(calculus, answer_count, creation_date)
equations <- select(equations, answer_count, creation_date)





analisis$creation_date <- month(analisis$creation_date)
proba$creation_date <- month(proba$creation_date)
algebra$creation_date <- month(algebra$creation_date)
calculus$creation_date <- month(calculus$creation_date)
equations$creation_date <- month(equations$creation_date)

meses.proba <- split(proba,proba$creation_date)
meses.analisis <- split(analisis,analisis$creation_date)
meses.algebra <- split(algebra,algebra$creation_date)
meses.calculus <- split(calculus,calculus$creation_date)
meses.equations <- split(equations,equations$creation_date)




set.seed(314)
t <-sample(1:874,50)
tt <- sample(1:535,50)
ttt <- sample(1:1141,100)
tttt <- sample(1:1059, 85)

meses.proba$`12` <- meses.proba$`12`[t,]
meses.proba$`11` <- meses.proba$`11`[tttt,]
meses.proba$`10` <- meses.proba$`10`[ttt,]
meses.proba$`9` <- meses.proba$`9`[tt,]


enero <- sum(meses.proba$`1`$answer_count)
 
febrero <- sum(meses.proba$`2`$answer_count)
marzo <- sum(meses.proba$`3`$answer_count)
abril <- sum(meses.proba$`4`$answer_count)
mayo <- sum(meses.proba$`5`$answer_count)
junio <- sum(meses.proba$`6`$answer_count)
julio <- sum(meses.proba$`7`$answer_count)
agosto <- sum(meses.proba$`8`$answer_count)
septiembre <- sum(meses.proba$`9`$answer_count, na.rm = T)
octubre <- sum(meses.proba$`10`$answer_count, na.rm = T)
noviembre <- sum(meses.proba$`11`$answer_count, na.rm = T)
diciembre <- sum(meses.proba$`12`$answer_count, na.rm = T)


serie <- c(enero,febrero,marzo, abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre)
serie2 <- 1:12
se <- cbind(serie2,serie)



 ggplot(as.data.frame(se), aes(x = serie2, y = serie)) + geom_line(stat = "identity", aes(group = 1) ) + scale_x_continuous(name = "Month",breaks = 1:12,labels = c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) + ylab("Respuestas") + ggtitle("Oscilación Anual Tag: Probability") 

######3
 
 
 
 set.seed(314)
 a <-sample(1:482,95)
 aa <- sample(1:1396,110)
 
 meses.algebra$`12` <- meses.algebra$`12`[aa,]
 meses.algebra$`11` <- meses.algebra$`11`[a,]
 
 
 enero2 <- sum(meses.algebra$`1`$answer_count)
 
 febrero2 <- sum(meses.algebra$`2`$answer_count)
 marzo2 <- sum(meses.algebra$`3`$answer_count)
 abril2 <- sum(meses.algebra$`4`$answer_count)
 mayo2 <- sum(meses.algebra$`5`$answer_count)
 junio2 <- sum(meses.algebra$`6`$answer_count)
 julio2 <- sum(meses.algebra$`7`$answer_count)
 agosto2 <- sum(meses.algebra$`8`$answer_count)
 septiembre2 <- sum(meses.algebra$`9`$answer_count)
 octubre2 <- sum(meses.algebra$`10`$answer_count)
 noviembre2 <- sum(meses.algebra$`11`$answer_count,na.rm=T)
 diciembre2 <- sum(meses.algebra$`12`$answer_count,na.rm=T)
 
 
 serie.algebra <- c(enero2,febrero2,marzo2, abril2,mayo2,junio2,julio2,agosto2,septiembre2,octubre2,noviembre2,diciembre2)
 serie2 <- 1:12
 se2 <- cbind(serie2,serie.algebra)
 
 
 
 ggplot(as.data.frame(se2), aes(x = serie2, y = serie.algebra)) + geom_line(stat = "identity", aes(group = 1) ) + scale_x_continuous(name = "Month",breaks = 1:12,labels = c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) + ylab("Respuestas") + ggtitle("Oscilación Anual Tag: Algebra") 

 
 #####

 
 set.seed(314)
 an <-sample(1:1852,120)
 
 
 meses.analisis$`12` <- meses.analisis$`12`[an,]

 
 
 enero3 <- sum(meses.analisis$`1`$answer_count)
 
 febrero3 <- sum(meses.analisis$`2`$answer_count)
 marzo3 <- sum(meses.analisis$`3`$answer_count)
 abril3 <- sum(meses.analisis$`4`$answer_count)
 mayo3 <- sum(meses.analisis$`5`$answer_count)
 junio3 <- sum(meses.analisis$`6`$answer_count)
 julio3 <- sum(meses.analisis$`7`$answer_count)
 agosto3 <- sum(meses.analisis$`8`$answer_count)
 septiembre3 <- sum(meses.analisis$`9`$answer_count)
 octubre3 <- sum(meses.analisis$`10`$answer_count)
 noviembre3 <- sum(meses.analisis$`11`$answer_count)
 diciembre3 <- sum(meses.analisis$`12`$answer_count, na.rm = T)
 
 
 serie.analisis <- c(enero3,febrero3,marzo3, abril3,mayo3,junio3,julio3,agosto3,septiembre3,octubre3,noviembre3,diciembre3)
 serie2 <- 1:12
 se3 <- cbind(serie2,serie.analisis)
 
 
 
 ggplot(as.data.frame(se3), aes(x = serie2, y = serie.analisis)) + geom_line(stat = "identity", aes(group = 1) ) + scale_x_continuous(name = "Month",breaks = 1:12,labels = c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) + ylab("Respuestas")  +ggtitle("Oscilación Anual Tag: Real-Analysis")
 
 
 ####
 
 set.seed(314)
 an <-sample(1:2115,225)
 
 
 meses.calculus$`12` <- meses.calculus$`12`[an,]
 
 
 
 enero4 <- sum(meses.calculus$`1`$answer_count)
 
 febrero4 <- sum(meses.calculus$`2`$answer_count)
 marzo4 <- sum(meses.calculus$`3`$answer_count)
 abril4 <- sum(meses.calculus$`4`$answer_count)
 mayo4 <- sum(meses.calculus$`5`$answer_count)
 junio4 <- sum(meses.calculus$`6`$answer_count)
 julio4 <- sum(meses.calculus$`7`$answer_count)
 agosto4 <- sum(meses.calculus$`8`$answer_count)
 septiembre4 <- sum(meses.calculus$`9`$answer_count)
 octubre4 <- sum(meses.calculus$`10`$answer_count)
 noviembre4 <- sum(meses.calculus$`11`$answer_count)
 diciembre4 <- sum(meses.calculus$`12`$answer_count, na.rm = T)
 
 
 serie.calculus <- c(enero4,febrero4,marzo4, abril4,mayo4,junio4,julio4,agosto4,septiembre4,octubre4,noviembre4,diciembre4)
 serie2 <- 1:12
 se4 <- cbind(serie2,serie.calculus)
 
 
 
 ggplot(as.data.frame(se4), aes(x = serie2, y = serie.calculus)) + geom_line(stat = "identity", aes(group = 1) ) + scale_x_continuous(name = "Month",breaks = 1:12,labels = c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) + ylab("Respuestas") + ggtitle("Oscilación Anual Tag: Calculus")  
 
 
 ####
 
 
 
 