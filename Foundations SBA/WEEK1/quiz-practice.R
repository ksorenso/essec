
# Foundations of strategic business analytics
# Week 1: Introduction to Strategic Business Analytics
# Practice Quizz Module 1

setwd("D:/Coursera/Strategic Business Analytics/Introduction/Week 1")
require(data.table)
require(tidyr)
require(ggplot2)


dt <-  fread('_eba2c079135882131db3690701bc9c97_PASTAPURCHASE_EDITED.csv',
                header = TRUE, 
                nrows=-1, 
                stringsAsFactors = FALSE, 
                na.strings='')
str(dt)

# 1. What is the correct mean and standard deviation of the quantity of pasta purchased by time unit by household?

dt[,.(mean_pasta = mean(PASTA), std_pasta=sd(PASTA))]


# 2. In which area are located (i) the poorest household and (ii) the wealthiest household?

dt[INCOME == min(dt$INCOME)]$AREA %>% unique()
dt[INCOME==max(dt$INCOME)]$AREA %>% unique()

# 3. What is the maximum pasta quantity a household has bought over the whole time period? 
#    (Sum the quantity of pasta by household over time and indicate the maximum)

dt[,.(hh_pasta_qty = sum(PASTA)), by='HHID'][order(-hh_pasta_qty)] %>% head(1)

# 4. What is the average income of households living in area 4?

dt[AREA==4 & TIME==1, mean(INCOME)]


# 5. How many households live in area 2, earn more than 20k, and have purchased more than 30 units of pasta over the whole time period?

dt[AREA==2 & INCOME > 20000, .(PastaTotal = sum(PASTA)), by=HHID][PastaTotal>30][,.N]


# 6. What is the correlation between the purchases of pasta and the exposures?

ggplot(data=dt,aes(y=PASTA,x=EXPOS)) + geom_point() + geom_jitter()
cor(as.matrix(dt[,.(PASTA,EXPOS)]))

# 7. Which of the following graphs reports the correct histogram by household of the total purchase of pasta made by the household over the whole period? 
#    (Sum the purchases by household and make a histogram.)

ggplot(dt[,.(PastaTotal=sum(PASTA)),by=HHID], aes(PastaTotal)) + geom_histogram(binwidth=5.0)


# 8. Which of the following graphs reports the correct time series of the overall total purchase of pasta?
#    (Sum the purchases by time units and plot the quantity by time unit.)

ggplot(dt[,.(PastaTotal=sum(PASTA)),by=TIME],aes(x=TIME,y=PastaTotal)) + geom_point()
