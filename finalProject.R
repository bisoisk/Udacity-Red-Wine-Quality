#final project
getwd()
setwd("~/Documents/Github/Red-wine-quality")
# first column don't have much infor
# 1pair
# 2 histogram of quality 
# 3 hist of alcoho
# 4corelation
# 5min median of 
# 6signal variable scater
# 7 find mean media plot agian
# 8 jitter
# 9 multiple variable color/ alchoh  and acid
# 10 other 3 gretter than 0.2  violate acid
# 11 suphates
# 12 citic acid
# 11 hist on alcolho, normalized 
# 12 hist of alcoho normalize same graph
# 13 box plot
# 14 smooth
# 15
# 16
# 17
# 18
# 19
# 20

library(ggplot2)
library(memsic)

redwine<-read.csv('wineQualityReds.csv')
names(redwine)

#1 pair
ggpairs(rw,params=c(shape=I('.'),outlier.shap=I('.')))
svg("matrix.svg",height=8,width=8)
ggpairs(data) 
dev.off()
#there are some interesting correlation between component but we are not interested in that
# summary of all variables

#2 hist of quality
ggplot(aes(x = quality), data = redwine)+ geom_bar()


#3
#scatter plot
cor.test(rw$quality, rw$alcohol,method='pearson') 
ggplot(aes(x=quality,y=sulphates),data=redwine)+
  geom_point()
# a little over plot

#4 add mean and variance
ggplot(aes(x=quality,y=alcohol),data=redwine)+
  geom_point(color='#F79420',alpha=1/4)+
  geom_line(stat='summary',fun.y = mean)+
  geom_line(stat='summary',fun.y = quantile,probs = .5,
            linetype =2, color='blue')


#5 sulphates
ggplot(aes(x=quality,y=sulphates),data=redwine)+
  geom_point(color='#F79420',alpha=1/4)+
  geom_line(stat='summary',fun.y = mean)+
  geom_line(stat='summary',fun.y = quantile,probs = .5,
            linetype =2, color='blue')

#6 volatile.acidity
ggplot(aes(x=quality,y=volatile.acidity),data=redwine)+
  geom_point(color='#F79420',alpha=1/4)+
  geom_line(stat='summary',fun.y = mean)+
  geom_line(stat='summary',fun.y = quantile,probs = .5,
            linetype =2, color='blue')


  
# regression

#5 box, how to plot more
ggplot(aes(x=quality,y=alcohol),data=redwine)+
  geom_boxplot()
  

#5 box
# use percantage to show that
ggplot(aes(x = alcohol), data = redwine)+ facet_wrap( ~ quality) +geom_histogram(aes(y=..count../sum(..count..)))
#ggplot(aes(x = price), data = diamonds) + facet_wrap( ~ color) + geom_histogram(aes(fill = cut)) +scale_x_log10()+ scale_y_continuous(limits = c(0,600)) + scale_fill_brewer(type = 'qual')
ggplot(aes(x = alcohol), data = redwine)+
  geom_histogram(aes(y=..count../sum(..count..)))+ 
  facet_wrap( ~ quality) 


# 8  line plot
library(dplyr)
redwine.median_by_quality<-redwine %>%
  #  filter(!is.na(gender))%>%
  group_by(quality)%>%
  summarise(median_sulphates = median(sulphates),
            median_volatile.acidity = median(volatile.acidity),
            median_alcohol = median(alcohol),
            median_citric.acid = median(citric.acid),
            n=n()) %>%
  ungroup() %>%
  arrange(quality)

ggplot(aes(x=quality),data=redwine.median_by_quality)+
  geom_line(aes(y=median_volatile.acidity/max(median_volatile.acidity)))+
  geom_line(aes(y=median_alcohol/max(median_alcohol)))+
  geom_line(aes(y=median_sulphates/max(median_sulphates)))+
  geom_line(aes(y=median_citric.acid/max(median_citric.acid)))


cor(rw$density, rw$quality)

# turn to categroical 
redwine$grade<-cut(redwine$quality, c(2.5,4.5,6.5,8.5),labels=c('low','mid','high'))

# box plot
ggplot(redwine, aes(x=grade, y=alcohol)) + geom_boxplot()
ggplot(redwine, aes(x=grade, y=volatile.acidity)) + geom_boxplot()
ggplot(redwine, aes(x=grade, y=citric.acid)) + geom_boxplot()
ggplot(redwine, aes(x=grade, y=sulphates)) + geom_boxplot()

# 7 multivarialbe
ggplot(aes(x = volatile.acidity, y = citric.acid), data = redwine) +
  geom_point(aes(color =  grade))

#8
ggplot(aes(x = volatile.acidity, y = alcohol), data = redwine) +
  geom_point(aes(color =  grade))

#9
# Overlaid histograms with means
ggplot(redwine, aes(x=alcohol, fill=grade)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Overlaid histograms with means
ggplot(redwine, aes(x=alcohol, fill=grade)) +
  geom_histogram(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")

# Overlaid histograms with means
ggplot(redwine, aes(x=alcohol, fill=grade)) +
  geom_density(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")

ggplot(redwine, aes(x=volatile.acidity, fill=grade)) +
  geom_density(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")

ggplot(redwine, aes(x=residual.sugar, fill=grade)) +
  geom_density(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")

ggplot(redwine, aes(x=sulphates, fill=grade)) +
geom_density(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")


# top wines
# turn to categroical 
redwine$grade_number<-cut(redwine$quality, c(2.5,3.5,4.5,5.5,6.5,7.5,8.5),labels=c('3','4','5','6','7','8'))

# subset 7 and 8 wind
top_wine<-subset(redwine,grade_number=='7' | grade_number=='8' )

ggplot(top_wine, aes(x=volatile.acidity, fill=grade_number)) +
  geom_density(aes(y=..density..),binwidth=.5, alpha=.5, position="identity")

# 8  line plot
library(dplyr)
redwine.median_by_quality<-redwine %>%
  #  filter(!is.na(gender))%>%
  group_by(quality)%>%
  summarise(median_sulphates = median(sulphates),
            median_volatile.acidity = median(volatile.acidity),
            median_alcohol = median(alcohol),
            median_citric.acid = median(citric.acid),
            n=n()) %>%
  ungroup() %>%
  arrange(quality)
stat='summary',fun.y = mean

# Bars with other dataset; fill depends on cond2
ggplot(top_wine, aes(x=grade_number) )+ 
geom_bar(aes(y=alcohol),stat='summary',fun.y = mean)+   # fill depends on cond2
  geom_bar(aes(y=sulphates),stat='summary',fun.y = mean)
colour="black",    # Black outline for all
position=position_dodge()) # Put bars side-by-side instead of stacked

# regression
m1<-lm(quality ~ volatile.acidity,data=redwine)
m2<-update(m1,~. + alcohol)
mtable(m1,m2)

# reference
# http://en.wikipedia.org/wiki/Wine_fault
