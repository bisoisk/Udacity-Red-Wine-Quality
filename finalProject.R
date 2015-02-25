#final project
# first column
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

redwine<-read.csv('wineQualityReds.csv')
names(redwine)



#pair
ggpairs(rw,params=c(shape=I('.'),outlier.shap=I('.')))
svg("matrix.svg",height=8,width=8)
ggpairs(data) 
dev.off()
#there are some interesting correlation between component but we are not interested in that
# summary of all variables

#1 hist of quality
ggplot(aes(x = quality), data = redwine)+geom_histogram()



#2
#scatter plot
cor.test(rw$quality, rw$alcohol,method='pearson') 
ggplot(aes(x=quality,y=sulphates),data=redwine)+
  geom_point()
# a little over plot

#3 add alpha
ggplot(aes(x=quality,y=sulphates),data=redwine)+
  geom_point(color='#F79420',alpha=1/2)
 



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

# 7 multivarialbe
ggplot(aes(x = quality, y = alcohol), data = redwine) +
  geom_point(aes(color =  sulphates))
# how to make scale more visible
  
# regression

#5 box, how to plot more
ggplot(aes(x=quality,y=alcohol),data=redwine)+
  geom_boxplot()
  


ggplot(aes(x=www_likes_received,y=likes_received),data=pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+ #zoom in percentile 
  geom_smooth(method='lm',color='red')
cor.test(pf$www_likes_received, pf$likes_received,method='pearson')    


#5 box
# use percantage to show that
ggplot(aes(x = alcohol), data = rw)+ facet_wrap( ~ quality) +geom_histogram()
#ggplot(aes(x = price), data = diamonds) + facet_wrap( ~ color) + geom_histogram(aes(fill = cut)) +scale_x_log10()+ scale_y_continuous(limits = c(0,600)) + scale_fill_brewer(type = 'qual')

# line plot
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

