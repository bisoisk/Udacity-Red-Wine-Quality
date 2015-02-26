#lesson 4
names(pf)
qplot(x=age,y=friend_count,data=pf)
# ggplot syntax
ggplot(aes(x=age,y=friend_count),data=pf)+
  geom_point(alpha=1/20, position = position_jitter(h=0))+
#  geom_jitter(alpha=1/20)+
  xlim(13,90)+
  coord_trans(y = 'sqrt')
#add more layer
#set transperiencey
#how to summarize, the mean and medium, not just plot everything

#instal dplyr
install.packages('dplyr')
library(dplyr)
age_groups<-group_by(pf,age)
pf.fc_by_age<-summarise(age_groups,
          friend_count_mean=mean(friend_count),
          friend_count_median=median(friend_count),
          n = n()) #n is the number of data
pf.fc_by_age<-arrange(pf.fc_by_age,age) #rearrange the data by age
head(pf.fc_by_age) #print out the first 6 lines

# or use the alternative method
pf.fc_by_age<-pf %.%  #change comment
  group_by(age) %.%
  summarise(friend_count_mean=mean(friend_count),
            friend_count_median=median(friend_count),
            n = n()) %.%
  arrange(age)
head(pf.fc_by_age,20)
ggplot(aes(x=age,y=friend_count_median),data=pf.fc_by_age)+geom_line()
  
# overlay
ggplot(aes(x=age,y=friend_count),data=pf)+
  xlim(13,90)+
  geom_point(alpha=1/20, 
             position = position_jitter(h=0),
             color = 'orange')+
  coord_trans(y = 'sqrt')+
  geom_line(stat='summary',fun.y = mean)+ # summary line
  geom_line(stat='summary',fun.y = quantile,probs = .1,
            linetype =2, color='blue')+
  geom_line(stat='summary',fun.y = quantile,probs = .9,
            linetype =2, color='blue')+
  geom_line(stat='summary',fun.y=quantile, probs = .5,
             color='blue')

#plot quantrio of the data
# correlation 
cor.test(pf$age, pf$friend_count,method='pearson')    
# subset
with(subset(pf,age<=70),cor.test(age,friend_count))
# 0.3 meaning but small  
  
ggplot(aes(x=www_likes_received,y=likes_received),data=pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received,0.95))+
  ylim(0,quantile(pf$likes_received,0.95))+ #zoom in percentile 
  geom_smooth(method='lm',color='red')
cor.test(pf$www_likes_received, pf$likes_received,method='pearson')    

#install package
install.packages('alr3')
library(alr3)
data(Mitchell)
?Mitchell
ggplot(aes(x=Month,y=Temp),data=Mitchell)+geom_point()+
  scale_x_discrete(breaks=seq(0,203,12))
# get perspactive of the data
cor.test(Mitchell$Month, Mitchell$Temp,method='pearson')    

#age with month
pf$age_with_months<-pf$age+(12-pf$dob_month)/12 #$data frame
ggplot(aes(x=age_with_months,y=friend_count),data=pf)+geom_line()
# group age by month
age_groups<-group_by(pf,age_with_months)
pf.fc_by_age_months <-summarise(age_groups,
                                friend_count_mean=mean(friend_count),
                                friend_count_median=median(friend_count),
                                n = n()) #n is the number of data

library(dplyr)
pf.fc_by_age_months<-pf %>%
  group_by(age_with_months)%>%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count),
            n=n())%>%
  arrage(age_with_months)
head(pf.fc_by_age_months) # not quit work
# alternative
age_with_months_groups<-group_by(pf,age_with_months)
pf.fc_by_age_months2<-summarise(age_with_months_groups,
                                friend_count_mean=mean(friend_count),
                                friend_count_median=median(friend_count),
                                n=n())
pf.fc_by_age_months2<-arrange(pf.fc_by_age_months2,age_with_months)
head(pf,fc_by_age_months2)


ggplot(aes(x=x,y=price),data=diamonds)+geom_point()
cor.test(diamonds$x, diamonds$price,method='pearson')    
cor.test(diamonds$y, diamonds$price,method='pearson')    
cor.test(diamonds$z, diamonds$price,method='pearson')    

ggplot(aes(x=depth,y=price),data=diamonds)+geom_point(alpha=1/20)+
  xlim(50,70)
  scale_x_discrete(breaks=seq(50,70,2))
cor.test(diamonds$depth, diamonds$price,method='pearson')    

ggplot(aes(x=price,y=carat),data=diamonds)+geom_point(alpha=1/20)+
  xlim(0, quantile(diamonds$price, 0.99))+
  ylim(0, quantile(diamonds$carat, 0.99))
diamonds.volume<-diamonds$x*diamonds$y*diamonds$z
ggplot(aes(x=price,y=volume),data=diamonds)+geom_point(alpha=1/20)+
  ylim(0, quantile(diamonds$volume, 0.90))

diamonds$volume <- diamonds$x*diamonds$y*diamonds$z
with(subset(diamonds, diamonds$volume !=0 & diamonds$volume <800), cor.test(price, volume, method = 'pearson'))
# cool works

#chian operator
library(dplyr)
diamonds.diamondsByClarity<-diamonds %>%
  group_by(clarity)%>%
  summarise(price_mean = mean(price),
            price_median = median(price),
            price_max = max(price),
            price_min = min(price),
            n=n())%>%
  arrage(clarity)

library(dplyr)
diamonds.diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(as.numeric(price)),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)
head(diamonds.diamondsByClarity) #works good
  
library(gridExtra)
p1 <- ggplot(aes(x= clarity, y=mean_price),data= diamonds_mp_by_clarity)+
  geom_point()
p2<- ggplot(aes(x= color, y=mean_price),data= diamonds_mp_by_color)+
  geom_point()
grid.arrange(p1,p2,ncol=1)



