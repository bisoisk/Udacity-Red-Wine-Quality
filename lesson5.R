ggplot(aes(x=gender,y=age),
       data=subset(pf,!is.na(gender)))+geom_boxplot()+
  stat_summary(fun.y=mean,geom='point',shape=4)

ggplot(aes(x=age,y=friend_count),
       data=subset(pf,!is.na(gender)))+
  geom_line(aes(color=gender),stat='summary',fun.y=median)

library(dplyr)
pf.fc_by_age_gender<-pf %.%
  filter(!is.na(gender))%.%
  group_by(age,gender)%.%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(as.numeric(friend_count)),
            n=n()) %.%
  ungroup() %.%
  arrange(age)

ggplot(aes(x=age,y=median_friend_count),
       data=pf.fc_by_age_gender)+
  geom_line(aes(color=gender))

#long data shap -> wide data shape

install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide<-dcast(pf.fc_by_age_gender,
                                age~gender,
                                value.var='median_friend_count')
head(pf.fc_by_age_gender.wide)
pf.fc_by_age_gender.wide$ratio_fm=pf.fc_by_age_gender.wide$female/pf.fc_by_age_gender.wide$male

ggplot(aes(x=age,y=ratio_fm),
       data=pf.fc_by_age_gender.wide)+
       geom_line()
#or
ggplot(aes(x=age,y= female / male),
       data=pf.fc_by_age_gender.wide)+
  geom_line()+
  geom_hline(yintercept = 1, alpha =0.3, linetype=2)

pf$year_joined<-floor(2014- pf$tenure/365)

summary(pf$year_joined)
table(pf$year_joined)

pf$year_joined.bucket<-cut(pf$year_joined,
                           c(2004,2009,2011,2012,2014))
table(pf$year_joined.bucket, useNA='ifany')

ggplot(aes(x=age,y=friend_count),
       data=subset(pf,!is.na(gender)))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=mean)+
  geom_line(stat = 'summary',fun.y=mean,linestype=1)
#plot median just change fun.y=median

with(subset(pf,tenure>=1),summary(friend_count / tenure))
     #subset

ggplot(aes(x=tenure,y=friendships_initiated / tenure),
       data=subset(pf, tenure>=1 ))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=median)
#plot median just change fun.y=median
#bin xaxis to reduce noise
ggplot(aes(x=30* round(tenure/30),y=friendships_initiated / tenure),
       data=subset(pf, tenure>=1 ))+
  geom_line(aes(color=year_joined.bucket),stat='summary',fun.y=median)


#smooth
ggplot(aes(x= tenure ,y=friendships_initiated / tenure),
       data=subset(pf, tenure>=1 ))+
  geom_smooth(aes(color=year_joined.bucket))

# the yogurt dataset
yo<-read.csv('yogurt.csv')
str(yo)
# change it from int to a factor
yo$id<-factor(yo$id)
str(yo)
qplot(yo$price,binwidth = 10)
unique(yo$price) # different prince

yo<- transform(yo, all.purchases=strawberry+blueberry+pina.colada
               +plain+mixed.berry)
summary(yo$all.purchases)

ggplot(aes(x= time ,y=price),data=yo)+
  geom_jitter(alpha=1/4, shope=21, fill=I('#F79420'))

# set seed to make reproducable
set.seed(4321)
sample.ids<-sample(levels(yo$id),16)
ggplot(aes(x=time,y=price),
       data = subset(yo,id %in% sample.ids))+
       facet_wrap(~id)+
       geom_line()+
       geom_point(aes(size=all.purchases),pch=1)

#create number of scatter plot automaticly
# scatter plot matrices
install.packages(GGally)
library(GGally)
theme_set(theme_minimal(20))
set.seed(1836)
pf_subset<-pf[,c(2:15)]
names(pf_subset)
ggparis(pf_subset[sample.int(nrow(pf_subset),1000)])