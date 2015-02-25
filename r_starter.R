setwd('~/Downloads')  #set directory
statesInfo<- read.csv('stateData.csv') #read data
data(mtcars)
str(mtcar)
subset(mtcars,mpg >=30|hp<60) #make a subset of data

reddit<-read.csv('reddit.csv')
table(reddit$employment.status)  # summary data

str(reddit)
levels(reddit$age.range) #different values of ages
getwd() #current directionary
pf<-read.csv('pseudo_facebook.tsv',sep='\t') #load data
names(pf) #types of data
install.packages('ggplot2') #install
library(ggplot2) 
qplot(x=dob_day,data=pf)+ #histo gram
  scale_x_discrete(breaks=1:31)+#discrite histogram
    facet_wrap(~dob_month, ncol=3) #3 column histogram
  # facet_grid(vertic)
qplot(x=friend_count,data=pf,xlim=c(0,1000)) #
qplot(x=friend_count,data=pf)+
  scale_x_continuous(limits=c(0,1000))#layer
```{r}
qplot(x=friend_count,data=pf,binwidth=25)+
  scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,50))+#layer
facet_wrap(~gender) #another later
#```
table(pf$gender) #number of female and male
by(pf$friend_count,pf$gender,summary)
qplot(x=tenure, data=pf,binwidth =30, color = I('black'),fill=I('#099DD9'))
  #ajust bin and color #measure in days
qplot(x=tenure/365, data=pf,
      xlab ='Number of years using Facebook',
      ylab = 'Number of users in sample',
      binwidth =0.25, color = I('black'),fill=I('#F79420'))+
  scale_x_continuous(breaks=seq(1,7,1),limits=c(0,7))
#ajust tenure in years

#user age
qplot(x=age, data=pf,binwidth =1, color = I('black'),fill=I('#099DD9'))+
  scale_x_discrete(breaks=seq(0,113,5))
  summary(pf$age)#summary on age
#overdispersion change it to normal distribuiton
summary(log10(pf$friend_count+1))
summary(sqrt(pf$friend_count+1))
qplot(x=friend_coun, data=pf)
qplot(x=log10(pf$friend_count+1), data=pf, color = I('black'),fill=I('#099DD9'))
qplot(x=sqrt(pf$friend_count+1), data=pf, color = I('black'),fill=I('#099DD9'))
#instal library
install.packages('gridExtra') 
library(gridExtra)

p1<-qplot(x=friend_count, data=pf,color = I('black'),fill=I('#099DD9'))
p2<-qplot(x=log10(pf$friend_count+1), data=pf, color = I('black'),fill=I('#099DD9'))
p3<-qplot(x=sqrt(pf$friend_count+1), data=pf, color = I('black'),fill=I('#099DD9'))
grid.arrange(p1,p2,p3,ncol=1)

# add a mask of log10
qplot(x=friend_count,data=pf)+
  scale_x_log10()
#frequency polygone
qplot(x=friend_count,y=..count../sum(..count..),data=subset(pf,!is.na(gender)),
      binwidth = 10, geom ='freqpoly',color=gender)+
  scale_x_continuous(lim=c(0,1000),breaks=seq(0,1000,50))
#gender is assign to a color

#frequncy poly www likes 
qplot(x=www_likes,y=..count../sum(..count..),data=subset(pf,!is.na(gender)),
       geom ='freqpoly',color=gender)+
  scale_x_continuous()+
  scale_x_log10()
#is.na(gender) remove gender that is na
#summary
by(pf$www_likes,pf$gender,sum)

#box plots
qplot(x=gender, y=friend_count,
      data=subset(pf,!is.na(gender)),
      geom='boxplot')+
 scale_y_continuous(lim=c(0,100),breaks=seq(0,100,25)) #ajust the psamsis
#or
qplot(x=gender, y=friend_count,
      data=subset(pf,!is.na(gender)),
      geom='boxplot')+
  coord_cartesian(ylim=c(0,250)) 
# box plot can understand the distribuiton and the outlays
by(pf$friendships_initiated,pf$gender,summary)
# a lot of 0 data, ture of false. 
summary(pf$mobile_likes>0) 
mobile_check_in<-NA
pf$mobile_check_in<-ifelse(pf$mobile_likes>0,1,0)
summary(pf$mobile_check_in)
sum(pf$mobile_check_in ==1)/length(pf$mobile_check_in) # percentage of people use the applliant

#diamond exercise
data(diamonds)
names(diamonds)
by(diamonds$price,diamonds$color,summary)

qplot(x=price,data=diamonds)
summary(diamonds$price)
#subset
d1<- subset(diamonds,price >=15000)
d2<-(subset(diamonds,price <500))
d3<-(subset(diamonds,price <250))

# one hist
qplot(x=diamonds$price, data=diamonds, color = I('black'),fill=I('#099DD9'))+
  scale_x_continuous(lim=c(0,2000),breaks=seq(0,2000,50))

# hisogram by cut
qplot(x=price,data=diamonds,binwidth=100)+
  facet_wrap(~cut) #another later
by(diamonds$price,diamonds$cut,summary)
by(diamonds$price,diamonds$cut,min)

qplot(x = price, data = diamonds) + 
  facet_wrap(~cut,scales="free_y")

qplot(x =log10( price/carat), data = diamonds) + 
  facet_wrap(~cut,scales="free_y")

#box plot
qplot(x=cut, y=price/carat, data=diamonds, geom='boxplot')+
  scale_y_continuous(lim=c(0,10000),breaks=seq(0,10000,1000)) #ajust the psamsis
#by(diamonds$price,diamonds$color,summary)

#frequencypoly
qplot(x=carat,data=diamonds,
      binwidth = 0.1, geom ='freqpoly')+
  scale_x_continuous(lim=c(0,3),breaks=seq(0,3,0.2))+
  scale_y_continuous(lim=c(0,2000),breaks=seq(0,2000,500))




