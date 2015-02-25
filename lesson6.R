#lesson 6
ggplot(aes(x=carat,y=price),data=diamonds)+
  geom_point(color='#F79420',alpha=1/4)+
  stat_smooth(method = 'lm')+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))
# nolinear, dispersion increases

install.packages('GGally')
install.packages('scales')
install.packages('lattice')
install.packages('MASS')
install.packages('plyr')
install.packages('memisc')

library(ggplot2)
library(GGally)
library(scales)
library(memisc)

#sample 10,000
set.seed(20022012)
diamond_samp<-diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_samp,params=c(shape=I('.'),outlier.shap=I('.')))