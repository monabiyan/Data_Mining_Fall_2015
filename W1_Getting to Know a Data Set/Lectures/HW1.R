# Drawing the Cauchy Distribution

require(ggplot2)
##############
L<-10000
for (i in 0:L)
{
  x[i]<-(-5+i/(L/10))
}
length(x)
##########
x0<-0;
gama<-0.5;
y1<-1/(3.14*gama)*(gama^2/((x-x0)^2+gama^2))

############
x0<-0;
gama<-1;
y2<-1/(3.14*gama)*(gama^2/((x-x0)^2+gama^2))

###########
x0<-0;
gama<-2;
y3<-1/(3.14*gama)*(gama^2/((x-x0)^2+gama^2))

############
x0<-(-2);
gama<-1;
y4<-1/(3.14*gama)*(gama^2/((x-x0)^2+gama^2))

###########

cauchy<-data.frame(x,y1,y2,y3,y4)
head(cauchy)


  
g <- ggplot(cauchy, aes(x=x))
g <- g + geom_line(aes(y=y1), colour="red")
g <- g + geom_line(aes(y=y2), colour="green")
g <- g + geom_line(aes(y=y3), colour="orange")
g <- g + geom_line(aes(y=y4), colour="blue")
g<-g+xlim(-5,5)+ylim(0,0.7)
g<-g+theme_bw()
g<-g+theme(axis.text = element_text(colour = "blue"))
g<-g+ggtitle("Cauchy Probability density function")
g<-g+xlab("x")+ylab("P(x)")
g

#####################################################



DATA<-read.csv("C:/Users/nabian.m/OneDrive/Fall 2015/intr. Data mining and Machine learning/Week1/HomeWork/M01_Lesson_02_Q1.csv",header=TRUE)
head(DATA)

###########################
#Column A

g0<-ggplot(data=DATA)
g1<-g0+aes(x=A)+geom_density()
g1
vect<-DATA$A
summary(vect)
lowerq = quantile(vect)[2]
upperq = quantile(vect)[4]
innerq = upperq - lowerq #Or use IQR(data)
threshold.upper = (innerq * 1.5) + upperq
threshold.lower = lowerq - (innerq * 1.5)
outlier<-sum((vect>threshold.upper))+sum((vect<threshold.lower))
outlier
##############################
#Column B

g0<-ggplot(data=DATA)
g2<-g0+aes(x=B)+geom_density()
g2
vect<-DATA$B
summary(vect)
lowerq = quantile(vect)[2]
upperq = quantile(vect)[4]
innerq = upperq - lowerq #Or use IQR(data)
threshold.upper = (innerq * 1.5) + upperq
threshold.lower = lowerq - (innerq * 1.5)
outlier<-sum((vect>threshold.upper))+sum((vect<threshold.lower))
outlier
##############################
#Column C

g0<-ggplot(data=DATA)
g3<-g0+aes(x=C)+geom_density()
g3
vect<-DATA$C
summary(vect)
lowerq = quantile(vect)[2]
upperq = quantile(vect)[4]
innerq = upperq - lowerq #Or use IQR(data)
threshold.upper = (innerq * 1.5) + upperq
threshold.lower = lowerq - (innerq * 1.5)
outlier<-sum((vect>threshold.upper))+sum((vect<threshold.lower))
outlier
###############################
#Column D

g0<-ggplot(data=DATA)
g4<-g0+aes(x=D)+geom_density()
g4
vect<-DATA$D
summary(vect)
lowerq = quantile(vect)[2]
upperq = quantile(vect)[4]
innerq = upperq - lowerq #Or use IQR(data)
threshold.upper = (innerq * 1.5) + upperq
threshold.lower = lowerq - (innerq * 1.5)
outlier<-sum((vect>threshold.upper))+sum((vect<threshold.lower))
outlier
###############################
#Column E

g0<-ggplot(data=DATA)
g5<-g0+aes(x=E)+geom_density()
g5
vect<-DATA$E
summary(vect)
lowerq = quantile(vect)[2]
upperq = quantile(vect)[4]
innerq = upperq - lowerq #Or use IQR(data)
threshold.upper = (innerq * 1.5) + upperq
threshold.lower = lowerq - (innerq * 1.5)
outlier<-sum((vect>threshold.upper))+sum((vect<threshold.lower))
outlier
###############################

