
require(ggplot2)


## Data



# Load our data
data(mpg)
data(economics)
data(diamonds)
data_url <- 'http://54.88.34.236/YouTube/MachineLearning/M01/M01_quasi_twitter.csv'
twitter <- read.csv(url(data_url))


str(diamonds)
names(diamonds)


str(twitter)
names(twitter)

mean(twitter$followers_count)
range(twitter$followers_count)
diff(range(twitter$followers_count))
head(twitter[,"followers_count"])
median(twitter$followers_count)
mean(twitter$followers_count)-median(twitter$followers_count)
var(twitter$followers_count)
sd(twitter$followers_count)
sqrt(var(twitter$followers_count))
summary(twitter$followers_count)
summary(twitter)

summary(diamonds$price)

summary(diamonds)

qplot(1,price, data=diamonds, geom="boxplot")

qplot(cut,price, data=diamonds, geom="boxplot")

qplot(cut,carat, data=diamonds)+geom_boxplot()

qplot(reorder(cut,carat),carat, data=diamonds)+geom_boxplot()

qplot(cut,carat, data=diamonds,fill=color)+geom_boxplot()
qplot(cut,carat, data=diamonds,fill=color)+geom_boxplot() + scale_y_log10()

qplot(cut,carat, data=diamonds)+geom_violin()

g<-qplot(cut,carat, data=diamonds)
g+geom_point()+geom_violin()
g+geom_violin()+geom_point()
g+geom_jitter()+geom_point()+geom_violin()

qplot(cut, data=diamonds)

qplot(cut, data=diamonds, fill=color)

qplot(cut, data=diamonds, fill=color, position="stack")

qplot(cut, data=diamonds, fill=color)

qplot(cut, data=diamonds, fill=color, position="dodge")

qplot(cut, data=diamonds, fill=color, position="identity")


qplot(cut,data=diamonds,fill=color,position = "identity")
qplot(color,data=diamonds,fill=cut,position = "identity")
qplot(color,data=diamonds,fill=cut,position = "stack") 


qplot(cut, data=diamonds, fill=color, position="fill")

qplot(carat, data = diamonds, geom = "histogram")

qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1,xlim = c(0,3))

qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.5,
      xlim = c(0,3))

qplot(carat, data = diamonds, geom = "density")



qplot(carat, data = diamonds, geom = "density", colour = color)

qplot(depth, data=diamonds)
qplot(depth, data=diamonds) + xlim(55, 70)
qplot(depth, data=diamonds, fill=cut) + xlim(55, 70)
qplot(depth, data=diamonds, fill=cut, geom="density") + xlim(55, 70)
qplot(depth, data=diamonds, fill=cut, geom="density", alpha=1/4) + xlim(55, 70)
qplot(depth, data=diamonds, fill=cut, geom="density", alpha=carat) + xlim(55, 70)


qplot(x=dob_day, data=twitter)


qplot(x=dob_day, data=twitter)+ facet_wrap(~dob_month) # a grid on one extra variable

qplot(x=dob_year, data=twitter)




qplot(x=dob_day, data=twitter) + scale_x_discrete(breaks=1:31)




qplot(x=followers_count, data=twitter,color=I('#17331F'), fill=I('#CC0000'))


qplot(x=log(followers_count+1,2), data=twitter,color=I('#17331F'), fill=I('#CC0000'))



qplot(x=followers_count, data=twitter,xlim=c(0,10000),color=I('#17331F'), fill=I('#CC0000'))



qplot(x=followers_count, data=twitter,color=I('#17331F'), fill=I('#CC0000'))+scale_x_continuous(limits=c(0,5000))



qplot(x=followers_count, data=twitter, binwidth=50,color=I('#17331F'), fill=I('#CC0000'))+scale_x_continuous(limits=c(0,5000), breaks=seq(0,5000,50))





qplot(x=followers_count, data=twitter,xlim=c(0,10000),geom="density", color=I('#615445'), fill=I('#CC0000'))





by(twitter$followers_count,twitter$gender,summary)





qplot(gender,followers_count, data =twitter, geom="boxplot",ylim=c(0,10000))+xlab("Gender")





qplot(followers_count, data=twitter,xlim=c(0,10000))





qplot(followers_count, data=twitter, fill=gender,xlim=c(0,10000))





qplot(followers_count, data=twitter, fill=gender, position="stack",xlim=c(0,10000))





qplot(followers_count, data=twitter, fill=gender, position="dodge",xlim=c(0,10000))





qplot(followers_count, data=twitter, fill=gender, position="identity",xlim=c(0,10000))





qplot(followers_count, data=twitter, fill=gender, position="fill",xlim=c(0,10000))




qplot(followers_count, data=twitter, binwidth=50, xlim=c(0,10000), geom="freqpoly", color=gender)





qplot(wage, data=twitter, binwidth=5, geom="freqpoly", color=gender)


data(economics)
str(economics)
names(economics)



qplot(date,pop, data=economics)+geom_line()



qplot(date, unemploy / pop, data = economics, geom = "line")





qplot(date, uempmed, data = economics, geom = "line")


# Set a seed value for randomization
set.seed(333)
# create some random data
trails<-333
r_data <-data.frame(A=rnorm(n=trails,mean=33,sd=3),
B=rnorm(n=trails,mean=33,sd=9),
C=1:trails+rnorm(n=trails,sd=3),
D=1:trails+rnorm(n=trails,sd=3), 
E=1:trails+rnorm(n=trails,sd=33),
F=1:trails+rnorm(n=trails,sd=33),                       
age=factor(sample(c(1,2,3,4),size=trails,replace=T),
levels=c(1,2,3,4),labels=c("Toddler","Child","Teen","Adult")),
gender=factor(sample(c(1,2),size=trails,replace=T),
levels=c(1,2),labels=c("Male","Female")),
wealth=factor(sample(c(1,2,3),size=trails,replace=T), levels=c(1,2,3),labels=c("Poor","Middle","Rich")))



qplot(A,B, data=r_data) # random 




qplot(A,B, data=r_data) + geom_point(shape=r_data$gender)

A<-1:trails
B<-A*5
r_linear<-data.frame(A,B)
qplot(A,B, data=r_linear) # linear


B<-A^2
r_llog<-data.frame(A,B)
qplot(A,B, data=r_llog) # quadratic
qplot(log(A,2),log(B,2), data=r_llog) # log-log




qplot(C,D, data=r_data) #  homoscedastic 





qplot(E,F, data=r_data) + geom_point(shape=1)  #  homoscedastic 



qplot(E,F, data=r_data) + geom_point(shape=2) + geom_smooth(method=lm)





qplot(E,F, data=r_data) + geom_point(shape=3) + geom_smooth(method=lm, se=FALSE)





qplot(E,F, data=r_data) + geom_point(shape=4) + geom_smooth()





qplot(E,F, data=r_data,colour=gender)




str(mpg)
names(mpg)
qplot(cty,hwy,data=mpg) 
qplot(cty,hwy,data=mpg,position = "jitter")
qplot(cty,hwy,data=mpg,position = "jitter") # jitter changes



## Playing with scatter plots 





qplot(carat, price, data = diamonds)





qplot(log(carat,2), log(price,2), data = diamonds)





qplot(carat, x * y * z, data = diamonds)





qplot(carat, price, data = diamonds, colour = color)





qplot(carat, price, data = diamonds, shape = cut)





qplot(carat, price, data = diamonds, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = "smooth")
qplot(carat, price, data = diamonds)+geom_smooth()






qplot(carat, price, data = diamonds, geom = c("point", "smooth"),
span = 0.2)






qplot(color, price / carat, data = diamonds, geom = "jitter",
alpha = I(1 / 50))


qplot(carat, price, data = diamonds, col = clarity)





qplot(carat, price, data = diamonds, facets = ~ clarity)



qplot(carat, price, data = diamonds, facets = ~ color)







qqnorm(r_data$A)
qqline(r_data$A)


help(qqnorm)



qqnorm(r_linear$A)
qqline(r_linear$A)




help(qqnorm)



qqnorm(r_llog$B)
qqline(r_llog$B)



help(qqnorm)

qqnorm(twitter$followers_count)
qqline(twitter$followers_count)



