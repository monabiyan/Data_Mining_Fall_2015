require(ggplot2)

set.seed(333)
trails <- 99999  #how many numbers to be generated
min <- 1                                  
max <- 100
uni_dist<-runif(trails,min,max+1) #between min and max
uni_dist<-data.frame(x=uni_dist)
head(uni_dist)
str(uni_dist)
names(uni_dist)

#Plotting
qplot(x=x,data=uni_dist)
ggplot(uni_dist) + aes(x=x) + geom_histogram()
qplot(x=x,data=uni_dist,binwidth=10)
qplot(x=x,data=uni_dist,binwidth=10)+ scale_x_continuous(breaks=seq(0,100,10))



min <- 1                                  
max <- 6
die <- as.integer(runif(trails,min,max+1))
die<-data.frame(roll=die)
head(die)
qplot(x=roll,data=die, color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=1:6)
qplot(x=roll,data=die, color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=1:6) + labs(title="Uniform Distribution Single Die", y="Count", x="Die roll Random Variable n=3333 Min=1 Max=6")



dice <- as.integer(runif(trails,min,max+1)+runif(trails,min,max+1))   #Summation of two dice droping 3333 times
dice<-data.frame(rolls=dice)
head(dice)
summary(dice)
dice <- as.integer(runif(trails,min,max+0.5)+runif(trails,min,max+0.5))
dice<-data.frame(rolls=dice)
head(dice)
summary(dice)
qplot(x=rolls,data=dice, color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=2:12) +  labs(title="Distribution Sum of Two Dice", y="Count", x="Sum of Two Uniform Random Variables (Dice) n=3333 Min=1 Max=6")



####################################  Normal Distribution


norm_dist<-rnorm(trails) # norm_dist<-rnorm(3333, mean=0, sd=1)
norm_denisty<-dnorm(norm_dist)

ggplot(data.frame(x=norm_dist,y=norm_denisty)) + aes(x=x,y=y) + geom_point() + labs(title="Standard Normal Distribution", y="Count", x="Normal Random Variable n=3333 mean=0, SD=1")

norm_p<-pnorm(norm_dist)  #pnorm gives the culmulative probability of x from -infinity

ggplot(data.frame(x=norm_dist,y=norm_p)) + aes(x=x,y=y) + geom_point() + labs(title="Standard Normal Distribution", y="Count", x="Normal Random Variable n=3333 mean=0, SD=1")


pnorm(c(-3,-2,-1,0,1,2,3))
head(pnorm(norm_dist))

pnorm(1)-pnorm(-1) # +/- sd=1
sqrt(0.2) # sd^2=var=0.2  sqrt(0.2)
r_normal <-data.frame(A=rnorm(n=trails,mean=0,sd=sqrt(0.2)),
                      B=rnorm(n=trails,mean=0,sd=sqrt(1.0)),
                      C=rnorm(n=trails,mean=0,sd=sqrt(5.0)),
                      D=rnorm(n=trails,mean=-2,sd=sqrt(0.5)))  
head(r_normal)
summary(r_normal)

require(reshape2)
rnd <- melt(data=r_normal)    #Changing the form of data frame
head(rnd)

ggplot(data=rnd) + aes(x=value)+geom_density(aes(group=variable,color=variable,fill=variable))+ labs(title="Wikipedia Normal Distributions", y="??,sigma^2(X)", x=" x n=3333")

########################################################


bi_dist<-rbinom(n=1,size=9,prob=0.33) # n=1  means one draw size=9 means nine trails 
bi_dist # we expect 0.33* 9 or about 3 success
bi_dist<-rbinom(n=5,size=9,prob=0.33) # n=5  means five draws size=9 means nine trails 
bi_dist # we expect each trial to be around 3 success
bernoulli_dist<-rbinom(n=1,size=1,prob=0.33) # Bernoulli distribution 
bi_dist<-rbinom(n=trails,size=9,prob=0.33) # same as bi_dist<-rbi(3333)
summary(bi_dist)
qplot(x=x,data=data.frame(x=bi_dist), color=I('#615445'), fill=I('#CC0000')) + scale_x_discrete(breaks=0:9) + labs(title="Binomial Distribution", y="Count", x="Binomial n=3333,size=9,prob=0.33")
r_binomial <-data.frame(A=rbinom(n=trails,size=20,prob=0.5),
                        B=rbinom(n=trails,size=20,prob=0.7),
                        C=rbinom(n=trails,size=40,prob=0.5))
head(r_binomial)
summary(r_binomial)
rnd <- melt(data=r_binomial)
head(rnd)
ggplot(rnd)+ aes(x=value)+ geom_density(aes(group=variable,color=variable,fill=variable)) + labs(title="Wikipedia Binomial Distributions", y="??,sigma^2(X)", x=" successes")


#############################################Poission Dist.

poisson_dist <-data.frame(lambda1=rpois(n=trails,lambda=1), lambda3=rpois(n=trails,lambda=3), lambda5=rpois(n=trails,lambda=5), lambda10=rpois(n=trails,lambda=10))
head(poisson_dist)
str(poisson_dist)
poisson <- melt(data=poisson_dist, variable.name = "lambda", value.name = "count")
head(poisson)
ggplot(poisson, aes(x=count)) + geom_density(aes(group=lambda,color=lambda,fill=lambda)) + labs(title="Poisson Distribution", y="Count", x=" n=3333 lambda=1,3,5 & 10")
#ggplot(poisson, aes(x=value)) + geom_density(aes(group=variable,color=variable,fill=variable)) + labs(title="Poisson Distribution", y="Count", x=" n=3333 lambda=1,3,5 & 10")

#############################################################


trails<-333
d_binomial <-data.frame(A=dbinom(1:trails, size=trails, prob=0.25),
                        B=dbinom(1:trails, size=trails, prob=0.5),
                        C=dbinom(1:trails, size=trails, prob=0.75))
head(d_binomial)
plot(1:trails, d_binomial$A, type="o", col=2, ylab="P(X=k)", xlab="k", main="Binomial distribution with n=333, p=0.25,0.5,0.75")
points(1:trails,d_binomial$B,col=3)
points(1:trails,d_binomial$C,col=4)


#################################################################


trails<-100
d_binomial <-data.frame(A=dbinom(1:trails, size=200, prob=0.25),
                        B=dbinom(1:trails, size=100, prob=0.5),
                        C=dbinom(1:trails, size=1000, prob=0.05))
plot(1:trails, d_binomial$A, type="o", col=2, ylab="P(X=k)", xlab="k", main="Binomial distribution with size*p = 50")
points(1:trails,d_binomial$B,col=3)
points(1:trails,d_binomial$C,col=4)

################################################################




trails<-100
d_binomial <-data.frame(A=dbinom(1:trails, size=trails, prob=0.02),
                        B=dpois(1:trails,2))
head(d_binomial)
plot(1:trails, d_binomial$A, type="o", col=2, ylab="P(X=k)", xlab="Successes", main="Binomial p=0.02 vs Poisson lambda=2, n = 100")
points(1:trails,d_binomial$B,col=3)

##################################################################

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

plot(1:trails, d_binomial$A, type="o", col=2, ylab="P(X=k)", xlab="Successes", main="Binomial p=0.02, n = 100")

plot(1:trails, d_binomial$B, type="o", col=3, ylab="P(X=k)", xlab="Successes", main="Poisson lambda=2, n = 100")

n<-333
A<-1:n
B<-A^2
llog<-data.frame(A,B)
qplot(A,B, data=llog)
qplot(log(A,2),log(B,2), data=llog)

###############################################################



