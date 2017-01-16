# Drawing the Cauchy Distribution
install.packages('ggplot2')
require(ggplot2)
##############

cauchy<-data.frame(A=rcauchy(100000,0,0.5),B=rcauchy(100000,0,1),
                   C=rcauchy(100000,0,2),D=rcauchy(100000,-2,1))

require(reshape2)
rnd <- melt(data=cauchy)    #Changing the form of data frame
head(rnd)
g<-ggplot(data=rnd)+ aes(x=value)+aes(group=variable)+aes(color=variable)
g<-g+geom_density()
g<-g+xlim(-5,5)+ylim(0,0.7)
g<-g+labs(title="Wikipedia Cauchy Distributions", y="P(x)", x="x")
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

##############  Part 2###############

#First, load the file M01_quasi_twitter.csv 
#Next, answer the following questions for the data in each column:
 # How is the data distributed?
#What happens when you test distribution assumptions (e.g. normal distributions or skewed, etc)?
#What are the summary statistics?
#Are there anomalies/outliers?
#Can you identify the following:
  #useful raw data and transforms (e.g. log(x))
#data quality problems
#outliers
#subsets of interest
#Finally, suggest any functional relationships.

install.packages("ISLR")
require(ISLR)
install.packages("ggplot2")
require(ggplot2)

Outlier = function(vector)
{
  lowerq = quantile(vector)[2]
  upperq = quantile(vector)[4]
  innerq = upperq - lowerq #Or use IQR(data)
  threshold.upper = (innerq * 1.5) + upperq
  threshold.lower = lowerq - (innerq * 1.5)
  NO_outlier<-sum((vector>threshold.upper))+sum((vector<threshold.lower))
  print(NO_outlier)
  outliers<-vector[(vector>threshold.upper)|(vector<threshold.lower)]
  return(outliers)
}

twitter<-read.csv(file='C:/Users/nabian.m/Desktop/M01_quasi_twitter.csv',header=TRUE)
dim(twitter)
head(twitter)

########################################

#1st Column ="Screen Name"

class(twitter$screen_name)

#The summary says that 
screen<-as.data.frame(table(twitter$screen_name))
dim(screen)
head(screen)
summary(screen$Freq)

# So this means we have all frequencies=1. Which means for each screen name we have only one tweet. Which in fact is what we need to do an unbiased study.
#Since we have 21916 factors with frequencies=1 it does no make sence to plot them. 




#2nd Column= created_at_month
class(twitter$created_at_month)
vector<-twitter$created_at_month
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g
#since median is slightly smaller than the mean, that means the data a slightly left skewed. 
#My expectation was a uniform distribution over the months, however, we see in months 3 and 4 we recieve about 50% to 70 % more frequent tweet. 
#from the month 5 to 12 the distribution is almost uniform.

Outlier(vector)
#as we expected there is no outlier.


#3rd Column= created_at_day
class(twitter$created_at_day)
vector<-twitter$created_at_day
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g
summary(vector)
Outlier(vector)
#So as we see it is almost a uniform distribution.

#4th Column= created_at_year
class(twitter$created_at_year)
vector<-twitter$created_at_year
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_histogram()+xlim(2006,2015)
g

#the data collected from twitter is not uniform in terms of years. In fact it has the most amount of data from 2009 and least amount in 2007. From 2010 to 2015 data is almost uniform with the exception of 2015 which is slightly higher. 

summary(vector)
Outlier(vector)
#There is no outlier for year as expected. 


#5th Column= country
country<-as.data.frame(table(twitter$country))
country <- country[order(country$Freq),]
head(country)
tail(country)
dim(country)
summary(country$Freq)

#as we see the results are intensively biased toward USA having 14905 tweets in comparision to the 2nd most frequent conuntry (Canada) with only 943 tweets.
# which is about  6% of U.S.A. All the tweets collected in the file is about 22000 and we have about 15000 from USA and 7000 from other countries mostly western developed countries.
# So with a good approximation we can say the data belongs mostly to the US People.  

g<-ggplot(data.frame(twitter$country))
g<-g+aes(x=twitter$country)+geom_histogram()
g

#This plot also demonstrate this bais toward US users.




#7th Column= friends

class(twitter$friends_count)
vector<-twitter$friends_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,10000)
g


# The plot demosntrate the a very high amount of users have friends less than 500. Which in fact make sense. 
# We can also observe a acute increase in users having around 2000 connections. After that we see a smooth uniform and low value distribution. 
# It is quite expected to have such left skewed plot for connections since most people are ordinary and not either so famous or social. 


summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
Outlier(vector)
#Based on the conventional definition of outlier, we have 2409 outliers in the file. However,since the distribution is highly left skewed 
# we might change our definition. Moreover, the values in here is showing the reality of a network systems that a short percentage are dominantly attached to 
#other nodes. As a result, It all depends on how we want to analyze and present the data. 





#8th Column= followers

class(twitter$followers_count)
vector<-twitter$followers_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,10000)
g


# The plot demosntrate the a very high amount of users have followers less than 500. Which in fact make sense. 
# Based on our previous observation of friendship counts, I was completely expecting the same trend of having a sharply left skewed distribution.




summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
Outlier(vector)
#Based on the conventional definition of outlier, we have 3026 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 



#9th Column= statuses

class(twitter$statuses_count)
vector<-twitter$statuses_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,40000)
g


# This plot indicates that most of people have low statuses however, a few have so many. This is expected to be like this. 


summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 2743 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 


#10th Column = Favorites Count

class(twitter$favourites_count)
vector<-twitter$favourites_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,4000)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 


#11th Column = favourited_count

class(twitter$favourited_count)
vector<-twitter$favourited_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,4000)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 

#12th Column = dob_day

class(twitter$dob_day)
vector<-twitter$dob_day
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,30)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 

#############################################################

#13th Column = dob_year

class(twitter$dob_year)
vector<-twitter$dob_year
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(1900,2015)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 

#########################################################

#14th Column = dob_year

class(twitter$dob_month)
vector<-twitter$dob_month
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(1,12)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.
h<-Outlier(vector)
#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 

###################################################


#15th Column = gender

class(twitter$gender)
vector<-twitter$gender
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_histogram()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

###################################################

#16th Column = mobile_favourites_count

class(twitter$mobile_favourites_count)
vector<-twitter$mobile_favourites_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

###################################################

#17th Column = education

class(twitter$education)
vector<-twitter$education
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_histogram()+xlim(1,20)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 


###################################################

#18th Column = experience

class(twitter$experience)
vector<-twitter$experience
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_histogram()+xlim(0,20)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
#19th Column = age

class(twitter$age)
vector<-twitter$age
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()+xlim(0,100)
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################

#########################################
#20th Column = race

class(twitter$race)
vector<-twitter$race
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_histogram()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
#20th Column = wage

class(twitter$wage)
vector<-twitter$wage
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
#21th Column =retweeted_count

class(twitter$wage)
vector<-twitter$wage
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
#21th Column =retweet_count

class(twitter$wage)
vector<-twitter$wage
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################


class(twitter$wage)
vector<-twitter$wage
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
#########################################


class(twitter$wage)
vector<-twitter$wage
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################

class(twitter$retweeted_count)
vector<-twitter$retweeted_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################

class(twitter$retweet_count)
vector<-twitter$retweet_count
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################
class(twitter$height)
vector<-twitter$height
g<-ggplot(data.frame(vector))
g<-g+aes(x=vector)+geom_density()
g


# This plot indicates that most of people have low Favorits count however, a few have so little. This is expected to be like this. 

summary(vector)
#Since it is the median is highly lower than the mean, the distribution is left skewed.

#Based on the conventional definition of outlier, we have 3031 outliers in the file. However,  since the distribution is highly left skewed 
# we might change our definition.So It all depends on how we want to analyze and present the data. 
#########################################



