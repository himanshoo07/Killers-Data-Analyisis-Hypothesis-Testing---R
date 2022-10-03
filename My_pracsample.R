setwd("/Users/himanshoo/Course_Work/Statistical Theory & Methods/Practical_R")

load(file = "killersandmotives.Rdata")
createsample(16) 

mysample

dim(mysample)

#unique(mysample$Race)
#unique(mysample$InsanityPlea)
#unique(mysample$Sentence)
unique(mysample$Motive)


#which(mysample$AgeFirstKill == '99999')
#mysample[mysample$AgeFirstKill=='99999',c('KillerID','AgeFirstKill')]

mysample <- mysample[mysample$AgeFirstKill!='99999',] 
dim(mysample)    #771-9 rows = 762 rows remaining

mysample <- mysample[!is.na(mysample$Motive),]
dim(mysample)  # 762- 6 = 756

mysample <- mysample[ (mysample$AgeFirstKill + mysample$YearBorn ) > 1900, ]
dim(mysample) #756 - 8 = 748
#mysample$CareerDuration <- mysample$AgeLastKill - mysample$AgeFirstKill

mysample["CareerDuration"]<- mysample$AgeLastKill - mysample$AgeFirstKill

#data cleaning part done!

mys <- mysample

#Analysing AgeFirstKill

mean(mys$AgeFirstKill)    #30.66845

sd(mys$AgeFirstKill)      # 8.440022

var(mys$AgeFirstKill)
quantile(mys$AgeFirstKill)
max(mys$AgeFirstKill)

boxplot(mys$AgeFirstKill,
        ylab = "Age first kill (in years)",
        main = "Serial killer: Age first kill")

# Histogram to analyze variables
#FIRST VARIABLE : Age First Kill

#Normal Distribution of the age first kill


v <- seq(from = 0, to = 80, by = 5)

hist(mysample$AgeFirstKill ,
     xlab = "Age at first kill",
     ylab = "Density",
     main = "Serial Killer: Age at first kill distribution",
     breaks = v,
     freq = FALSE)

afk <- mysample$AgeFirstKill
afkm <- mean(mysample$AgeFirstKill)
afksd <- sd(mysample$AgeFirstKill)
afkdnorm <- dnorm(sort(afk), mean= afkm , sd = afksd)

lines( sort(afk), afkdnorm , type = "l" , col = "red" )


#Second VARIABLE : Age last Kill

mean(mysample$AgeLastKill)    #34.99198
max(mysample$AgeLastKill)    #66
sd(mysample$AgeLastKill)      #9.496561
quantile(mysample$AgeLastKill)

var(mys$AgeLastKill)

boxplot(mysample$AgeLastKill,
        ylab = "Age last kill (in years)",
        main = "Serial killer: Age last kill")

# Histogram to analyze variables

v <- seq(from = 0, to = 80, by = 5)

hist(mysample$AgeLastKill ,
     xlab = "Age at last kill",
     ylab = "Density",
     main = "Serial Killer: Age last kill distribution",
     breaks = v,
     freq = FALSE,
     ylim = c( 0 , 0.05))

alk <- mysample$AgeLastKill
alkm <- mean(mysample$AgeLastKill)
alksd <- sd(mysample$AgeLastKill)
alkdnorm <- dnorm(sort(alk), mean= alkm , sd = alksd)

lines( sort(alk), alkdnorm , type = "l" , col = "blue" )



alkm  #agelastkillmean
afkm  #agefirstkillmean

# third Variable - Career Duration

mean(mysample$CareerDuration)    #4.323529

sd(mysample$CareerDuration)     #5.988801

quantile(mysample$CareerDuration)
max(mysample$CareerDuration)

boxplot(mysample$CareerDuration,
        ylab = "Career duration (in years)",
        main = "Serial killer: Career Duration")

# Histogram to analyze variables


v_cd <- seq(from = 0, to = 50, by = 5)

#length(mysample$CareerDuration)  #748
cd_data <- mysample[mysample$CareerDuration >=0,]

hist(cd_data$CareerDuration,
     xlab = "Career Duration",
     ylab = "Density",
     main = "Serial Killer:Career Duration distribution",
     breaks = v_cd,
     freq = FALSE,
     ylim = c( 0 , 0.15),
     right = FALSE)

#Exponential distribution of Career Duration

cd <- cd_data$CareerDuration
cdm <- mean(cd_data$CareerDuration)
cd_lambda = 1/cdm         
cdsd <- sd(cd_data$CareerDuration)
cddnorm <- dexp(sort(cd) , rate = cd_l)
points( sort(cd), cddnorm , col = "red" , type = "l" )

#Estimating parameter lambda using maximum likelihood (prac 6)

#x1 <-sample(cd_data$CareerDuration,250)

#xbar <-mean(x1)

xbar <- cdm

loglik <- function(lambda){
  L <- (lambda^250)*exp(-lambda*250*xbar)
  return(log(L))
}

lambda <- (1:200)/250 # 4000 equally spaced points between 0 and 40.

plot(lambda, loglik(lambda), type = "l",
     xlab = "lambda", ylab = "log likelihood")


(1/xbar)   # by Mom value of our parameter lambda is 0.2309

cdm
cd_lambda #0.2309 in my case!


par(mfrow = c(1, 1))

plot(mysample$AgeFirstKill,mysample$AgeLastKill,
     pch = 16, cex = 1, col = "blue", 
     main = "Relationship between Age at first kill \n and Age at last kill",
     xlab = "Age at first kill", 
     ylab = "Age at last kill")


plot(mysample$AgeFirstKill,mysample$CareerDuration,
     pch = 16, cex = 1, col = "red", 
     main = "Relationship between Age at first kill \n and Career duration",
     xlab = "Age at first kill", 
     ylab = "Career Duration (in years)")

cor(mysample$AgeFirstKill,mysample$AgeLastKill)


dim(mys)

m1 <- mys[mys$Motive=='Angel of Death',c('AgeFirstKill')]
length(m1)  #23

m2 <- mys[mys$Motive=='Enjoyment or power',c('AgeFirstKill')]
length(m2) #703

m3 <- mys[mys$Motive=='Escape or avoid arrest',c('AgeFirstKill')]
length(m3)  #22

mean(m1)  #32.34
sd(m1)    #8.70
min(m1)   #21
max(m1)   #58

qqnorm(m1)  
abline(mean(m1),sd(m1), col = "red")     #not normality qqnorm normality
#hist(m1)
t.test(m1, alternative = "two.sided", mu = 27, conf.level = 0.95)

#One Sample t-test

#data:  m1
#t = 2.9462, df = 22, p-value = 0.007469
#alternative hypothesis: true mean is not equal to 27
#95 percent confidence interval:
 # 28.58336 36.11229
#sample estimates:
 # mean of x 
#32.34783 


mean(m2)  #30.52
sd(m2)    #8.44
min(m2)   #10
max(m2)   #66

qqnorm(m2)
abline(mean(m2),sd(m2), col = "red")  #Normality check done by qnorm
#hist(m2)
z.test(m2,alternative = "two.sided",mu = 27,sigma.x = afksd,conf.level = 0.95)

#One-sample z-Test

#data:  m2
#z = 11.069, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 27
#95 percent confidence interval:
# 29.89957 31.14737
#sample estimates:
#  mean of x 
#30.52347 


mean(m3)  #33.54
sd(m3)    #7.46
min(m3)   #23
max(m3)   #53

qqnorm(m3)
abline(mean(m3),sd(m3), col = "red")    #normality check done by qnorm
#hist(m3)

z.test(m3,alternative = "two.sided",mu = 27,sigma.x = afksd,conf.level = 0.95)

#One-sample z-Test

#data:  m3
#z = 3.6375, p-value = 0.0002753
#alternative hypothesis: true mean is not equal to 27
#95 percent confidence interval:
  #30.01866 37.07225
#sample estimates:
 # mean of x 
#33.54545 



#Two sample t- test

t.test(x = m1, y = m2, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#Welch Two Sample t-test

#data:  m1 and m2
#t = 0.98991, df = 23.376, p-value = 0.3324
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
  #-1.984670  5.633381
#sample estimates:
 # mean of x mean of y 
#32.34783  30.52347 


t.test(x = m2, y = m3, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#Welch Two Sample t-test

#data:  m2 and m3
#t = -1.8609, df = 22.715, p-value = 0.07575
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
  #-6.3836822  0.3397148
#sample estimates:
 # mean of x mean of y 
#30.52347  33.54545 


t.test(x = m1, y = m3, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#Welch Two Sample t-test

#data:  m1 and m3
#t = -0.49599, df = 42.513, p-value = 0.6225
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # -6.068838  3.673581
#sample estimates:
 # mean of x mean of y 
#32.34783  33.54545 














