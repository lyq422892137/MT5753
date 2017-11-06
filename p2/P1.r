EIA <- read.csv('d:/R/EIA.csv')

names(EIA)

head(EIA)

str(EIA)

#### add EIA$density into EIA
EIA$density <- EIA$count/EIA$area

####attach(EIA)

#### create 3 histograms for EIA$density
par(mfrow = c(1,2))

## 1. the raw EIA$density data displayed suing equal sized bins
hist(EIA$density, main = "Distribution of counts per unit area", col = 2)

## 2. a random smaple of values sampled from a Normal Distribution with the mean 
##    set to the mean of the density data
hist(rnorm(nrow(EIA),mean(EIA$density)), main = "Distribution of Normal Data", xlab = "Distribution of Normal Data", col = 4)

## 3. the raw density data displayed using unequally sized bins using the qplot 
##    function in the ggplot2 library
##    if the density follows a normal distribution, its log should also follow a 
##    normal distribution
require(ggplot2)
qplot(EIA$density, log = "x", data = EIA, xlab = "Density shown with a logged x-axis")


#### summary statistics of the density data
mean(EIA$density)
sd(EIA$density)

#######################################################
#### 1.5 CIs
#### using the code in P1's instruction to repeatedly sample (with replacement) 
#### from our data:
gen.sample.means <- function(data, n.samp = 999, n = 100) {
  res <- numeric(n.samp)
  for(i in 1:n.samp) {
    newdata <- sample(data,n,replace = T)
    res[i]<- mean(newdata)
  }
  return(res)
}


harvest <- gen.sample.means(EIA$density, n.samp = 500, n = 5)
hist(harvest, main = "Sample means, n =5")
mean(harvest)
sd(harvest)

harvest <- gen.sample.means(EIA$density, n.samp = 500, n = 30)
hist(harvest, main = "Sample means, n =30")
mean(harvest)
sd(harvest)

harvest <- gen.sample.means(EIA$density, n.samp = 500, n = 1000)
hist(harvest, main = "Sample means, n =1000")
mean(harvest)
sd(harvest)

##################################################################
###### bootstrap with T test for known sd
#### calculte the 95% CI for the mean desnigty using SE
harvest <- gen.sample.means(EIA$density, n.samp = 999, n = 1000)
hist(harvest, main = "Sample means, n =1000")
mean(harvest)
sd(harvest)

lowerLimit <- mean(harvest) - abs(qnorm(0.025)) * sd(harvest) / sqrt(998)
lowerLimit ## 1.271283
upperLimit <-  mean(harvest) + abs(qnorm(0.025)) * sd(harvest) / sqrt(998)
upperLimit ## 1.292156

## parametric bootstrap CI (1.271283, 1.292156)

###### bootstrap with T test for known sd
lowerLimit <- mean(harvest) - abs(qt(0.025,998)) * sd(harvest) / sqrt(998)
lowerLimit ## 1.27127
upperLimit <-  mean(harvest) + abs(qt(0.025,998)) * sd(harvest) / sqrt(998)
upperLimit ## 1.292169
## parametric bootstrap CI (1.27127, 1.292169)

#### calculate the 95% CI for the mean density using "the percentile method"
#### lower limit: ¦Á/2 * (b+1)% th
#### 0.025 * 1000% = 25
#### upper limit: (1 - ¦Á/2) (b+1)% th
#### £¨1 - 0.025£©*1000% = 975
bootstrap1 <- sort(harvest)
hist(harvest, main = "Sample means, n = 1000")
hist(bootstrap1, main = "Sample means, n = 1000")
mean(harvest)
sd(harvest)

bootstrap1[25]
## > bootstrap1[25]
## [1] 0.9506694

bootstrap1[975]
## > bootstrap1[975]
## [1] 1.624619

#### non-parametric bootstrap CI: (0.9506694, 1.624619)

################################################
#### 1.6 Comparing pre-impact and post-impact density
#### visualising the density data across impact categories
## to examine the density distribution for each impact category, we can use side-by-side
## impact: whether the construction has occurred (Pre = 0, Post = 1)
## histograms on the rao or log scale:
require(lattice)
histogram(~EIA$density|as.factor(impact), xlab = 'Density values by impact category', main = 'Distribution of density by impact category')

histogram(~EIA$density|as.factor(impact), xlab = "Density values by impact category on the log scale", main = "Distribution of density by impact category", sacles = list(x = list(log = "e")))

## no obvious differences
#### visualising the spatial distribution of the data across impact categories
require(fields)
col <- colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),max = 255)))(100)
col
par(mfrow = c(1,2))
quilt.plot(x.pos[impact == 0], y.pos[impact == 0], EIA$density[impact ==0]/area[impact == 0], nrow = 7, ncol = 9, xlab = "X-position", ylab = "Y-position", asp = 1, zlim = c(0,10), col = col, main = "Density pre-construction")
quilt.plot(x.pos[impact == 1], y.pos[impact == 1], EIA$density[impact ==1]/area[impact == 1], nrow = 7, ncol = 9, xlab = "X-position", ylab = "Y-position", asp = 1, zlim = c(0,10), col = col, main = "Density post-construction")

#### compare mean density for each impact category using traditional confidence intervals
## impact = 0
sd(EIA$density[impact == 0])
mean(EIA$density[impact == 0])
lower1 <- mean(EIA$density[impact == 0]) - abs(qt(0.025,length(EIA$density[impact == 0])-1)) * sd(EIA$density[impact == 0]) / sqrt(length(EIA$density[impact == 0])-1)
lower1 ## 1.425054
higher1 <- mean(EIA$density[impact == 0]) + abs(qt(0.025,length(EIA$density[impact == 0])-1)) * sd(EIA$density[impact == 0]) / sqrt(length(EIA$density[impact == 0])-1)
higher1 ## 1.626044
## CI: (1.42505,1.62604)

## impact == 1
sd(EIA$density[impact == 1])
mean(EIA$density[impact == 1])
lower1 <- mean(EIA$density[impact == 1]) - abs(qt(0.025,length(EIA$density[impact == 1])-1)) * sd(EIA$density[impact == 1]) / sqrt(length(EIA$density[impact == 1])-1)
lower1 ## 0.978084
higher1 <- mean(EIA$density[impact == 1]) + abs(qt(0.025,length(EIA$density[impact == 1])-1)) * sd(EIA$density[impact == 1]) / sqrt(length(EIA$density[impact == 1])-1)
higher1 ## 1.120651
## CI: (0.97808,1.12065)


#### Q8: with known sd
## impact = 0
mean(EIA$density[impact == 0]) ## 1.52555
lower1 <- mean(EIA$density[impact == 0]) - abs(qt(0.025,length(EIA$density[impact == 0])-1)) * sd(EIA$density[impact == 0]) / sqrt(length(EIA$density[impact == 0]))
lower1 ## 1.425058
higher1 <- mean(EIA$density[impact == 0]) + abs(qt(0.025,length(EIA$density[impact == 0])-1)) * sd(EIA$density[impact == 0]) / sqrt(length(EIA$density[impact == 0]))
higher1 ## 1.62604
## CI: (1.425058,1.62604)

#### Q9
## impact = 1
mean(EIA$density[impact == 1]) ## 1.52555
lower1 <- mean(EIA$density[impact == 1]) - abs(qt(0.025,length(EIA$density[impact == 1])-1)) * sd(EIA$density[impact == 1]) / sqrt(length(EIA$density[impact == 1]))
lower1 ## 0.9780865
higher1 <- mean(EIA$density[impact == 1]) + abs(qt(0.025,length(EIA$density[impact == 1])-1)) * sd(EIA$density[impact == 1]) / sqrt(length(EIA$density[impact == 1]))
higher1 ## 1.120648
## CI: (0.9780865,1.120648)


########################################
#### two-sided t test 95% 
t.test(EIA$density[impact == 0],EIA$density[impact == 1])
# Welch Two Sample t-test
# 
# data:  EIA$density[impact == 0] and EIA$density[impact == 1]
# t = 7.5758, df = 25058, p-value = 3.691e-14
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.3529818 0.5993818
# sample estimates:
#   mean of x mean of y 
# 1.525549  1.049367


#### two-sided t test 90% 
t.test(EIA$density[impact == 0],EIA$density[impact == 1],conf.level = 0.9)
# Welch Two Sample t-test
# 
# data:  EIA$density[impact == 0] and EIA$density[impact == 1]
# t = 7.5758, df = 25058, p-value = 3.691e-14
# alternative hypothesis: true difference in means is not equal to 0
# 90 percent confidence interval:
#   0.3727903 0.5795733
# sample estimates:
#   mean of x mean of y 
# 1.525549  1.049367


##############################################################
###### 1.7 compare mean EIA$density across years
#### using the plotmeans function in the gplots library to plot side-by-side 95% CIs
#### for the mean dnesity in each year
require(gplots)
par(mfrow = c(1,1))
plotmeans(EIA$density ~ Year, pch = 20, xlab = "Year", ylab = "EIA$density", main = "95% Confidence Intervals")

require(lattice)
histogram(~EIA$density|as.factor(Year))

#### ANOVA
data = rbind(
  data.frame(Year = 9, density = EIA$density[Year == 9]),
  data.frame(Year = 10, density = EIA$density[Year == 10]),
  data.frame(Year = 11, density = EIA$density[Year == 11]),
  data.frame(Year = 12, density = EIA$density[Year == 12])
)
data

aggregate(data$EIA$density, by = list(data$Year), FUN = mean)

aggregate(data$EIA$density, by = list(data$Year), FUN = sd)

fit = aov(EIA$density~as.factor(Year), data = data)
fit
# Call:
#   aov(formula = EIA$density ~ as.factor(Year), data = data)
# 
# Terms:
#   as.factor(Year) Residuals
# Sum of Squares           1598.8  763143.0
# Deg. of Freedom               3     27794
# 
# Residual standard error: 5.239953
# Estimated effects may be unbalanced

summary(fit)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(Year)     3   1599   532.9   19.41 1.45e-12 ***
#   Residuals       27794 763143    27.5                     
# ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1

df1 <- summary(fit)[[1]][,1][1]
df1 ## 3

sa <- summary(fit)[[1]][,2][1]
sa ## 1598.821

df2 <- summary(fit)[[1]][,1][2]
df2 ## 27794

se <- summary(fit)[[1]][,2][2]
se ## 763143

a <- (sa/df1)/(se/df2)
a ## 19.40991

pvalue <- 1 - pf(a,df1,df2)
pvalue ## 1.445954e-12


TukeyHSD(aov(EIA$density~as.factor(Year), data = data))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = EIA$density ~ as.factor(Year), data = data)
# 
# $`as.factor(Year)`
# diff        lwr        upr     p adj
# 10-9   0.08167509 -0.2050614  0.3684116 0.8843610
# 11-9  -0.44789331 -0.6281683 -0.2676183 0.0000000
# 12-9  -0.50934958 -0.7960861 -0.2226131 0.0000298
# 11-10 -0.52956839 -0.8163049 -0.2428319 0.0000124
# 12-10 -0.59102467 -0.9542554 -0.2277939 0.0001708
# 12-11 -0.06145627 -0.3481928  0.2252802 0.9464092

plot(TukeyHSD(aov(EIA$density~as.factor(Year), data = data)))