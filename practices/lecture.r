#################
EIA <- read.csv("d:/R/EIA2.csv")
attach(EIA)
#### lecture notes p6 
#### 1.2 exploratory data analysis
require(gplots)
plotmeans(EIA$count/EIA$area ~ EIA$phase, pch=20, xlab="Phase", ylab="Density")
par(las=2) #Make axis labels perpendicular to axis
plotmeans(EIA$count/EIA$area ~ EIA$yearmonth, pch=20, xlab="Year/Month", ylab="Density", n.label=FALSE)
par(las=0) #Put back to default
abline(v=c(9.5, 22.5))
legend(1,12, c("Phase A"), bty="n")
legend(13,12, c("Phase B"), bty="n")
legend(22,12, c("Phase C"), bty="n")


year <- c(2000:2018)
sales <-c(1203:1221)
salesData <- data.frame(year,sales)

salesData$year <- as.factor(salesData$year)
salesData$sales <- as.factor(salesData$sales)

model <- lm(salesData$sales~salesData$year, data = salesData)
summary(model)

# Call:
#   lm(formula = salesData$sales ~ salesData$year, data = salesData)
# 
# Residuals:
#   ALL 19 residuals are 0: no residual degrees of freedom!
#   
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)               1         NA      NA       NA
# salesData$year2001        1         NA      NA       NA
# salesData$year2002        2         NA      NA       NA
# salesData$year2003        3         NA      NA       NA
# salesData$year2004        4         NA      NA       NA
# salesData$year2005        5         NA      NA       NA
# salesData$year2006        6         NA      NA       NA
# salesData$year2007        7         NA      NA       NA
# salesData$year2008        8         NA      NA       NA
# salesData$year2009        9         NA      NA       NA
# salesData$year2010       10         NA      NA       NA
# salesData$year2011       11         NA      NA       NA
# salesData$year2012       12         NA      NA       NA
# salesData$year2013       13         NA      NA       NA
# salesData$year2014       14         NA      NA       NA
# salesData$year2015       15         NA      NA       NA
# salesData$year2016       16         NA      NA       NA
# salesData$year2017       17         NA      NA       NA
# salesData$year2018       18         NA      NA       NA
# 
# Residual standard error: NA on 0 degrees of freedom
# Multiple R-squared:     NA,	Adjusted R-squared:     NA 
# F-statistic:    NA on 18 and 0 DF,  p-value: NA


linearDepth<-lm(EIA$count/EIA$area ~ EIA$impactr, data=EIA)
summary(linearDepth)


plot(fitted(linearDepth),rstandard(linearDepth), col="darkgrey")
abline(h=0)
lines(lowess(fitted(linearDepth),rstandard(linearDepth)), col="red",lwd=2)


require(car)
ncvTest(linearDepth)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 1117.204    Df = 1     p = 6.021885e-245


#sorting the data in time order
EIA<- EIA[order(EIA$Year),]
#re-fitting the model (just in case they weren't in order before)
linearDepth<- lm(EIA$count/EIA$area ~ EIA$impact, data=EIA)
#plotting the residuals in order
par(mfrow=c(1,2))
plot(1:nrow(EIA), rstandard(linearDepth),xlab="Observation Order")


library(car)
durbinWatsonTest(linearDepth)
# lag Autocorrelation D-W Statistic p-value
# 1       0.2186814      1.547736   0.244
# Alternative hypothesis: rho != 0


par(mfrow=c(1,2))
hist(rstandard(linearDepth), main="Model residuals")
qqnorm(rstandard(linearDepth))
qqline(rstandard(linearDepth))
par(mfrow=c(1,1))

a <- c(1:100)
levels(a)