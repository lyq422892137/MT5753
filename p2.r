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




################################################################
#################################################################
####### Q1
##### first group the data by fridcodes and find the mean density for each cell
require(dplyr)
newdata <- group_by(EIA, GridCode) %>%
summarise(x.pos = first(x.pos), y.pos = first(y.pos), area = first(area), density = mean (density))

#### pick a nice colour scheme
col<-colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),max=255)))(100)


#### plot the data
p<-ggplot(newdata)
p<-p + geom_tile(aes(x=x.pos, y=y.pos, fill=density, height=1000, width=1000)) + scale_fill_gradientn(colours=col, space="Lab", na.value="grey50", guide="colourbar")
p + theme_bw() + coord_equal()


######Q2
?factors


###### Q3
fit.full<- lm(density ~ tidestate + observationhour + DayOfMonth + MonthOfYear + impact + Year + x.pos + y.pos, data=EIA)
fit.full

# Call:
#   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
#        MonthOfYear + impact + Year + x.pos + y.pos, data = EIA)
# 
# Coefficients: 10
#   (Intercept)   tidestateFLOOD   tidestateSLACK  observationhour       DayOfMonth  
# 2.102e+00        3.317e-02        3.248e-01       -1.205e-01        2.307e-03  
# MonthOfYear           impact             Year            x.pos            y.pos  
# 2.005e-02       -7.287e-01        1.263e-01        2.151e-04        9.976e-05  
summary(fit.full)


# Call:
#   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
#        MonthOfYear + impact + Year + x.pos + y.pos, data = EIA)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.162  -1.568  -1.105  -0.533 136.644 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      2.102e+00  1.051e+00   1.999 0.045612 *  
#   tidestateFLOOD   3.317e-02  7.497e-02   0.443 0.658111    
# tidestateSLACK   3.248e-01  7.875e-02   4.124 3.73e-05 ***
#   observationhour -1.205e-01  9.721e-03 -12.394  < 2e-16 ***
#   DayOfMonth       2.307e-03  3.760e-03   0.614 0.539490    
# MonthOfYear      2.005e-02  1.276e-02   1.572 0.115987    
# impact          -7.287e-01  2.213e-01  -3.293 0.000994 ***
#   Year             1.263e-01  1.061e-01   1.189 0.234259    
# x.pos            2.151e-04  2.084e-05  10.320  < 2e-16 ***
#   y.pos            9.976e-05  1.583e-05   6.303 2.97e-10 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 5.214 on 27788 degrees of freedom
# Multiple R-squared:  0.01219,	Adjusted R-squared:  0.01187 
# F-statistic: 38.11 on 9 and 27788 DF,  p-value: < 2.2e-16


# month as a factor
fit.full.fac<- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + impact + Year + x.pos + y.pos, data=EIA)
fit.full.fac

# Call: 20
#   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
#        as.factor(MonthOfYear) + impact + Year + x.pos + y.pos, data = EIA)
# 
# Coefficients:
#   (Intercept)            tidestateFLOOD            tidestateSLACK  
# 6.406e+00                 1.314e-02                 3.405e-01  
# observationhour                DayOfMonth   as.factor(MonthOfYear)2  
# -1.236e-01                 8.876e-04                 4.784e-01  
# as.factor(MonthOfYear)3   as.factor(MonthOfYear)4   as.factor(MonthOfYear)5  
# 2.196e-01                -2.263e-01                -2.001e-01  
# as.factor(MonthOfYear)6   as.factor(MonthOfYear)7   as.factor(MonthOfYear)8  
# -2.546e-01                -1.661e-01                 7.000e-02  
# as.factor(MonthOfYear)9  as.factor(MonthOfYear)10  as.factor(MonthOfYear)11  
# -3.058e-01                -7.213e-03                 1.595e-02  
# as.factor(MonthOfYear)12                    impact                      Year  
# 4.265e-02                 1.579e-01                -3.170e-01  
# x.pos                     y.pos  
# 2.151e-04                 9.976e-05  

summary(fit.full.fac)

# 
# Call:
#   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
#        as.factor(MonthOfYear) + impact + Year + x.pos + y.pos, data = EIA)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.150  -1.582  -1.105  -0.509 136.719 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               6.406e+00  2.051e+00   3.123  0.00179 ** 
#   tidestateFLOOD            1.314e-02  7.616e-02   0.173  0.86304    
# tidestateSLACK            3.405e-01  7.970e-02   4.273 1.94e-05 ***
#   observationhour          -1.236e-01  1.027e-02 -12.036  < 2e-16 ***
#   DayOfMonth                8.876e-04  4.104e-03   0.216  0.82876    
# as.factor(MonthOfYear)2   4.784e-01  1.721e-01   2.780  0.00543 ** 
#   as.factor(MonthOfYear)3   2.196e-01  1.802e-01   1.218  0.22307    
# as.factor(MonthOfYear)4  -2.263e-01  2.492e-01  -0.908  0.36383    
# as.factor(MonthOfYear)5  -2.001e-01  2.521e-01  -0.794  0.42735    
# as.factor(MonthOfYear)6  -2.546e-01  2.621e-01  -0.972  0.33123    
# as.factor(MonthOfYear)7  -1.661e-01  2.754e-01  -0.603  0.54649    
# as.factor(MonthOfYear)8   7.000e-02  2.624e-01   0.267  0.78960    
# as.factor(MonthOfYear)9  -3.058e-01  2.670e-01  -1.145  0.25218    
# as.factor(MonthOfYear)10 -7.213e-03  2.624e-01  -0.027  0.97807    
# as.factor(MonthOfYear)11  1.595e-02  2.747e-01   0.058  0.95371    
# as.factor(MonthOfYear)12  4.265e-02  2.672e-01   0.160  0.87321    
# impact                    1.579e-01  4.094e-01   0.386  0.69970    
# Year                     -3.170e-01  2.023e-01  -1.567  0.11704    
# x.pos                     2.151e-04  2.083e-05  10.323  < 2e-16 ***
#   y.pos                     9.976e-05  1.582e-05   6.304 2.94e-10 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 5.213 on 27778 degrees of freedom
# Multiple R-squared:  0.01307,	Adjusted R-squared:  0.01239 
# F-statistic: 19.36 on 19 and 27778 DF,  p-value: < 2.2e-16


#####################################

sqrt(diag(vcov(fit.full)))
# sqrt(diag(vcov(fit.full)))
# (Intercept)  tidestateFLOOD  tidestateSLACK observationhour      DayOfMonth 
# 1.051433e+00    7.496540e-02    7.875052e-02    9.720965e-03    3.760440e-03 
# MonthOfYear          impact            Year           x.pos           y.pos 
# 1.275774e-02    2.213154e-01    1.061470e-01    2.083838e-05    1.582853e-05 
t.test(Year)
# One Sample t-test
# 
# data:  Year
# t = 1579.6, df = 27797, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   10.18499 10.21029
# sample estimates:
#   mean of x 
# 10.19764 

1.263e-01 +qt(0.025,27788)*1.061470e-01
#-0.08175336
  1.263e-01 -qt(0.025,27788)*1.061470e-01
#0.3343534
  
  library(car)
  vif(fit.full.fac)
  
  # GVIF Df GVIF^(1/(2*Df))
  # tidestate               1.064937  2        1.015853
  # observationhour         1.128065  1        1.062104
  # DayOfMonth              1.308798  1        1.144027
  # as.factor(MonthOfYear)  8.817205 11        1.104001
  # impact                 42.860812  1        6.546817
  # Year                   48.499037  1        6.964125
  # x.pos                   1.257333  1        1.121309
  # y.pos                   1.257333  1        1.121309
  AIC(fit.full.fac)
  #170706.9
  step.linearAllAIC<- step(fit.full.fac, direction="both")
  
  # 
  # Start:  AIC=91817.56
  # density ~ tidestate + observationhour + DayOfMonth + MonthOfYear + 
  #   impact + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - DayOfMonth       1      10.2 755429 91816
  # - Year             1      38.5 755457 91817
  # <none>                         755419 91818
  # - MonthOfYear      1      67.2 755486 91818
  # - impact           1     294.7 755713 91826
  # - tidestate        2     553.3 755972 91834
  # - y.pos            1    1079.8 756499 91855
  # - x.pos            1    2895.5 758314 91922
  # - observationhour  1    4176.2 759595 91969
  # 
  # Step:  AIC=91815.94
  # density ~ tidestate + observationhour + MonthOfYear + impact + 
  #   Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - Year             1      30.6 755460 91815
  # <none>                         755429 91816
  # - MonthOfYear      1      58.2 755487 91816
  # + DayOfMonth       1      10.2 755419 91818
  # - impact           1     286.4 755715 91824
  # - tidestate        2     549.0 755978 91832
  # - y.pos            1    1079.8 756509 91854
  # - x.pos            1    2895.5 758324 91920
  # - observationhour  1    4166.9 759596 91967
  # 
  # Step:  AIC=91815.06
  # density ~ tidestate + observationhour + MonthOfYear + impact + 
  #   x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - MonthOfYear      1      28.3 755488 91814
  # <none>                         755460 91815
  # + Year             1      30.6 755429 91816
  # + DayOfMonth       1       2.4 755457 91817
  # - tidestate        2     548.6 756008 91831
  # - y.pos            1    1079.8 756539 91853
  # - impact           1    1575.8 757035 91871
  # - x.pos            1    2895.5 758355 91919
  # - observationhour  1    4144.8 759604 91965
  # 
  # Step:  AIC=91814.11
  # density ~ tidestate + observationhour + impact + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                         755488 91814
  # + MonthOfYear      1      28.3 755460 91815
  # + DayOfMonth       1       1.0 755487 91816
  # + Year             1       0.8 755487 91816
  # - tidestate        2     564.5 756052 91831
  # - y.pos            1    1079.8 756568 91852
  # - impact           1    1575.8 757064 91870
  # - x.pos            1    2895.5 758383 91918
  # - observationhour  1    4117.5 759605 91963
  # > 
  #   
  fit.full2<- lm(density ~ DayOfMonth + MonthOfYear + Year, data=EIA)
  fit.full2
  summary(fit.full2)
  
  
  # Call:
  #   lm(formula = density ~ DayOfMonth + MonthOfYear + Year, data = EIA)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -1.623  -1.499  -1.126  -0.992 137.415 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  3.617095   0.337506  10.717  < 2e-16 ***
  #   DayOfMonth  -0.003791   0.003638  -1.042    0.297    
  # MonthOfYear -0.013272   0.009863  -1.346    0.178    
  # Year        -0.215289   0.030139  -7.143 9.34e-13 ***
  #   ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  # 
  # Residual standard error: 5.241 on 27794 degrees of freedom
  # Multiple R-squared:  0.001848,	Adjusted R-squared:  0.00174 
  # F-statistic: 17.15 on 3 and 27794 DF,  p-value: 4.005e-11
  # 
  sqrt(diag(vcov(fit.full2)))
  
  # (Intercept)  DayOfMonth MonthOfYear        Year 
  # 0.337505504 0.003637977 0.009863393 0.030138770 
  
  
  -0.215289 +qt(0.025,27794)*0.030138770 
  #-0.08175336 -0.2743625
  -0.4233424/-0.2743625
  ##1.543004
  -0.215289 -qt(0.025,27794)*0.030138770 
  #0.3343534 -0.1562155
  0.3343534/-0.1562155
  ##-2.140334
  
  
  library(pedometrics)
  stepVIF(fit.full)
  
  # Call:
  #   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
  #        MonthOfYear + impact + x.pos + y.pos, data = EIA)
  # 
  # Coefficients:
  #   (Intercept)   tidestateFLOOD   tidestateSLACK  observationhour       DayOfMonth  
  # 3.338e+00        2.594e-02        3.208e-01       -1.198e-01        1.076e-03  
  # MonthOfYear           impact            x.pos            y.pos  
  # 1.000e-02       -4.762e-01        2.151e-04        9.976e-05 
  
  library(pedometrics)
  stepVIF(fit.full.fac)
  # Call:
  #   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
  #        as.factor(MonthOfYear) + impact + x.pos + y.pos, data = EIA)
  # 
  # Coefficients:
  #   (Intercept)            tidestateFLOOD            tidestateSLACK  
  # 3.206e+00                 2.335e-02                 3.418e-01  
  # observationhour                DayOfMonth   as.factor(MonthOfYear)2  
  # -1.246e-01                 3.032e-03                 4.971e-01  
  # as.factor(MonthOfYear)3   as.factor(MonthOfYear)4   as.factor(MonthOfYear)5  
  # 3.738e-01                 9.431e-02                 1.142e-01  
  # as.factor(MonthOfYear)6   as.factor(MonthOfYear)7   as.factor(MonthOfYear)8  
  # 7.234e-02                 1.717e-01                 3.895e-01  
  # as.factor(MonthOfYear)9  as.factor(MonthOfYear)10  as.factor(MonthOfYear)11  
  # 2.517e-02                 3.141e-01                 3.519e-01  
  # as.factor(MonthOfYear)12                    impact                     x.pos  
  # 3.709e-01                -4.762e-01                 2.151e-04  
  # y.pos  
  # 9.976e-05  
  # 
  
  # AIC(fit.full.fac)
  # [1] 170702.2
  # >   #170706.9
  #   >   step.linearAllAIC<- step(fit.full.fac, direction="both")
  # Start:  AIC=91812.91
  # density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + 
  #   impact + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - DayOfMonth              1       1.3 754750 91811
  # - impact                  1       4.0 754753 91811
  # <none>                                754749 91813
  # - Year                    1      66.7 754816 91813
  # - as.factor(MonthOfYear) 11     736.7 755486 91818
  # - tidestate               2     633.3 755383 91832
  # - y.pos                   1    1079.8 755829 91851
  # - x.pos                   1    2895.5 757645 91917
  # - observationhour         1    3935.9 758685 91955
  # 
  # Step:  AIC=91810.96
  # density ~ tidestate + observationhour + as.factor(MonthOfYear) + 
  #   impact + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - impact                  1       6.4 754757 91809
  # <none>                                754750 91811
  # - Year                    1      82.2 754833 91812
  # + DayOfMonth              1       1.3 754749 91813
  # - as.factor(MonthOfYear) 11     736.6 755487 91816
  # - tidestate               2     632.1 755383 91830
  # - y.pos                   1    1079.8 755830 91849
  # - x.pos                   1    2895.5 757646 91915
  # - observationhour         1    3947.0 758698 91954
  # 
  # Step:  AIC=91809.19
  # density ~ tidestate + observationhour + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                                754757 91809
  # + impact                  1       6.4 754750 91811
  # + DayOfMonth              1       3.6 754753 91811
  # - as.factor(MonthOfYear) 11     969.8 755727 91823
  # - tidestate               2     627.4 755384 91828
  # - y.pos                   1    1079.8 755837 91847
  # - Year                    1    1651.6 756408 91868
  # - x.pos                   1    2895.5 757652 91914
  # - observationhour         1    3965.4 758722 91953
  # > 
  # 
  # 
  ###############################
  #### Q8
  
  fit.fullfac.noimp <- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + Year + x.pos + y.pos, data=EIA)
  summary(fit.fullfac.noimp)
  
  # 
  # Call:
  #   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
  #        as.factor(MonthOfYear) + Year + x.pos + y.pos, data = EIA)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -3.153  -1.582  -1.105  -0.508 136.721 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)               5.630e+00  3.932e-01  14.318  < 2e-16 ***
  #   tidestateFLOOD            1.562e-02  7.588e-02   0.206  0.83689    
  # tidestateSLACK            3.408e-01  7.969e-02   4.277 1.90e-05 ***
  #   observationhour          -1.238e-01  1.025e-02 -12.082  < 2e-16 ***
  #   DayOfMonth                1.409e-03  3.875e-03   0.364  0.71608    
  # as.factor(MonthOfYear)2   4.829e-01  1.717e-01   2.813  0.00491 ** 
  #   as.factor(MonthOfYear)3   2.571e-01  1.517e-01   1.694  0.09023 .  
  # as.factor(MonthOfYear)4  -1.483e-01  1.457e-01  -1.018  0.30875    
  # as.factor(MonthOfYear)5  -1.236e-01  1.558e-01  -0.794  0.42742    
  # as.factor(MonthOfYear)6  -1.751e-01  1.618e-01  -1.082  0.27909    
  # as.factor(MonthOfYear)7  -8.392e-02  1.746e-01  -0.481  0.63076    
  # as.factor(MonthOfYear)8   1.477e-01  1.681e-01   0.879  0.37948    
  # as.factor(MonthOfYear)9  -2.253e-01  1.666e-01  -1.352  0.17635    
  # as.factor(MonthOfYear)10  7.093e-02  1.668e-01   0.425  0.67067    
  # as.factor(MonthOfYear)11  9.765e-02  1.749e-01   0.558  0.57670    
  # as.factor(MonthOfYear)12  1.225e-01  1.690e-01   0.725  0.46871    
  # Year                     -2.399e-01  3.090e-02  -7.766 8.40e-15 ***
  #   x.pos                     2.151e-04  2.083e-05  10.323  < 2e-16 ***
  #   y.pos                     9.976e-05  1.582e-05   6.304 2.94e-10 ***
  #   ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  # 
  # Residual standard error: 5.212 on 27779 degrees of freedom
  # Multiple R-squared:  0.01306,	Adjusted R-squared:  0.01242 
  # F-statistic: 20.42 on 18 and 27779 DF,  p-value: < 2.2e-16
  
  anova(fit.full.fac,fit.fullfac.noimp)
  # Analysis of Variance Table
  # 
  # Model 1: density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + 
  #   impact + Year + x.pos + y.pos
  # Model 2: density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos
  # Res.Df    RSS Df Sum of Sq      F Pr(>F)
  # 1  27778 754749                           
  # 2  27779 754753 -1   -4.0428 0.1488 0.6997
  
  library(car)
  Anova(fit.full.fac,fit.fullfac.noimp)
  # Anova Table (Type II tests)
  # 
  # Response: density
  # Sum Sq    Df  F value    Pr(>F)    
  # tidestate                 633     2  11.6550 8.718e-06 ***
  #   observationhour          3936     1 144.8642 < 2.2e-16 ***
  #   DayOfMonth                  1     1   0.0468   0.82876    
  # as.factor(MonthOfYear)    737    11   2.4649   0.00443 ** 
  #   impact                      4     1   0.1488   0.69969    
  # Year                       67     1   2.4567   0.11703    
  # x.pos                    2896     1 106.5704 < 2.2e-16 ***
  #   y.pos                    1080     1  39.7441 2.939e-10 ***
  #   Residuals              754753 27779                       
  # ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  
  AIC(fit.fullfac.noimp)
  AICc(fit.fullfac.noimp)
  
  
  step.linearAllAIC<- step(fit.fullfac.noimp, direction="both")
  # Start:  AIC=91811.06
  # density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - DayOfMonth              1       3.6 754757 91809
  # <none>                                754753 91811
  # - as.factor(MonthOfYear) 11     972.5 755726 91825
  # - tidestate               2     630.6 755384 91830
  # - y.pos                   1    1079.8 755833 91849
  # - Year                    1    1638.5 756392 91869
  # - x.pos                   1    2895.5 757649 91916
  # - observationhour         1    3966.1 758719 91955
  # 
  # Step:  AIC=91809.19
  # density ~ tidestate + observationhour + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                                754757 91809
  # + DayOfMonth              1       3.6 754753 91811
  # - as.factor(MonthOfYear) 11     969.8 755727 91823
  # - tidestate               2     627.4 755384 91828
  # - y.pos                   1    1079.8 755837 91847
  # - Year                    1    1651.6 756408 91868
  # - x.pos                   1    2895.5 757652 91914
  # - observationhour         1    3965.4 758722 91953
  # > 
  
  
  require(MuMIn)
  options(na.action='na.fail')
  dredge(fit.fullfac.noimp)
  
  
  
  step.linearAllBIC<- step(fit.fullfac.noimp, direction="both", k=log(nrow(EIA)))
  
  # 
  # 
  # Start:  AIC=91967.48
  # density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - as.factor(MonthOfYear) 11     972.5 755726 91891
  # - DayOfMonth              1       3.6 754757 91957
  # <none>                                754753 91967
  # - tidestate               2     630.6 755384 91970
  # - y.pos                   1    1079.8 755833 91997
  # - Year                    1    1638.5 756392 92018
  # - x.pos                   1    2895.5 757649 92064
  # - observationhour         1    3966.1 758719 92103
  # 
  # Step:  AIC=91890.72
  # density ~ tidestate + observationhour + DayOfMonth + Year + x.pos + 
  #   y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - DayOfMonth              1       0.9 755727 91881
  # - tidestate               2     539.6 756265 91890
  # <none>                                755726 91891
  # - y.pos                   1    1079.8 756806 91920
  # - Year                    1    1337.0 757063 91930
  # + as.factor(MonthOfYear) 11     972.5 754753 91967
  # - x.pos                   1    2895.5 758621 91987
  # - observationhour         1    4123.8 759850 92032
  # 
  # Step:  AIC=91880.52
  # density ~ tidestate + observationhour + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # - tidestate               2     542.0 756269 91880
  # <none>                                755727 91881
  # + DayOfMonth              1       0.9 755726 91891
  # - y.pos                   1    1079.8 756806 91910
  # - Year                    1    1337.0 757064 91919
  # + as.factor(MonthOfYear) 11     969.8 754757 91957
  # - x.pos                   1    2895.5 758622 91977
  # - observationhour         1    4137.0 759864 92022
  # 
  # Step:  AIC=91879.98
  # density ~ observationhour + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                                756269 91880
  # + tidestate               2     542.0 755727 91881
  # + DayOfMonth              1       3.3 756265 91890
  # - y.pos                   1    1079.8 757348 91909
  # - Year                    1    1359.6 757628 91920
  # + as.factor(MonthOfYear) 11     884.4 755384 91960
  # - x.pos                   1    2895.5 759164 91976
  # - observationhour         1    4144.6 760413 92022
  # 
  
  fit.full123<- lm(density ~ tidestate + observationhour + DayOfMonth + as.factor(MonthOfYear) + Year + x.pos + y.pos, data=EIA)
  fit.full123
  
  fit.full123<-update(fit.full123, .~. + Year:x.pos + Year:y.pos)
  summary(fit.full123)
  
  # 
  # Call:
  #   lm(formula = density ~ tidestate + observationhour + DayOfMonth + 
  #        as.factor(MonthOfYear) + Year + x.pos + y.pos + Year:x.pos + 
  #        Year:y.pos, data = EIA)
  # 
  # Residuals:
  #   Min      1Q  Median      3Q     Max 
  # -3.289  -1.566  -1.095  -0.514 136.679 
  # 
  # Coefficients:
  #   Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)               6.697e+00  6.435e-01  10.407  < 2e-16 ***
  #   tidestateFLOOD            1.562e-02  7.588e-02   0.206  0.83689    
  # tidestateSLACK            3.408e-01  7.969e-02   4.277 1.90e-05 ***
  #   observationhour          -1.238e-01  1.025e-02 -12.083  < 2e-16 ***
  #   DayOfMonth                1.409e-03  3.874e-03   0.364  0.71607    
  # as.factor(MonthOfYear)2   4.829e-01  1.717e-01   2.813  0.00490 ** 
  #   as.factor(MonthOfYear)3   2.571e-01  1.517e-01   1.694  0.09022 .  
  # as.factor(MonthOfYear)4  -1.483e-01  1.457e-01  -1.018  0.30873    
  # as.factor(MonthOfYear)5  -1.236e-01  1.558e-01  -0.794  0.42740    
  # as.factor(MonthOfYear)6  -1.751e-01  1.618e-01  -1.082  0.27906    
  # as.factor(MonthOfYear)7  -8.392e-02  1.746e-01  -0.481  0.63074    
  # as.factor(MonthOfYear)8   1.477e-01  1.681e-01   0.879  0.37946    
  # as.factor(MonthOfYear)9  -2.253e-01  1.666e-01  -1.352  0.17633    
  # as.factor(MonthOfYear)10  7.093e-02  1.668e-01   0.425  0.67066    
  # as.factor(MonthOfYear)11  9.765e-02  1.749e-01   0.558  0.57669    
  # as.factor(MonthOfYear)12  1.225e-01  1.690e-01   0.725  0.46869    
  # Year                     -3.446e-01  5.874e-02  -5.867 4.49e-09 ***
  #   x.pos                     6.144e-04  1.985e-04   3.096  0.00196 ** 
  #   y.pos                     3.107e-04  1.508e-04   2.061  0.03929 *  
  #   Year:x.pos               -3.916e-05  1.935e-05  -2.024  0.04302 *  
  #   Year:y.pos               -2.069e-05  1.470e-05  -1.407  0.15936    
  # ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  # 
  # Residual standard error: 5.212 on 27777 degrees of freedom
  # Multiple R-squared:  0.01322,	Adjusted R-squared:  0.01251 
  # F-statistic:  18.6 on 20 and 27777 DF,  p-value: < 2.2e-16
  
  
  Anova(fit.full123)
  # Anova Table (Type II tests)
  # 
  # Response: density
  # Sum Sq    Df  F value    Pr(>F)    
  # tidestate                 631     2  11.6060 9.155e-06 ***
  #   observationhour          3966     1 145.9881 < 2.2e-16 ***
  #   DayOfMonth                  4     1   0.1323 0.7160725    
  # as.factor(MonthOfYear)    973    11   3.2542 0.0001842 ***
  #   Year                     1638     1  60.3108 8.378e-15 ***
  #   x.pos                    2896     1 106.5796 < 2.2e-16 ***
  #   y.pos                    1080     1  39.7476 2.933e-10 ***
  #   Year:x.pos                111     1   4.0949 0.0430220 *  
  #   Year:y.pos                 54     1   1.9804 0.1593564    
  # Residuals              754634 27777                       
  # ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  
  
  step.linearAllBIC111<- step(fit.full123, direction="both", k=log(nrow(EIA)))
  # Step:  AIC=91879.98
  # density ~ observationhour + Year + x.pos + y.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                                756269 91880
  # + tidestate               2     542.0 755727 91881
  # + Year:x.pos              1      65.7 756203 91888
  # + Year:y.pos              1       8.3 756260 91890
  # + DayOfMonth              1       3.3 756265 91890
  # - y.pos                   1    1079.8 757348 91909
  # - Year                    1    1359.6 757628 91920
  # + as.factor(MonthOfYear) 11     884.4 755384 91960
  # - x.pos                   1    2895.5 759164 91976
  # - observationhour         1    4144.6 760413 92022
  
  
  step.linearAllAIC111<- step(fit.full123, direction="both")
  # Step:  AIC=91808.77
  # density ~ tidestate + observationhour + as.factor(MonthOfYear) + 
  #   Year + x.pos + y.pos + Year:x.pos
  # 
  # Df Sum of Sq    RSS   AIC
  # <none>                                754691 91809
  # + Year:y.pos              1      53.8 754637 91809
  # - Year:x.pos              1      65.7 754757 91809
  # + DayOfMonth              1       3.6 754688 91811
  # - as.factor(MonthOfYear) 11     969.8 755661 91822
  # - tidestate               2     627.4 755319 91828
  # - y.pos                   1    1079.8 755771 91847
  # - observationhour         1    3965.4 758657 91952
  
  AICc(fit.full123)
  
  require(MuMIn)
  options(na.action='na.fail')
  dredge(fit.full123)
  
  
  fit.full1234<- lm(density ~ tidestate + observationhour + as.factor(MonthOfYear) + Year + x.pos + y.pos, data=EIA)
  fit.full1234
  
  fit.full1234<-update(fit.full1234, .~. + Year:x.pos + Year:y.pos)
  summary(fit.full1234)
  Anova(fit.full1234)
  # Anova Table (Type II tests)
  # 
  # Response: density
  # Sum Sq    Df  F value    Pr(>F)    
  # tidestate                 627     2  11.5477 9.705e-06 ***
  #   observationhour          3965     1 145.9666 < 2.2e-16 ***
  #   as.factor(MonthOfYear)    970    11   3.2454 0.0001911 ***
  #   Year                     1652     1  60.7947 6.556e-15 ***
  #   x.pos                    2896     1 106.5829 < 2.2e-16 ***
  #   y.pos                    1080     1  39.7488 2.932e-10 ***
  #   Year:x.pos                111     1   4.0950 0.0430188 *  
  #   Year:y.pos                 54     1   1.9805 0.1593499    
  # Residuals              754637 27778                       
  # ---
  #   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
  
  plot(c(1:100))
  plot(fit.full123)
  plot(fit.fullfac.noimp)
  plot(fit.fullfac.noimp-fit.full123)

  summary(fit.full123)
  summary(fit.fullfac.noimp)
  
  
  ?ncvTest
  
  
  acf(residuals(fit.full123))
  durbinWatsonTest(fit.full123)
  # lag Autocorrelation D-W Statistic p-value
  # 1      0.09346066      1.813072       0
  # Alternative hypothesis: rho != 0
  
  
  
  ##############################################
  #####################################Q19
  EIA$sqrtdensity<-sqrt(density)
  library("nlme")
  ##fit.gls<-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + x.pos:impact + y.pos:impact, data = EIA, method='ML', weights=????)
  fit.gls<-gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + x.pos:impact + y.pos:impact, data = EIA, method='ML')

  fit.gls
  # Generalized least squares fit by maximum likelihood
  # Model: sqrtdensity ~ tidestate + observationhour + impact + x.pos +      y.pos + MonthOfYear + x.pos:impact + y.pos:impact 
  # Data: EIA 
  # Log-likelihood: -40837.5
  # 
  # Coefficients:
  #   (Intercept)  tidestateFLOOD  tidestateSLACK observationhour          impact 
  # 9.123489e-01    9.277956e-03    7.174862e-02   -2.959807e-02   -1.554046e-01 
  # x.pos           y.pos     MonthOfYear    x.pos:impact    y.pos:impact 
  # 5.624827e-05    2.125645e-05    2.801426e-03   -1.753025e-05   -8.239727e-06 
  # 
  # Degrees of freedom: 27798 total; 27788 residual
  # Residual standard error: 1.05142 
  
  summary(fit.gls)
  # Length Class     Mode     
  # modelStruct      0  glsStruct list     
  # dims             3  -none-    list     
  # contrasts        1  -none-    list     
  # coefficients    10  -none-    numeric  
  # varBeta        100  -none-    numeric  
  # sigma            1  -none-    numeric  
  # apVar            0  -none-    NULL     
  # logLik           1  -none-    numeric  
  # numIter          1  -none-    numeric  
  # groups           0  -none-    NULL     
  # call             4  -none-    call     
  # method           1  -none-    character
  # fitted       27798  -none-    numeric  
  # residuals    27798  -none-    numeric  
  # parAssign        9  -none-    list     
  # na.action        0  -none-    NULL 
  
  plot(fitted(fit.gls), residuals(fit.gls, type='response'))
  cut.fit<-cut(fitted(fit.gls), breaks=quantile(fitted(fit.gls), probs=seq(0,1,length=20)))
  means1<- tapply(fitted(fit.gls), cut.fit, mean)
  vars1<- tapply(residuals(fit.gls),cut.fit,var)
  plot(means1,vars1, xlab="Fitted Values",ylab="Variance of the residuals",main="mean-variance plot",pch=16)
  abline(h=summary(fit.gls)$sigma**2,lwd=2)
  
  
  par(mfrow=c(1,2))
  acf(residuals(fit.gls, type='response'))
  acf(residuals(fit.gls, type='normalized'))
  
  
  EIA$block<-paste(Year, MonthOfYear, DayOfMonth, GridCode, sep='')
  require(dplyr)
  EIA2<-arrange(EIA, block, Year, MonthOfYear, DayOfMonth, GridCode)
  
  library("nlme")
  workingModel_GLScorr<- gls(sqrtdensity ~ tidestate + observationhour + impact + x.pos + y.pos + MonthOfYear + x.pos:impact + y.pos:impact, data = EIA,weights=varExp(),
                             correlation=corAR1(form =~1|block),method="ML")
 
 
 summary(workingModel_GLScorr)
 # Generalized least squares fit by maximum likelihood
 # Model: sqrtdensity ~ tidestate + observationhour + impact + x.pos +      y.pos + MonthOfYear + x.pos:impact + y.pos:impact 
 # Data: EIA 
 # AIC      BIC    logLik
 # 72186.55 72293.58 -36080.28
 # 
 # Correlation Structure: AR(1)
 # Formula: ~1 | block 
 # Parameter estimate(s):
 #   Phi 
 # 0.5134957 
 # Variance function:
 #   Structure: Exponential of variance covariate
 # Formula: ~fitted(.) 
 # Parameter estimates:
 #   expon 
 # 1.26813 
 # 
 # Coefficients:
 #   Value  Std.Error    t-value p-value
 # (Intercept)      0.8959911 0.04457693  20.099885  0.0000
 # tidestateFLOOD   0.0279952 0.01526959   1.833398  0.0668
 # tidestateSLACK   0.0895393 0.01361160   6.578166  0.0000
 # observationhour -0.0290912 0.00237651 -12.241180  0.0000
 # impact          -0.1459943 0.03709040  -3.936175  0.0001
 # x.pos            0.0000651 0.00000884   7.359235  0.0000
 # y.pos           -0.0000016 0.00000638  -0.252535  0.8006
 # MonthOfYear      0.0009043 0.00260150   0.347616  0.7281
 # x.pos:impact    -0.0000202 0.00001173  -1.724299  0.0847
 # y.pos:impact     0.0000005 0.00000854   0.056798  0.9547
 # 
 # Correlation: 
 #   (Intr) tFLOOD tSLACK obsrvt impact x.pos  y.pos  MnthOY impct:x.
 # tidestateFLOOD  -0.139                                                          
 # tidestateSLACK  -0.102  0.554                                                   
 # observationhour -0.664 -0.036 -0.037                                            
 # impact          -0.480  0.000  0.000  0.000                                     
 # x.pos            0.551  0.000  0.000  0.000 -0.662                              
 # y.pos            0.333  0.000  0.000  0.000 -0.400  0.417                       
 # MonthOfYear     -0.328 -0.010 -0.025 -0.045  0.000  0.000  0.000                
 # x.pos:impact    -0.415  0.000  0.000  0.000  0.866 -0.753 -0.314  0.000         
 # y.pos:impact    -0.249  0.000  0.000  0.000  0.538 -0.311 -0.747  0.000  0.422  
 # 
 # Standardized residuals:
 #   Min         Q1        Med         Q3        Max 
 # -0.4843916 -0.4325440 -0.3737026 -0.2636188 10.0194096 
 # 
 # Residual standard error: 0.5988873 
 # Degrees of freedom: 27798 total; 27788 residual
 
 
 workingModel_GLScorr2<-update(workingModel_GLScorr,
                               corr = corARMA(p = 2, q = 0, form = ~ 1 | block))
 summary(workingModel_GLScorr2)
 
 # Generalized least squares fit by maximum likelihood
 # Model: sqrtdensity ~ tidestate + observationhour + impact + x.pos +      y.pos + MonthOfYear + x.pos:impact + y.pos:impact 
 # Data: EIA 
 # AIC      BIC    logLik
 # 70317.97 70433.22 -35144.98
 # 
 # Correlation Structure: ARMA(2,0)
 # Formula: ~1 | block 
 # Parameter estimate(s):
 #   Phi1      Phi2 
 # 0.3484719 0.3181314 
 # Variance function:
 #   Structure: Exponential of variance covariate
 # Formula: ~fitted(.) 
 # Parameter estimates:
 #   expon 
 # 1.256588 
 # 
 # Coefficients:
 #   Value  Std.Error    t-value p-value
 # (Intercept)      0.8996255 0.04724198  19.042926  0.0000
 # tidestateFLOOD   0.0315160 0.01391591   2.264746  0.0235
 # tidestateSLACK   0.0863840 0.01220437   7.078117  0.0000
 # observationhour -0.0302268 0.00235322 -12.844862  0.0000
 # impact          -0.1400594 0.04114370  -3.404152  0.0007
 # x.pos            0.0000613 0.00000980   6.252101  0.0000
 # y.pos            0.0000006 0.00000712   0.084635  0.9326
 # MonthOfYear      0.0008115 0.00288528   0.281240  0.7785
 # x.pos:impact    -0.0000179 0.00001304  -1.372715  0.1699
 # y.pos:impact    -0.0000014 0.00000953  -0.146846  0.8833
 # 
 # Correlation: 
 #   (Intr) tFLOOD tSLACK obsrvt impact x.pos  y.pos  MnthOY impct:x.
 # tidestateFLOOD  -0.108                                                          
 # tidestateSLACK  -0.077  0.561                                                   
 # observationhour -0.632 -0.047 -0.044                                            
 # impact          -0.500  0.000  0.000  0.000                                     
 # x.pos            0.572  0.000  0.000  0.000 -0.657                              
 # y.pos            0.354  0.000  0.000  0.000 -0.407  0.419                       
 # MonthOfYear     -0.359 -0.011 -0.022 -0.031  0.000  0.000  0.000                
 # x.pos:impact    -0.430  0.000  0.000  0.000  0.863 -0.751 -0.315  0.000         
 # y.pos:impact    -0.264  0.000  0.000  0.000  0.545 -0.313 -0.747  0.000  0.424  
 # 
 # Standardized residuals:
 #   Min         Q1        Med         Q3        Max 
 # -0.4856895 -0.4326939 -0.3741100 -0.2622258 10.0270279 
 # 
 # Residual standard error: 0.6027399 
 # Degrees of freedom: 27798 total; 27788 residual
 
 
 #fit an AR(3) process
 workingModel_GLScorr3<-update(workingModel_GLScorr,
                                 corr = corARMA(p = 3, q = 0, form = ~ 1 | block))
 summary(workingModel_GLScorr3)
 
 # Generalized least squares fit by maximum likelihood
 # Model: sqrtdensity ~ tidestate + observationhour + impact + x.pos +      y.pos + MonthOfYear + x.pos:impact + y.pos:impact 
 # Data: EIA 
 # AIC      BIC    logLik
 # 69677.78 69801.28 -34823.89
 # 
 # Correlation Structure: ARMA(3,0)
 # Formula: ~1 | block 
 # Parameter estimate(s):
 #   Phi1      Phi2      Phi3 
 # 0.2727694 0.2371618 0.2344253 
 # Variance function:
 #   Structure: Exponential of variance covariate
 # Formula: ~fitted(.) 
 # Parameter estimates:
 #   expon 
 # 1.238794 
 # 
 # Coefficients:
 #   Value  Std.Error    t-value p-value
 # (Intercept)      0.9239912 0.04726503  19.549151  0.0000
 # tidestateFLOOD   0.0332493 0.01264622   2.629190  0.0086
 # tidestateSLACK   0.0839449 0.01163569   7.214430  0.0000
 # observationhour -0.0322063 0.00221381 -14.547902  0.0000
 # impact          -0.1421081 0.04249071  -3.344452  0.0008
 # x.pos            0.0000613 0.00001010   6.068351  0.0000
 # y.pos            0.0000027 0.00000735   0.361936  0.7174
 # MonthOfYear      0.0010964 0.00298114   0.367782  0.7130
 # x.pos:impact    -0.0000193 0.00001348  -1.432727  0.1519
 # y.pos:impact    -0.0000036 0.00000987  -0.368806  0.7123
 # 
 # Correlation: 
 #   (Intr) tFLOOD tSLACK obsrvt impact x.pos  y.pos  MnthOY impct:x.
 # tidestateFLOOD  -0.091                                                          
 # tidestateSLACK  -0.060  0.532                                                   
 # observationhour -0.606 -0.051 -0.053                                            
 # impact          -0.514  0.000  0.000  0.000                                     
 # x.pos            0.588  0.000  0.000  0.000 -0.655                              
 # y.pos            0.369  0.000  0.000  0.000 -0.410  0.420                       
 # MonthOfYear     -0.379 -0.016 -0.026 -0.020  0.000  0.000  0.000                
 # x.pos:impact    -0.441  0.000  0.000  0.000  0.861 -0.749 -0.315  0.000         
 # y.pos:impact    -0.275  0.000  0.000  0.000  0.549 -0.313 -0.745  0.000  0.425  
 # 
 # Standardized residuals:
 #   Min         Q1        Med         Q3        Max 
 # -0.4902118 -0.4338976 -0.3734663 -0.2583996 10.0856739 
 # 
 # Residual standard error: 0.6057904 
 # Degrees of freedom: 27798 total; 27788 residual
 
 
 AIC(fit.gls, workingModel_GLScorr,
     workingModel_GLScorr2,
     workingModel_GLScorr3)
 # df      AIC
 # fit.gls               11 81697.00
 # workingModel_GLScorr  13 72186.55
 # workingModel_GLScorr2 14 70317.97
 # workingModel_GLScorr3 15 69677.78
 # 
 
 BIC(fit.gls, workingModel_GLScorr,
     workingModel_GLScorr2,
     workingModel_GLScorr3)
 # df      BIC
 # fit.gls               11 81787.56
 # workingModel_GLScorr  13 72293.58
 # workingModel_GLScorr2 14 70433.22
 # workingModel_GLScorr3 15 69801.28
 
 
 #compare acf plots for each
  #compare acf plots for each
 #fitting a log-y model

    plot(acf(residuals(fit.gls), lag.max=15), main="ACF function for the residuals")
  x<-0:15
 lines(x,ARMAacf(ar = c(0.2705746), ma = 0,lag.max = 15, pacf = FALSE),lwd=1)
  lines(x,ARMAacf(ar = c(0.2248092 ,0.1664164),ma = 0, lag.max = 15, pacf = FALSE),lwd=2)
  lines(x,ARMAacf(ar = c(0.21422922, 0.15190470, 0.06316328 ),ma = 0, lag.max = 15, pacf = FALSE),lwd=4)
  legend(10,0.9, c("AR(1)", "AR(2)", "AR(3)"), lwd=c(1,2,3), bty="n")
  
  anova(fit.gls, type="marginal")
  # Denom. DF: 27788 
  # numDF  F-value p-value
  # (Intercept)         1 797.0649  <.0001
  # tidestate           2  11.9367  <.0001
  # observationhour     1 229.2208  <.0001
  # impact              1  38.3383  <.0001
  # x.pos               1  89.5533  <.0001
  # y.pos               1  22.1662  <.0001
  # MonthOfYear         1   2.1357  0.1439
  # impact:x.pos        1   4.3492  0.0370
  # impact:y.pos        1   1.6654  0.1969
  
  
  workingModel_GLScorr4<- gls(density ~ tidestate + observationhour + impact + x.pos + y.pos + x.pos:impact , data = EIA,weights=varExp(),
                             correlation=corAR1(form =~1|block),method="ML")
  

  myprediction<-MuMIn:::predict.gls(workingModel_GLScorr4, newdata = newdat, se.fit=TRUE)
  myprediction
  newOvary <- data.frame(tidestate = "SLACK", observationhour = 10, x.pos = 1500, y.pos = 1000, impact = 0)
  predict(myprediction,newOvary)
  # a) tidestate = SLACK
  # b) observation hour = 10am
  # c) month of the year = 6
  # d) x-position = 1500
  # e) y-position = 1000
  # f ) impact = 0 and 1
  
  # fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
  #            correlation = corAR1(form = ~ 1 | Mare))
  # newOvary <- data.frame(Time = c(-0.75, -0.5, 0, 0.5, 0.75))
  # predict(fm1, newOvary)
  
  
  