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
  AIC(fit.full)
  #170706.9
  step.linearAllAIC<- step(fit.full, direction="both")
  
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
  sqrt(diag(vcov(fit.full2)))
  (Intercept)  DayOfMonth MonthOfYear        Year 
  0.337505504 0.003637977 0.009863393 0.030138770 
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
  
  -0.215289 +qt(0.025,27794)*1.061470e-01
  #-0.08175336 -0.4233424
  -0.4233424/-0.08175336 
  ##5.178287
  -0.215289 -qt(0.025,27794)*1.061470e-01
  #0.3343534 -0.007235643
  0.3343534/-0.007235643
  ##-46.20922
  
  
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
  