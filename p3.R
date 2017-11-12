################################
###############1.1
EIA <- read.csv("d:/MT5753/EIA.csv")

EIA$impact <- as.factor(EIA$impact)
EIA$MonthOfYear <- as.factor(EIA$MonthOfYear)
EIA$Year <- as.factor(EIA$Year)
attach(EIA)

#####################
################1.1.1
fit.poisSqrt <- glm(count~tidestate + observationhour + DayOfMonth + MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, data = EIA, family = poisson(link = 'sqrt'))

fit.poisSqrt
# Call:  glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#              MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#            family = poisson(link = "sqrt"), data = EIA)
# 
# Coefficients:
#   (Intercept)   tidestateFLOOD   tidestateSLACK  observationhour       DayOfMonth  
# 2.144e+00        2.231e-03        1.443e-01       -5.574e-02       -4.792e-05  
# MonthOfYear2     MonthOfYear3     MonthOfYear4     MonthOfYear5     MonthOfYear6  
# 1.483e-01        7.261e-02       -1.026e-01       -8.897e-02       -1.125e-01  
# MonthOfYear7     MonthOfYear8     MonthOfYear9    MonthOfYear10    MonthOfYear11  
# -1.143e-01        2.377e-02       -1.401e-01       -1.157e-02       -3.510e-02  
# MonthOfYear12           Year10           Year11           Year12            x.pos  
# -3.255e-04        1.436e-02       -2.746e-01       -4.201e-01        1.239e-04  
# y.pos     Year10:x.pos     Year11:x.pos     Year12:x.pos     Year10:y.pos  
# 4.405e-05        3.714e-05       -3.889e-05       -2.290e-05        3.144e-05  
# Year11:y.pos     Year12:y.pos  
# -1.014e-05       -3.403e-05  
# 
# Degrees of Freedom: 27797 Total (i.e. Null);  27771 Residual
# Null Deviance:	    159700 
# Residual Deviance: 151100 	AIC: 166700

summary(fit.poisSqrt)


# Call:
#   glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#         MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#       family = poisson(link = "sqrt"), data = EIA)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.8160  -1.6854  -1.3654  -0.9634  29.4126  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.144e+00  2.904e-02  73.802  < 2e-16 ***
#   tidestateFLOOD   2.231e-03  7.305e-03   0.305 0.760103    
# tidestateSLACK   1.443e-01  7.645e-03  18.879  < 2e-16 ***
#   observationhour -5.574e-02  9.849e-04 -56.598  < 2e-16 ***
#   DayOfMonth      -4.792e-05  3.936e-04  -0.122 0.903117    
# MonthOfYear2     1.483e-01  1.651e-02   8.986  < 2e-16 ***
#   MonthOfYear3     7.261e-02  1.729e-02   4.200 2.66e-05 ***
#   MonthOfYear4    -1.026e-01  2.390e-02  -4.290 1.79e-05 ***
#   MonthOfYear5    -8.897e-02  2.418e-02  -3.680 0.000234 ***
#   MonthOfYear6    -1.125e-01  2.514e-02  -4.476 7.61e-06 ***
#   MonthOfYear7    -1.143e-01  2.642e-02  -4.328 1.51e-05 ***
#   MonthOfYear8     2.377e-02  2.517e-02   0.945 0.344800    
# MonthOfYear9    -1.401e-01  2.561e-02  -5.470 4.50e-08 ***
#   MonthOfYear10   -1.157e-02  2.517e-02  -0.459 0.645901    
# MonthOfYear11   -3.510e-02  2.635e-02  -1.332 0.182836    
# MonthOfYear12   -3.255e-04  2.563e-02  -0.013 0.989868    
# Year10           1.436e-02  2.773e-02   0.518 0.604405    
# Year11          -2.746e-01  1.332e-02 -20.615  < 2e-16 ***
#   Year12          -4.201e-01  2.773e-02 -15.152  < 2e-16 ***
#   x.pos            1.239e-04  3.155e-06  39.270  < 2e-16 ***
#   y.pos            4.405e-05  2.396e-06  18.381  < 2e-16 ***
#   Year10:x.pos     3.714e-05  7.097e-06   5.233 1.67e-07 ***
#   Year11:x.pos    -3.889e-05  4.462e-06  -8.716  < 2e-16 ***
#   Year12:x.pos    -2.290e-05  7.097e-06  -3.226 0.001254 ** 
#   Year10:y.pos     3.144e-05  5.391e-06   5.833 5.45e-09 ***
#   Year11:y.pos    -1.014e-05  3.389e-06  -2.991 0.002785 ** 
#   Year12:y.pos    -3.403e-05  5.391e-06  -6.313 2.74e-10 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 159744  on 27797  degrees of freedom
# Residual deviance: 151135  on 27771  degrees of freedom
# AIC: 166715
# 
# Number of Fisher Scoring iterations: 6



fit.pois<- glm(count ~ tidestate + observationhour + DayOfMonth +
                 MonthOfYear + Year + x.pos + y.pos +
                 Year:x.pos + Year:y.pos, data=EIA, family=poisson)

fit.pois


# Call:  glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#              MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#            family = poisson, data = EIA)
# 
# Coefficients:
#   (Intercept)   tidestateFLOOD   tidestateSLACK  observationhour       DayOfMonth  
# 2.027e+00       -2.787e-02        2.549e-01       -1.040e-01        2.833e-04  
# MonthOfYear2     MonthOfYear3     MonthOfYear4     MonthOfYear5     MonthOfYear6  
# 3.783e-01        1.718e-01       -1.835e-01       -2.045e-01       -2.173e-01  
# MonthOfYear7     MonthOfYear8     MonthOfYear9    MonthOfYear10    MonthOfYear11  
# -1.688e-01        8.254e-02       -2.627e-01        4.750e-02        3.424e-02  
# MonthOfYear12           Year10           Year11           Year12            x.pos  
# 6.260e-02        1.088e-02       -4.110e-01       -6.487e-01        2.081e-04  
# y.pos     Year10:x.pos     Year11:x.pos     Year12:x.pos     Year10:y.pos  
# 1.014e-04        6.372e-05       -3.190e-05        8.477e-06        5.502e-05  
# Year11:y.pos     Year12:y.pos  
# -8.373e-06       -2.718e-05  
# 
# Degrees of Freedom: 27797 Total (i.e. Null);  27771 Residual
# Null Deviance:	    159700 
# Residual Deviance: 151000 	AIC: 166600

summary(fit.pois)
# 
# 
# 
# Call:
#   glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#         MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#       family = poisson, data = EIA)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -3.470  -1.649  -1.335  -1.012  29.268  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.027e+00  5.237e-02  38.695  < 2e-16 ***
#   tidestateFLOOD  -2.787e-02  1.395e-02  -1.998 0.045709 *  
#   tidestateSLACK   2.549e-01  1.381e-02  18.459  < 2e-16 ***
#   observationhour -1.040e-01  1.940e-03 -53.614  < 2e-16 ***
#   DayOfMonth       2.833e-04  7.327e-04   0.387 0.698980    
# MonthOfYear2     3.783e-01  2.995e-02  12.631  < 2e-16 ***
#   MonthOfYear3     1.718e-01  3.210e-02   5.352 8.69e-08 ***
#   MonthOfYear4    -1.835e-01  4.253e-02  -4.313 1.61e-05 ***
#   MonthOfYear5    -2.045e-01  4.300e-02  -4.757 1.97e-06 ***
#   MonthOfYear6    -2.173e-01  4.597e-02  -4.728 2.27e-06 ***
#   MonthOfYear7    -1.688e-01  4.909e-02  -3.439 0.000583 ***
#   MonthOfYear8     8.254e-02  4.511e-02   1.830 0.067283 .  
# MonthOfYear9    -2.627e-01  4.612e-02  -5.696 1.23e-08 ***
#   MonthOfYear10    4.750e-02  4.469e-02   1.063 0.287853    
# MonthOfYear11    3.424e-02  4.663e-02   0.734 0.462754    
# MonthOfYear12    6.260e-02  4.492e-02   1.394 0.163431    
# Year10           1.088e-02  4.267e-02   0.255 0.798678    
# Year11          -4.110e-01  2.242e-02 -18.331  < 2e-16 ***
#   Year12          -6.487e-01  4.850e-02 -13.375  < 2e-16 ***
#   x.pos            2.081e-04  5.504e-06  37.807  < 2e-16 ***
#   y.pos            1.014e-04  4.179e-06  24.269  < 2e-16 ***
#   Year10:x.pos     6.372e-05  1.220e-05   5.222 1.77e-07 ***
#   Year11:x.pos    -3.190e-05  8.525e-06  -3.742 0.000183 ***
#   Year12:x.pos     8.477e-06  1.459e-05   0.581 0.561168    
# Year10:y.pos     5.502e-05  9.219e-06   5.968 2.40e-09 ***
#   Year11:y.pos    -8.373e-06  6.472e-06  -1.294 0.195746    
# Year12:y.pos    -2.718e-05  1.113e-05  -2.442 0.014616 *  
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 159744  on 27797  degrees of freedom
# Residual deviance: 151014  on 27771  degrees of freedom
# AIC: 166593
# 
# Number of Fisher Scoring iterations: 7


########################
# with area


fit.pois_log_offset <- glm(count ~ tidestate + observationhour + DayOfMonth +
                             MonthOfYear + Year + x.pos + y.pos +
                             Year:x.pos + Year:y.pos, data=EIA, family=poisson, offset = log(area))

fit.pois_log_offset
# Call:  glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#              MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#            family = poisson, data = EIA, offset = log(area))
# 
# Coefficients:
#   (Intercept)   tidestateFLOOD   tidestateSLACK  observationhour       DayOfMonth  
# 2.281e+00       -2.787e-02        2.549e-01       -1.040e-01        2.833e-04  
# MonthOfYear2     MonthOfYear3     MonthOfYear4     MonthOfYear5     MonthOfYear6  
# 3.783e-01        1.718e-01       -1.835e-01       -2.045e-01       -2.173e-01  
# MonthOfYear7     MonthOfYear8     MonthOfYear9    MonthOfYear10    MonthOfYear11  
# -1.688e-01        8.254e-02       -2.627e-01        4.750e-02        3.424e-02  
# MonthOfYear12           Year10           Year11           Year12            x.pos  
# 6.260e-02        4.596e-02       -4.278e-01       -6.463e-01        2.536e-04  
# y.pos     Year10:x.pos     Year11:x.pos     Year12:x.pos     Year10:y.pos  
# 1.111e-04        8.063e-05       -4.016e-05        1.071e-05        6.317e-05  
# Year11:y.pos     Year12:y.pos  
# -1.125e-05       -2.822e-05  
# 
# Degrees of Freedom: 27797 Total (i.e. Null);  27771 Residual
# Null Deviance:	    152200 
# Residual Deviance: 142900 	AIC: 158500


summary(fit.pois_log_offset)


# Call:
#   glm(formula = count ~ tidestate + observationhour + DayOfMonth + 
#         MonthOfYear + Year + x.pos + y.pos + Year:x.pos + Year:y.pos, 
#       family = poisson, data = EIA, offset = log(area))
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.2829  -1.6585  -1.2915  -0.8327  28.4081  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.281e+00  5.270e-02  43.292  < 2e-16 ***
#   tidestateFLOOD  -2.787e-02  1.395e-02  -1.998 0.045710 *  
#   tidestateSLACK   2.549e-01  1.381e-02  18.458  < 2e-16 ***
#   observationhour -1.040e-01  1.940e-03 -53.614  < 2e-16 ***
#   DayOfMonth       2.833e-04  7.327e-04   0.387 0.698981    
# MonthOfYear2     3.783e-01  2.995e-02  12.631  < 2e-16 ***
#   MonthOfYear3     1.718e-01  3.210e-02   5.352 8.69e-08 ***
#   MonthOfYear4    -1.835e-01  4.253e-02  -4.313 1.61e-05 ***
#   MonthOfYear5    -2.045e-01  4.300e-02  -4.757 1.97e-06 ***
#   MonthOfYear6    -2.173e-01  4.597e-02  -4.728 2.27e-06 ***
#   MonthOfYear7    -1.688e-01  4.909e-02  -3.439 0.000583 ***
#   MonthOfYear8     8.254e-02  4.511e-02   1.830 0.067285 .  
# MonthOfYear9    -2.627e-01  4.612e-02  -5.696 1.23e-08 ***
#   MonthOfYear10    4.750e-02  4.469e-02   1.063 0.287856    
# MonthOfYear11    3.424e-02  4.663e-02   0.734 0.462756    
# MonthOfYear12    6.260e-02  4.492e-02   1.394 0.163434    
# Year10           4.596e-02  4.447e-02   1.034 0.301304    
# Year11          -4.278e-01  2.420e-02 -17.676  < 2e-16 ***
#   Year12          -6.463e-01  5.097e-02 -12.680  < 2e-16 ***
#   x.pos            2.536e-04  6.187e-06  40.984  < 2e-16 ***
#   y.pos            1.111e-04  4.410e-06  25.205  < 2e-16 ***
#   Year10:x.pos     8.063e-05  1.378e-05   5.849 4.94e-09 ***
#   Year11:x.pos    -4.015e-05  9.561e-06  -4.200 2.67e-05 ***
#   Year12:x.pos     1.071e-05  1.643e-05   0.652 0.514532    
# Year10:y.pos     6.317e-05  9.711e-06   6.505 7.76e-11 ***
#   Year11:y.pos    -1.125e-05  6.830e-06  -1.647 0.099495 .  
# Year12:y.pos    -2.822e-05  1.176e-05  -2.399 0.016418 *  
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 152195  on 27797  degrees of freedom
# Residual deviance: 142926  on 27771  degrees of freedom
# AIC: 158505
# 
# Number of Fisher Scoring iterations: 7

library(car)
vif(fit.pois_log_offset)
# GVIF Df GVIF^(1/(2*Df))
# tidestate         1.097953  2        1.023637 ok
# observationhour   1.188096  1        1.089998 ok
# DayOfMonth        1.367343  1        1.169335 ok
# MonthOfYear       8.483341 11        1.102066 ok
# Year            118.469558  3        2.216160 ok
# x.pos             3.015281  1        1.736456 ok
# y.pos             3.015205  1        1.736434 ok
# Year:x.pos       41.896764  3        1.863646 ok
# Year:y.pos       10.277175  3        1.474503 ok
