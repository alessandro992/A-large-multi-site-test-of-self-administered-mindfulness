#We calculate Omega of the two variables of interest

library(lavaan)
library(semTools)
library(psych)
library(MBESS)

ipipOmega <- cbind(ipip$ipip1 + ipip$ipip2 + ipip$ipip3 + ipip$ipip4 + ipip$ipip5 + ipip$ipip6 + ipip$ipip7 + ipip$ipip8 + ipip$ipip9 + ipip$ipip10 +
                     ipip$ipip11_r + ipip$ipip12_r + ipip$ipip13_r + ipip$ipip14_r + ipip$ipip15_r + ipip$ipip16_r + ipip$ipip17_r + ipip$ipip18_r +
                     ipip$ipip19_r + ipip$ipip20_r)

mod1f <- 'ipipOmega =~ V1'
fit1f <- cfa(mod1f, data=ipipOmega, std.lv=T, missing='direct', estimator='MLR')

summary(fit1f, fit.measures=T)


reliability(fit1f)
