#We calculate Omega of the variables of interest

#Ipip neuroticism
ipipOmega <- cbind(ipip$ipip1,ipip$ipip2,ipip$ipip3,ipip$ipip4,ipip$ipip5,ipip$ipip6,ipip$ipip7,ipip$ipip8,ipip$ipip9,ipip$ipip10,
                     ipip$ipip11_r,ipip$ipip12_r,ipip$ipip13_r,ipip$ipip14_r,ipip$ipip15_r,ipip$ipip16_r,ipip$ipip17_r,ipip$ipip18_r,
                     ipip$ipip19_r,ipip$ipip20_r)

#We Specify the one-factor model for the ipip neuroticism scale

mod1f <- 'ipipOmega =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14
+ V15 + V16 + V17 + V18 + V19 + V20'

#We Estimate the one-factor model 
fit1f <- cfa(mod1f, data=ipipOmega, std.lv=T, missing='direct', estimator='MLR')

#We Obtain the results summary
summary(fit1f, fit.measures=T)

#We Obtain coefficient omega as an estimate of the reliability of the ipip neuroticism scale
reliability(fit1f)

#Stai

StaiOmega <- cbind(Stai$Stai1_r, Stai$Stai2_r, Stai$Stai3, Stai$Stai4, Stai$Stai5_r, Stai$Stai6, 
                   Stai$Stai7, Stai$Stai8_r, Stai$Stai9,Stai$Stai10_r, Stai$Stai11_r, 
                   Stai$Stai12, Stai$Stai13, Stai$Stai14, Stai$Stai15_r, Stai$Stai16_r, Stai$Stai17, 
                   Stai$Stai18,Stai$Stai19_r, Stai$Stai20_r)

mod1f <- 'StaiOmega =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14
+ V15 + V16 + V17 + V18 + V19 + V20'

#We Estimate the one-factor model 
fit1f <- cfa(mod1f, data=StaiOmega, std.lv=T, missing='direct', estimator='MLR')

#We Obtain the results summary
summary(fit1f, fit.measures=T)

#We Obtain coefficient omega as an estimate of the reliability of the ipip neuroticism scale
reliability(fit1f)
semTools::reliability(fit1f)
                   



