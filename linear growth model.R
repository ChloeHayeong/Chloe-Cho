library(haven)
library(nlme)

nlsy <- read_sav('NLSY97_long.sav')
names(nlsy) = c('id', 'r1235800', 'male', 'Black', 'Hispanic', 'White', 'year', 'fam_rou', 
                'time', 'age', 'race', 'M_fri', 'M_sch', 'F_fri', 'F_sch', 'puberty')

## Model A: Means only ##
model_a <- nlme(fam_rou ~ beta1 + d1i,
                data = nlsy,
                fixed = beta1~1,
                random = d1i~1,
                group = ~id,
                start = c(beta1 = 0),
                na.action = "na.omit")

summary(model_a)


## Model B: Unconditional linear growth ##
model_b <- nlme(fam_rou ~ (beta1 + d1i) + (beta2 + d2i)*time, 
                data = nlsy,
                fixed = beta1 + beta2~1, 
                random = d1i + d2i~1, 
                groups = ~id, 
                start = c(beta1 = 5, beta2 = 0), 
                na.action = "na.omit")
summary(model_b)

## Model C: Linear growth model with gender for both intercept and slope ##
Model_c <- nlme(fam_rou ~ (beta01 + beta11*male + d1i) + (beta02 + beta12*male + d2i)*time, 
                data = nlsy,
                fixed = beta01 + beta11 + beta02 + beta12~1, 
                random = d1i + d2i~1, 
                groups = ~id, 
                start = c(beta01 = 5, beta11 = 0, beta02 = 0, beta12 = 0), 
                na.action="na.omit")
summary(Model_c)


## Model D: Linear Growth model with M_sch Allowing the effect of a TV predictor to vary over time ##
Model_d <- nlme(fam_rou ~ (beta1+d1i) + (beta2+d2i)*(time) + (beta3*M_sch) + (beta4*M_sch*time), 
                data = nlsy,
                fixed = beta1 + beta2 + beta3 + beta4~1, 
                random = d1i + d2i~1, 
                groups = ~id, 
                start =c(beta1=5, beta2=0, beta3=0, beta4=0), 
                na.action = "na.omit")
summary(Model_d)


## Model E: Linear growth model with gender and M_sch ##
Model_e <- nlme(fam_rou ~ (beta01 + beta11*male + d1i) + (beta02 + beta12*male + d2i)*(time) + (beta03 + beta13*male + d3i)*(M_sch),
                data = nlsy,
                fixed = beta01 + beta11 + beta02 + beta12 + beta03 + beta13~1, 
                random = d1i + d2i + d3i~1, 
                groups = ~id, 
                start = c(beta01 = 5, beta02 = 0, beta03 = 0, beta11 = 0, beta12 = 0, beta13 = 0), 
                na.action="na.omit")

summary(Model_e)
