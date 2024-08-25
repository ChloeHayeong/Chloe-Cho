library(haven)
ALDA_Unemployment <- read_sav("C:/Users/minjkim/Box Sync/Class/Longi2020/Week07_Linear growth model_TVC/ALDA_Unemployment.sav")
View(ALDA_Unemployment)

names(ALDA_Unemployment)<-c('id','month','cesd','unemp')
summary(ALDA_Unemployment)

library(nlme)

#unconditional growth
cesd.tvc.nlme<-nlme(cesd~(beta_1+d_1i)+(beta_2+d_2i)*month,
                    data=ALDA_Unemployment,
                    fixed = beta_1+beta_2~1,
                    random = d_1i+d_2i~1,
                    group = ~id,
                    start = c(20, 5),
                    na.action = na.omit)
summary(cesd.tvc.nlme)

#unemp main effect only
cesd.tvc.nlme2<-nlme(cesd~(beta_1+d_1i)+(beta_2+d_2i)*month+(beta_3)*unemp,
                    data=ALDA_Unemployment,
                    fixed = beta_1+beta_2+beta_3~1,
                    random = d_1i+d_2i~1,
                    group = ~id,
                    start = c(20, 5, 5),
                    na.action = na.omit)
summary(cesd.tvc.nlme2)

#unemp interaction effect model
cesd.tvc.nlme3<-nlme(cesd~(beta_1+d_1i)+(beta_2+d_2i)*month+(beta_3)*unemp+(beta_4)*month*unemp,
                     data=ALDA_Unemployment,
                     fixed = beta_1+beta_2+beta_3+beta_4~1,
                     random = d_1i+d_2i~1,
                     group = ~id,
                     start = c(20, 5, 5, 5),
                     na.action = na.omit)
summary(cesd.tvc.nlme3)
