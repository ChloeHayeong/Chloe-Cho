install.packages("haven")
install.packages("foreign")
library(foreign)

#read all the datasets that are needed for this analysis
#dataset from ICPSR (public data)
#If you want to look at the dataset 
#here is the URL for it: https://www.icpsr.umich.edu/web/NACDA/studies/25502/datadocumentation



blood_pressure <- read.spss("C:/Users/chloe/OneDrive/Desktop/blood pressure.sav",
                            to.data.frma = TRUE, use.value.labels = FALSE)
alcohol <- read.spss("C:/Users/chloe/OneDrive/Desktop/alcohol use.sav",
                     to.data.frma = TRUE, use.value.labels = FALSE)
body_measurement<- read.spss("C:/Users/chloe/OneDrive/Desktop/body measurement.sav",
                             to.data.frma = TRUE, use.value.labels = FALSE)
cardio_level <- read.spss("C:/Users/chloe/OneDrive/Desktop/cardio level.sav",
                          to.data.frma = TRUE, use.value.labels = FALSE)
cholesterol <- read.spss("C:/Users/chloe/OneDrive/Desktop/chorlesterol.sav",
                         to.data.frma = TRUE, use.value.labels = FALSE)
demographics <- read.spss("C:/Users/chloe/OneDrive/Desktop/demographic.sav",
                          to.data.frma = TRUE, use.value.labels = FALSE)
diabetes <- read.spss("C:/Users/chloe/OneDrive/Desktop/diabetes.sav",
                      to.data.frma = TRUE, use.value.labels = FALSE)
disease <- read.spss("C:/Users/chloe/OneDrive/Desktop/disease.sav",
                     to.data.frma = TRUE, use.value.labels = FALSE)
drug_use <- read.spss("C:/Users/chloe/OneDrive/Desktop/drug use.sav",
                      to.data.frma = TRUE, use.value.labels = FALSE)
general_health <- read.spss("C:/Users/chloe/OneDrive/Desktop/general health.sav",
                            to.data.frma = TRUE, use.value.labels = FALSE)
gly <- read.spss("C:/Users/chloe/OneDrive/Desktop/gly.sav",
                 to.data.frma = TRUE, use.value.labels = FALSE)
insurance <- read.spss("C:/Users/chloe/OneDrive/Desktop/health insurance.sav",
                       to.data.frma = TRUE, use.value.labels = FALSE)
hearing <- read.spss("C:/Users/chloe/OneDrive/Desktop/hearing prob.sav",
                     to.data.frma = TRUE, use.value.labels = FALSE)
hepA <- read.spss("C:/Users/chloe/OneDrive/Desktop/hepA.sav",
                  to.data.frma = TRUE, use.value.labels = FALSE)
hepB <- read.spss("C:/Users/chloe/OneDrive/Desktop/hepB.sav",
                  to.data.frma = TRUE, use.value.labels = FALSE)
herp <- read.spss("C:/Users/chloe/OneDrive/Desktop/herp I and II.sav",
                  to.data.frma = TRUE, use.value.labels = FALSE)
kidney <- read.spss("C:/Users/chloe/OneDrive/Desktop/kidney.sav",
                    to.data.frma = TRUE, use.value.labels = FALSE)
pills <- read.spss("C:/Users/chloe/OneDrive/Desktop/pills over the counter.sav",
                   to.data.frma = TRUE, use.value.labels = FALSE)
glucose <- read.spss("C:/Users/chloe/OneDrive/Desktop/plasma glucose.sav",
                     to.data.frma = TRUE, use.value.labels = FALSE)
skin <- read.spss("C:/Users/chloe/OneDrive/Desktop/skin.sav",
                  to.data.frma = TRUE, use.value.labels = FALSE)

##Merging the sub-datasets into one dataset

library(haven)
data1 <- merge(demographics, alcohol, 
               by = "SEQN",
               all = TRUE)

data2 <- merge(data1, blood_pressure,
                by = "SEQN",
                all = TRUE)

data3 <- merge(data2, body_measurement,
               by = "SEQN",
               all = TRUE)

data4 <- merge(data3, cardio_level,
               by = "SEQN",
               all = TRUE)

data5 <- merge(data4, cholesterol,
               by = "SEQN",
               all = TRUE)

data6 <- merge(data5, diabetes,
               by = "SEQN",
               all = TRUE)

data7 <- merge(data6, disease,
               by = "SEQN",
               all = TRUE)

data8 <- merge(data7, drug_use,
               by = "SEQN",
               all = TRUE)

data9 <- merge(data8, general_health,
               by = "SEQN",
               all = TRUE)

data10 <- merge(data9, gly,
               by = "SEQN",
               all = TRUE)

data11 <- merge(data10, insurance,
               by = "SEQN",
               all = TRUE)

data12 <- merge(data11, hearing,
                by = "SEQN",
                all = TRUE)

data13 <- merge(data12, hepA,
                by = "SEQN",
                all = TRUE)

data14 <- merge(data13, hepB,
                by = "SEQN",
                all = TRUE)

data15 <- merge(data14, herp,
                by = "SEQN",
                all = TRUE)

data16 <- merge(data15, kidney,
                by = "SEQN",
                all = TRUE)

data17 <- merge(data16, pills,
                by = "SEQN",
                all = TRUE)

data18 <- merge(data17, glucose,
                by = "SEQN",
                all = TRUE)

data19 <- merge(data18, skin,
                by = "SEQN",
                all = TRUE)

##checking the the variables and the datset
View(data19)
summary(data19)

##removing some columns due to categorical vars

install.packages("dplyr")
library("dplyr")

reduced <- data19 %>% select(-c(RIAGENDR, RIDRETH1,DMDCITZN, DMDMARTL, HSD010,
                                URXUGC, URXUCL, HID030B,HIQ210, KIQ022, DED011, 
                                DED031, DED061, AUQ130,DMQMILIT, CVDFITLV, RXD300, 
                                BMXLEG, BMXCALF, BMXARML, BMXARMC, BMXTRI, BMXSUB,
                                HID010, HID030A, HID030A, HID030C, LBXHE2,RIDAGEMN,
                                HID030D, HID030E, HID040, LBXHA, LBXHBS, LBXHE1,
                                ALQ120U, DUQ100, DIQ010, LBXGLU,SEQN))

View(reduced)

##check missing data mechanism w/ little's MCAR test 

install.packages("naniar")
library(naniar)

mcar_test(reduced)

##mising data is not MAR
#checking missing data pattern 
install.packages("VIM")
library(VIM)
aggr(reduced)
nrow(na.omit(reduced))


##Will perform MI 
##starting multiple imputation 
install.packages("mice")
library(mice)

summary(reduced)
md.pattern(reduced)

#listwise deletion with by ordinary regression
reg.fit <- lm(LBXTC ~ PEASCTM1+BMXWT+BMXHT+BMXWAIST+BMXBMI+BMXTHICR+
                LBXTC+LBDHDL+LBXGH, data=reduced)
summary(reg.fit)

##4593 obs were deleted 
##Multiple imputation
imp.data <- mice(reduced, m=50)
imp.data

imp.datasets <- complete(imp.data,"long")
imp.datasets


##PCA
install.packages("corrr")
library('corrr')    

install.packages("ggcorrrplot")
library(ggcorrplot)

install.packages("FactoMineR")
library(FactoMineR)

colSums(is.na(imp.datasets))

##normalize the data
nor_data <- scale(imp.datasets)
head(nor_data)
View(nor_data)

##PCA
data.pca <- princomp(nor_data)
summary(data.pca)

data.pca$loadings[, 1:2]


##scree Plot
install.packages("factoextra")
library(factoextra)

fviz_eig(data.pca, addlables = TRUE)

##Biplot of the attribute
fviz_pca_var(data.pca, col.var = "black")

##contribution of each variable 
fviz_cos2(data.pca, choice = 'var', axes = 1:2)

##Biplot combined with cos2
fviz_pca_var(data.pca, col.var = "cos2", 
             gradient.cols = c("red", "blue", "green"),repel=TRUE)
             

##t-SNE
install.packages("Rtsne")
library(Rtsne)
View(nor_data)
nor_unique <- unique(nor_data)

matrix_tsne <- as.matrix(nor_unique[,1:11])
set.seed(12345)


tsne_out <- Rtsne(matrix_tsne) # Run TSNE

# Show the objects in the 2D tsne representation
##put the unique data into matrix format
##checking if the data is atomic vector
is.atomic(nor_unique)

##changin it to data frame
nor_uni_frame <- as.data.frame(t(nor_unique))
nor_uni_frame$ed

##geting t-sne plot 

plot(tsne_out$Y,col=nor_uni_frame$SEQN)



##UMAP projection

install.packages("umap")
library(umap)

install.packages("plotly")
library(plotly) 

umap.defaults

custom.settings = umap.defaults
custom.settings$n_neighbor = 5
custom.settings

ufinal.umap = umap(nor_data[, 1:10])
ufinal.umap
head(ufinal.umap$layout)

ufinal.data = imp.datasets[, grep("Sepal|Petal", colnames(imp.datasets))] 
ufinal.labels = imp.datasets[, ".id"] 
is.matrix(ufinal.data)

ufinal.matrix <- as.matrix.data.frame(ufinal.data)
is.matrix(ufinal.matrix)

ufinal.umap = umap(ufinal.matrix, n_components = 11, random_state = 30) 
layout <- nor.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, imp.datasets$.id) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~imp.datasets$.id, 
               colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', 
               mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='.id')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 
nor.umap = umap(imp.datasets, n_components = 3, random_state = 15) 
layout <- nor.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, nor.data$SEQN) 

fig2 <- plot_ly(final, x = ~X1, y = ~X2, z = ~X3, color = ~imp.datasets$.id, 
                colors = c('#636EFA','#EF553B','#00CC96')) 
fig2 <- fig2 %>% add_markers() 
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = '0'), 
                                     yaxis = list(title = '1'), 
                                     zaxis = list(title = '2'))) 

fig 


