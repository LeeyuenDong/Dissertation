###GWRF & RF
library(MLmetrics)
library(GWmodel)      
library(plyr)         ## Data management
library(sp)           ## Spatial Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         ## plot
library(gridExtra)    ## Multiple plot
library(ggplot2)      ##  plotting
library(tidyverse)    ## data 
library(SpatialML)    ## geographically machine learning
library(tmap)
library(fs)
library(dplyr)

#Load Data
#Create training, validation and test data
grf_data <- read.csv("C:\\Users\\asus\\Desktop\\code\\GWRF&RF\\london_house_price_data_GWRF.csv", header = TRUE)  %>% 
  dplyr::select(PURCHASE, FLOORSZ, TYPEDETCH, TYPETRRD, TYPEFLAT, 
                BLDPWW1, BLD60S, BLD70S, BLD80S, BLD90S, BATH2,  
                BEDS2, GARAGE1, CENTHEAT, PROF, BLDINTW, X, Y, GSS_CODE, Type)


set.seed(1)

# prepare data
grf_test<-grf_data %>% 
  filter(Type == 3 & PURCHASE > 0)
grf_train<-grf_data %>%
  filter(Type == 1 & PURCHASE > 0)

#Scale covariates
grf_test[, 2:16] = scale(grf_test[, 2:16])
grf_train[, 2:16] = scale(grf_train[, 2:16])

#GWRF function
gwrf <- function(bw, ntree){

Coords<-grf_train[ ,17:18]
grf.model <- grf(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                     BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                     BEDS2 + GARAGE1 + CENTHEAT + PROF + BLDINTW, 
                   dframe=grf_train,
                   
                   bw= bw,             # a positive number, in the case of an "adaptive kernel" or a real in the case of a "fixed kernel".
                   ntree= ntree,       #the number of trees
                   mtry = 10,          # n integer referring to the number of trees to grow for each of the local random forests.
                   kernel="adaptive",  # yhe kernel to be used in the regression. Options are "adaptive" or "fixed".
                   forests = TRUE,     # a option to save and export (TRUE) or not (FALSE) all the local forests
                   importance = T,
                   coords=Coords)      # a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations
  
  # Prediction
  # Creat a data-frame
FIPS.xy<-grf_test[,17:19]
FIPS.xy3<-grf_train[,17:19]
FIPS.xy$PURCHASE_test<-grf_test[,1]

FIPS.xy3$PURCHASE_train<-grf_train[,1]
  
  # training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)

  # Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  
  #Local result
R2_train = R2_Score(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
RMSE_value_train= RMSE(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
cat("RF train: " , "R2:",R2_train, "   ", "RMSE:",RMSE_value_train, "    ")

R2_test = R2_Score(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
RMSE_value_test= RMSE(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
cat("RF test: " , "R2:",R2_test, "   ", "RMSE:",RMSE_value_test)
}

#############Please note that if run many different combinations of code simultaneously may cause the computer to crash################
#In the RF and GWRF model, there will be randomness in each run of the algorithm, so the outputs will slightly differenteach time
#To solve this problem, please run the each algorithm by many times (e.g. 10) and take the average value

######GWRF#####

#ntree = 100
gwrf(bw= 20,ntree= 100)

gwrf(bw= 30,ntree= 100)

gwrf(bw= 50,ntree= 100)

gwrf(bw= 100,ntree= 100)

gwrf(bw= 150,ntree= 100)

gwrf(bw= 200,ntree= 100)

#ntree = 200
gwrf(bw= 20,ntree= 200)

gwrf(bw= 30,ntree= 200)

gwrf(bw= 50,ntree= 200)

gwrf(bw= 100,ntree= 200)

gwrf(bw= 150,ntree= 200)

gwrf(bw= 200,ntree= 200)

#ntree = 500
gwrf(bw= 20,ntree= 500)

gwrf(bw= 30,ntree= 500)

gwrf(bw= 50,ntree= 500)

gwrf(bw= 100,ntree= 500)

gwrf(bw= 150,ntree= 500)

gwrf(bw= 200,ntree= 500)




######RF (Global result)#####
#####RF funtion#####
rf <- function(ntree){
  
Coords<-grf_train[ ,17:18]
grf.model <- grf(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                     BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                     BEDS2 + GARAGE1 + CENTHEAT + PROF + BLDINTW, 
                   dframe=grf_train,
                   
                   bw= 20,             # in the RF model, bw wil not impact on the result, any value of bw is suitable. We use 20 here.
                   ntree= ntree, 
                   mtry = 10,           # n integer referring to the number of trees to grow for each of the local random forests.
                   kernel="adaptive",  # yhe kernel to be used in the regression. Options are "adaptive" or "fixed".
                   forests = TRUE,     # a option to save and export (TRUE) or not (FALSE) all the local forests
                   importance = T,
                   coords=Coords)      # a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations
  
  # Prediction
  # Creat a data-frame
FIPS.xy<-grf_test[,17:19]
FIPS.xy3<-grf_train[,17:19]
FIPS.xy$PURCHASE_test<-grf_test[,1]
FIPS.xy3$PURCHASE_train<-grf_train[,1]
  
  # training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
  
  # Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
  
#Local result
R2_train = R2_Score(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
RMSE_value_train= RMSE(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
cat("RF train: " , "R2:",R2_train, "   ", "RMSE:",RMSE_value_train, "    ")


R2_test = R2_Score(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
RMSE_value_test= RMSE(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
cat("RF test: " , "R2:",R2_test, "   ", "RMSE:",RMSE_value_test)
}

#####three different ntree outputs####
rf(ntree = 100)
rf(ntree = 200)
rf(ntree = 500)



