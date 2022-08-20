###GWR & OLS
library(sp)
library(rgeos)
library(maptools) #load sp data
library(spdep)  
library(RColorBrewer)
library(GWmodel)   # GW model
library(gstat) 
library(raster)
library(MLmetrics)
library(rsq)
library(tidyverse)
#import data
load(file='C:\\Users\\asus\\Desktop\\code\\GWR&OLS\\LHP.rda')
LNHP <- lhGDF


#####Inspection data basics information and basic pre-work analysis######
LN.bou <- readShapePoly("C:\\Users\\asus\\Desktop\\code\\GWR&OLS\\LondonBorough\\LondonBorough",verbose = T,proj4string = CRS("+init=epsg:27700"))
#The spatial neighborhood of the point data£¨KNN£©
LNHPnb <- knn2nb(knearneigh(LNHP,k=4,longlat = TRUE)) 
LNHPnb_s <- make.sym.nb(LNHPnb)
plot(LNHP)
plot(nb2listw(LNHPnb_s),cbind(LNHP$X,LNHP$Y),pch=20)
#or
plot(nb2listw(LNHPnb_s),coordinates(LNHP),pch=20)
#moran's I 
col.W <- nb2listw(LNHPnb_s,style = "W")
moi <- moran(LNHP$PURCHASE,col.W,length(LNHP$PURCHASE),Szero(col.W))
moi

#####prepare data#####
ind <- sample(2,nrow(LNHP),replace = TRUE,prob = c(0.7,0.3))
train_data <- LNHP[ind==1,]
test_data <- LNHP[ind==2,]

####GWR & OLS model#####
#In the model alogrithms, there will be randomness in each run of the algorithm, so the outputs will slightly different each time
#To solve this problem, please run the each algorithm by many times (e.g. 10) and take the average value

#using the training data to find the optimal bandwidth
bw.gwr.train <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                         BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)


#using the bandwidth obtained before train to fit the training data
gwr.res_train <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                             BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                             BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                           data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train


# 
# y_true_train = gwr.res_train[["SDF"]]@data[["y"]]
# y_pre_train<-gwr.res_train[["SDF"]]@data[["yhat"]]
# R2_Score(y_pre_train, y_true_train)
# RMSE(y_pre_train, y_true_train)
# 

gwr.res_predict = gwr.predict (PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                                 BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                                 BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
                               data = train_data, predictdata = test_data,
                               bw=bw.gwr.train, kernel = "gaussian",adaptive = TRUE)

gwr.res_predict

y_true = test_data$PURCHASE

#########gwr metrics indicators######
gwr_prediction = gwr.res_predict$SDF$prediction

R2_gwr = R2_Score(gwr_prediction, y_true)
R2_gwr

RMSE_value_gwr = RMSE(gwr_prediction, y_true)
RMSE_value_gwr

#########ols metrics indicators######
ols.pred<-gwr.predict(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                        BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                        BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
                      data = train_data, predictdata = test_data, bw=Inf)


ols_predition = ols.pred$SDF$prediction

R2_ols = R2_Score(ols_predition, y_true)
R2_ols


RMSE_value_ols = RMSE(ols_predition, y_true)
RMSE_value_ols


