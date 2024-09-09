library(here)
library(mltools)
library(keras)
library(tfdatasets)
library(data.table)
library(dplyr)
library(foreign)
library(spatial)
library(gstat)
library(sp)
library(raster)
library(dplyr)
library(tidyverse)
library(yaImpute)
library(randomForest)
library(forcats)
library(gower)

setwd("G:\\Workspace\\carbonrisk")

options("scipen"=100, "digits"=4)

xtable.all <- read.dbf("x_table_DWM.dbf")

dwm.uniqueplots <- read.csv("TM2014_dwm_uniqueplot_remove_NAs.csv")
mall <- xtable.all

mall.orig <- mall
fortypcd.vec <- as.factor(xtable.all$FORTYPCD)

p4s.latlong <- CRS("+proj=longlat +datum=NAD83") 
sptemp <- SpatialPoints(data.frame(x = mall$ACTUAL_LON, y = mall$ACTUAL_LAT), proj4string = p4s.latlong)
setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\target_data_reclassified_final\\z04")
flist.tif <- Sys.glob("*.tif")
raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)
setwd("G:\\Workspace\\carbonrisk")
xytemp <- spTransform(sptemp, CRS(p4s.albers))
aspect.temp <- mall$ASPECT
rad.temp <- (pi/180)*aspect.temp
northing.temp <- cos(rad.temp)
easting.temp <- sin(rad.temp)
mall$easting <- easting.temp
mall$northing <- northing.temp
mall$pointx <- coordinates(xytemp)[,1]
mall$pointy <- coordinates(xytemp)[,2]
median.x <- median(mall$pointx)
mall.orig <- mall

##Dividing locations between east and west, this value works well but if something else makes sense go for it
cutoff <- -1500000
mall.west <- mall[mall$pointx < cutoff,]
mall.east <- mall[mall$pointx >=cutoff,]
mall <- mall.west
mall <- data.frame(mall)
#mall$FLDSZCD <- as.factor(mall$FLDSZCD)
mall$FLDTYPCD  <- as.factor(mall$FORTYPCD )
mall$disturb_code  <- as.factor(mall$disturb_co )

###Remove bad plots

badplots <- (c("31359775010690", "291609573489998", "145805440010854"))

allcns <- as.character(xtable.all$X_table_fi)
subcns <- as.character(mall$X_table_fi)

epp <- read.csv("east-predicted-plots.csv")
epp.cns <- as.character(epp$PredCN)

##Dealing with no data values
if(is.any(epp.cns)){
  badrows <- allcns %in% badplots 
  
  
  nbadrows <- length(badplots)
  whichrow <- rep(NA, nbadrows)
  for(i in 1:nbadrows)
  {
    curbadrow <- which(epp.cns == badplots[i])
    if(length(curbadrow) > 0)
    {
      whichrow[i] <- curbadrow
    }
    
  }
}

###Predictors:
# 
# balive, 
# badead, 
# numberlive, 
# numberdead,
# FLDSZCD,
# pcdlumped,
# FLDAGE,
# SLOPE,
# ELEV,
# PARI,
# PPTI,
# RELHUMI,
# TMAXI,
# TMINI,
# VPDI,
# canopy_cover,
# canopy_height,
# pointx,
# pointy,
# northing, 
# easting  ,



x <- mall$FLDTYPCD
na.pcd <- is.na(x)
mall <- mall[as.logical(1-na.pcd),]

x <- mall$FLDTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped <- x %>% fct_lump_n(30)
barplot(table(pcd.lumped))
mall$pcdlumped <- pcd.lumped
X.tibble <- as_tibble(mall)
X.tibble <- X.tibble%>% dplyr::select(
  
 # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
 # FLDSZCD,
  pcdlumped,
#  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting )

Y.tibble <- as_tibble(mall)

Y.tibble.fwdsmdry <- Y.tibble%>% dplyr::select(
  FWD_SM_DRY)

Y.tibble.fwdmddry <- Y.tibble%>% dplyr::select(
  FWD_MD_DRY)

Y.tibble.fwdlgdry <- Y.tibble%>% dplyr::select(
  FWD_LG_DRY)

Y.tibble.duff <- Y.tibble%>% dplyr::select(
  DUFF_BIOMA)

Y.tibble.litter <- Y.tibble%>% dplyr::select(
  LITTER_BIO)

Y.tibble.cwd <- Y.tibble%>% dplyr::select(
  CWD_DRYBIO)

# FWD_MD_DRYBIO_UNADJ,
# FWD_LG_DRYBIO_UNADJ ,
# DUFF_BIOMASS ,
# LITTER_BIOMASS,
# FLDTYPCD,
# disturb_code_bin,
# disturb_year,
# fwd_bio_sum,
# CWD_DRYBIO_UNADJ





#FWD_SM
Y.df <- data.frame(Y.tibble.fwdsmdry)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id

dim(train.x)
train.x <- train.x[,1:16]
test.x <- test.x[,1:16]

yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title('FWD_SM')
cor(test.y[,1], Y.pred)

X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.fwdsm <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.fwdsm, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "FWD-SM-PREDCNS.csv", sep = ",", row.names = F)


#FWD_MD
Y.df <- data.frame(Y.tibble.fwdmddry)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id
yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title('FWD_MD')
cor(test.y[,1], Y.pred)



X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.fwdmd <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.fwdmd, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "FWD-MD-PREDCNS.csv", sep = ",", row.names = F)




#FWD_LG
Y.df <- data.frame(Y.tibble.fwdlgdry)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id
yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title('FWD_LG')
cor(test.y[,1], Y.pred)


X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.fwdlg <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.fwdlg, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "FWD-LG-PREDCNS.csv", sep = ",", row.names = F)




#DUFF
Y.df <- data.frame(Y.tibble.duff)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id
yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title("DUFF")
cor(test.y[,1], Y.pred)


X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.duff <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.duff, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "DUFF-PREDCNS.csv", sep = ",", row.names = F)



#Litter
Y.df <- data.frame(Y.tibble.litter)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id
yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title("LITTER")
cor(test.y[,1], Y.pred)


X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.litter <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.litter, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "LITTER-PREDCNS.csv", sep = ",", row.names = F)



#CWD_DRYBIO_UNADJ
Y.df <- data.frame(Y.tibble.cwd)
Y.df[,1] <- as.numeric(as.character(Y.df[,1]))
X.df <- data.frame(X.tibble)
X.df$BALIVE <- as.numeric(as.character(X.df$BALIVE))
X.df$id <- 1:nrow(X.df)
alldf <- cbind(X.df, Y.df)
train <- alldf %>% dplyr::sample_frac(.75)
test  <- dplyr::anti_join(alldf, train, by = 'id')
train.x <- train[,1:17]
train.y <- train[,18]
test.x <- test[,1:17]
test.y <- test[,18]
test.y <- data.frame(test.y)
train.y <- data.frame(train.y)
rownames(train.x) <- train.x$id
rownames(train.y) <- train.x$id
rownames(test.x) <- test.x$id
rownames(test.y) <- test.x$id
yai.mod <- yai(x = train.x, y = train.y, method = "randomForest", ntree = 200)
X.df.pred <- X.df
rownames(X.df.pred) <- paste("pred-", rownames(X.df))
yai.imputed <- newtargets(yai.mod, test.x)
trgrows <- yai.imputed$neiIdsTrgs
Y.pred <- train.y[trgrows,]
plot(test.y[,1], Y.pred)
title("CWD")

cor(test.y[,1], Y.pred)

X.df.final <- X.df[,1:16]
X.df.final <- X.df.final[,-c(1,2)]

yai.all.cwd <- yai(x = X.df.final, y = Y.df, method = "randomForest", ntree = 200)

###Predicting for missing plots

X.pred.west <- as_tibble(mall.west)
x <- X.pred.west$FORTYPCD
na.pcd <- is.na(x)
length(table(x))
x %>% table()
#> .
#>  A  B  C  D  E  F  G  H  I 
#> 40 10  5 27  1  1  1  1  1 
x %>% fct_lump_n(50) %>% table()

pcd.lumped.west <- x %>% fct_lump_n(30)

X.pred.west$pcdlumped <- pcd.lumped.west
X.pred.west <-  X.pred.west%>% dplyr::select(
  # balive, 
  BALIVE, 
  #numberlive, 
  #numberdead,
  # FLDSZCD,
  pcdlumped,
  #  FLDAGE,
  SLOPE,
  ELEV,
  PARI,
  PPTI,
  RELHUMI,
  TMAXI,
  TMINI,
  VPDI,
  canopy_cov,
  canopy_hei,
  pointx,
  pointy,
  northing, 
  easting)

rnames.west <- rownames(X.pred.west)
rnames.west <- paste("West-", rnames.west)
X.pred.df.west <- data.frame(X.pred.west, stringsAsFactors = FALSE)
X.pred.ba <- as.character(X.pred.df.west$BALIVE)
X.pred.ba[is.na(X.pred.ba)] <- 0
X.pred.ba.temp <- as.numeric(X.pred.ba)
X.pred.df.west$BALIVE <- X.pred.ba.temp
X.pred.ba[is.na(X.pred.ba.temp)] <- 0
rownames(X.pred.df.west) <- mall.west$X_table_fi
fsw <- newtargets(yai.all.cwd, X.pred.df.west)


mall.out.pred <- mall[as.numeric(fsw$neiIdsTrgs ),]
out.cn <- mall.out.pred$X_table_fi
pred.df.west.out <- data.frame(obscn = mall.west$X_table_fi, predcn = out.cn)
write.table(pred.df.west.out, "CWD-PREDCNS.csv", sep = ",", row.names = F)




head(yai.imputed$yRefs)

plot(as.numeric(as.character(Y.tibble[,1]$FWD_SM_DRY)), yai.imputed$yRefs$FWD_SM_DRY)
##Want to predict:
# FWD_SM_DRYBIO_UNADJ (1-hour fuels),
# FWD_MD_DRYBIO_UNADJ (10-hour fuels), 
# FWD_LG_DRYBIO_UNADJ (100-hour fuels),
# CWD_DRYBIO_UNADJ (1000-hr fuels)
# "DUFF_BIO" 
# "LITTER_BIO"
# 
