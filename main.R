# Names: Lazaros Spaias, Christos Sotiropoulos
# Lesson 8: Exercise

library (raster)
load("AdvancedRasterAnalysis/data/GewataB1.rda")
load("AdvancedRasterAnalysis/data/GewataB2.rda")
load("AdvancedRasterAnalysis/data/GewataB3.rda")
load("AdvancedRasterAnalysis/data/GewataB4.rda")
load("AdvancedRasterAnalysis/data/GewataB5.rda")
load("AdvancedRasterAnalysis/data/GewataB7.rda")
load("AdvancedRasterAnalysis/data/vcfGewata.rda")
load("AdvancedRasterAnalysis/data/trainingPoly.rda")

#Remove cloud and water flags
vcfGewata[vcfGewata > 100] <- NA


#make brisk layer
gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)

#calculate reflectance percentage
gewata <- calc(gewata, fun=function(x) x / 10000)

#add layer
covs <- addLayer(gewata,vcfGewata)

names(covs) <- c("band1","band2", "band3", "band4", "band5", "band7", "VCF")
class(covs)

#See the correlation between the Landsat bands and the Vcf dataset. 
#The highest correlation is between bands 7 and 3 and the vcf dataset corellates best with bands 7 and 3.
pairs(covs)

#create a linear model
valuetable <- getValues(covs)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)
model <-lm(VCF ~ band1 + band2 + band3 + band4+ band5 + band7, data = valuetable)
summary(model)
#band 3 and 1 have the lowest Std Error meaning that the distance from the fitted line is smallest
#than the other bands and therefore more important in predicting tree cover

#plot the predicted tree cover raster and compare with the original VCF raster
predictForRaster <- predict(covs, model = model, na.rm = TRUE)
names(predictForRaster) <- "PredVcf"
par(mfrow = c(2,2))
plot(predictForRaster, main = "Predicted tree raster", zlim = c(0,100) )
plot(vcfGewata, main = "Vcf raster")
hist(predictForRaster)
hist(vcfGewata)


# RMSE
rmse <- sqrt(cellStats((vcfGewata-predictForRaster)^2, stat = mean))
rmse
#The rmse is 8.606

#find out the RMSE for each class
#Create a brick that contains the original and predicted values
simobs <- brick(vcfGewata, predictForRaster)
#transfer values to a raster and add column class to training poly data
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, predictForRaster, field = "Class")
zones <- zonal(simobs, classes, fun = mean)

#rmse formula
class1rmse <- sqrt(mean((zones[1,2]-zones[1,3])^2 )) #rmse for class cropland is 3.960
class2rmse <- sqrt(mean((zones[2,2]-zones[2,3])^2 )) #rmse for class forest 2 is 2.315
class3rmse <- sqrt(mean((zones[3,2]-zones[3,3])^2 )) #rmse for class wetland is 6.907

#Test
aaa
