library(ggplot2)
library(corrplot)
library(car) #VIF
library(ISLR) # Split data into train and test 
library(dplyr)# for removing columns in dataset; select function
library(pROC) #how we determine AUC
library(sp)#spatial coordinates
library(rpart) #Contains CART
library(rpart.plot)


#Set Working Directory
setwd("")

#Bring in Flood Data (test/train)
data <- read.csv("Red_River_Flood_Samples.csv") #This reads your csv file into R and names it "data"
attach(data)
head(data)

##reads coordinates from datafile
coordinates<-SpatialPoints(data[,c("POINT_X", "POINT_Y")])

#Check Pearson's Correlation!
correlation_P <- cor(data[,3:10], use="complete.obs", method="pearson")
write.csv(correlation_P, "correlation_P.csv")
head(correlation_P) #have a peek!
pdf("Pearson_cor.pdf")
corrplot(correlation_P, type = "upper", order = "hclust", method = "color",
         tl.cex = 0.7, tl.col = "black", tl.srt = 45)
dev.off()

#Check Variance Inflation Factors (VIF) by first creating a simple linear regression model
model_simpler <- lm(Flood_Binary ~ DEM_cut_rs + Soil_types  + River_dens + Distance_t +
                      Land_use_2 + Curve_Stud + Aspect_Stu + Slope_Stud, data = data)
vif(model_simpler)#Determine Variance Inflation Factor
sqrt(vif(model_simpler)) > 2 #Test if the VIF value is too high (False is a pass)


#Split dataset into 70% Training, 30% Testing data
smp_siz = floor(0.70*nrow(data))
set.seed(123)
train_ind = sample(seq_len(nrow(data)),size = smp_siz)
train =data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=data[-train_ind,] # creates the test dataset excluding the row numbers mentioned in train_ind



#######  Classification and Regression Tree  code  #########

FloodTrees = rpart(Flood_Binary ~ DEM_cut_rs  + River_dens + Distance_t +
                     Land_use_2 + Curve_Stud + Aspect_Stu + Slope_Stud, data = train, 
                   method = "poisson") # should be the training data

#Next 3 lines are to determine the optimal cp (complexity parameter) using the cp value with the least Cross-validation error
xmat <- xpred.rpart(FloodTrees)
xerr <- (xmat - train$Flood_Binary)^2
apply(xerr, 2, sum) # cross-validated error estimate

#Prune trees using optimal cp
FloodTrees_prune = prune(FloodTrees, cp = 0.01)
prp(FloodTrees_prune)

#Prediction on the test data
PredictCART = predict(FloodTrees_prune, newdata = test, type = "vector")
head(PredictCART)

#Classification Accuracy
1 - sum(abs(test$Flood_Binary - PredictCART)) / length(PredictCART)

#Now for the AUC disgnostic
par(pty = "s")
roc(test$Flood_Binary, PredictCART, plot = TRUE, legacy.axes = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage")



#######  Creating Map  code  #########

#insert the full dataset
fulldata <- read.csv("FullData.csv") 
attach(fulldata)
head(fulldata, 10)

PredictCART = predict(FloodTrees_prune, newdata = fulldata, type = "vector")

# CART MAP!!!!
xyz_CART_bind <- rbind(fulldata$POINT_X, fulldata$POINT_Y, PredictCART) #create xyz data
xyz_CART <- data.frame(t(xyz_CART_bind)) #switches the rows and column (so that columns are x,y,z)
colnames(xyz_CART) <- c("x", "y", "Flood_suscept")
remove(xyz_CART_bind) #Not needed anymore so let's just delete it

library(raster) #this masks out the 'select' funcion from dplyr, so be careful using this
CART_e <- extent(xyz_CART[,(1:2)])
CART_r <- raster(CART_e, ncol=1000, nrow=1000, crs= "+proj=longlat +datum=WGS84") #create raster with projection and rows/columns

CART_r_new <- rasterize(xyz_CART[,1:2], CART_r, xyz_CART[,3], fun=mean)

rgb.pal <- colorRampPalette(c("red","red1","red2","red4","royalblue4", "royalblue3","royalblue1"), space = "rgb")

png(paste("CART_Flood_suscept",".png",sep=""))
plot(CART_r_new, col=rgb.pal(200), xlab="Lon", ylab="Lat", main=paste("Flood Susceptibility in Red River Valley (CART)"),
     legend.args=list(text=paste0("Flood_Suscept Index"), side=4, font=2, line=2.5, cex=0.8))
dev.off()
