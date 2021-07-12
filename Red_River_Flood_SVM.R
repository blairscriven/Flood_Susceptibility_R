library(ggplot2)
library(corrplot)
library(car) #VIF
library(e1071) # Contains SVM
library(ISLR) # Split data into train and test
library(dplyr)# for removing columns in dataset; select function
library(pROC) #how we determine AUC

#Set Working Directory
setwd("")

#Bring in Flood Data (test/train)
data <- read.csv("Red_River_Flood_Samples.csv") #This reads your csv file into R and names it "data"
attach(data)
head(data)

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



#######   Supervised Vector Machine code  #########

#Source of code: https://www.youtube.com/watch?v=pS5gXENd3a4
svm_model <- svm(Flood_Binary ~ DEM_cut_rs + Soil_types  + River_dens + Distance_t +
                   Land_use_2 + Curve_Stud + Aspect_Stu + Slope_Stud, data = train) # should be the training data
summary(svm_model)
pred <- predict(svm_model, test)
pred_cut = cut(pred, breaks = 5) #seq(0, 1, by = 10))
tab <- table(Predicted = pred_cut, Actual = test$Flood_Binary)
head(tab)

# Define a prediction score of >=0.6 as a flood, below 0.6 is a non-flood 
SVM_Flood = ifelse(pred>=0.6 , 1 , 0)

#Classification Accuracy (Kappa)
1 - sum(abs(test$Flood_Binary - SVM_Flood)) / length(SVM_Flood)

#Now for the AUC disgnostic
par(pty = "s")
roc(test$Flood_Binary, pred, plot = TRUE, legacy.axes = TRUE, 
    xlab = "Flase Positive Percentage", ylab = "Tre Positive Percentage")

#######  Creating Map  code  #########

#insert the full dataset
fulldata <- read.csv("FullData.csv") 
attach(fulldata)
head(fulldata, 10)

pred_full_SVM <- predict(svm_model, fulldata)

# SVM MAP!!!!
xyz_SVM_bind <- rbind(fulldata$POINT_X, fulldata$POINT_Y, pred_full_SVM) #create xyz data
xyz_SVM <- data.frame(t(xyz_SVM_bind)) #switches the rows and column (so that columns are x,y,z)
colnames(xyz_SVM) <- c("x", "y", "Flood_suscept")
remove(xyz_SVM_bind) #Not needed anymore so let's just delete it

library(raster) #this masks out the 'select' funcion from dplyr, so be careful using this
SVM_e <- extent(xyz_SVM[,(1:2)])
SVM_r <- raster(SVM_e, ncol=1334, nrow=1337, crs= "+proj=longlat +datum=WGS84") #create raster with projection and rows/columns

SVM_r_new <- rasterize(xyz_SVM[,1:2], SVM_r, xyz_SVM[,3], fun=mean)

rgb.pal <- colorRampPalette(c("red","red1","red2","red4","royalblue4", "royalblue3","royalblue1"), space = "rgb")

png(paste("SVM_Flood_suscept",".png",sep=""))
plot(SVM_r_new, col=rgb.pal(200), xlab="Lon", ylab="Lat", main=paste("Flood Susceptibility in Red River Valley (SVM)"),
     legend.args=list(text=paste0("Flood_Suscept Index"), side=4, font=2, line=2.5, cex=0.8))
dev.off()
