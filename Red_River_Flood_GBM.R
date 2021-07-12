library(ggplot2)
library(corrplot)
library(car) #VIF
library(ISLR) # Split data into train and test 
library(gbm) #for the Gradiant Boosting Machine 
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

#remove soil types variable
data = select(data, -Soil_types)

#Split dataset into 70% Training, 30% Testing data
smp_siz = floor(0.70*nrow(data))
set.seed(123)
train_ind = sample(seq_len(nrow(data)),size = smp_siz)
train =data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=data[-train_ind,] # creates the test dataset excluding the row numbers mentioned in train_ind


#######   Gradiant Boosting Machine code  #########

#source of code: https://www.r-bloggers.com/using-a-gbm-for-classification-in-r/

#FIRST PART: test for the optimal ntrees by creating training and testing data without Flood binary
# and then run an iterative fit GBM model. This model and optimal # of trees will be used for prediction

flooded = train$Flood_Binary
train_gbm = select(train, -flooded)#remove the flood binary column
test_gbm = select(test, -flooded)#remove the flood binary column
end_trn = nrow(train_gbm)

#combine the two datasets into one to make sure n=both train and test data have similar
#variable manipulations
all = rbind(train_gbm, test_gbm)
end = nrow(all)
ntrees = 2000

all = select(all, DEM_cut_rs, River_dens, Distance_t,
             Land_use_2,  Curve_Stud,  Aspect_Stu, Slope_Stud)

#I will say, how this guy wrote this code is weird. He creates this "all" dataset, which is just a combined
# version test and trained data, but like, why did they need to be combined? In the GBM model, you will see
# "x = all[1: end_trn,]" which is just the training data...but like...why not just insert the training data
# without train$Flood_Binary?
Gbm_model = gbm.fit(x = all[1: end_trn,], y = flooded, distribution = "bernoulli", n.trees = ntrees, shrinkage = 0.05,
                    interaction.depth = 3, n.minobsinnode = 10, nTrain = round(end_trn * 0.08), verbose = TRUE)

summary(Gbm_model)
gbm.perf(Gbm_model) # determines the best number of trees for the prediction model


#SECOND PART: Test
TestPrediction = predict(object = Gbm_model, newdata = all[(end_trn+1):end,], 
                         n.trees = gbm.perf(Gbm_model, plot.it = FALSE), type = "response")


# Define a prediction score of >=0.6 as a flood (1), below 0.6 is a non-flood (0) 
GBM_Flood = ifelse(TestPrediction>=0.6 , 1 , 0)


#Just to isually compare the test results with the flooded binary values of the test data
head(test$Flood_Binary, n = 20)
head(GBM_Flood, n = 20)

#Classification Accuracy
1 - sum(abs(test$Flood_Binary - GBM_Flood)) / length(GBM_Flood)

#Now for the AUC disgnostic
par(pty = "s")
roc(test$Flood_Binary, TestPrediction, plot = TRUE, legacy.axes = TRUE,
    xlab = "False Positive Percentage", ylab = "True Positive Percentage")



#######  Creating Maps  code  #########

#insert the full dataset
XY_fix <- read.csv("FullData.csv") 
fulldata <- read.csv("FullData_GBM_noSoil.csv") #need a .csv file with just the explanatory vairbles, no xy
attach(fulldata)
head(fulldata)


GBM_pred = predict(object = Gbm_model, newdata = fulldata, 
                          n.trees = gbm.perf(Gbm_model, plot.it = FALSE), type = "response")

# SVM MAP!!!!
xyz_GBM_bind <- rbind(XY_fix$POINT_X, XY_fix$POINT_Y, GBM_pred) #create xyz data
xyz_GBM <- data.frame(t(xyz_GBM_bind)) #switches the rows and column (so that columns are x,y,z)
colnames(xyz_GBM) <- c("x", "y", "Flood_suscept")
remove(xyz_GBM_bind) #Not needed anymore so let's just delete it

library(raster) #this masks out the 'select' funcion from dplyr, so be careful using this
GBM_e <- extent(xyz_GBM[,(1:2)])
GBM_r <- raster(GBM_e, ncol=1000, nrow=1000, crs= "+proj=longlat +datum=WGS84") #create raster with projection and rows/columns

GBM_r_new <- rasterize(xyz_GBM[,1:2], GBM_r, xyz_GBM[,3], fun=mean)

rgb.pal <- colorRampPalette(c("red","red1","red2","red4","royalblue4", "royalblue3","royalblue1"), space = "rgb")

png(paste("GBM_Flood_suscept",".png",sep=""))
plot(GBM_r_new, col=rgb.pal(200), xlab="Lon", ylab="Lat", main=paste("Flood Susceptibility in Red River Valley (GBM)"),
     legend.args=list(text=paste0("Flood_Suscept Index"), side=4, font=2, line=2.5, cex=0.8))
dev.off()