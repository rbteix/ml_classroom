###############################################################################################################
# Setup environment
###############################################################################################################

if (!"xgboost" %in% rownames(installed.packages())) # if xgboost does not exists install it
    install.packages("xgboost")

if (!"pROC" %in% rownames(installed.packages())) # if pROC does not exists install it
    install.packages('pROC')

library(pROC) 
library(xgboost)
library(caret)# for confusionmatrix function
library(e1071)#for confusionmatrix function
library(Ckmeans.1d.dp)# for xgb.ggplot.importance
 # Defining root directory containg data.csv

setwd("E:/Documentos/Mestrado/Projeto")


###############################################################################################################
# Loading dataset
###############################################################################################################

data.ens <- read.csv("data.ens.csv", sep = ",", header=T)
###############################################################################################################
# Split dataset into train and test. 75% for train and 25% for test. 
######################################################

smp_size <- floor(0.75 * nrow(data.ens))

set.seed(123) # Defining a random seed to get the same results for all executions

train.index <- sample(seq_len(nrow(data.ens)), size = smp_size)

x.train <- as.matrix(data.ens[train.index, ]$Values)
y.train <- as.matrix(data.ens[train.index, ]$KMEANS - 1)

x.test  <- as.matrix(data.ens[-train.index, ]$Values)
y.test  <- as.matrix(data.ens[-train.index, ]$KMEANS - 1)

###############################################################################################################
# Training a XGBoost model
###############################################################################################################

dtrain <- xgb.DMatrix(data = x.train, label = y.train) # building input respecting xbg requirements

model <- xgboost(data = dtrain, num_class = length(unique(y.train)),
                     max_depth = 100, eta = 1, nthread = 5, 
                     early_stopping_rounds = 15,
                     nrounds = 100, objective = "multi:softmax")

y.hat <- predict(model, x.test)

x.tes <- (data.ens[-train.index, ]$Date)
x.tes1 <- (data.ens[-train.index, ]$Values)
x.tes = as.data.frame(x.tes)
x.tes[, "v"] = c(x.tes1)
colnames(x.tes) = c("Date", "V")
y.hat = as.data.frame(y.hat)
x.tes = as.data.frame(x.tes)

windows()
xg.plot <- ggplot( x.tes, aes(A, V, color = y.hat)) +geom_point()
xg.plot + scale_color_gradientn(colours = rainbow(5))


windows()
xg.plot <- ggplot( x.tes, aes(x.tes[,1], V ,  color = y.hat)) +geom_line()
xg.plot + scale_color_gradientn(colours = rainbow(5))


###############################################################################################################
# Evaluating results
###############################################################################################################

roc.multi <- multiclass.roc(y.test + 1, y.hat + 1)
windows()
par(mfrow=c(2,3)) 

rs <- roc.multi[['rocs']]
for (i in 1:5) {
    plot.roc(rs[[i]])
    legend("bottomright",legend=paste('Class', i))
}

