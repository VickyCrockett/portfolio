
#Import, view and gather a quick insight of the data
Mushrooms = read.csv("Mushrooms.csv")
View(Mushrooms)
summary(Mushrooms)

#Check for missing values (None found, phew!)
Mushrooms |>
  is.na() |>
  as.numeric() |>
  sum()

Mushrooms$class |>
  is.na() |>
  as.numeric() |>
  sum()

#The above is equivalent to:
#sum(as.numeric(is.na(Mushrooms)))

class <- as.factor(Mushrooms$class)

#Target column is 'class'. This is edible, e, or poisonous, p.
#Other columns are characters, to see how many levels:
for ( i in colnames(Mushrooms) ){
  Mushrooms[[i]] <- as.factor(Mushrooms[[i]])
  print(i)
  print(levels(Mushrooms[[i]]))
}

#Aside
Mushrooms$class
#is same as
Mushrooms[['class']]

#Convert binary columns and build data frame for modelling
for ( i in colnames(Mushrooms) ){
  if ( length(levels(Mushrooms[[i]])) == 2 ){
    print(i)
    print(levels(Mushrooms[[i]]))
    Mushrooms[[i]] <- as.logical((as.numeric(Mushrooms[[i]])-1))
  }
}

#Swap some of the T/F's for intuition later
Mushrooms$class <- !Mushrooms$class #edible => True
Mushrooms$gill.attachment <- !Mushrooms$gill.attachment #gills attached => True
Mushrooms$gill.spacing <- !Mushrooms$gill.spacing #crowded gills => False
Mushrooms$gill.size <- !Mushrooms$gill.size #broad gills => True
Mushrooms$stalk.shape <- !Mushrooms$stalk.shape #enlarging (wider at the base) => True

#Clearer picture now:
summary(Mushrooms)
#There are many results across the majority of columns that have small samples for their subgroups and may give weaker classification scores.
#e.g. c and s from the cap.shape column, representing conical or sunken cap-shapes

library(stringr)
CapShape <- str_replace(Mushrooms$cap.shape,"b", "Bell")
CapShape <- str_replace(CapShape,"c", "Conical")
CapShape <- str_replace(CapShape,"f", "Flat")
CapShape <- str_replace(CapShape,"k", "Knobbed")
CapShape <- str_replace(CapShape,"s", "Sunken")
CapShape <- str_replace(CapShape,"x", "Convex")
CapShape <- as.factor(CapShape)

plot(CapShape, col = 'pink', ylab = 'Mushroom Cap Shape')

#Create test and train sets
set.seed(1)
MushroomLength <- nrow(Mushrooms)
TF <- sample(c(TRUE,FALSE), MushroomLength, replace = TRUE, prob = c(0.8,0.2))
MushTrain <- Mushrooms[TF,]
MushTest <- Mushrooms[!TF,]

#Decision tree
library(rpart)
library(rpart.plot)


DTInfoModel <- rpart(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                 method = "class",
                 data = MushTrain,
                 control = rpart.control(maxdepth=5, minsplit=1, cp=0.001),
                 parms=list(split='information')
                 )
summary(DTInfoModel)

rpart.plot(DTInfoModel, type=4, extra=1, clip.right.labs = FALSE, varlen = 0, faclen = 0)

DTInfoPred <- predict(DTInfoModel, newdata = MushTest[,-1], type = "class")

DTInfoPredBool <- !as.logical(as.numeric(DTInfoPred)-1)
ActualBool <- !as.logical(as.numeric(as.factor(MushTest$class))-1)

#True positives
TP <- DTInfoPredBool & ActualBool
TP <- sum(TP)
#True negatives
TN <- (DTInfoPredBool == FALSE) & (ActualBool == FALSE)
TN <- sum(TN)
#False positives
FP <- (DTInfoPredBool == TRUE) & (ActualBool == FALSE)
FP <- sum(FP)
#False negatives
FN <- (DTInfoPredBool == FALSE) & (ActualBool == TRUE)
FN <- sum(FN)

Accuracy <- (100*(TP+TN))/(TP+TN+FP+FN)
TPR <- TP/(TP+FN) #Recall
FPR <- FP/(FP+TN)
FNR <- FN/(TP+FN)
Precision <- TP/(TP+FP)

#Logistic regression
LogTrain <- MushTrain
LogTest <- MushTest

#Target column is 'class'. This is edible, e, or poisonous, p.
for ( i in colnames(LogTrain) ){
  LogTrain[[i]] <- as.factor(LogTrain[[i]])
  LogTest[[i]] <- as.factor(LogTest[[i]])
}

LogTrain$class <- as.logical((as.numeric(LogTrain$class)-1))
LogTrain$class <- !LogTrain$class #edible => True
LogTrain$class <- as.numeric(LogTrain$class) #1 => edible

LogTest$class <- as.logical((as.numeric(LogTest$class)-1))
LogTest$class <- !LogTest$class #edible => True
LogTest$class <- as.numeric(LogTest$class) #1 => edible

for ( i in colnames(LogTrain) ){
  LogTrain[[i]] <- as.numeric(LogTrain[[i]])
  LogTest[[i]] <- as.numeric(LogTest[[i]])
}

LogRegModel <- glm(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                   data = LogTrain,
                   family = binomial(link = "logit")
                     )
summary(LogRegModel)
#Check p-values:
#remove cap.shape, gill.attachment, maybe stalk.color.below.ring, veil.type, veil.color, maybe spore.print.color, maybe habitat.

LogRegModel2 <- glm(class ~ cap.surface + cap.color + bruises + odor + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + ring.number + ring.type + spore.print.color + population + habitat,
                   data = LogTrain,
                   family = binomial(link = "logit")
)
summary(LogRegModel2)
#Check p-values:
#remove maybe bruises, stalk.surface.below.ring, stalk.color.below.ring, maybe spore.print.color, habitat
LogRegModel3 <- glm(class ~ cap.surface + cap.color + bruises + odor + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.color.above.ring + ring.number + ring.type + spore.print.color + population,
                    data = LogTrain,
                    family = binomial(link = "logit")
)
summary(LogRegModel3)
#remove bruises, maybe spore.print.color
LogRegModel4 <- glm(class ~ cap.surface + cap.color + odor + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.color.above.ring + ring.number + ring.type + spore.print.color + population,
                    data = LogTrain,
                    family = binomial(link = "logit")
)
summary(LogRegModel4)
#remove spore.print.color
LogRegModel5 <- glm(class ~ cap.surface + cap.color + odor + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.color.above.ring + ring.number + ring.type + population,
                    data = LogTrain,
                    family = binomial(link = "logit")
)
summary(LogRegModel5)

library(ROCR)
predicted <- predict(LogRegModel5, type="response")
pObj <- prediction(predicted, LogTrain$class)
rocObj <- performance(pObj, measure = "tpr", x.measure = "fpr")
aucObj <- performance(pObj, measure = "auc")
plot(rocObj, main = paste("AUC:", round(aucObj@y.values[[1]],3)))

predNew <- predict(LogRegModel5, newdata = LogTest[,-1], type="response")
pObjNew <- prediction(predNew, LogTest$class)
rocObjNew <- performance(pObjNew, measure = "tpr", x.measure = "fpr")
aucObjNew <- performance(pObjNew, measure = "auc")
plot(rocObjNew, main = paste("AUC:", round(aucObjNew@y.values[[1]],3)))

predTest <- predNew
for (count in 1:1688){
  if (predTest[[count]] >= 0.5){
    predTest[[count]] <- 1
  }else{predTest[[count]] <- 0}
}
predTest <- as.logical(predTest)
actual <- as.logical(LogTest$class)

#True positives
TP <- predTest & actual
TP <- sum(TP)
#True negatives
TN <- (predTest == FALSE) & (actual == FALSE)
TN <- sum(TN)
#False positives
FP <- (predTest == TRUE) & (actual == FALSE)
FP <- sum(FP)
#False negatives
FN <- (predTest == FALSE) & (actual == TRUE)
FN <- sum(FN)

Accuracy <- (100*(TP+TN))/(TP+TN+FP+FN)
TPR <- TP/(TP+FN) #Recall
FPR <- FP/(FP+TN)
FNR <- FN/(TP+FN)
Precision <- TP/(TP+FP)
#Decision tree was more accurate and precise.

#Naive Bayes
library(e1071)
NBModel <- naiveBayes(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                      MushTrain)
NBModel

NBPrediction <- predict(NBModel, MushTest[,-1])
NBPrediction

#True positives
TP <- (MushTest$class == "e") & (NBPrediction == "e")
TP <- sum(TP)
#True negatives
TN <- (NBPrediction == "p") & (MushTest$class == "p")
TN <- sum(TN)
#False positives
FP <- (NBPrediction == "e") & (MushTest$class == "p")
FP <- sum(FP)
#False negatives
FN <- (NBPrediction == "p") & (NBPrediction == "e")
FN <- sum(FN)

Accuracy <- (100*(TP+TN))/(TP+TN+FP+FN)
TPR <- TP/(TP+FN) #Recall
FPR <- FP/(FP+TN)
FNR <- FN/(TP+FN)
Precision <- TP/(TP+FP)





NBModelLaplace <- naiveBayes(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                             MushTrain,
                             laplace = 0.01)
NBwLapPrediction <- predict(NBModelLaplace, MushTest[,-1])

#True positives
TP <- (MushTest$class == "e") & (NBwLapPrediction == "e")
TP <- sum(TP)
#True negatives
TN <- (NBwLapPrediction == "p") & (MushTest$class == "p")
TN <- sum(TN)
#False positives
FP <- (NBwLapPrediction == "e") & (MushTest$class == "p")
FP <- sum(FP)
#False negatives
FN <- (NBwLapPrediction == "p") & (NBPrediction == "e")
FN <- sum(FN)

Accuracy <- (100*(TP+TN))/(TP+TN+FP+FN)
TPR <- TP/(TP+FN) #Recall
FPR <- FP/(FP+TN)
FNR <- FN/(TP+FN)
Precision <- TP/(TP+FP)

#Slightly better accuracy with Laplace smoothing, but not as good as decision tree.
