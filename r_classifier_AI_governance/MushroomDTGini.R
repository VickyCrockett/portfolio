#Gini - not as good

#Import, view and gather a quick insight of the data
Mushrooms <- read.csv("Mushrooms.csv")
#View(Mushrooms)

#Create test and train sets
set.seed(1)
MushroomLength <- nrow(Mushrooms)
TF <- sample(c(TRUE,FALSE), MushroomLength, replace = TRUE, prob = c(0.8,0.2))
MushTrain <- Mushrooms[TF,]
MushTest <- Mushrooms[!TF,]


library(rpart)
library(rpart.plot)


DTGiniModel <- rpart(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                     method = "class",
                     data = MushTrain,
                     control = rpart.control(maxdepth=4, minsplit=10, cp=0.0001),
                     parms=list(split='gini')
)

rpart.plot(DTGiniModel, type=4, extra=1, clip.right.labs = FALSE, varlen = 0, faclen = 0)

DTGiniPred <- predict(DTGiniModel, newdata = MushTest[,-1], type = "class")

DTGiniPredBool <- !as.logical(as.numeric(DTGiniPred)-1)
ActualBool <- as.logical(as.numeric(as.factor(MushTest$class))-1)

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

#FPR is the dangerous one - the rate it says it's edible when it is poisonous!
FPR