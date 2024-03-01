#Random Forest

#Import, view and gather a quick insight of the data
Mushrooms <- read.csv("Mushrooms.csv")
#View(Mushrooms)

#Create test and train sets
set.seed(1)
MushroomLength <- nrow(Mushrooms)
TF <- sample(c(TRUE,FALSE), MushroomLength, replace = TRUE, prob = c(0.1,0.9))
MushTrain <- Mushrooms[TF,]
MushTest <- Mushrooms[!TF,]


library(ranger)
#library(caret)

MushTrain$class <- as.factor(!as.logical(as.numeric(as.factor(MushTrain$class))-1)) #1 for edible, 0 for poisonous
MushTest$class <- as.factor(!as.logical(as.numeric(as.factor(MushTest$class))-1)) #1 for edible, 0 for poisonous

RFModel <- ranger(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                     data = MushTrain,
                     importance = "impurity"
)


RFPred <- predict(RFModel, data = MushTest[,-1])

#confusionMatrix(RFPred$predictions, MushTest$class)
#doh. caret won't install right now, so done it below by hand.


#True positives
TP <- (RFPred$predictions==TRUE) & (MushTest$class ==TRUE)
TP <- sum(TP)
#True negatives
TN <- (RFPred$predictions==FALSE) & (MushTest$class ==FALSE)
TN <- sum(TN)
#False positives
FP <- (RFPred$predictions==TRUE) & (MushTest$class ==FALSE)
FP <- sum(FP)
#False negatives
FN <- (RFPred$predictions==FALSE) & (MushTest$class ==TRUE)
FN <- sum(FN)

Accuracy <- (100*(TP+TN))/(TP+TN+FP+FN)
TPR <- TP/(TP+FN) #Recall
FPR <- FP/(FP+TN)
FNR <- FN/(TP+FN)
Precision <- TP/(TP+FP)

#FPR is the dangerous one - the rate it says it's edible when it is poisonous!
FPR

#importance <- data.frame(Importance = importance(RFModel))
#importance <- cbind(Features = rownames(importance), importance)
#RFModel
#sortedImportance <- importance[order(importance$Importance),]

