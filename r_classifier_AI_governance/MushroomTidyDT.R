#Decision tree tidied

#Import, view and gather a quick insight of the data
Mushrooms <- read.csv("Mushrooms.csv")
#View(Mushrooms)

library(stringr)
Mushrooms$odor <- str_replace(Mushrooms$odor, "^a", "almond" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^l", "anise" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^c", "creosote" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^f", "foul" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^y", "fishy" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^m", "musty" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^n", "none" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^p", "pungent" )
Mushrooms$odor <- str_replace(Mushrooms$odor, "^s", "spicy" )

Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^b", "buff" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^k", "black" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^n", "brown" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^h", "chocolate" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^r", "green" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^o", "orange" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^u", "purple" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^w", "white" )
Mushrooms$spore.print.color <- str_replace(Mushrooms$spore.print.color, "^y", "yellow" )

Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^b", "buff" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^n", "brown" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^c", "cinnamon" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^g", "gray" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^o", "orange" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^p", "pink" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^e", "red" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^w", "white" )
Mushrooms$stalk.color.below.ring <- str_replace(Mushrooms$stalk.color.below.ring, "^y", "yellow" )

Mushrooms$cap.surface <- str_replace(Mushrooms$cap.surface, "^s", "smooth" )
Mushrooms$cap.surface <- str_replace(Mushrooms$cap.surface, "^f", "fibrous" )
Mushrooms$cap.surface <- str_replace(Mushrooms$cap.surface, "^g", "grooves" )
Mushrooms$cap.surface <- str_replace(Mushrooms$cap.surface, "^y", "scaly" )

Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^c", "club" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^r", "rooted" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "?", "missing" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^e", "equal" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^u", "cup" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^b", "bulbous" )
Mushrooms$stalk.root <- str_replace(Mushrooms$stalk.root, "^z", "rhizomorphs" )

Mushrooms$class <- str_replace(Mushrooms$class, "^e", "Possibly Edible" )
Mushrooms$class <- str_replace(Mushrooms$class, "^p", "No" )



#Create test and train sets
set.seed(1)
MushroomLength <- nrow(Mushrooms)
TF <- sample(c(TRUE,FALSE), MushroomLength, replace = TRUE, prob = c(0.8,0.2))
MushTrain <- Mushrooms[TF,]
MushTest <- Mushrooms[!TF,]


library(rpart)
library(rpart.plot)


DTInfoModel5 <- rpart(class ~ cap.shape + cap.surface + cap.color + bruises + odor + gill.attachment + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + veil.type + veil.color + ring.number + ring.type + spore.print.color + population + habitat,
                     method = "class",
                     data = MushTrain,
                     control = rpart.control(maxdepth=4, minsplit=10, cp=0, xval=10),
                     parms=list(split='information')
)

rpart.plot(DTInfoModel5, type=5, extra=0, fallen.leaves = TRUE, varlen = 0, faclen = 0, box.palette = c("BuPu"))

DTInfoPred <- predict(DTInfoModel5, newdata = MushTest[,-1], type = "class")

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

#FPR is the dangerous one - the rate it says it's edible when it is poisonous!
FPR

printcp(DTInfoModel5)
