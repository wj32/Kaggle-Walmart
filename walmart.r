library(randomForest)
library(dplyr)
library(plyr)
library(xgboost)

# Run Walmart.fsx first to process the data

data = read.csv("G:/box/walmart/train_processed.csv")
data = na.exclude(data)
data$TripType = factor(data$TripType)
#n.eval = 30000
#data.eval = data[(nrow(data)-n.eval+1):nrow(data),]
#data = data[1:(nrow(data)-n.eval+1),]

trip.type.revalued = revalue(factor(data[,"TripType"]))
trip.type = as.numeric(trip.type.revalued)-1.

encode.trip.type = function(t) {
  as.numeric(factor(t, levels=levels(trip.type.revalued)))
}

decode.trip.type = function(t) {
  sapply(t, function(t) as.numeric(levels(trip.type.revalued)[as.integer(t + 1)]))
}

process.data = function(data) {
  # These are really notes, and shouldn't help with the actual model
  
  # 38 vs 39 is tricky
  data$Groceries = data$PRODUCE - data$PERSONAL.CARE + data$GROCERY.DRY.GOODS - data$HOUSEHOLD.CHEMICALS.SUPP * 0.3 + data$DAIRY + data$BAKERY - data$BEAUTY
  data$Adult = data$PHARMACY.OTC + data$PERSONAL.CARE + data$CANDY..TOBACCO..COOKIES + data$BEAUTY
  # 22 looks like entertainment
  data$Entertainment = data$PLAYERS.AND.ELECTRONICS + data$MEDIA.AND.GAMING + data$ELECTRONICS
  # 9 looks like men
  data$Men = data$SPORTING.GOODS + data$SHOES + data$OFFICE.SUPPLIES + data$MENS.WEAR + data$AUTOMOTIVE
  # 30 looks like fashion
  data$Fashion = 3 * data$SHOES + 2 * data$JEWELRY.AND.SUNGLASSES + data$ACCESSORIES + data$PERSONAL.CARE
  
  data
}

process.pred = function(data) {
  focus.category = "NOT.IN.USE" # Change this and use the code in randomStuff to do some PCA for a particular department
  pred = data[,Filter(function(c) c != "TripType" &&c != "Weekday" && !grepl("^RETURN.", c) && !grepl(focus.category, c), colnames(data))]
  
  # 43
  pred$RETURN.TOYS = data$RETURN.TOYS
  
  pred
}

data = process.data(data)
pred = process.pred(data)

randomStuff = function() {
  pred.focus = data[,Filter(function(c) c != "TripType" && c != "Weekday" && grepl(paste0("^", focus.category), c), colnames(data))]
  pr.focus = princomp(pred.focus, scores=T)
  
  n.pr = 20
  for (i in 1:n.pr) {
    pred[,paste0(focus.category, "PR.", i)] = pr.focus$scores[,i]
  }
  pred[,paste0(focus.category, "PR.REST")] = rep(0,nrow(pred))
  for (i in (n.pr+1):length(pr.focus$center)) {
    pred[,paste0(focus.category, "PR.REST")] = pred[,paste0(focus.category, "PR.REST")] + pr.focus$scores[,i]
  }
  
  data.full = read.csv("G:/box/walmart/train.csv")
  
  qplot(DepartmentDescription, data=filter(data.full, (TripType == decode.trip.type(31) | TripType == decode.trip.type(35)) & ScanCount > 0), weight=ScanCount) + facet_grid(. ~ TripType) + coord_flip()
  
  pred = data[,Filter(function(c) c != "TripType" && !grepl("^RETURN.", c), colnames(data))]
}

# Binary RF
binaryRF = function(tripType, n = 1000) {
  randomForest(x=pred[1:n,],y=factor(as.numeric(data[1:n,"TripType"] == tripType)))
}

# Multiclass RF
multiclassRF = function(n = 10000, mtry=20, ntree=100, do.trace=F) {
  randomForest(x=pred[1:n,],y=factor(data[1:n,"TripType"]),importance=T,mtry=mtry,ntree=ntree,do.trace=do.trace)
}

# Multiclass xgboost
multiclassXgb = function(n = 95674) {
  xgboost(
    data=apply(t(data.matrix(pred[1:n,])), 1, as.numeric),
    label=data.matrix(trip.type[1:n]),
    objective="multi:softprob",
    num_class=max(trip.type)+1,
    eval_metric="mlogloss",
    eta=0.05,
    gamma=3,
    max_depth=9,
    nrounds=200,
    verbose=1
  )
}

multiclassXgb.cv = function(n = 95674) {
  xgb.cv(
    data=apply(t(data.matrix(pred[1:n,])), 1, as.numeric),
    label=data.matrix(trip.type[1:n]),
    objective="multi:softmax",
    num_class=max(trip.type)+1,
    eval_metric="mlogloss",
    eta=0.05,
    gamma=3,
    max_depth=9,
    nrounds=200,
    nfold=4,
    verbose=1,
    prediction=T
  )
}

write.output = function(prediction) {
  write.table(
    prediction,
    sep=",",
    dec=".",
    row.names=F,
    col.names=Map(function(t) paste0("TripType_", t), levels(data[,"TripType"])),
    qmethod="double",
    file="G:/box/walmart/test_prediction.csv"
  )
}
