#Businesses
biz <- readRDS("biz.rds")
#from Edinburgh
biz <- biz[which(biz$city == "Edinburgh"), ]
#open
biz <- biz[which(biz$open==TRUE), ]
#categorised as Restaurant
biz <- biz[which(grepl("Restaurants", biz$categories)), -c(2, 4, 6, 8, 9, 11, 15)]


#Flatten the data frame to avoid nested structure,
#which comes from the former json nature of the data
library(jsonlite)
biz <- flatten(biz, recursive = TRUE)

#NA in Attributes is considered to be FALSE
attrib <- grepl("attributes.", names(biz))
biz[attrib] <- apply(biz[attrib], 2, function(x) as.factor(x))
biz[attrib] <- replace(biz[attrib], is.na(biz[attrib]), FALSE)


#NA in Hours is considered to be 00:00
hours <- grepl("hours.", names(biz))
biz[hours] <- replace(biz[hours], is.na(biz[hours]), "00:00")

#Reviews
reviews <- readRDS("review.rds")
reviews <- reviews[, -c(7)]
reviews <- flatten(reviews, recursive = TRUE)

#Master
master <- merge(biz, reviews, by = "business_id")
master <- master[,-c(1,2)]
rm(biz, reviews) #always being memory-concious

master$stars.diff <- master$stars.x - master$stars.y


#Random forest to predict stars
library(h2o)
localH2O <- h2o.init(nthreads = -1)

master.hex <- as.h2o(master)

features <- colnames(master)[-c(1,3,6,9)]
fit.rf <- h2o.randomForest(x=features, y="stars.x",
                           training_frame=master.hex, ntrees=50, max_depth=100)

