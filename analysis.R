#############
# Load data #
#############

#Businesses
biz <- readRDS("biz.rds")
##from Edinburgh,
biz <- biz[which(biz$city == "Edinburgh"), ]
##open,
biz <- biz[which(biz$open==TRUE), ]
##categorised as Restaurant
biz <- biz[which(grepl("Restaurants", biz$categories)), -c(2, 4, 6, 8, 9, 11, 15)]


#Flatten the data frame to avoid nested structure,
#which comes from the former json nature of the data
library(jsonlite)
biz <- flatten(biz, recursive = TRUE)

#NA in Attributes is considered to be FALSE
#See http://stackoverflow.com/questions/2991514/r-preventing-unlist-to-drop-null-values
#to solve NULL creating problems when flattening list variables
biz$`attributes.Accepts Credit Cards`[sapply(biz$`attributes.Accepts Credit Cards`, is.null)] <- NA
biz$`attributes.Accepts Credit Cards` <- unlist(biz$`attributes.Accepts Credit Cards`,
                                                recursive = T, use.names = T)


attrib <- grepl("attributes.", names(biz))
biz[attrib] <- replace(biz[attrib], is.na(biz[attrib]), FALSE)
biz[attrib] <- lapply(biz[attrib], as.factor)


#NA in Hours is considered to be 00:00
hours <- grepl("hours.", names(biz))
biz[hours] <- replace(biz[hours], is.na(biz[hours]), "00:00")
biz[hours] <- lapply(biz[hours], as.factor)

#Reviews
reviews <- readRDS("review.rds")
##Drop text (left for further analysis, not in this research)
reviews <- reviews[, -c(6, 7)]
reviews <- flatten(reviews, recursive = TRUE)

#Users
users <- readRDS("user.rds")
##exclude friend network, it would be interesting to analyse this,
##but I leave it for another research project
users <- users[,-c(4, 6, 9, 11)] 
users <- flatten(users, recursive = TRUE)
users$yelping_since <- as.factor(users$yelping_since)
users[,c(4:19)][is.na(users[,c(4:19)])] <- 0

#Master
master <- merge(biz, reviews, by = "business_id")
master <- master[,-c(1,2)]
rm(reviews) #always being memory-concious

master$stars.diff <- master$stars.x - master$stars.y

#Expand master with user details
master <- merge(master, users, by = "user_id")

##################################
# Random forest to predict stars #
##################################

library(h2o)
localH2O <- h2o.init(nthreads = -1)

biz.hex <- as.h2o(biz[,-c(1,2)])
master.hex <- as.h2o(master)

#from business characteristics only
features <- colnames(biz)[-c(1, 2, 5)]
fit.rf.biz <- h2o.randomForest(x=features, y="stars.x",
                               training_frame=biz.hex, ntrees=50, max_depth=100)
fit.rf.biz
h2o.varimp(fit.rf.biz)

#from business characteristics augmented with user reviews
features.review <- colnames(master)[-c(1, 4, 98, 100, 105:122)]
fit.rf.review <- h2o.randomForest(x=features.review, y="stars.x",
                                  training_frame=master.hex, ntrees=50, max_depth=100)
fit.rf.review
h2o.varimp(fit.rf.review)

#from business characteristics augmented with user reviews and user characteristics
features.users <- colnames(master)[-c(1, 4, 98, 100)]
fit.rf.users <- h2o.randomForest(x=features.users, y="stars.x",
                                  training_frame=master.hex, ntrees=50, max_depth=100)
fit.rf.users
h2o.varimp(fit.rf.users)
