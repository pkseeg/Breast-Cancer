# Data: Breast Cancer Diagnostics from Wisconsin
# Link: https://www.kaggle.com/uciml/breast-cancer-wisconsin-data

# Read in the file
cancer <- read.csv("breastcancer.csv", header=TRUE, stringsAsFactors=FALSE, sep = ",")

# Response Variable (binary outcome)
# B = Benign, non-cancerous M = Malignant, cancerous
table(cancer$diagnosis) 
prop.table(table(cancer$diagnosis))

# Explanatory Variables
colnames(cancer)

# Random Forest Analysis

# Make train and test data sets
set.seed(12)
n.cancer <- dim(cancer)[1]
train.rows <- sample(n.cancer,264)
cancer.train <- cancer[train.rows,]
cancer.test <- cancer[-train.rows,]

library(randomForest)
out.cancer <- randomForest(x=cancer.train[,-2],y=cancer.train$diagnosis,
                          xtest=cancer.test[,-2],ytest=cancer.test$diagnosis,
                          replace=TRUE, # use bootstrap samples
                          keep.forest=TRUE, # store all trees to make pred
                          ntree=50, # num of trees
                          mtry=5, # rule of thumb is p(explvar)/3 (round down)
                          nodesize=25 # how many observations? Go for big trees
)


# I just learned that this doesn't work, because the randomForest analysis doesn't
# work when the response variable (in this case, cancerous or not) is categorical
# What sort of a model could I fit if my response variable is just yes or no?

# Wait! I just learned from this article:
# https://www.r-bloggers.com/binary-classification-a-comparison-of-titanic-proportions-between-logistic-regression-random-forests-and-conditional-trees/
# that I actually can. Let's see if his code can be used here.
# randomForest(as.factor(survived) ~ pclass + sex + age + sibsp, 
# data=titanic.train,ntree=5000, importance=TRUE)

out.cancer.try <- randomForest(as.factor(diagnosis) ~ radius_worst + perimeter_worst + smoothness_worst, 
                           data = cancer.test,ntree=50, importance=TRUE)
out.cancer.try

# Dude this is freaking tight! (I think)


# Create a dataset with half successes (warning), half failures (ticket)
# Let failure be "B", no sign of cancer, and success be "M," cancerous

all.fail <- subset(cancer,diagnosis=="B")
n.fail <- dim(all.fail)[1]

# SRS W/o replacement from successes
all.success <- subset(cancer,diagnosis=="M")
n.success <- dim(all.success)[1]

set.seed(12)
row.fail <- sample(n.fail,n.success)
sample.fail <- all.fail[row.fail,]

# Combine all.fail and sample.goods
cancer.model <- rbind(all.success,sample.fail)
# Confirm half success and half fail
dim(cancer.model)
table(cancer.model$diagnosis)

# Create Train and Test datasets

n.cancer.model <- dim(cancer.model)[1]
cancer.rows <- sample(n.cancer.model,(n.cancer.model/2))
cancer.train <- cancer.model[cancer.rows,]
cancer.test <- cancer.model[-cancer.rows,]

# Confirm similarity between the two
table(cancer.train$diagnosis)
table(cancer.test$diagnosis)


# Random Forest Model

out.cancer <- randomForest(x=cancer.train[,-2],y=cancer.train$diagnosis,
                           xtest=cancer.test[,-2],ytest=cancer.test$diagnosis,
                           replace=TRUE, # use bootstrap samples
                           keep.forest=TRUE, # store all trees to make pred
                           ntrees=50, # num of trees
                           mtry=5, # rule of thumb is p(explvar)/3 (round down)
                           nodesize=25 # how many observations? Go for big trees
)

