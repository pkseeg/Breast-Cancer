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