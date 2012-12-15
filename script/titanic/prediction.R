# ライブラリの読み込み
library('randomForest')
library('e1071')
library('nnet')

# データの読み込み
train <- read.table('~/Documents/workspace/R/kaggle/data/titanic/train.csv', header=T, sep=',')
test <- read.table('~/Documents/workspace/R/kaggle/data/titanic/train.csv', header=T, sep=',')

# データ加工
train$age = ifelse(is.na(train$age), mean(na.omit(train$age)), train$age)
test$age = ifelse(is.na(test$age), mean(na.omit(test$age)), test$age)

# 回帰
train.lr <- lm(survived~pclass+sex+age+sibsp+parch+fare, data=train)
pred <-predict(train.lr, test, interval='prediction')
pred_b <- ifelse(pred[,1] > 0.5, 1, 0)
table(train$survived, pred_b)

# ランダムフォレスト
train.rf <- randomForest(formula=survived~pclass+sex+age+sibsp+parch+fare, data=train)
pred <- predict(train.rf, test)
pred_b <- ifelse(pred > 0.5, 1, 0)
table(train$survived, pred_b)

# SVM
train.svm <- svm(formula=survived~pclass+sex+age+sibsp+parch+fare, data=train)
pred <- predict(train.svm, test)
pred_b <- ifelse(pred > 0.5, 1, 0)
table(train$survived, pred_b)

# ニューラルネットワーク
train.nnet <- nnet(formula=survived~pclass+sex+age+sibsp+parch+fare, size=5, data=train)
pred <- predict(train.nnet, test)
pred_b <- ifelse(pred > 0.5, 1, 0)
table(train$survived, pred_b)
