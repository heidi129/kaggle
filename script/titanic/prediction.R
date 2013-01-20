# ライブラリの読み込み
library('randomForest')
library('e1071')
library('nnet')
library('mi')
library('imputation')

# データの読み込み
train <- read.table('~/Documents/workspace/R/kaggle/data/titanic/train.csv', header=TRUE, sep=',', stringsAsFactors=TRUE)
test <- read.table('~/Documents/workspace/R/kaggle/data/titanic/test.csv', header=TRUE, sep=',',  stringsAsFactors=TRUE)

# 生存したか否かをカテゴリカル変数として扱う
train$survived = as.factor(train$survived)

# 名前の敬称変数の作成
train$title <- NA
train[grep('Mr[. ]', train$name), 12]       <- 'Mr'
train[grep('Don[. ]', train$name), 12]      <- 'Don'
train[grep('Dr[. ]', train$name), 12]       <- 'Dr'
train[grep('Major[. ]', train$name), 12]    <- 'Major'
train[grep('Jonkheer[. ]', train$name), 12] <- 'Jonkheer'
train[grep('Master[. ]', train$name), 12]   <- 'Master'
train[grep('Col[. ]', train$name), 12]      <- 'Col'
train[grep('Capt[. ]', train$name), 12]     <- 'Capt'
train[grep('Mrs[. ]', train$name), 12]      <- 'Mrs'
train[grep('Mme[. ]', train$name), 12]      <- 'Mme'
train[grep('Countess[. ]', train$name), 12] <- 'Countess'
train[grep('Ms[. ]', train$name), 12]       <- 'Ms'
train[grep('Miss[. ]', train$name), 12]     <- 'Miss'
train[grep('Mlle[. ]', train$name), 12]     <- 'Mlle'
train[grep('Rev[. ]', train$name), 12]      <- 'Rev'
test$title <- NA
test[grep('Mr[. ]', test$name), 11]         <- 'Mr'
test[grep('Don[. ]', test$name), 11]        <- 'Don'
test[grep('Dr[. ]', test$name), 11]         <- 'Dr'
test[grep('Major[. ]', test$name), 11]      <- 'Major'
test[grep('Jonkheer[. ]', test$name), 11]   <- 'Jonkheer'
test[grep('Master[. ]', test$name), 11]     <- 'Master'
test[grep('Col[. ]', test$name), 11]        <- 'Col'
test[grep('Capt[. ]', test$name), 11]       <- 'Capt'
test[grep('Mrs[. ]', test$name), 11]        <- 'Mrs'
test[grep('Mme[. ]', test$name), 11]        <- 'Mme'
test[grep('Countess[. ]', test$name), 11]   <- 'Countess'
test[grep('Ms[. ]', test$name), 11]         <- 'Ms'
test[grep('Miss[. ]', test$name), 11]       <- 'Miss'
test[grep('Mlle[. ]', test$name), 11]       <- 'Mlle'
test[grep('Rev[. ]', test$name), 11]        <- 'Rev'

# 未成年か否か
train$immature <- ifelse(train$title=='Master', TRUE, FALSE)
train$immature <- ifelse(train$title=='Miss', TRUE, train$immature)
train$immature <- as.factor(train$immature)
test$immature <- ifelse(test$title=='Master', TRUE, FALSE)
test$immature <- ifelse(test$title=='Miss', TRUE, test$immature)
test$immature <- as.factor(test$immature)

# 聖職者・軍人・船長・爵位保持者などか否か
train$noble <- FALSE
train$noble <- ifelse(train$title=='Don', TRUE, train$noble)
train$noble <- ifelse(train$title=='Dr', TRUE, train$noble)
train$noble <- ifelse(train$title=='Major', TRUE, train$noble)
train$noble <- ifelse(train$title=='Jonkheer', TRUE, train$noble)
train$noble <- ifelse(train$title=='Col', TRUE, train$noble)
train$noble <- ifelse(train$title=='Capt', TRUE, train$noble)
train$noble <- ifelse(train$title=='Countess', TRUE, train$noble)
train$noble <- ifelse(train$title=='Rev', TRUE, train$noble)
train$noble <- as.factor(train$noble)
test$noble <- FALSE
test$noble <- ifelse(test$title=='Don', TRUE, test$noble)
test$noble <- ifelse(test$title=='Dr', TRUE, test$noble)
test$noble <- ifelse(test$title=='Major', TRUE, test$noble)
test$noble <- ifelse(test$title=='Jonkheer', TRUE, test$noble)
test$noble <- ifelse(test$title=='Col', TRUE, test$noble)
test$noble <- ifelse(test$title=='Capt', TRUE, test$noble)
test$noble <- ifelse(test$title=='Countess', TRUE, test$noble)
test$noble <- ifelse(test$title=='Rev', TRUE, test$noble)
test$noble <- as.factor(test$noble)

# 客室の場所が船の右側か左側か
train$cabin_pos <- ifelse(train$cabin=='', 'other', 'left')
train[grep('[13579]$', train$cabin), 15] = 'right'
train$cabin_pos <- as.factor(train$cabin_pos)
test$cabin_pos <- ifelse(test$cabin=='', 'other', 'left')
test[grep('[13579]$', test$cabin), 14] = 'right'
test$cabin_pos <- as.factor(test$cabin_pos)

# 客室が何階にあるか
train$cabin_floor <- 'other'
train[grep('^A', train$cabin), 16] <- '7'
train[grep('^B', train$cabin), 16] <- '6'
train[grep('^C', train$cabin), 16] <- '5'
train[grep('^D', train$cabin), 16] <- '4'
train[grep('^E', train$cabin), 16] <- '3'
train[grep('^F', train$cabin), 16] <- '2'
train[grep('^G', train$cabin), 16] <- '1'
train$cabin_floor <- as.factor(train$cabin_floor)
test$cabin_floor <- 'other'
test[grep('^A', test$cabin), 15] <- '7'
test[grep('^B', test$cabin), 15] <- '6'
test[grep('^C', test$cabin), 15] <- '5'
test[grep('^D', test$cabin), 15] <- '4'
test[grep('^E', test$cabin), 15] <- '3'
test[grep('^F', test$cabin), 15] <- '2'
test[grep('^G', test$cabin), 15] <- '1'
test$cabin_floor <- as.factor(test$cabin_floor)

# チケット番号
train$ticket_no <- gsub('[a-zA-Z0-9/.]+ ', '', train$ticket)
test$ticket_no <- gsub('[a-zA-Z0-9/.]+ ', '', test$ticket)

# ラインか否か
train$line <- ifelse(train$ticket_no=='LINE', TRUE, FALSE)
train$ticket_no <- ifelse(train$line==TRUE, 9999999, train$ticket_no)
train$ticket_no <- as.numeric(train$ticket_no)
test$line <- ifelse(test$ticket_no=='LINE', TRUE, FALSE)
test$ticket_no <- ifelse(test$line==TRUE, 9999999, test$ticket_no)
test$ticket_no <- as.numeric(test$ticket_no)

# 年齢の欠損値を平均値で補完
train$age_ave = ifelse(is.na(train$age), mean(na.omit(train$age)), train$age)
test$age_ave = ifelse(is.na(test$age), mean(na.omit(test$age)), test$age)

# 年齢の欠損値をMultiple Imputationで補完
## 訓練データとテストデータを合わせて補完を行う
data <- rbind(train[,2:19], test)
## 予測に使わないデータは除外
data$name <- NULL
data$ticket <- NULL
data$cabin <- NULL
data$cabin_pos <- NULL
data$cabin_floor <- NULL
data$ticket_no <- NULL
data$line <- NULL
data$title <- NULL
data$age_ave <- NULL
## プロット
mp.plot(data, clustered=FALSE) #プロット
## 補完の予測式の確認
data.info <- mi.info(data)
data.info$imp.formula
## 補完の前処理
data.pre <- mi.preprocess(data)
attr(data, 'mi.info')
## 補完の実行
data.imp <- mi(data.pre, R.hat=2.5)
data.imp1 <- mi.data.frame(data.imp, m=1)
## 補完データの元データの挿入
train$age_imp <- data.imp1[1:891, 3]
test$age_imp <- data.imp1[892:1309, 3]

# 訓練データを半分に分割してクロスバリデーション
train.cv1 <- train[1:446,]
train.cv2 <- train[447:891,]

# ランダムフォレスト
## CV1
train.cv1.rf <- randomForest(formula<-survived~pclass+sex+age_imp+sibsp+parch+fare+immature+noble+cabin_pos+cabin_floor+ticket_no+line, data=train.cv1)
pred <- predict(train.cv1.rf, train.cv2)
pred_b <- ifelse(pred > 0.5, 1, 0)
table(train.cv2$survived, pred_b)
## CV2
train.cv2.rf <- randomForest(formula<-survived~pclass+sex+age_imp+sibsp+parch+fare+immature+noble+cabin_pos+cabin_floor+ticket_no+line, data=train.cv2)
pred <- predict(train.cv2.rf, train.cv1)
pred_b <- ifelse(pred > 0.5, 1, 0)
table(train.cv1$survived, pred_b)

# テスト
train.rf <- randomForest(formula<-survived~pclass+sex+age_imp+sibsp+parch+fare+immature+noble+cabin_pos+cabin_floor+ticket_no+line, data=train)
print(train.rf)
train.rf$importance
varImpPlot(train.rf)
pred <- predict(train.rf, test)

# 結果の書き出し
write.csv(pred, '~/Documents/workspace/R/kaggle/data/titanic/predict.csv')