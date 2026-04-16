set.seed(123)
## 잘못한 경우 (변수선택한 후에 train/test 분할)
# 1. 데이터 생성
n <- 1000       # sample size
p <- 10000      # number of variables

# Y는 0과 1을 각각 50%로
Y <- sample(c(0, 1), n, replace = TRUE)

# X는 Y와 독립적인 표준 정규분포 변수들
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# ⚠️ 2. 전체 데이터에서 변수 선택 (정보 누설 발생)
cor_vals <- apply(X, 2, function(x) cor(x, Y))
top_100_idx <- order(abs(cor_vals), decreasing = TRUE)[1:100]

# 3. 데이터 분할 (변수 선택 이후!)
train_idx <- sample(1:n, size = 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

X_train <- X[train_idx, top_100_idx]
Y_train <- Y[train_idx]

X_test <- X[test_idx, top_100_idx]
Y_test <- Y[test_idx]

# 4. 로지스틱 회귀 학습
df_train <- data.frame(Y = Y_train, X_train)
model <- glm(Y ~ ., data = df_train, family = binomial)

# 5. 테스트 정확도 계산
df_test <- data.frame(X_test)
pred_probs <- predict(model, newdata = df_test, type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

accuracy <- mean(pred_class == Y_test)
cat("Test Accuracy (with data leakage):", accuracy, "\n")

###########################################################
## 제대로 한 경우 (train/test 분할 후에 변수선택)

set.seed(123)

# 1. 데이터 생성
n <- 1000       # sample size
p <- 10000      # number of variables

# Y는 0과 1을 각각 50%로
Y <- sample(c(0, 1), n, replace = TRUE)

# X는 Y와 독립적인 표준 정규분포 변수들
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# 2. 데이터 분할: train/test 70%/30%
train_idx <- sample(1:n, size = 0.7 * n)
test_idx <- setdiff(1:n, train_idx)

X_train <- X[train_idx, ]
Y_train <- Y[train_idx]

X_test <- X[test_idx, ]
Y_test <- Y[test_idx]

# 3. train 데이터에서 각 변수와 Y의 상관계수 계산
cor_vals <- apply(X_train, 2, function(x) cor(x, Y_train))
top_100_idx <- order(abs(cor_vals), decreasing = TRUE)[1:100]

# 4. 선택된 100개의 변수로 로지스틱 회귀 모델 학습
df_train <- data.frame(Y = Y_train, X_train[, top_100_idx])
model <- glm(Y ~ ., data = df_train, family = binomial)

# 5. test 데이터에 적용
df_test <- data.frame(X_test[, top_100_idx])
pred_probs <- predict(model, newdata = df_test, type = "response")
pred_class <- ifelse(pred_probs > 0.5, 1, 0)

# 6. 정확도 계산
accuracy <- mean(pred_class == Y_test)
cat("Test Accuracy:", accuracy, "\n")
