library(MASS)
setwd("D:/University/arshad/ML/Project")
df <- read.table("newdata.txt",
                 sep = "\t",
                 fill = TRUE,
                 header = TRUE)
label <- df[, ncol(df)]
n = (ncol(df) - 1)
data <- df[, 2:(n + 1)]
cm <- colMeans(data[,], na.rm = TRUE)
indx <- which(is.na(data), arr.ind = TRUE)
data[indx] <- cm[indx[, 2]]
data <- as.data.frame(lapply(data, data_norm))
set.seed(123)
samples <- sample(nrow(data), size = nrow(data), replace = FALSE)
test.1 <- data[samples[1:floor(.2 * length(samples))], ]
train.1 <- data[-samples[1:floor(.2 * length(samples))], ]
test.2 <-
  data[samples[floor(.2 * length(samples)):(floor(.4 * length(samples)))], ]
train.2 <-
  data[-samples[floor(.2 * length(samples)):(floor(.4 * length(samples)))], ]
test.3 <-
  data[samples[(floor(.4 * length(samples))):(floor(.6 * length(samples)))], ]
train.3 <-
  data[-samples[(floor(.4 * length(samples))):(floor(.6 * length(samples)))], ]
test.4 <-
  data[samples[(floor(.6 * length(samples))):(floor(.8 * length(samples)))], ]
train.4 <-
  data[-samples[(floor(.6 * length(samples))):(floor(.8 * length(samples)))], ]
test.5 <-
  data[samples[(floor(.8 * length(samples))):length(samples)], ]
train.5 <-
  data[-samples[(floor(.8 * length(samples))):length(samples)], ]
data_norm <- function(x) {
  #print(typeof(x))
  if (typeof(x) == "double" || typeof(x) == "integer") {
    x = as.numeric(x)
    return((x - min(x)) / (max(x) - min(x)))
  }
  else
    return (x)
}
accuracy <- function(pred, test) {
  mismatches <- which(pred != test)
  WrongPerc = length(mismatches) * 100 / length(test)
  return(100 - WrongPerc)
}
n = (ncol(train.1) - 1)
lda.fit = lda(Toxicity ~ ., data = train.1)
summary(lda.fit)
lda.pred = predict(lda.fit, test.1)
names(lda.pred)
print(accuracy(lda.pred$class, test.1[, (n + 1)]))
print(validation(lda.pred$class, test.1[, (n + 1)]))
validation <- function(pred, test) {
  trues <- test[which(pred == test)]
  falses <- test[which(pred != test)]
  tp <- length(which(trues == 1))
  tn <- length(which(trues == 0))
  fp <- length(which(falses == 1))
  fn <- length(which(falses == 0))
  print(c(tp, tn, fp, fn))
  sensitivity = tp / (tp + fn)
  specificity = tn / (tn + fp)
  ppv = tp / (tp + fp)
  npv = tn / (tn + fn)
  f1 = (2 * tp) / ((2 * tp) + fp + fn)
  MCC = ((tp * tn) - (fp * fn)) / sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) *
                                         as.numeric(tn + fn))
  print (c (sensitivity, specificity, ppv, npv, f1 , MCC))
}
LDA <- function(train, test) {
  n = (ncol(train) - 1)
  lda.fit = lda(Toxicity ~ ., data = train)
  summary(lda.fit)
  lda.pred = predict(lda.fit, test)
  names(lda.pred)
  totalaccuracy <<-
    totalaccuracy + accuracy(lda.pred$class, test[, (n + 1)])
  totalvalid <<-
    totalvalid + validation(lda.pred$class, test[, (n + 1)])
}
totalaccuracy <- 0
totalvalid <- c(0, 0, 0, 0, 0, 0)
LDA(train.1, test.1)
LDA(train.2, test.2)
LDA(train.3, test.3)
LDA(train.4, test.4)
LDA(train.5, test.5)
print(c(totalaccuracy / 5, totalvalid / 5))
