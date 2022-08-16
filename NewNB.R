setwd("D:/University/arshad/ML/Project")
df <- read.table("NT.txt",
                 sep = "\t",
                 fill = TRUE,
                 header = TRUE)
assossiation <- df[, ncol(df)]
n = ncol(df) - 1
data <- df[, 2:n]
cm <- colMeans(data[, 4:ncol(data)], na.rm = TRUE)
indx <- which(is.na(data), arr.ind = TRUE)
data[indx] <- cm[indx[, 2]]
max.val = max(assossiation, na.rm = T)
min.val = min(assossiation, na.rm = T)
dist = (max.val - min.val) / 2
bin = min.val + dist
data$Toxicity = "low"
for (i in 1:length(assossiation)) {
  if (assossiation[i] >= bin) {
    data$Toxicity[i] = "high"
  }
}
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
naivebayes(train.1, test.1)
naivebayes(train.2, test.2)
naivebayes(train.3, test.3)
naivebayes(train.4, test.4)
naivebayes(train.5, test.5)
nb <- c(
  naivebayes(train.1, test.1),
  naivebayes(train.2, test.2),
  naivebayes(train.3, test.3),
  naivebayes(train.4, test.4),
  naivebayes(train.5, test.5)
)
barplot(
  nb,
  xlab = "Accuracy",
  names.arg = c(1, 2, 3, 4, 5),
  ylim = c(0, 100),
  col = rainbow(5)
)
naivebayes <- function(train.1, test.1) {
  lows <- subset(train.1, Toxicity == "low")
  highs <- subset(train.1, Toxicity == "high")
  #counts <- table(data$Toxicity)
  #barplot(counts)
  plow = nrow(lows) / nrow(train.1)
  phigh = nrow(highs) / nrow(train.1)
  n = ncol(lows) - 1
  meanslow <- colMeans(lows[, 4:n])
  varslow <- apply(lows[, 4:n], 2, sd)
  n = ncol(highs) - 1
  meanshigh <- colMeans(highs[, 4:n])
  varshigh <- apply(highs[, 4:n], 2, sd)
  #print(train(train.1))
  pred <- c()
  n = nrow(test.1)
  m = ncol(test.1) - 1
  for (i in 1:n) {
    #print(line <- test.1[i,])
    #print("i: ", i)
    line <- test.1[i, ]
    line <- line[4:m]
    #print("line: ")
    #print(line)
    likeli <-
      c(
        likelihood(line, meanslow, varslow) * plow,
        likelihood(line, meanshigh, varshigh) * phigh
      )
    #print("likli: ")
    #print(likeli)
    index = which.max(likeli)
    
    if (index == 1) {
      pred <- c(pred, "low")
    } else if (index == 2) {
      pred <- c(pred, "high")
    }
  }
  print(pred)
  print(accuracy(pred, test.1$Toxicity))
  return(accuracy(pred, test.1$Toxicity))
}
likelihood <- function(y, mu, sigma2) {
  sumall = 0
  for (i in 1:length(y)) {
    #print(y[i])
    x = y[1, i]
    #print(sigma2[i])
    if (sigma2[i] == 0) {
      singlelikelihoods = 0
    } else{
      singlelikelihoods = dnorm(x,
                                mean = mu[i],
                                sd = sigma2[i],
                                log = T)
    }
    
    #print("single")
    #print(singlelikelihoods)
    sumall = sumall + singlelikelihoods
  }
  return(sumall)
}
accuracy <- function(pred, test) {
  mismatches <- which(pred != test)
  WrongPerc = length(mismatches) * 100 / length(test)
  return(100 - WrongPerc)
}
