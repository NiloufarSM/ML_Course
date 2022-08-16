setwd("D:/University/arshad/ML/Project")
df <- read.table("NT.txt",
                 sep = "\t",
                 fill = TRUE,
                 header = TRUE)
assossiation <- df[, ncol(df)]
n = (ncol(df) - 1)
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
data_norm <- function(x) {
  #print(typeof(x))
  if (typeof(x) == "double" || typeof(x) == "integer") {
    x = as.numeric(x)
    return((x - min(x)) / (max(x) - min(x)))
  }
  else
    return (x)
}

k_nearest <- function(train, test, k)
{
  train <- as.data.frame(lapply(train, data_norm))
  n = (ncol(test))
  test <- as.data.frame(lapply(test[, 4:n], data_norm))
  pred <- c()
  for (i in c(1:nrow(test))) {
    dist = c()
    eu_char = c()
    low = 0
    high = 0
    for (j in c(1:nrow(train))) {
      dist <- c(dist, euclidian_dist(test[i, ], train[j, ]))
      eu_char <- c(eu_char, as.character(train[j, ][[n]]))
    }
    eu <- data.frame(eu_char, dist)
    eu <- eu[order(eu$dist), ]
    eu <- eu[1:k, ]
    #print(eu)
    for (l in c(1:nrow(eu))) {
      if (as.character(eu[l, "eu_char"]) == "low") {
        low = low + 1
      } else if (as.character(eu[l, "eu_char"]) == "high")
        high = high + 1
    }
    result <- c(high, low)
    #print(result)
    # index=0
    # max=0
    # for (i in 1:3)
    #   if (max < result[i])
    #   {
    #     max = result[i]
    #
    #     index = i
    #
    #   }
    # highest = "high"
    # if (index == 3)
    #   highest = "low"
    # else if (index == 2)
    #   highest = "mid"
    if (result[1] > result[2])
      highest = "high"
    else
      highest = "low"
    pred <- c(pred, highest)
  }
  print(pred)
  print(accuracy (pred, test$Toxicity))
  return(accuracy (pred, test$Toxicity))
}
euclidian_dist <- function(x, y) {
  d = 0
  for (i in c(1:(length(x) - 1)))
  {
    d = d + (x[[i]] - y[[i]]) ^ 2
  }
  d = sqrt(d)
  return(d)
}
accuracy <- function(pred, test) {
  mismatches <- which(pred != test)
  WrongPerc = length(mismatches) * 100 / length(test)
  return(100 - WrongPerc)
}
for (k in 1:5) {
  knn = c(
    k_nearest(train.1, test.1, k),
    k_nearest(train.2, test.2, k),
    k_nearest(train.3, test.3, k),
    k_nearest(train.4, test.4, k),
    k_nearest(train.5, test.5, k)
  )
  x11()
  barplot(
    knn,
    names.arg = c(1, 2, 3, 4, 5),
    xlab = k,
    ylim = c(0, 100),
    col = c(heat.colors(5))
  )
  
}
