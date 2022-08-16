setwd("D:/University/arshad/ML/Project")
df <- read.table("NT.txt",
                 sep = "\t",
                 fill = TRUE,
                 header = TRUE)
assossiation <- df[, ncol(df)]
n = (ncol(df))
data <- df[, 2:n]
cm <- colMeans(data[, 4:ncol(data)], na.rm = TRUE)
indx <- which(is.na(data), arr.ind = TRUE)
data[indx] <- cm[indx[, 2]]
max.val = max(assossiation, na.rm = T)
min.val = min(assossiation, na.rm = T)
# dist = (max.val - min.val) / 2
# bin = min.val + dist
# data$Toxicity = "low"
# for (i in 1:length(assossiation)) {
#   if (assossiation[i] >= bin) {
#     data$Toxicity[i] = "high"
#   }
# }
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

fit <- lm(train.1$cell.association ~ ., data=train.1[,4:25])
summary(fit) 
print(fit)
plot(fit)
predict.lm(fit,test.1[,4:25])
