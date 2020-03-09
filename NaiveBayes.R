data <- read.csv("train.csv")[-679,]
data <- select(data,Survived, Pclass, Sex, Age,SibSp, Parch, Fare, Embarked)
data<- tidyimpute::impute(data, na.tools::na.mean)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)

index <- sample(c(1,2),nrow(data), replace = T,prob = c(0.8,0.2))
train <- data[index == 1,]
test <- data[index == 2,]




#NaiveBayes 
NaiveBayes <- function(data, class, predictor = "all"){
  library(dplyr)
  if(predictor == "all"){
    pred <- select(data, -class)
  } else {
    pred <- select(data, predictor)
  }
  if(is.factor(data[,class]) == FALSE ) stop("the independent variable must be categorical variable")
  ycount <- xtabs(~ data[,class])
  mod <- list("Target" = class, 
              "levels" = levels(data[,class]), 
              "py"= table(data[,class])/nrow(data)
              )
  j <- 4
  for(i in names(pred)){
    if (is.factor(pred[,i])== TRUE) {
      px <- t(apply(xtabs(~setNames(pred[,i],i) + setNames(data[,class],class)),1,function(a) a/ycount))
      px <- cbind(px, c(table(data[,i])/nrow(data)))
    } else if(is.factor(pred[,i])== FALSE){
      px <- data %>% group_by_(.dots = class) %>% summarise_(
        setNames(paste0("mean(",i,")"),"mean"),
        setNames(paste0("sd(",i,")"),"sd"))
      px <- rbind(px,c("all", mean(pred[,i]),sd(pred[,i])))
    } else {
      stop("one of the predictors is not categorical or continuous variables")
    }
    mod[j] <- list(px)
    names(mod)[j] <- i
    j <- j+1
  }
  return(mod)
}

a <- NaiveBayes(train, "Survived")
a


test_Naive <- function(model, new_data, prob = FALSE, thread = 0.5){
  class <- model$Target
  prediction <- c()
  if(sum(is.na(test)) != 0) stop("There are missing values in The Test Set")
  test <- select(new_data, - class)
  first_lev <- model$levels[1]
  for(j in 1:nrow(new_data)){
    p.x_y <- 1
    p.x <- 1
    for(i in names(test)){
      x <- test[j,i]
      if (is.factor(test[,i])== TRUE) {
        p.x_i <- model[[i]][x,3]
        p.x_y_i <- model[[i]][x,first_lev]
      } else if(is.factor(test[,i])== FALSE){
        p.x_i <- dnorm(x, mean = as.numeric(model[[i]][is.na(model[[i]][class]),2]), 
                       sd = as.numeric(model[[i]][is.na(model[[i]][class]),3]))
        p.x_y_i <- dnorm(x, mean = as.numeric(model[[i]][model[[i]][class] == first_lev,2][1,1]), 
                         sd = as.numeric(model[[i]][model[[i]][class] == first_lev,3][1,1]))
      } else {
        stop("one of the predictors is not categorical or continuous variables")
      }
      p.x_y <- p.x_y*p.x_y_i
      p.x <- p.x*p.x_i
    }
    prediction[j] <- (p.x_y*model$py[first_lev])/p.x
  }
  result <- sapply(prediction, function(a) ifelse(a > thread, a <- first_lev, a <- model$levels[2]))
  ifelse(prob == TRUE,return(prediction ),return(result))
}


result <- test_Naive(model = a, new_data = test,thread = 0.85)
result
sum(result == test$Survived,na.rm = T)/nrow(test)

library(naivebayes)
valimodel <- naive_bayes(Survived ~ . ,data = train)
summary(valimodel)
valipred <- predict(valimodel, test)
valipred
sum(valipred == test$Survived)/nrow(test)

