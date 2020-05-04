library(rpart)
library(caTools)
library(rattle)
library(pdp)
library(caret)
library(DMwR)
library(UBL)
library(mgcv)

dev_poiss <- function(ytrue, ypred, wcase = 1) -2 * mean(dpois(ytrue, ypred, log = TRUE) - dpois(ytrue, ytrue, log = TRUE), na.rm = TRUE)

Frequency_data <- read.csv("C:/Users/Samuel Tober/Desktop/Projekt/KEX/Data/Frequency_data.csv")
Frequency_data$FREQ_CLASS <- factor(Frequency_data$FREQ_CLASS, ordered=FALSE)
Frequency_data$OFFSET <- offset(log(Frequency_data$EXP_COV))

keeps <- c("LIVE_AREA", "AGE_INSUR_PERS", "NO_INSUR", "ACCOM_TYPE_NAME", 
           "NO_CLAIM_NOT_NULL", "OFFSET")

Frequency_data <- Frequency_data[keeps]
sample <- sample.split(Frequency_data, SplitRatio = 0.85)
train = subset(Frequency_data, sample==TRUE)
test = subset(Frequency_data, sample==FALSE)

response <-     NO_CLAIM_NOT_NULL ~ 
  AGE_INSUR_PERS + 
  NO_INSUR + 
  LIVE_AREA + 
  ACCOM_TYPE_NAME + 
  OFFSET

#########################################################################################################################################
#######################################CROSS VALIDATION TUNING##########################################################
#########################################################################################################################################

K = 6
fold_indices = 1:K
  
#Randomly shuffle the data
data <- Frequency_data[sample(nrow(Frequency_data)),]
params <- setNames(data.frame(matrix(NA, nrow = 6, ncol = 2)), c("optnodes", "optdepth"))

#Create K equally size folds
folds <- cut(seq(1, nrow(data)), breaks=K, labels=FALSE)
test_error_k = c()

hyper_grid <- expand.grid(
  interaction.depth = c(1,3,5,7),
  bag.fraction = c(0.5, 0.75, 1),   
  n.minobsinnode = c(100, 500, 1000)
)

for (k in 1:K) {
    print(paste(c("Fold: ", k), collapse = " "))
    test_set = data[which(folds==k, arr.ind=TRUE), ]
    validation_error_k = c()
  
     for (i in 1:nrow(hyper_grid)) {
          print(paste(c("params: ", hyper_grid[i,]), collapse = " "))
          validation_error_sum = 0
          
          for (l in setdiff(fold_indices, k)) {
              print(paste(c("Validation set: ", l), collapse = " "))
              validation_set = data[which(folds==l, arr.ind=TRUE), ]
              train_set_kl = data[which((folds!=k & folds!=l), arr.ind=TRUE), ]
              gbm_kl <- gbm::gbm(
                formula = response,
                distribution = "poisson",
                data = train_set_kl,
                n.trees = 7000,
                interaction.depth = hyper_grid$interaction.depth[i],
                shrinkage = 0.01,
                n.minobsinnode = hyper_grid$n.minobsinnode[i],
                bag.fraction = hyper_grid$bag.fraction[i],
                train.fraction = .75,
                n.cores = NULL, # will use all cores by default
                verbose = FALSE
              )

              pred_kl <- predict(gbm_kl, validation_set)
              validation_error_kl = dev_poiss(validation_set[,5], exp(pred_kl))
              
              print(validation_error_kl)
              validation_error_sum = validation_error_sum + validation_error_kl
          }
          validation_error_k = c(validation_error_k, validation_error_sum/5) 
    }
    
    train_set_k = data[which(folds!=k, arr.ind=TRUE), ]
    optimal_params = hyper_grid[which.min(validation_error_k),]
    
    opt_depth <- optimal_params$interaction.depth
    opt_nodes <- optimal_params$n.minobsinnode
    opt_fraction <- optimal_params$bag.fraction
    
    params$optdepth[k] <- opt_depth
    params$optnodes[k] <- opt_nodes
    params$optfraction[k] <- opt_fraction

    gbm_k <- gbm::gbm(
      formula = response,
      distribution = "poisson",
      data = train_set_k,
      n.trees = 7000,
      interaction.depth = opt_depth,
      shrinkage = 0.01,
      n.minobsinnode = opt_nodes,
      bag.fraction = opt_fraction,
      train.fraction = .75,
      n.cores = NULL, # will use all cores by default
      verbose = FALSE
    )
    
    pred_k = predict(gbm_k, test_set)
    test_error = dev_poiss(test_set[,5], exp(pred_k))
    test_error_k = c(test_error_k, test_error)
    
    save(test_error_k, file="test_error.RData")
}
