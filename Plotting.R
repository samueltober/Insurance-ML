library(ggplot2)
library(tidyr)

###############NOTE:###################################################
###############ALL THE BELOW ASSUMES YOU HAVE A GBM,###################
###############RANDOM FOREST AND DECISION TREE MODEL################### 
###############STORED IN YOU WORKSPACE#################################

############VARIABLE IMPORTANCE PLOTS#######################

##RANDOM FOREST####
importance_rf <- distRforest::importance_rforest(rf_freq)
ordering <- seq_len(nrow(importance_rf))
importance_rf <- importance_rf[c("variable", "importance")]
importance_rf <- importance_rf[-1,]
names_rf <- importance_rf[,1]
variables <- importance_rf[,2]
variables <- round(variables/sum(variables) * 100)

###TREE######
importance_tree <- tree_freq$variable.importance
importance_tree <- importance_tree[c("NO_INSUR", "AGE_INSUR_PERS", "ACCOM_TYPE_NAME", "LIVE_AREA")]
importance_tree <- round(100*importance_tree/sum(importance_tree))
importance_tree <- sort(importance_tree, decreasing=TRUE)
names_tree <- names(importance_tree)

####GBM####
sum_gbm <- summary(gbm_freq)
sum_gbm <- sum_gbm[-1,]
importance_gbm <- sum_gbm$rel.inf
importance_gbm <- round(100*importance_gbm/sum(importance_gbm))
names_gbm <- sum_gbm$var
ordering <- seq_len(length(importance_gbm))

par(mar=c(5,10,5,3))
barplot(importance_tree[ordering[length(importance_tree):1]], 
        horiz=TRUE, 
        col=rainbow(length(importance_tree),
                    start=3/6,end=4/6),
        names.arg=names_tree[ordering[length(importance_tree):1]],
        xlab="Relative Importance (%)",
        cex.names = 1,
        las=1,
        xlim=c(0,75),
        main="Variable Importance for Decision Tree")

par(mar=c(5,10,5,3))
barplot(variables[ordering[length(variables):1]], 
        horiz=TRUE, 
        names.arg=names_rf[ordering[length(variables):1]], 
        col=rainbow(nrow(importance_rf),
                    start=3/6,end=4/6),
        xlab="Relative Importance (%)",
        cex.names = 1,
        las=1,
        main="Variable Importance for Random Forest")

par(mar=c(5,7,5,3))
barplot(importance_gbm[ordering[length(importance_gbm):1]], 
        horiz=TRUE, 
        names.arg=names_gbm[ordering[length(names_gbm):1]], 
        col=rainbow(length(importance_gbm),
                    start=3/6,end=4/6),
        xlab="Relative Importance (%)",
        cex.names = 1,
        las=1,
        xlim=c(0,40),
        main="Variable Importance for Gradient Boosting Machine")


gbm = c(0.1546552, 0.1582619, 0.1623110, 0.1530835, 0.1491646, 0.1503093)
rf = c(0.1587195, 0.1480451, 0.1502557, 0.1569966, 0.1526600, 0.1625464)
tree = c(0.1717359, 0.1602620, 0.1561908, 0.1543104, 0.1637123, 0.1582236)

fold = c(1, 2, 3, 4, 5, 6)
errors <- data.frame(tree, rf, gbm, fold)
dd = melt(errors, id=c("fold"))
names(dd)[names(dd) == 'variable'] <- 'Model'

ggplot(dd) + geom_line(aes(x=fold, y=value, colour=Model)) +
        geom_point(aes(x=fold, y=value, colour=Model)) +
        scale_colour_manual(values=c("red","green","blue")) +
        labs(x="Fold", y="Poisson Deviance", title="Poisson Deviance for Cross-validation Folds") +
        theme(plot.title = element_text(hjust = 0.5, face="bold"))


plot(, type="l", col="darkseagreen", lwd=2, 
     xlab="Fold", ylab="Poisson deviance", main="Poisson deviance of random forest and decision tree", 
     ylim=c(0.14,0.18))
lines(tree_error, type="l", col="orange", lwd=2)
points(test_error_k, pch=17)
points(tree_error, pch=15)
legend(1.5, 0.18, legend=c("Decision tree", "Random forest"),
       col=c("orange", "darkseagreen"), lty=1:2, cex=0.8,
       title="Models", text.font=4)


##########PARTIAL DEPENDENCY PLOTS######################
par_dep <- function(object, data, grid, pred_fun) {
        # Initialize a vector to save the effect
        pd_effect <- rep(0, nrow(grid))
        # Iterate over the grid values to calculate the effect
        for (i in seq_len(length(pd_effect))) {
                pd_effect[i] <- 
                        data %>% 
                        dplyr::mutate(!! names(grid) := grid[i, ]) %>% 
                        pred_fun(object, newdata = .) %>% 
                        mean()
        }
        return(pd_effect)
}

exp_pred <- function(object, newdata) {
        return(exp(predict(object, newdata)))
}

set.seed(123)
mtpl_trn_sample <- Frequency_data[sample(seq_len(nrow(Frequency_data)), size = 10000), ]
# Define the grid for the ages
grid_age <- data.frame('AGE_INSUR_PERS' = 18:100)
# Calculate the PD effect for each ML model
grid_age <- grid_age %>% 
        dplyr::mutate(tree = tree_freq %>% par_dep(data = mtpl_trn_sample,
                                                   grid = grid_age, pred_fun=predict),
                      rf = rf_freq %>% par_dep(data = mtpl_trn_sample,
                                               grid = grid_age, pred_fun=distRforest::predict.rforest),
                      gbm = gbm_freq %>% par_dep(data = mtpl_trn_sample,
                                                 grid = grid_age, pred_fun=exp_pred))

grid_no %>% reshape2::melt(id.vars = 'NO_INSUR',
                              value.name = 'pd',
                              variable.name = 'Model') %>% 
        ggplot(aes(x = NO_INSUR, y = pd)) + 
        geom_line(aes(group = Model, colour = Model)) + 
        labs(x="No. People in Household", y="Partial dependency", title="Partial Dependency Plot - Frequency/No. people") +
        theme(plot.title = element_text(hjust = 0.5, face="bold"))
        

#################PERCENTLE PLOTS##########################
quantilePlot(rf, test, NULL, NULL)

############DATA OVERVIEW#######################
claims <- table(Frequency_data$NO_CLAIM_NOT_NULL)
claims <- round(claims/sum(claims)*100)
expo <- Frequency_data$EXP_COV
              
h <- hist(expo)
h$density = h$counts/sum(h$counts)*100

plot(h, freq=FALSE, breaks=30, ylim=c(0,15), col="#3895D3", ylab="Percentage of Exposure Times (%)",
     xlab="Exposure Time (years)", main = "Histogram of Exposure Times")
barplot(claims, col="#3895D3", ylab="Percentage of claims (%)", ylim=c(0,100), xlab="Number of claims",
        main = "Histogram of Number of Claims")
