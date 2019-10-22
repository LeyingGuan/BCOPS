#' Training BCOPs score functions with a user-specified classifier
#' @param classifier a user-specified classifier. Note that the classifier can either takes input as feature matrix x and response y, or a dataframe contains features and response and a formula.
#' @param x n by p feature matrix for the training data to be used in constructing $v_k(x)$.
#' @param y length n class labels for the training data to be used in constructing $v_k(x)$.
#' @param labels labels for the K training classes.
#' @param xte m by p features for the test data to be used in constructing $v_k(x)$.
#' @param formula boolean variable indicating if the classifier takes data argument as classifier(formula, data,...) or  classifier(x, y,...). The function ranger is an example of the former, and the function cv.glmnet is an example of the later.
#' @param ... other arguments depending on the classifier. Note the argument is applied to the augmented data by stacking x and xte together when building a binary classifier.
#' @return models a list trained models that is used for predict the BCOPs score for each new observation.
#' @examples 
#' \dontrun{
#' data(mnist); 
#' xtrain = mnist[['data']][['x']]; ytrain = mnist[['data']][['y']]; 
#' xtest = mnist[['data_te']][['x']]; ytest=mnist[['data_te']][['y']]; 
#' ########split the training and test data into two halfs for conformal prediction##############
#' set.seed(123)
#' foldid = sample(1:2, length(ytrain), replace = TRUE); foldid_te = sample(1:2,length(ytest), replace = TRUE)
#' xtrain1 = xtrain[foldid==1,]; xtrain2 = xtrain[foldid==2,]; ytrain1 = ytrain[foldid==1]; ytrain2=ytrain[foldid==2]
#' xtest1 = xtest[foldid_te ==1,]; xtest2 = xtest[foldid_te==2,]; 
#' labels = sort(unique(ytrain)) 
#' #########example using cv.glmnet##############
#'  require(glmnet)
#'  models1 = BCOPS.train(cv.glmnet, xtrain2, ytrain2, labels, xtest2)
#'  models2 = BCOPS.train(cv.glmnet, xtrain1, ytrain1, labels, xtest1)
#' #########example using random forest##########
#'  require(ranger)
#'  models1 = BCOPS.train(ranger, xtrain2, ytrain2, labels, xtest2, formula = TRUE)
#'  models2 = BCOPS.train(ranger, xtrain1, ytrain1, labels, xtest1, formula = TRUE)
#'  }
#' @export
BCOPS.train <- function(classifier, x, y, labels, xte, formula = FALSE, ...){
  K = length(labels);models = list()
  for(k in 1:K){
    xk = rbind(xte,x[y==labels[k],])
    yk = c(rep(0, nrow(xte)),rep(1,sum(y==labels[k])))
    if(length(yk) == 0){
      stop(paste0("class ", labels[k], " does not exist in the training data!"))
    }
    if(!formula){
    models[[k]] = classifier(x = xk, y = yk, ...)
    }else{
      temp = ncol(xk)
      data = cbind(xk, yk); data = data.frame(data); colnames(data) = c(paste0("feature", (1:temp)), "response")
      models[[k]] = classifier(formula = response~., data = data, ...)
    }
  }
   class(models) = "model.list"
   return(models)
}
