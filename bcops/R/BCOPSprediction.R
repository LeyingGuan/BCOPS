#' BCOPS prediction function
#' @param models a list of models calculating the predicted $v_k(x)$ for each class, it is an object from BCOPS.train. 
#' @param x n by p feature matrix for the training data to be used in conformal inference.
#' @param y length n class labels for the training data to be used in the conformal inference.
#' @param labels Labels for the K classes when applying BCOPS.train.
#' @param xte m by p features for the test data to be used in the conformal inference.
#' @param formula boolean variable indicating if the classifier takes data argument as classifier(formula, data,...) or  classifier(x, y,...). The function ranger is an example of the former, and the function cv.glmnet is an example of the later.
#' @param prediction_only boolean variable indicating whether predict(classifier,...) returns directly the predictions (TRUE) or need to be accessed by a specific name.
#' @param name the name we can use to access the predictions. By default, name = "predictions".
#' @return prediction.conformal a m by K matrix for m test samples, it is the conformal constructed p-value for a test sample not from each of the K classes. If we want to control the type I error at alpha, then, we assign all class labels whose conformal p-value is no smaller than alpha to the test samples.
#' @return scores_test a m by K matrix for m test samples and K classes, each entry is the value evaluated at a test sample using score function for a training class. 
#' @return scores_train a n by K matrix for n training samples, each entry is the value evaluated at a training sample using score function for a training class. 
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
#'  prediction.conformal1 = BCOPS.prediction(models = models1, xtrain1, ytrain1, labels , xtest1)$prediction.conformal
#'  models2 = BCOPS.train(cv.glmnet, xtrain1, ytrain1, labels, xtest1)
#'  prediction.conformal2 = BCOPS.prediction(models = models2, xtrain2, ytrain2, labels , xtest2)$prediction.conformal
#' #########example using random forest##########
#'  require(ranger)
#'  models1 = BCOPS.train(ranger, xtrain2, ytrain2, labels, xtest2, formula = TRUE)
#'  prediction.conformal1 = BCOPS.prediction(models = models1, xtrain1, ytrain1, labels , xtest1, formula = TRUE, prediction_only = FALSE)$prediction.conformal
#'  models2 = BCOPS.train(ranger, xtrain1, ytrain1, labels, xtest1, formula = TRUE)
#'  prediction.conformal2 = BCOPS.prediction(models = models2, xtrain2, ytrain2, labels , xtest2,formula = TRUE, prediction_only = FALSE)$prediction.conformal
#'  }
#' @export
BCOPS.prediction <- function(models, x, y, labels, xte, formula = FALSE, prediction_only = TRUE, name = "predictions"){
  K = length(labels)
  s = matrix(NA, nrow = length(y), ncol = K); ste = matrix(NA, nrow = nrow(xte), ncol = K)
  #get the predicted scores
  if(formula){
    x = data.frame(x); xte = data.frame(xte);colnames(x) <- colnames(xte) <- paste0("feature", (1:ncol(x)))
  }
  for(k in 1:K){
    if(is.null(models[k])){
      stop(paste0("training class ", k, " does not exist in trained models!"))
    }else{
      if(sum(y==labels[[k]]) > 0){
        temp1 = predict(models[[k]], x)
        temp2 = predict(models[[k]], xte)
        if(!prediction_only){
          temp1 = temp1[name];temp2 = temp2[name]
          if(is.null(temp1) | is.null(temp2)){
            stop("can't find the predictions!")
          }
          temp1 = temp1[[1]]; temp2 = temp2[[1]]
        }
        temp3 = ncol(temp1)
        if(is.null(temp3)){
          s[,k] = temp1
          ste[,k] = temp2
        }else{
          s[,k] = temp1[,temp3]
          ste[,k] = temp2[,temp3]
        }
    }
    }
  }
  prediction.conformal = conformal_scores(ste = ste, s = s, y = y, labels = labels)
  return(list(prediction.conformal = prediction.conformal, score_test = ste, score_train = s))
}

