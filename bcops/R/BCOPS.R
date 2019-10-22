#' BCOPS function that does both training and prediction.
#' @param classifier a user-specified classifier. Note that the classifier can either takes input as feature matrix x and response y, or a dataframe contains features and response and a formula.
#' @param x1 n1 by p feature matrix for the first fold of training data.
#' @param y1 length n1 class labels for the first fold of training data.
#' @param xte1 m1 by p feature matrix for the first fold of test data.
#' @param x2 n1 by p feature matrix for the first fold of training data.
#' @param y2 length n1 class labels for the first fold of training data.
#' @param xte2 m2 by p feature matrix for the first fold of test data.
#' @param labels labels for the K training classes.
#' @param formula boolean variable indicating if the classifier takes data argument as classifier(formula, data,...) or  classifier(x, y,...). The function ranger is an example of the former, and the function cv.glmnet is an example of the later.
#' @param prediction_only boolean variable indicating whether predict(classifier,...) returns directly the predictions (TRUE) or need to be accessed by a specific name.
#' @param name the name we can use to access the predictions. By default, name = "predictions".
#' @param ... other arguments depending on the classifier. Note the argument is applied to the augmented data by stacking x and xte together when building a binary classifier.
#' @return conformal.scores1: The m1 by K conformal scores for the first fold of test samples xte1.
#' @return conformal.scores2: The m2 by K conformal scores for the second fold of test samples xte2.
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
#'  bcops1 = BCOPS(cv.glmnet, xtrain1, ytrain1, xtest1, 
#'                  xtrain2, ytrain2, xtest2, labels, formula = FALSE)
#' #########example using random forest##########
#'  require(ranger)
#'  bcops2 = BCOPS(ranger, xtrain1, ytrain1, xtest1, xtrain2, ytrain2, xtest2, labels, formula = TRUE, prediction_only = FALSE)
#'  }
#' @export
BCOPS <- function(classifier, x1, y1, xte1,
                  x2, y2, xte2, labels,  
                  formula = FALSE, prediction_only = TRUE, name = "predictions", ...){
  ##training
  models1 = BCOPS.train(classifier = classifier, x = x2, y = y2, labels = labels,
                        xte = xte2, formula = formula,...)
  models2 = BCOPS.train(classifier = classifier, x = x1, y = y1, labels = labels,
                        xte = xte1, formula = formula,...)
  ##prediction
  prediction1 = BCOPS.prediction(models = models1, x1, y1, labels , xte1,formula = formula, prediction_only = prediction_only, name = name)$prediction.conformal
  prediction2 = BCOPS.prediction(models = models2, x2, y2, labels , xte2,formula = formula, prediction_only = prediction_only, name = name)$prediction.conformal
  return(list(conformal.scores1 = prediction1, conformal.scores2 = prediction2))
}
