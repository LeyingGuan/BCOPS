#' Evaluate  the conformal prediction results
#' @param prediction the m by K prediction matrix containing the constructed conformal p-value for m test samples and K training classes.
#' @param yte class labels for the test samples.
#' @param labels the K class labels for the training samples.
#' @param alpha the targeted type I error.
#' @return a result table with the columns being the classes in the test samples, and the rows being the classes in the training samples. 
#' The entry at row j and column k represents the percent of samples in class j assigned label k.
#' @examples 
#' ### Examples
#' \dontrun{
#' data(mnist); 
#' xtrain = mnist[['data']][['x']]; ytrain = mnist[['data']][['y']]; 
#' xtest = mnist[['data_te']][['x']]; ytest=mnist[['data_te']][['y']]; 
#' #########data splitting##############
#' set.seed(123)
#' foldid = sample(1:2, length(ytrain), replace = TRUE); foldid_te = sample(1:2,length(ytest), replace = TRUE)
#' xtrain1 = xtrain[foldid==1,]; xtrain2 = xtrain[foldid==2,]; 
#' ytrain1 = ytrain[foldid==1]; ytrain2=ytrain[foldid==2]
#' xtest1 = xtest[foldid_te ==1,]; xtest2 = xtest[foldid_te==2,]; 
#' labels = sort(unique(ytrain)) 
#' #########example using random forest##############
#' require(ranger)
#' bcops = BCOPS(ranger, xtrain1, ytrain1, xtest1, xtrain2, ytrain2, xtest2, labels, formula = TRUE, prediction_only = FALSE)
#' prediction.conformal = matrix(NA, ncol = length(labels), nrow = length(ytest))
#' prediction.conformal[foldid_te==1,] = bcops[[1]];
#' prediction.conformal[foldid_te==2,] = bcops[[2]];
#' evaluate.conformal(prediction.conformal, ytest, labels, 0.05)
#' }
#' @export
evaluate.conformal <-function(prediction, yte, labels, alpha = 0.05){
  labels_te = sort(unique(yte))
  res <- matrix(NA, nrow = length(labels_te), ncol = length(labels))
  for(i in 1:length(labels_te)){
    ii = which(yte == labels_te[i])
    res[i,] = apply(prediction[ii,] >=alpha, 2, mean)
  }
  res = data.frame(res)
  colnames(res) = as.character(labels)
  rownames(res) = as.character(labels_te)
  print("calculating percent of test samples in each of the test class (rows) assigned each of the labels in the training class (columns)")
  return(res)
}

