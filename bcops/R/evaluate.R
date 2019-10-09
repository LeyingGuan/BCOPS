#' Evaluate  the conformal prediction results
#' @param prediction the n by K prediction matrix containing the constructed conformal p-value for n test samples and K training classes.
#' @param yte actual class labels for the test samples.
#' @param labels the K class labels for the training samples.
#' @param alpha the targeted type I error.
#' @return a result table with the columns being the classes in the test samples, and the rows being the classes in the training samples. 
#' The entry at row j and column k represents the percent of samples in class j assigned lable k.
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

