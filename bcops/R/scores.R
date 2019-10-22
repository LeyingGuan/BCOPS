#' Conformal prediction for any given scores
#' @param ste the list of scores for test samples corresponding to each of the training class.
#' @param s the vector scores for training samples corresponding to its own class.
#' @param y labels for the training data.
#' @param labels labels for the K classes.
#' @import Rcpp
#' @useDynLib bcops
#' @return prediction_tr: conformal scores for the training samples below to its own class.
#' @return prediction_tr: conformal scores for the test samples corresponding to each of the training class.
conformal_scores <-function(ste, s, y, labels){
  prediction_te = matrix(NA, nrow(ste), ncol = length(labels))
  K = length(labels)
  for(k in 1:K){
    sk = s[y == labels[k],k]
    if(length(sk) == 0){
      print(paste0("class ", labels[k], " does not exist in the training data for conforml prediction!"))
    }else{
      prediction_te[,k] =conformalscore(ste[,k], sk)
      prediction_te[,k] = (prediction_te[,k]+1)/(length(sk)+1)
    }
  }
  return(prediction_te=prediction_te)
}
