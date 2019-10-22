#' mixtureEst estimates the proportion of each of the training classes in the test samples based on density estimation form the function kde.
#' @param scores_test m by K score matrix where m is the size of test samples and K is the number of training classes. 
#' @param scores_train n by K score matrix where n is the size of training samples and K is the number of training classes.
#' @param y label of the training samples.
#' @param labels K training class labels.
#' @param zeta for each class k, we do not consider samples whose density is below zeta quantile of density distribution of training samples in class k. By default, zeta = .2.
#' @param ... parameters to feed into function kde.
#' @return pi : estimated new mixture proportions.
#' @import ks
#' @import ranger
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
#' require(ranger)
#' models1 = BCOPS.train(ranger, xtrain2, ytrain2, labels, xtest2, formula = TRUE)
#' models2 = BCOPS.train(ranger, xtrain1, ytrain1, labels, xtest1, formula = TRUE)
#' prediction1=BCOPS.prediction(models = models1, xtrain1, ytrain1, labels , xtest1, formula = TRUE, prediction_only = FALSE)
#' scores_test = prediction1$score_test; scores_train = prediction1$score_train
#' pi1 = mixtureEst(scores_test, scores_train, ytrain1, labels = labels, zeta = .2, gridsize=512)
#' prediction2=BCOPS.prediction(models = models2, xtrain2, ytrain2, labels , xtest2, formula = TRUE, prediction_only = FALSE)
#' scores_test = prediction2$score_test; scores_train = prediction2$score_train
#' pi2 = mixtureEst(scores_test, scores_train, ytrain2, labels = labels, zeta = .2, gridsize=512)
#' pi = (pi1+pi2)/2
#' }
#' @export
mixtureEst <-function(scores_test, scores_train, y, labels , zeta = .2, ...){
  K = length(labels)
  ker_model <- list()
  for(k in 1:K){
    sk = scores_train[y == labels[k],k]
    ker_model[[k]] = kde(sk,...)
  }
  density_train = matrix(0, ncol = K, nrow = nrow(scores_train))
  density_test = matrix(0, ncol = K, nrow = nrow(scores_test))
  thr = rep(NA,K)
  S = matrix(NA, K,K); Y = rep(NA, K)
  for(k in 1:K){
    density_train[,k] = predict(ker_model[[k]], x=scores_train[,k])
    density_test[,k] = predict(ker_model[[k]], x=scores_test[,k])
    thr[k] = quantile(density_train[y==labels[k],k],zeta)
    Y[k] = mean(density_test[,k] >= thr[k])
    S[k,] = sapply(1:K, function(i) mean(density_train[y==labels[i],k] >= thr[k]))
  }
  pi = lm(Y~S-1)$coefficients
  names(pi) = labels
  return(pi)
}