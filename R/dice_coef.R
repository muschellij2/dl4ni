#' @title Dice Coefficient
#' @description This function computes the Dice coefficient between a true and a predicted classifications.
#' 
#' @param y_true (vector) True classes
#' @param y_pred (vector) Predicted classes
#' @param smooth (real) Smoothness factor in the computation, Default: 1
#' 
#' @return The Dice coefficient.
#' 
#' @export 
#' 
dice_coef <- function(y_true, y_pred, smooth = 1.0) {

  K <- keras::backend()
  
  y_true_f <- K$flatten(y_true)
  y_pred_f <- K$flatten(y_pred)
  
  intersection <- K$sum(y_true_f * y_pred_f)
  
  result <- (2 * intersection + smooth) / 
    (K$sum(y_true_f) + K$sum(y_pred_f) + smooth)
  
  return(result)
  
}

#' @title Dice Loss
#' @description This function computes the Dice loss between a true and a predicted classifications.
#' 
#' @param y_true (vector) True classes
#' @param y_pred (vector) Predicted classes
#' @param smooth (real) Smoothness factor in the computation, Default: 1
#' 
#' @return The Dice loss as \code{1 - Dice Coefficient}.
#' 
#' @export 
#' 
dice_loss <- function(y_true, y_pred, smooth = 1.0) {
  
  result <- 1 - dice_coef(y_true, y_pred, smooth)
  
  return(result)
  
}

#' @title Binary Cross Entropy and Dice Loss
#' @description This function computes the Binary Cross Entropy and Dice Loss between 
#' a true and a predicted classifications.
#' 
#' @param y_true (vector) True classes
#' @param y_pred (vector) Predicted classes
#' 
#' @return The Dice coefficient.
#' 
#' @export 
#' 
bce_dice_loss <- function(y_true, y_pred) {
  
  result <- keras::loss_binary_crossentropy(y_true, y_pred) + dice_loss(y_true, y_pred)
  
  return(result)
  
}
