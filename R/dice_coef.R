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


#' Weighted Dice Coefficient
#'
#' @param y_true (vector) True classes
#' @param y_pred (vector) Predicted classes
#' @param axis   (vector) Axis where to perform operations
#' @param smooth (real) Smoothness factor in the computation, Default: 0.00001
#'
#' @return The weighted dice coefficient
#' @export
#'
weighted_dice_coefficient <- function(y_true, 
                                      y_pred, 
                                      axis = c(-3L, -2L, -1L), 
                                      smooth = 0.00001) {
  
  K <- keras::backend()
  
  K$mean(2. * (K$sum(y_true * y_pred,
                     axis = axis) + smooth / 2) / 
           (K$sum(y_true, axis = axis) + 
              K$sum(y_pred, axis = axis) + smooth))
  
}


weighted_dice_coefficient_loss <- function(y_true, y_pred) {
  
  return(-weighted_dice_coefficient(y_true, y_pred))
}

# def label_wise_dice_coefficient(y_true, y_pred, label_index):
#   return dice_coefficient(y_true[:, label_index], y_pred[:, label_index])
# 
# 
# def get_label_dice_coefficient_function(label_index):
#   f = partial(label_wise_dice_coefficient, label_index=label_index)
# f.__setattr__('__name__', 'label_{0}_dice_coef'.format(label_index))
# return f
# 
# 
# dice_coef = dice_coefficient
# dice_coef_loss = dice_coefficient_loss
