identity_matrix <- function() {
  
  M <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), ncol = 4)
  
  return(M)
  
}

translation_matrix <- function(tx = 0, ty = 0, tz = 0) {
  
  M <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, tx, ty, tz, 1), ncol = 4)
  
  return(M)
  
}

rotation_matrix_along_axis <- function(angle = 0, ax = 1, ay = 0, az = 0) {
  
  radAngle <- angle * pi / 180.0
  cos_angle <- cos(radAngle / 2)
  sin_agle <- sin(radAngle / 2)
  
  q0 <- cos_angle;
  q1 <- sin_agle * ax
  q2 <- sin_agle * ay
  q3 <- sin_agle * az
  
  M <- matrix(0, nrow = 4, ncol = 4)
  
  M[1, 1] <- 1 - 2 * (q2 * q2 + q3 * q3)
  M[1, 2] <-     2 * (q1 * q2 + q3 * q0)
  M[1, 3] <-     2 * (q3 * q1 - q2 * q0)
  M[1, 4] <- 0
  
  M[2, 1] <-     2 * (q1 * q2 - q3 * q0)
  M[2, 2] <- 1 - 2 * (q3 * q3 + q1 * q1)
  M[2, 3] <-     2 * (q2 * q3 + q1 * q0)
  M[2, 4] <- 0
  
  M[3, 1] <-     2 * (q3 * q1 + q2 * q0)
  M[3, 2] <-     2 * (q2 * q3 - q1 * q0)
  M[3, 3] <- 1 - 2 * (q2 * q2 + q1 * q1)
  M[3, 4] <- 0
  
  M[4, 1] <- 0
  M[4, 2] <- 0
  M[4, 3] <- 0
  M[4, 4] <- 1
  
  return(M)
  
}

rotation_matrix <- function(angle_x = 0, angle_y = 0, angle_z = 0) {
  
  M <- rotation_matrix_along_axis(angle = angle_z, ax = 0, ay = 0, az = 1) %*%
    rotation_matrix_along_axis(angle = angle_y, ax = 0, ay = 1, az = 0) %*%
    rotation_matrix_along_axis(angle = angle_x, ax = 1, ay = 0, az = 0)
  
  return(M)
  
}

scale_matrix <- function(sx = 1, sy = 1, sz = 1) {
  
  M <- matrix(c(sx, 0, 0, 0, 0, sy, 0, 0, 0, 0, sz, 0, 0, 0, 0, 1), ncol = 4)
  
  return(M)
  
}

transformation_matrix <- function(scales = c(1, 1, 1), 
                                  rotations = c(0, 0, 0), 
                                  translations = c(0, 0, 0)) {
  
  M <- translation_matrix(translations[1], translations[2], translations[3]) %*%
    rotation_matrix(rotations[1], rotations[2], rotations[3]) %*%
    scale_matrix(scales[1], scales[2], scales[3])
  
  return(M)
  
}

random_transformation_matrix <- function(scale_range = c(0.9, 1.1),
                                         rotation_range = c(-10, 10),
                                         translation_range = c(-3, 3)) {
  
  scales <- runif(3) * diff(scale_range) + min(scale_range)
  rotations <- runif(3) * diff(rotation_range) + min(rotation_range)
  translations <- runif(3) * diff(translation_range) + min(translation_range)
  
  M <- transformation_matrix(scales = scales,
                             rotations = rotations,
                             translations = translations)
  
  return(M)
  
}

