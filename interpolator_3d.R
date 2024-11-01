# Function to create a 3D interpolator object with distance-based weighting
interpolator_3d <- function(x, y, z, values) {
  
  # Check that all inputs have the same length
  if (length(x) != length(y) || length(y) != length(z) || length(z) != length(values)) {
    stop("All input vectors (x, y, z, values) must have the same length")
  }
  
  # Define the predict method using inverse distance weighting
  predict_3d <- function(x_target, y_target, z_target) {
    
    # Calculate distances from the target point to each known point
    distances <- sqrt((x - x_target)^2 + (y - y_target)^2 + (z - z_target)^2)
    
    # Avoid division by zero if a point is exactly at the target location
    if (any(distances == 0)) {
      return(values[which.min(distances)])  # Return the exact match
    }
    
    # Calculate weights as inverse of distances
    weights <- 1 / distances
    
    # Perform weighted average based on inverse distances
    interp_value <- sum(weights * values) / sum(weights)
    
    return(interp_value)
  }
  
  # Return the interpolator object with the predict method
  return(list(predict = predict_3d))
}

# # Example usage:
# x <- c(1, 2, 3, 4, 5)
# y <- c(5, 4, 3, 2, 1)
# z <- c(1, 2, 3, 4, 5)
# values <- c(10, 20, 30, 40, 50)
# 
# # Create the interpolator object
# interpolator <- interpolator_3d(x, y, z, values)
# 
# # Use the predict method to interpolate at a new point
# interp_value <- interpolator$predict(1, 4.8, 1)
# print(interp_value)
