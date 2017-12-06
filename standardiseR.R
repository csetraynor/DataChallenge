#standardise function
standardise <- function(x){
  fmean <- mean(x, na.rm = TRUE)
  fsd <- sd(x, na.rm = TRUE)
  standardisedata <- (x-fmean)/fsd
}
