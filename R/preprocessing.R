#' Reshape data into monthly, long, scaled version
#'
#' @param data the raw data temperature data.frame
#' @param scale logical, default is false. If the data should be scaled
#' @return data.frame
#' @examples
#' monthlyAdj(data)
#' @importFrom dplyr select mutate arrange
#' @importFrom tidyr gather
#' @importFrom lubridate ymd
#' @export
monthlyAdj <- function(data, scale = F) {
  # Scale data if required (produces normalized data)
  if(scale == T) {
    scaledData <- scale(data[,2:13])
    data[,2:13] <- scaledData
  }
  # Change colnames to month numbers
  colnames(data)[2:13] <- 1:12
  # Data Manipulation: Gather data and create a Time variable
  mdata <- data %>%
    select(-V14) %>%
    gather(var, temp, -V1, na.rm =T) %>%
    mutate(Time = ymd(paste(V1, var, "01", sep = "-"))) %>%
    select(Time, temp) %>%
    arrange(Time)
  # Rename the temp column to Zm if scaling was required
  if(scale == T) colnames(mdata)[2] <- "Zm"
  # Return
  return(mdata)
}
