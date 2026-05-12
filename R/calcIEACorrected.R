calcIEACorrected <- function() {
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  # apply corrections to IEA data to cope with fragmentary time series
  data <- toolFixIEAdataForIndustrySubsectors(data, fixing = TRUE)

  return(list(
    x = data,
    weight = NULL,
    unit = "EJ",
    description = "IEA data with industry corrections"
  ))
}
