# Helper functions for GDP and Population scenario construction

###########################################
# Extrapolation function to extend SSP series until 2150
#
#
###########################################
# TODO: generalize the dates
bezierExtension <- function(data, time_extend) {

  extension <- new.magpie(getRegions(data), time_extend, getNames(data), fill = 0)

  slope = (data[,2100,]-data[,2095,])/5

  nr <- nregions(data)

  for (scen in getNames(data)) {
    y_start <- data[,2100,scen] %>% as.matrix()
    x_start <- matrix(2100, nrow = 249, ncol = 1)
    slope_start <- slope[,,scen] %>% as.matrix()

    slope_end <- slope_start / 2
    y_end <- y_start + slope_end * 50
    x_end <- matrix(2150, nrow = nr, ncol = 1)

    x_1 <- matrix(2110, nrow = nr, ncol = 1)
    y_1 <- y_start + slope_start * 10
    x_2 <- matrix(2140, nrow = nr, ncol = 1)
    y_2 <- y_end - slope_end * 10

    for (i in 1:nr) {
      if (y_start[i] == 0 || y_end[i] < 0) {
        next
      }

      p <- matrix(c(x_start[i],y_start[i], x_1[i],y_1[i], x_2[i],y_2[i], x_end[i],y_end[i]),
                  nrow = 4,
                  ncol = 2,
                  byrow = TRUE)

      # Get Bezier curve (Use max_dist method because it's super fast)
      bp <- bezier::bezier(t = seq(0, 1, length=10), p = p)
      pob <- bezier::pointsOnBezier(p = p, 
                                    method = "max_dist", 
                                    max.dist = max(abs(y_end[i] - y_start[i]), 50) / 100,
                                    print.progress = TRUE) 
        

      # Get y coordinates of points with x coordinates = time_extend 
      my_bezier_outflow <- pob$points %>% 
        tibble::as_tibble(.name_repair = ~ paste0("V", seq_along(.x))) %>% 
        # Complicatd / elegant use of function factories to get closest points to time_extend coordinates
        dplyr::mutate(dplyr::across(.data$V1, purrr::map(time_extend, ~ function(y){abs(y - .x)}) )) %>% 
        dplyr::filter(dplyr::if_any(tidyselect::contains("_"), ~.x == min(.x))) %>% 
        dplyr::pull(.data$V2)

      extension[i,,scen] <- my_bezier_outflow
    }
  }

  mbind(data, extension)
}


###########################################
# Miscellaneous functions 
#
#
###########################################

# Apply finishig touches to the combined object, as found in calcGDPppp and calcPopulation
finishingTouches <- function(combined, future, FiveYearSteps, naming) {
  if (FiveYearSteps){
    years <- findset("time") 
    combined <- combined[,years,]
  }

  combined <- clean_magpie(combined)
  combined[combined[] == "Inf"] <- 0    # LB: preliminary bug fix
  combined <- setNames(combined, getNames(future))

  if(naming == "indicator.scenario"){
    getNames(combined) <- sub("_",  ".", getNames(combined))
    getSets(combined) <- c("region", "year", "indicator", "scenario")
  } else if (naming != "indicator_scenario"){
    stop("unknown naming scheme")
  }

  combined
}

# Complete GDP or population data with data from "fill", and by time-interpolation
completeData <- function(data, fill) {
  fill <- time_interpolate(fill, interpolated_year = getYears(data), extrapolation_type = "constant")
  missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  
  if (!all(missing %in% getRegions(fill))) {
     missing <- intersect(missing, getRegions(fill))
     warning("Some countries with missing data can't be filled.")
  }

  data[missing,,] <- fill[missing,,]

  still_missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  partially_missing <- setdiff(where(data == 0)$true$region, still_missing)
  for(i in partially_missing){
    missingyears <- where(data[i,,] == 0)$true$years
    data[i, missingyears,] <- time_interpolate(dataset = data[i,,][,missingyears,,invert = TRUE],
                                               interpolated_year = missingyears,
                                               extrapolation_type = "constant")
  }
  data
}








###########################################
# Harmonization functions 
# 
# -> These functions combine past and future data
###########################################

harmonizePast <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- dimSums(future / setYears(future[,firstyear,],NULL) * setYears(past[,firstyear,], NULL), 
                 dim = 3.2)
  tmp[is.nan(tmp)] <- 0
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstyear)]
    tmp2 <- setNames(past[,years_past,rep(1, ndata(future))], getNames(future))
    combined <- mbind(tmp2, tmp)
  } else {
    combined <- tmp
  }
  combined
}

popHarmonizePast <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- future / setYears(future[,firstyear,], NULL)
  tmp[is.nan(tmp)] <- 1
  tmp <- dimSums(tmp * setYears(past[,firstyear,], NULL), dim = 3.2)
  if (firstyear>min(getYears(past,as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past,as.integer = TRUE)<firstyear)]
    tmp2       <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
    combined   <- mbind(tmp2,tmp)
  } else {
    combined <- tmp
  }
}

harmonizeFuture <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- dimSums(past/setYears(past[,firstyear,],NULL) * setYears(future[,firstyear,],NULL),
                 dim = 3.2)
  tmp[is.nan(tmp)] <- 0
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past<-getYears(past)[which(getYears(past, as.integer = TRUE) < firstyear)]
    tmp2 <- setNames(tmp[,years_past,rep(1, ndata(future))], getNames(future))
    combined <- mbind(tmp2, future)
  } else {
    combined <- future
  }
  combined
}

harmonizeTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  # generate past data for all future scenarios
  firstyear <- min(getYears(future, as.integer = TRUE))
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) <= firstyear)]
    tmpPast <- setNames(past[,years_past,rep(1,ndata(future))], getNames(future))
    years_trans <- getYears(future, as.integer = TRUE)[which(getYears(future, as.integer = TRUE) >= firstyear 
                                                             & getYears(future, as.integer = TRUE) <= yEnd)]
    tmpTrans    <- new.magpie(getRegions(future), years_trans, getNames(future), fill=0)
    for(t in years_trans) {
      tmpTrans[,t,] <- (  (max(years_trans) - t)/(max(years_trans) - min(years_trans)) * setYears(tmpPast[,firstyear,],t) 
                        + (t - min(years_trans))/(max(years_trans) - min(years_trans)) * setYears(future[,yEnd,],t)       )
    }  
    combined   <- mbind(tmpPast[,which(getYears(tmpPast,as.integer=TRUE) < firstyear),],
                        tmpTrans,
                        future[,which(getYears(future,as.integer=TRUE) > yEnd),])
  } else {
    stop("The past and future data need to have some overlap")
  }
  combined
}

harmonizePastTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  firstyear <- min(getYears(future, as.integer = TRUE))
  # last year of past data, that also exist in future data
  lastyear <- max(intersect(getYears(past, as.integer = TRUE), getYears(future, as.integer = TRUE)))
  # generate past data for all future scenarios
  if (firstyear < max(getYears(past, as.integer = TRUE))) {                                                 
    years_past  <- getYears(past)[which(getYears(past,as.integer=TRUE) <= lastyear)]
    tmpPast     <- setNames(past[,years_past,rep(1, ndata(future))], getNames(future))
    years_trans <- getYears(future,as.integer=TRUE)[which(getYears(future,as.integer=TRUE) >= lastyear 
                                                          & getYears(future,as.integer=TRUE) <= yEnd)]
    diff_in_lastyear <- tmpPast[,lastyear,] - future[,lastyear,]
    tmpTrans <- new.magpie(getRegions(future), years_trans, getNames(future), fill=0)
    for(t in years_trans) {
      tmpTrans[,t,] <- future[,t,] + setYears(diff_in_lastyear, t) * ( (max(years_trans) - t)/(max(years_trans) - min(years_trans)) )  
    }  
    combined   <- mbind(tmpPast[,which(getYears(tmpPast, as.integer = TRUE) < lastyear),],
                        tmpTrans,
                        future[,which(getYears(future, as.integer = TRUE) > yEnd),])
  } else {  
    stop("The past and future data need to have some overlap")
  }
  combined
}






# TODO: USE PAST DATA UNITL 2019!!
harmonizePastGrFuture <- function(past, future) {
  lastPastYear <- max(intersect(getYears(past, as.integer = TRUE),
                                getYears(future, as.integer = TRUE)))
  firstFutureYear <- min(getYears(future, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
  }

  # Create past data for all future scenarios
  years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) <= lastPastYear)]
  tmpPast <- past[, years_past, rep(1, ndata(future))]
  tmpPast <- setNames(tmpPast, getNames(future))
  tmpPast[is.nan(tmpPast)] <- 0

  # Create transition magpie object for all future scenarios
  years_future <- getYears(future)[which(getYears(future, as.integer = TRUE) > lastPastYear)]
  tmpFuture <- new.magpie(getRegions(future), years_future, getNames(future), fill = 0)

  # Use growth rates of future object
  tmpFuture[,,] <- tmpPast[,lastPastYear,] * future[,years_future,] / future[,lastPastYear,]
  tmpFuture[is.nan(tmpFuture)] <- 0

  mbind(tmpPast, tmpFuture)
}

harmonizeFutureGrPast <- function(past, future) {
  firstFutureYear <- min(intersect(getYears(past, as.integer = TRUE),
                                   getYears(future, as.integer = TRUE)))
  lastPastYear <- max(getYears(past, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
  }

  # Create future data for all past scenarios
  years_future <- getYears(future)[which(getYears(future, as.integer = TRUE) >= firstFutureYear)]
  tmpFuture <- future[, years_future, rep(1, ndata(past))]
  tmpFuture <- setNames(tmpFuture, getNames(past))
  tmpFuture[is.nan(tmpFuture)] <- 0

  # Create transition magpie object for all future scenarios
  years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstFutureYear)]
  tmpPast <- new.magpie(getRegions(past), years_past, getNames(past), fill = 0)

  # Use growth rates of future object
  tmpPast[,,] <- tmpFuture[,firstFutureYear,] * past[,years_past,] / past[,firstFutureYear,]
  tmpPast[is.nan(tmpPast)] <- 0

  mbind(tmpPast, tmpFuture)
}

harmonizePastGrPEAPGrFuture <- function(past, future) {
  # Get PEAP data and fill in missing islands
  short_term <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  short_term <- completeData(short_term, fill)
  
  # Use PEAP growth rates until 2025
  tmp <- harmonizePastGrFuture(past, short_term[,getYears(short_term, as.integer = TRUE) <= 2025,])
  # Use future growth rates after that
  combined <- harmonizePastGrFuture(tmp, future)
  combined
}

harmonizeAriadne <- function(past, future) {
  combined <- harmonizePastGrPEAPGrFuture(past, future)

  # For SSP2-Ariadne: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries. 
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    filter(.data$RegionCode == "EUR") %>% 
    dplyr::pull(.data$CountryCode)

  fut_years <- getYears(future)[getYears(future) >= 2020]
  combined[EUR_countries, fut_years,] <- future[EUR_countries, fut_years,]

  combined
}

harmonizeAriadneGDP <- function(past, future) {
  ssp2_data <- calcOutput("GDPppp", 
                          GDPpppCalib = "fixHist_IMFgr_return2SSP",
                          GDPpppPast = "WDI_completed", 
                          GDPpppFuture = "SSP_bezierOut_completed",
                          aggregate = FALSE,
                          FiveYearSteps = FALSE) %>% 
  `[`(,,"gdp_SSP2")

  # For SSP2-Ariadne: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries. 
  EUR_countries <- where(readSource("ARIADNE_ReferenceScenario", "gdp_corona") != 0 )$true$regions
  fut_years <- getYears(future)[getYears(future, as.integer = TRUE) >= max(getYears(past, as.integer = TRUE))]

  ssp2Ariadne_data <- ssp2_data
  ssp2Ariadne_data[EUR_countries, getYears(past),] <- past[EUR_countries,,]
  ssp2Ariadne_data[EUR_countries, fut_years,] <- future[EUR_countries, fut_years,]

  # After 2070, transition to SSP2 values
  past_years <- getYears(future)[getYears(future, as.integer = TRUE) <= 2070]
  combined_Ariadne <- harmonizePastTransition(ssp2Ariadne_data[EUR_countries, past_years,],
                                              ssp2_data[EUR_countries,,],
                                              2150)

  combined <- ssp2_data
  combined[EUR_countries, getYears(combined_Ariadne),]  <- combined_Ariadne[EUR_countries,,]
  getNames(combined) <- "gdp_SSP2Ariadne"
  
  combined
}



# This function harmonizes past and future data, based off of GDP per capita.
harmonizeFixHistIMFSSP <- function(past, future, yEnd) {
  past_pop <- calcOutput("PopulationPast", 
                         PopulationPast = "WDI_completed",
                         aggregate = FALSE)
  future_pop <- calcOutput("PopulationFuture", 
                           PopulationFuture = "SSP_completed",
                           aggregate = FALSE)

  past_years <- intersect(getYears(past), getYears(past_pop))
  future_years <- intersect(getYears(future), getYears(future_pop))

  past_gdppc <- past[,past_years,] / past_pop[,past_years,]

  getNames(future) <- sub("gdp", "gdppc", getNames(future))
  getNames(future_pop) <- sub("pop", "gdppc", getNames(future_pop))
  future_gdppc <- future[,future_years,] / future_pop[,future_years,]
  
  imf_gdppc <- readSource("IMF", "GDPpc")
  imf_gdppc <- completeData(imf_gdppc, future_gdppc[,,"gdppc_SSP2"])

  # Use short term IMF growth rates (here, as far as possible = 2026)
  tmp_gdppc <- harmonizePastGrFuture(past_gdppc, imf_gdppc)

  # Transform into tibble, combine past and future tibbles and pass to special convergence function
  tmp_gdppc <- tmp_gdppc %>% 
    as.quitte() %>% 
    select(-.data$data) %>% 
    filter(.data$period != 2026)
  
  future_gdppc <- as.quitte(future_gdppc)

  combined_gdppc <- expand_grid(region = unique(tmp_gdppc$region),
                                period = unique(c(tmp_gdppc$period, future_gdppc$period)), 
                                scen = unique(future_gdppc$scen)) %>% 
    left_join(tmp_gdppc, by = c("region", "period")) %>% 
    left_join(future_gdppc %>% 
                select(.data$region, .data$period, .data$scen, "iiasa_gdppc" = .data$value),
              by = c("region", "period", "scen")) %>% 
    rename("SSP" = .data$scen) %>% 
    mutate(SSP = stringr::str_remove(.data$SSP, "^......"))

  combined_gdppc <- convergeSpecial(combined_gdppc)

  # Retransform into magpie
  combined_gdppc <- combined_gdppc %>% 
    select(.data$region, .data$period, "variable" = .data$SSP, .data$value) %>% 
    as.magpie()

  # Multiply by population to get GDP
  pop <- calcOutput("Population", 
                    PopulationCalib = "past_grPEAP_grFuture",
                    PopulationPast = "WDI_completed",
                    PopulationFuture = "SSP2018Update_completed",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)
  pop <- pop[,, c("pop_SSP1", "pop_SSP2", "pop_SSP3", "pop_SSP4", "pop_SSP5")]

  getNames(combined_gdppc) <- getNames(pop)
  combined <- combined_gdppc * pop 

  # Recompute bezier extension, since GDP behavior is dependent on GDPpc
  y <- getYears(combined, as.integer = TRUE)[getYears(combined, as.integer = TRUE) <= 2100]
  combined <- bezierExtension(combined[, y,], seq(2105, 2150, 5))

  combined
}

# This function gets the absolute difference in gdp per capita in 2025 between the past-gdp per capita series
# and the original IIASA-SSP gdp per capita series. (These two series have to be included in the tibble
# x = the function argument). Then, depending on the sign of the difference, and the SSP, convergence 
# back to the IIASA GDP per capita level is either accelerated or slowed down. By 2100, convergence is
# completed.
convergeSpecial <- function(x) {
  
  dif <- x %>%
    filter(.data$period == 2025) %>%
    mutate(d = .data$iiasa_gdppc - .data$value) %>%
    select(.data$region, .data$SSP, .data$d)
  
  x <- x %>%
    left_join(dif, by = c("region", "SSP")) %>%
    mutate(
      # Medium convergence
      value = dplyr::if_else(
        .data$period > 2025 & .data$SSP == "SSP2",
        dplyr::if_else(
          .data$period == 2030, 
          .data$iiasa_gdppc - .data$d, 
          dplyr::if_else(
            .data$period <= 2100,
            .data$iiasa_gdppc - .data$d * (2100 - .data$period) / 70,
            .data$iiasa_gdppc
          )
        ),
        .data$value
      ),
      # Fast convergence
      value = dplyr::if_else(
         .data$period > 2025 & 
            ((.data$SSP %in% c("SSP1", "SSP5") & .data$d >= 0) | 
             (.data$SSP %in% c("SSP3", "SSP4") & .data$d < 0)),
         dplyr::if_else(
             .data$period <= 2100, 
             .data$iiasa_gdppc - .data$d * (2100 - .data$period) / 75, 
             .data$iiasa_gdppc
         ),
         .data$value
      ),
      # Slow convergence
      value = dplyr::if_else(
        .data$period > 2025 & 
            ((.data$SSP %in% c("SSP3", "SSP4") & .data$d >=0 ) | 
             (.data$SSP %in% c("SSP1", "SSP5") & .data$d < 0)),
        dplyr::if_else(
            .data$period <= 2035,
            .data$iiasa_gdppc - .data$d,
            dplyr::if_else(
              .data$period <= 2100,
              .data$iiasa_gdppc - .data$d * (2100 - .data$period) / 65,
              .data$iiasa_gdppc
            )
        ),
        .data$value
      )
    ) %>%
    select(-.data$d)
  x
}



###########################################
#Additional functions to derive the SHAPE GDP scenarios from the SSP1 scenario

# Note that here we label the GDP scenarios with their scenario abbreviations, 
# and not with the name of the SHAPE economics dimensions. 

# Mapping: Scenario <-> economics dimension
# Economy-driven innovation (SDP_EI) <-> innovation-driven economy
# Resilient communities (SDP_RC) <-> society-driven economy
# Managing the global commons (SDP_MC) <-> service-driven economy

# The two alternative SHAPE scenarios will re-use these GDP trajectories, so they are not explicitly included here.
# Local solutions (SDP_LS) <-> society-driven economy (same as SDP_RC)
# Green and social market economy (SDP_GS) <-> service-driven economy (same as SDP_MC)

# for details see the SHAPE scenario spreadsheet
# https://docs.google.com/spreadsheets/d/1v8dlZDj-AW8_oiyd9rdV3rGfVBt_PBCja1_NyEEFRMA/edit?usp=sharing
###########################################

# calculate modified growth rates and resulting gdp/capita in forward simulation
compute_SHAPE_growth <- function(SHAPE_GDPscenario, gdppcap_SSP1, startFromYear){
  
  # calculation of growth rates
  yrs <- getYears(gdppcap_SSP1, as.integer = TRUE)
  # flexible timestep
  # TODO would be better to always use yearly timesteps in this computation, and select years afterwards
  timestep <- new.magpie(years = yrs)
  timestep[,,] <- dplyr::lead(yrs) - yrs
  # assign average growth rate g_t of period t -> t+ timestep
  # this means modifications of growth rate t will affect GDP in t+timestep
  yrs_shifted <- yrs[2:length(yrs)]
  yrs_base <- yrs[1:length(yrs)-1]
  growthrate_SSP1 <- 100* ((setYears(gdppcap_SSP1[,yrs_shifted,],yrs_base)/gdppcap_SSP1[,yrs_base,])^(1./timestep[,yrs_base,]) - 1)
  
  #modified growth rates and gdp/cap
  growthrate <- setNames(as.magpie(growthrate_SSP1),SHAPE_GDPscenario)
  gdppcap <- setNames(as.magpie(gdppcap_SSP1),SHAPE_GDPscenario)
  gdppcap[,yrs > startFromYear,] <- NA
  
  for (yr in yrs[1:length(yrs)-1]){
    # modify growth rates only for future period (default: from 2020 onwards) 
    if (yr >= startFromYear){
      # innovation-driven (SDP_EI): enhance growth rates for low-income countries by up to 15%
      if (SHAPE_GDPscenario == "gdp_SDP_EI"){
        modification_factor <- logistic_transition(gdppcap[,yr,], L0 = 1.15, L = 1, k = 20, x0 = 15e3, use_log10 = TRUE)
      }
      # service-driven (SDP_MC): growth rate reduced based on relative distance to technology frontier (given by the US)
      else if (SHAPE_GDPscenario == "gdp_SDP_MC"){
        # define US as technology frontier
        frontier <- gdppcap["USA",yr,]
        getRegions(frontier) <- "GLO"
        # countries with gdp/cap above US are treated the same as the US -> set diff = 0
        reldiff_to_frontier <- pmax((frontier[,yr,] - gdppcap[,yr,])/frontier[,yr,] , 0)
        modification_factor <- logistic_transition(reldiff_to_frontier[,yr,], L0 = 1, L = 0.5, k = -30, x0 = 0.2, use_log10 = FALSE)
      } 
      # society-driven (SDP_RC): gradual transition to zero growth for high-income countries
      else if (SHAPE_GDPscenario == "gdp_SDP_RC") {
        modification_factor <- logistic_transition(gdppcap[,yr,], L0 = 1, L = 0, k = 10, x0 = 30e3, use_log10 = TRUE)
      } else {
        stop("cannot create SHAPE GDP scenarios: unknown scenario")
      }
      
      # for service (SDP_MC) and society (SDP_RC) additionally add a smoothing for 2020 and 2025 timesteps
      # apply only 1/3 (2020-2024) and 2/3 (2025-2029) of the modification
      if (SHAPE_GDPscenario %in% c("gdp_SDP_MC","gdp_SDP_RC")){
        if (yr >= 2020 && yr < 2025){
          modification_factor[,yr,] <- 1/3.*(modification_factor[,yr,] - 1) + 1
        } else if (yr >= 2025 && yr < 2030) {
          modification_factor[,yr,] <- 2/3.*(modification_factor[,yr,] - 1) + 1
        }
      }
      growthrate[,yr,] <- growthrate[,yr,] * modification_factor[,yr,]
    }
    
    # calculate next gdp/cap based on current value and (modified) growth rate
    gdppcap[,yr+as.integer(timestep[,yr,]),] <- gdppcap[,yr,]*(1 + growthrate[,yr,]/100.)^timestep[,yr,]
  }
  return(gdppcap)
}

# helper function: smooth transition from LO to L, with steepness k and midpoint x0
logistic_transition <- function(x,L0,L,k,x0, use_log10 = FALSE){
  if (use_log10){
    x <- log10(x)
    x0 <- log10(x0)
  }
  logistic <- 1./(1+exp(-k*(x-x0)))
  return( L0 - (L0-L)*logistic )
} 
