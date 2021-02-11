#' @title calcFeedBasketsUncalibrated
#' @description Combines uncalibrated feed baskets of the past with scenario-dependent future feed baskets.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl, Benjamin Leon Bodirsky, Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FeedBasketsUncalibrated")
#' }
#' @importFrom magclass getNames getYears add_columns add_dimension magpie_expand
#' @importFrom utils tail

calcFeedBasketsUncalibrated <- function() {
  
  past<-findset("past")
  calib_year <- tail(past,1)
  
  out_eff <- calcOutput("FeedEfficiencyFuture", aggregate=FALSE)
  getSets(out_eff) = c("iso","year","sys","scen")
  
  central_feed_shr <- calcOutput("CentralFeedshares"   , aggregate=FALSE) 
  getSets(central_feed_shr) = c("iso","year","sys","scen")
  #missing = c("sys_chicken","sys_hen")
  #central_feed_shr <- add_columns(central_feed_shr,addnm = missing)
  #central_feed_shr[,,missing] = 0.5 ## its held constant anyhow
  
  fbask_raw = calcOutput("FeedBasketsSysPast", aggregate = FALSE)
  getSets(fbask_raw) = c("iso","year","sys","kall")
  fbask_sys <- setYears(fbask_raw[,calib_year,],NULL)
  
  # make sure there are no NANs
  fbask_sys_tmp=fbask_sys 
  fbask_sys_tmp[,,list(sys = c("sys_beef","sys_dairy"), kall = c("maiz","pasture"))] = fbask_sys[,,list(sys=c("sys_beef","sys_dairy"),kall=c("maiz","pasture"))]+10^-10 
  fbask_sys_tmp[,,list(sys = c("sys_pig"), kall = c("maiz","foddr"))] = fbask_sys[,,list(sys = c("sys_pig"), kall = c("maiz","foddr"))] + 10^-10 
  
  kli <- findset("kli")
  
  compose_ideal_feed<-function(
    main_shr,
    anti_shr,
    sys
  ){
    # esitmate the composition of the sub-baskets
    composition_main = fbask_sys_tmp[,,main_shr][,,sys]/dimSums(fbask_sys_tmp[,,main_shr][,,sys],dim = "kall")
    composition_anti = fbask_sys_tmp[,,anti_shr][,,sys]/dimSums(fbask_sys_tmp[,,anti_shr][,,sys],dim = "kall")
    
    # compose the value chain
    main_bask = out_eff[,,sys] * central_feed_shr[,,sys] * composition_main
    anti_bask = out_eff[,,sys] * (1-central_feed_shr[,,sys]) * composition_anti
    bask <- mbind(main_bask,anti_bask)
    return(bask)
  }
  
  ### Ruminants
  main_shr_rum = c("res_cereals","res_fibrous","res_nonfibrous","pasture")
  bask_rum = compose_ideal_feed(
    main_shr = main_shr_rum,
    anti_shr = setdiff(getNames(fbask_sys,dim = "kall"),main_shr_rum),
    sys = c("sys_beef","sys_dairy")
  )
  
  ### Pigs
  anti_shr_pig    <- c("tece","trce","maiz","rice_pro",
                       "others","potato","cassav_sp","puls_pro",         
                       "soybean","rapeseed","groundnut","sunflower","oilpalm","cottn_pro",    
                       "sugr_beet","sugr_cane",
                       "livst_rum","livst_pig","livst_chick","livst_egg","livst_milk", "fish")
  bask_pig = compose_ideal_feed(
    main_shr = setdiff(getNames(fbask_sys,dim = "kall"), anti_shr_pig),
    anti_shr = anti_shr_pig,
    sys = c("sys_pig")
  )
  
  ### Chicken
  # mainshr_poultry <- c("distillers_grain", "molasses", "oilcakes", "brans")
  sys = c("sys_chicken","sys_hen")
  bask_chick = out_eff[,,sys] * fbask_sys[,,sys]/dimSums(fbask_sys[,,sys],dim = "kall")
  
  out <- mbind(
    bask_rum,
    bask_pig,
    bask_chick
  )

  ### upper limits for animal feed based on history.
  # milk: max 0.17 t, to be replaced with third maiz third pulses third foddr
  # quantile(fbask_raw[,,list(sys = "sys_dairy", kall = "livst_milk")],0.95)
  exceedence = out[,,list(sys = "sys_dairy", kall = "livst_milk")] - 0.17
  exceedence[exceedence<0]=0
  exceedence=collapseNames(exceedence)
  out[,,list(sys = "sys_dairy", kall = "livst_milk")]=out[,,list(sys = "sys_dairy", kall = "livst_milk")]-exceedence
  out[,,list(sys = "sys_dairy", kall = c("maiz","puls_pro","foddr"))]=out[,,list(sys = "sys_dairy", kall = c("maiz","puls_pro","foddr"))]+exceedence/3

  # beef: max 1.26 t, to be replaced with half maiz half pulses
  # quantile(fbask_raw[,,list(sys = "sys_beef", kall = "livst_milk")],0.95)
  exceedence = out[,,list(sys = "sys_beef", kall = "livst_milk")] - 1.26
  exceedence[exceedence<0]=0
  exceedence=collapseNames(exceedence)
  out[,,list(sys = "sys_beef", kall = "livst_milk")]=out[,,list(sys = "sys_beef", kall = "livst_milk")]-exceedence
  out[,,list(sys = "sys_beef", kall = c("maiz","puls_pro","foddr"))]=out[,,list(sys = "sys_beef", kall = c("maiz","puls_pro","foddr"))]+exceedence/3
  
  out <- round(out,3)
  
  if(any(dimSums(out[,,kli],dim="kall") >= 2 )){
    stop("more livestock products in feed basket than being produced") 
  }

  #use livestock production as weight
  kli <- findset("kli")
  weight_kli <- collapseNames(calcOutput("FAOmassbalance_pre",aggregate = FALSE)[,,kli][,,"dm"][,,"production"])
  prod_sys_ratio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)
  
  weight_sys <- dimSums(weight_kli*prod_sys_ratio,dim="ItemCodeItem")
  weight_sys <- toolHoldConstantBeyondEnd(weight_sys)
  
  return(list(x=out,
              weight=weight_sys,
              unit="tDM / t DM",
              description="Uncalibrated feed requirements in DM per DM products generated for 5 livestock systems"))
  
}