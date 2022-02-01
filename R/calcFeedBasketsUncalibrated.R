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
  
  fbask_raw = calcOutput("FeedBasketsSysPast", aggregate = FALSE)[,past,]
  getSets(fbask_raw) = c("iso","year","sys","kall")
  fbask_sys <- setYears(fbask_raw[,calib_year,],NULL)
  
  # make sure there are no NANs
  fbask_sys_tmp=fbask_sys 
  fbask_sys_tmp[,,list(sys = c("sys_beef","sys_dairy"), kall = c("maiz","pasture"))] = fbask_sys[,,list(sys=c("sys_beef","sys_dairy"),kall=c("maiz","pasture"))]+10^-10 
  fbask_sys_tmp[,,list(sys = c("sys_pig"), kall = c("maiz","foddr"))] = fbask_sys[,,list(sys = c("sys_pig"), kall = c("maiz","foddr"))] + 10^-10 
  
  kli <- findset("kli")
  
  compose_ideal_feed<-function(
    main,
    anti,
    const,
    sys
  ){
    if(anyDuplicated(c(main,const,anti))!=0) {stop("duplicates in main, anti and const")}
    if(!all(c(main,const,anti)%in%getNames(fbask_sys_tmp,dim="kall"))){
      stop("not all feed items assigned to main,const,anti")
    }
    
    # esitmate the composition of the sub-baskets
    composition_main = fbask_sys_tmp[,,main][,,sys]/dimSums(fbask_sys_tmp[,,main][,,sys],dim = "kall")
    composition_anti = fbask_sys_tmp[,,anti][,,sys]/dimSums(fbask_sys_tmp[,,anti][,,sys],dim = "kall")
    
    constant = fbask_sys_tmp[,,const][,,sys]
    constant_sum = dimSums(constant[,,sys],dim="kall")
    
    # compose the value chain
    main_bask = out_eff[,,sys] * central_feed_shr[,,sys] 
    anti_bask = (out_eff[,,sys] * (1-central_feed_shr[,,sys])) - constant_sum
    
    reduce_constant = anti_bask
    reduce_constant[reduce_constant>0]=0
    reduction_factor_constant=toolConditionalReplace((constant_sum+reduce_constant)/constant_sum, "is.na()", 1)

    anti_bask[anti_bask<0] = 0
    main_bask = main_bask * composition_main
    anti_bask = anti_bask * composition_anti
    
    constant = (central_feed_shr[,,sys]*0+1) * constant * reduction_factor_constant# the first term just extends the time dimension
    
    bask <- mbind(main_bask,anti_bask,constant)
    return(bask)
  }
  
  ### Ruminants
  main_rum = c("res_cereals","res_fibrous","res_nonfibrous","pasture")
  const_rum = c("brans",findset("kap"),"potato","puls_pro","sugr_beet","sugr_cane","groundnut")
  bask_rum = compose_ideal_feed(
    main = main_rum,
    anti = setdiff(getNames(fbask_sys,dim = "kall"),c(main_rum,const_rum)),
    const = const_rum,
    sys = c("sys_beef","sys_dairy")
  )
  
  ### Pigs
  const_pig =  c(findset("kap"),"potato","puls_pro","sugr_beet","sugr_cane","groundnut")
  anti_pig    <- c("tece","trce","maiz","rice_pro",
                       "others","potato","cassav_sp","puls_pro",         
                       "soybean","rapeseed","groundnut","sunflower","oilpalm","cottn_pro",    
                       "sugr_beet","sugr_cane",
                       "livst_rum","livst_pig","livst_chick","livst_egg","livst_milk", "fish")
  anti_pig = setdiff(anti_pig,const_pig)
  bask_pig = compose_ideal_feed(
    main = setdiff(getNames(fbask_sys,dim = "kall"), c(anti_pig,const_pig)),
    anti = anti_pig,
    const =  const_pig,
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

  out <- round(out,3)
  
  if(any(out[,,"sys_dairy"][,,"livst_milk"] >= 1 )){
    stop("more livestock products in feed basket than being produced") 
  }
  if(any(out[,,"sys_hen"][,,"livst_egg"] >= 1 )){
    stop("more livestock products in feed basket than being produced") 
  }
  

  #use livestock production as weight
  kli <- findset("kli")
  weight_kli <- collapseNames(calcOutput("FAOmassbalance_pre",aggregate = FALSE)[,,kli][,,"dm"][,,"production"][,past,])
  prod_sys_ratio <- calcOutput("ProdSysRatioPast", aggregate = FALSE)[,past,]
  
  weight_sys <- dimSums(weight_kli*prod_sys_ratio,dim="ItemCodeItem")
  weight_sys <- toolHoldConstantBeyondEnd(weight_sys)
  
  return(list(x=out,
              weight=weight_sys,
              unit="tDM / t DM",
              description="Uncalibrated feed requirements in DM per DM products generated for 5 livestock systems"))
  
}
