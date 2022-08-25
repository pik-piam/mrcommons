toolNUEscenarios<-function(x,weight,rev=0.1,zhang=NULL,maccbase=TRUE){

  x<-setNames(toolHoldConstantBeyondEnd(x),"constant")
  weight<-setNames(toolHoldConstantBeyondEnd(weight),"constant")



  if(maccbase==FALSE){

    scenariosetting<-function(x,y2050,y2100,startyear){
      scenarioname=paste0("neff",y2050*100,"_",y2100*100,"_start",startyear)
      x<-add_columns(x,addnm = scenarioname,dim = 3.1)
      x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = y2050,start_year = startyear,end_year = "y2050",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2100,start_year = "y2050",end_year = "y2100",type = "linear")
      return(x)
    }

    #scenarios
    x<-scenariosetting(x,0.55,0.55,"y1990")
    x<-scenariosetting(x,0.60,0.60,"y1990")
    x<-scenariosetting(x,0.65,0.70,"y1990")
    x<-scenariosetting(x,0.65,0.70,"y2010")
    x<-scenariosetting(x,0.60,0.60,"y2010")
    x<-scenariosetting(x,0.55,0.60,"y2010")
    x<-scenariosetting(x,0.70,0.75,"y2010")
    x<-scenariosetting(x,0.75,0.80,"y2010")
    x<-scenariosetting(x,0.75,0.85,"y2010")
    x<-scenariosetting(x,0.80,0.85,"y2010")

    if (rev>=4.33) {
      x<-scenariosetting(x,0.85,0.85,"y2010")
    }
    if (rev >= 4.47){
      scenarioname="neff_ZhangBy2030_start2010"
      x<-add_columns(x,addnm = scenarioname,dim = 3.1)
      x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = zhang,start_year = "y2010", end_year = "y2030",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = 0.8,start_year = "y2030",end_year = "y2070",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = 0.8,start_year = "y2070",end_year = "y2100",type = "linear")

      scenarioname="neff_ZhangBy2050_start2010"
      x<-add_columns(x,addnm = scenarioname,dim = 3.1)
      x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = zhang,start_year = "y2010", end_year = "y2050",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = 0.8,start_year = "y2050",end_year = "y2100",type = "linear")
    }

  } else {

    # This implementation assumes that the NUE target can only be achieved when the baseline NUE and the maximum MACCs are combined
    # We combine the target NUE and the maximum MACCs potential to derive the baseline NUE.

    maxMaccs = calcOutput("MACCsN2O",source="PBL_MACC_2022",aggregate = FALSE)
    maxMaccs = setNames(maxMaccs[,,"n2ofert.Default.201"],"maxmacc")[,c("y2050","y2100"),]

    # NUE implicit to MACC curves
    # given that the maximum mitigaton is so similar across world regions, they likely
    # refer to an assumed uniform global NUE, which is likely close to 50 percent.
    implicit_nue = 0.5

    # MACCS that refer to input-dependent emission factors need to be transformed to become
    # NUE-MACCs.
    # See gams code for documentation of transformation of PBL curves
    maccs_transf <- maxMaccs * implicit_nue / (1 + maxMaccs * (implicit_nue - 1))

    scenariosetting2<-function(x,y2050,y2100,startyear){
      # (1-NUE_target) = (1-maccs_transf)*(1-NUE_base)
      # NUE_base = 1-(1-NUE_target)/(1-maccs_transf)
      scenarioname=paste0("maxeff_glob_",y2050*100,"_",y2100*100,"_start",startyear)
      y2050 = 1-(1-y2050)/(1-maccs_transf[,"y2050",])
      y2100 = 1-(1-y2100)/(1-maccs_transf[,"y2100",])

      x<-add_columns(x,addnm = scenarioname,dim = 3.1)
      x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = y2050,start_year = startyear,end_year = "y2050",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2100,start_year = "y2050",end_year = "y2100",type = "linear")
      return(x)
    }



    # ssp2: not very globalized, constant mangement without policies

    # boundary: globalized, optimal management
    x<-scenariosetting2(x,0.75,0.85,"y2020")
    # ssp1: globalized, sustainable management (sustainable) (policy likely)
    # ssp4: globalized, good management (neo-colonialist robofarm) (policy not too likely)
    # ssp5: globalized, good management (hightech) (policy not too likely)
    x<-scenariosetting2(x,0.75,0.8,"y2020")
    # ssp3: deterioation without policies (policies unlikely)
    x<-scenariosetting2(x,0.6,0.65,"y2020")

    x<-scenariosetting2(x,0.65,0.75,"y2020")

    scenariosetting3<-function(x,y2050,y2100,startyear, max=NULL){
      scenarioname=paste0("baseeff_add_",y2050*100,"_",y2100*100,"_start",startyear)
      if(!is.null(max)){
        scenarioname=paste0(scenarioname, "_max", max)
      }
      x<-add_columns(x,addnm = scenarioname,dim = 3.1)
      y2050 <- y2050+x[,,"constant"]
      y2100 <- y2100+x[,,"constant"]
      if(!is.null(max)){
        y2050[y2050>max/100] <- max/100
        y2100[y2100>max/100] <- max/100
      }
      x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = y2050,start_year = startyear,end_year = "y2050",type = "linear")
      x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2100,start_year = "y2050",end_year = "y2100",type = "linear")
      return(x)
    }


    #SSP1
    #- rapid improvement in less efficient countries,
    #- without mitigation, maximum levels stay close to current practices in best countries
    #- with mitiagion, maximum efficiency in 2100 can be achieved of 75% in current bad countries and up to 85-90% in current efficient countries
    x<-scenariosetting3(x,0.1,0.2,"y2020", max=75)
    #SSP2
    #- slight improvement in less efficient countries,
    #- without mitigation maximum efficiency in efficient countries drops
    #- with migigation, highes efficiency is around 80% in 2100
    x<-scenariosetting3(x,0.05,0.1,"y2020", max=65)
    #SSP3
    #- no progress in inefficient countries,
    #- without mitigation, in efficient countries maximum efficiency drops
    #- with mitigation, up to 75% achievable in 2100
    x<-scenariosetting3(x,0,0,"y2020", max=55)
    #SSP4
    #- neocolonialism and robotics allow for precision farming
    #- even more with market incentives
    #- still, efficient countries are able to make more progress
    x<-scenariosetting3(x,0.05,0.1,"y2020", max=75)
    #SSP5
    #- rapid technological progress, but first effectivity than efficiency,
    #- so technical progress only kicks in in second half of century in
    #- low efficient economies
    x<-scenariosetting3(x,0.05,0.15,"y2020", max=75)


    ### Zhang scenarios

    scenarioname="maxeff_ZhangBy2030_start2020"
    x<-add_columns(x,addnm = scenarioname,dim = 3.1)
    y2030 = 1-(1-zhang)/(1-maccs_transf[,"y2050",])
    y2050 = 1-(1-0.75)/(1-maccs_transf[,"y2050",])
    y2100 = 1-(1-0.8)/(1-maccs_transf[,"y2100",])
    x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = y2030,start_year = "y2020", end_year = "y2030",type = "linear")
    x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2050,start_year = "y2030",end_year = "y2070",type = "linear")
    x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2100,start_year = "y2070",end_year = "y2100",type = "linear")

    scenarioname="maxeff_ZhangBy2050_start2020"
    x<-add_columns(x,addnm = scenarioname,dim = 3.1)
    y2050 = 1-(1-zhang)/(1-maccs_transf[,"y2050",])
    y2100 = 1-(1-0.8)/(1-maccs_transf[,"y2100",])
    x[,,scenarioname]=convergence(origin = x[,,"constant"],aim = y2050,start_year = "y2020", end_year = "y2050",type = "linear")
    x[,,scenarioname]=convergence(origin = x[,,scenarioname],aim = y2100,start_year = "y2050",end_year = "y2100",type = "linear")

  }

  weight2<-x
  weight2[,,]<-setNames(weight,NULL)

  data<-toolNAreplace(x=x,weight=weight2)
  return(data)
}
