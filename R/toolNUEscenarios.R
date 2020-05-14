toolNUEscenarios<-function(x,weight,rev=0.1,zhang=NULL){
  x<-setNames(toolHoldConstantBeyondEnd(x),"constant")
  weight<-setNames(toolHoldConstantBeyondEnd(weight),"constant")
  
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
  } else if (rev >= 4.47){
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
  
  weight2<-x
  weight2[,,]<-setNames(weight,NULL)
  
  data<-toolNAreplace(x=x,weight=weight2)
  return(data)
}