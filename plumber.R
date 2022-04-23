# plumber.R

#* @preempt __first__
#* @get /
function(req, res) {
  res$status <- 302
  res$setHeader("Location", "./__docs__/")
  res$body <- "Redirecting..."
  res
}

#* Return the exceedance discharge of simulations
#* param input json of data
#* @get /nsfit
function(input){
  library(extRemes)
  library(jsonlite)
  
  data<- fromJSON(input)
  print(data)
  n<-length(data)
  yrs<-data[1:(n/2)]
  flow<-data[(n/2+1):n]
  data <- data.frame(cbind(yrs,flow))
  colnames(data) <- c("yrs","flow")
  
  GEV.NS <- extRemes::fevd(flow, data = data, location = ~yrs, type="GEV")
  
  # Non-stationary fit GEV distribution
  par_loc0.GEV.NS <- GEV.NS$results$par[[1]] #loc 1 = mu0
  par_loc1.GEV.NS <- GEV.NS$results$par[[2]] #loc 1 = mu0
  par_scale.GEV.NS <- GEV.NS$results$par[[3]] #scale
  par_shape.GEV.NS <- GEV.NS$results$par[[4]] # shape
  
  
  ## quantiles over time 
  ACE <- c(0.9, 0.99) # 10 yrs and 100 yrs 
  quant.yrs <- matrix(0,length(yrs),length(ACE))
  
  for(i in 1:length(ACE)){
    shape.1 <- par_shape.GEV.NS
    scale.1 <- par_scale.GEV.NS
    for(j in 1:length(yrs)){
      loc.1 <- par_loc0.GEV.NS + par_loc1.GEV.NS *j 
      quant.yrs[j,i] <- extRemes::qevd(ACE[i],loc = loc.1, scale = scale.1, shape = shape.1, type = "GEV") 
    }      
  }

  return(quant.yrs)
}
