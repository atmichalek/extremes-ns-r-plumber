# plumber.R
#* @apiTitle extRemes NSFIT
#* @apiDescription An API to send a JSON array of year and data value to fit the extRemes distributions of Gumbel, GEV, and exponential varying on time. Gumbel and GEV sets the location parameter varying linearly on time. Exponetial sets the scale parameter varying on time. The output returned is the following per year: years, probability quantiles, AIC, and the distribution parameters "mu0","mu1","sigma0","sigma1","shape".   
#* Return the exceedance discharge of simulations
#* param input json of data
#* param model which distribution
#* @get /nsfit
function(input,model){
  library(extRemes)
  library(jsonlite)
  
  #Read in JSON data
  data<- fromJSON(input)
  yrs<-data[,1]
  flow<-data[,2]
  data <- data.frame(cbind(yrs,flow))
  colnames(data) <- c("yrs","flow")
 
  #Primary File Paramaters -------
  ACE <- c(0.5,0.8,0.9,0.96,0.99,0.996) # 2, 5, 10, 25, 50, 100 and 250 yrs
  quant.yrs <- matrix(0,length(yrs),length(ACE))
  data <- data.frame(cbind(yrs,flow))
  colnames(data) <- c("yrs","flow")
  
  # Distribution Fits -----------
  if(model == 1){ #Gumbel
    GEV.NS <- extRemes::fevd(flow, data = data, location = ~yrs, type="Gumbel")
    # Non-stationary fit GEV distribution
    par_loc0.GEV.NS <- GEV.NS$results$par[[1]] #loc 1 = mu0
    par_loc1.GEV.NS <- GEV.NS$results$par[[2]] #loc 1 = mu0
    par_scale.GEV.NS <- GEV.NS$results$par[[3]] #scale
    AIC_GEV.NS <- (summary(GEV.NS,silent=TRUE))$AIC
    #Calculate Quantiles
    for(i in 1:length(ACE)){
      scale.1 <- par_scale.GEV.NS
      for(j in 1:length(yrs)){
        loc.1 <- par_loc0.GEV.NS + par_loc1.GEV.NS *j 
        quant.yrs[j,i] <- extRemes::qevd(ACE[i],loc = loc.1, scale = scale.1, type = "Gumbel") 
      }      
    }
    result <- data.frame(cbind(yrs,quant.yrs,AIC_GEV.NS,par_loc0.GEV.NS,par_loc1.GEV.NS,par_scale.GEV.NS,0,0))
    colnames(result) <- c("yrs","q_0.5","q_0.8","q_0.9","q_0.96","q_0.99","q_0.996","AIC","mu0","mu1","sigma0","sigma1","shape")
  }else if(model == 2){ #GEV 
    GEV.NS <- extRemes::fevd(flow, data = data, location = ~yrs, type="GEV")
    # Non-stationary fit GEV distribution
    par_loc0.GEV.NS <- GEV.NS$results$par[[1]] #loc 1 = mu0
    par_loc1.GEV.NS <- GEV.NS$results$par[[2]] #loc 1 = mu0
    par_scale.GEV.NS <- GEV.NS$results$par[[3]] #scale
    par_shape.GEV.NS <- GEV.NS$results$par[[4]] # shape
    AIC_GEV.NS <- (summary(GEV.NS,silent=TRUE))$AIC
    #Calculate Quantiles
    for(i in 1:length(ACE)){
      shape.1 <- par_shape.GEV.NS
      scale.1 <- par_scale.GEV.NS
      for(j in 1:length(yrs)){
        loc.1 <- par_loc0.GEV.NS + par_loc1.GEV.NS *j 
        quant.yrs[j,i] <- extRemes::qevd(ACE[i],loc = loc.1, scale = scale.1, shape = shape.1, type = "GEV")   
      }
    }
    result <- data.frame(cbind(yrs,quant.yrs,AIC_GEV.NS,par_loc0.GEV.NS,par_loc1.GEV.NS,par_scale.GEV.NS,0,par_shape.GEV.NS))
    colnames(result) <- c("yrs","q_0.5","q_0.8","q_0.9","q_0.96","q_0.99","q_0.996","AIC","mu0","mu1","sigma0","sigma1","shape")
    
  }else{ #Exponential
    Exp.NS <- extRemes::fevd(flow, data = data, threshold =0, scale = ~yrs, type="Exponential")
    # Non-stationary fit GEV distribution
    par_scale0.Exp.NS <- Exp.NS$results$par[[1]] #sigma 0
    par_scale1.Exp.NS <- Exp.NS$results$par[[2]] # sigma 1
    AIC_Exp.NS <- (summary(Exp.NS,silent=TRUE))$AIC
    #Calulate Quantiles
    for(i in 1:length(ACE)){
      threshold <- 0 
      for(j in 1:length(yrs)){
        sigma.1 <- par_scale0.Exp.NS + par_scale1.Exp.NS *j
        quant.yrs[j,i] <- extRemes::qevd(ACE[i],threshold =0, scale = sigma.1, type = "Exponential")   
      }
    }
  result <- data.frame(cbind(yrs,quant.yrs,AIC_Exp.NS,0,0,par_scale0.Exp.NS,par_scale1.Exp.NS,0))
  colnames(result) <- c("yrs","q_0.5","q_0.8","q_0.9","q_0.96","q_0.99","q_0.996","AIC","mu0","mu1","sigma0","sigma1","shape")
  }
  ## quantiles over time 
  return(result)
}
