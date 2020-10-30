# simplified script to calculate full index 

# Preamble

  #load libraries (and install if needed)
    library(nFactors)
    library(data.table)
    library(dplyr)

# Functions needed for calculations

  # Quantile calcs for top 5% / (5%-95%)
    idr <- function(x){((x-quantile(x, 0.5, na.rm=TRUE)))/
        ((quantile(x, 0.95, na.rm=TRUE))-(quantile(x, 0.05, na.rm=TRUE)))}
    
  # Function for exponential transformations
    # Exponential transformation  ~>   X=-23 ln(1-R(1-exp(-100/23)))
      exp_trans <- function(x,y){-23*log(1-(x/nrow(y))*(1-exp(-100/23)), base = exp(1))}
    # Use of Rankit rank-based normalisation
      exp_default <- function(x,y){(x-0.5)/nrow(y)} # Use of Rankit rank-based normalisation
    
# Read in Dataset
  data <- read.csv("allvariableslsoawdeciles.csv")
    
# Ranking
  # rank each of the distance data sets
  # some are inverted so low = good and high = bad
    
#read in Dataset 
  data <- read.csv("allvariableslsoawdeciles.csv")

# Ranking
  # rank each of the distance data sets
  # some are inverted so low = good and high = bad
  
  #health
  data$gpp_dist <- rank(data$gpp_dist,ties.method= "min")
  data$ed_dist <- rank(data$ed_dist,ties.method= "min")
  data$dent_dist <- rank(data$dent_dist,ties.method= "min")
  data$pharm_dist <- rank(data$pharm_dist,ties.method= "min")
  data$leis_dist <- rank(data$leis_dist,ties.method= "min")
  
  #retail
  data$gamb_dist <- rank(data$gamb_dist,ties.method= "min")
  data$gamb_dist <- rank(-data$gamb_dist) # Invert ranking
  data$ffood_dist <- rank(data$ffood_dist,ties.method= "min")
  data$ffood_dist <- rank(-data$ffood_dist) # Invert ranking
  data$pubs_dist <- rank(data$pubs_dist,ties.method= "min")
  data$pubs_dist <- rank(-data$pubs_dist) # Invert ranking
  data$off_dist <- rank(data$off_dist,ties.method= "min")
  data$off_dist <- rank(-data$off_dist) # Invert ranking
  data$tobac_dist<- rank(data$tobac_dist,ties.method= "min")
  data$tobac_dist <- rank(-data$tobac_dist) # Invert ranking
  
  #greenspace
  data$blue_dist <- rank(data$blue_dist,ties.method= "min")
  data$green_act <- rank(data$green_act,ties.method= "min")
  data$green_pas <- rank(data$green_pas,ties.method= "min")
  data$green_pas <- rank(-data$green_pas) # Invert ranking
  
  #environment
  data$no2_mean <- rank(data$no2_mean,ties.method= "min")
  data$pm10_mean <- rank(data$pm10_mean,ties.method= "min")
  data$so2_mean <- rank(data$so2_mean,ties.method= "min")

# Use of Rankit rank-based normalisation

  #health
  data$gpp_dist <- exp_default(data$gpp_dist, data)
  data$gpp_dist <- qnorm(data$gpp_dist, mean = 0, sd = 1)
  data$ed_dist <- exp_default(data$ed_dist, data)
  data$ed_dist <- qnorm(data$ed_dist, mean = 0, sd = 1)
  data$dent_dist <- exp_default(data$dent_dist, data)
  data$dent_dist <- qnorm(data$dent_dist, mean = 0, sd = 1)
  data$pharm_dist <- exp_default(data$pharm_dist, data)
  data$pharm_dist <- qnorm(data$pharm_dist, mean = 0, sd = 1)
  
  #retail
  data$gamb_dist <- exp_default(data$gamb_dist, data)
  data$gamb_dist <- qnorm(data$gamb_dist, mean = 0, sd = 1)
  data$ffood_dist <- exp_default(data$ffood_dist, data)
  data$ffood_dist <- qnorm(data$ffood_dist, mean = 0, sd = 1)
  data$pubs_dist <- exp_default(data$pubs_dist, data)
  data$pubs_dist <- qnorm(data$pubs_dist, mean = 0, sd = 1)
  data$leis_dist <- exp_default(data$leis_dist, data)
  data$leis_dist <- qnorm(data$leis_dist, mean = 0, sd = 1)
  data$off_dist <- exp_default(data$off_dist, data)
  data$off_dist <- qnorm(data$off_dist, mean = 0, sd = 1)
  data$tobac_dist <- exp_default(data$tobac_dist, data)
  data$tobac_dist <- qnorm(data$tobac_dist, mean = 0, sd = 1)
  
  #greenspace
  data$blue_dist <- exp_default(data$blue_dist, data)
  data$blue_dist <- qnorm(data$blue_dist, mean = 0, sd = 1)
  data$green_act <- exp_default(data$green_act, data)
  data$green_act <- qnorm(data$green_act, mean = 0, sd = 1)
  data$green_pas <- exp_default(data$green_pas, data)
  data$green_pas <- qnorm(data$green_pas, mean = 0, sd = 1)
  
  #air quailty
  data$no2_mean <- exp_default(data$no2_mean, data)
  data$no2_mean <- qnorm(data$no2_mean, mean = 0, sd = 1)
  data$pm10_mean <- exp_default(data$pm10_mean, data)
  data$pm10_mean <- qnorm(data$pm10_mean, mean = 0, sd = 1)
  data$so2_mean <- exp_default(data$so2_mean, data)
  data$so2_mean <- qnorm(data$so2_mean, mean = 0, sd = 1)


# save as RDS (temp file save)
  # Write data 
  saveRDS(data,"norm_data_all_variables.rds")

# Load data
  data <- readRDS("norm_data_all_variables.rds")


# Domain scores
  # weight scores for each domain
  #retail
  data$r_domain <- (0.20 * data$gamb_dist +
                      0.20 * data$ffood_dist +
                      0.20 * data$pubs_dist +
                      0.20 * data$off_dist +
                      0.20 * data$tobac_dist)
  #health
  data$h_domain <- (0.20 * data$gpp_dist +
                      0.20 * data$ed_dist +
                      0.20 * data$dent_dist +
                      0.20 * data$pharm_dist +
                      0.20 * data$leis_dist)
  #greenspace
  data$g_domain <- ((1/3) * data$green_act + 
                      (1/3) * data$green_pas + 
                      (1/3) * data$blue_dist)
  
  #environment - Air Quality
  data$e_domain <- ((1/3) * data$no2_mean + 
                      (1/3) * data$pm10_mean + 
                      (1/3) * data$so2_mean)

# Rank the domain scores

  data$r_rank <- rank(data$r_domain,ties.method= "min")
  #data$r_rank <- rank(-data$r_rank) # Inverse ranking
  data$h_rank <- rank(data$h_domain,ties.method= "min")
  #data$h_rank <- rank(-data$h_rank) # Inverse ranking
  data$g_rank <- rank(data$g_domain,ties.method= "min")
  #data$e_rank <- rank(-data$e_rank) # Inverse ranking
  data$e_rank <- rank(data$e_domain,ties.method= "min")
  #data$e_rank <- rank(-data$e_rank) # Inverse ranking
  
  # apply the exponential transformation to the scores
  # Exp domains
  data$r_exp <- exp_trans(data$r_rank,data)
  data$h_exp <- exp_trans(data$h_rank,data)
  data$g_exp <- exp_trans(data$g_rank,data)
  data$e_exp <- exp_trans(data$e_rank,data)

#combine (at equal weighting) to make the AHAH score
  data$ahah <- (0.25 * data$r_exp + 
                0.25 * data$h_exp +
                0.25 * data$g_exp +
                0.25 * data$e_exp)

# Create Ranks and Deciles
  data$r_ahah <- rank(data$ahah,ties.method= "min")

  data$d_ahah <- ntile(data$ahah, 10)
  data$d5_ahah <- ntile(data$ahah, 5)
  
  data$r_dec <- ntile(data$r_exp,10)
  data$h_dec <- ntile(data$h_exp,10)
  data$g_dec <- ntile(data$g_exp,10)
  data$e_dec <- ntile(data$e_exp,10)

#write out csv file
  write.csv(data,"2020-09-25-all.csv",quote = FALSE, row.names = FALSE)

#select out specific columns
  data_ph <- fread("fin_vars_lsoa_26_02_2019.csv")
  data_ph <- data_ph[ ,c(2,36:55)]
  write.csv(data_ph,"2020-09-25-summary.csv",quote = FALSE, row.names = FALSE)

