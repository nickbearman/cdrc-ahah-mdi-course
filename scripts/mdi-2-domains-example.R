# Simplified script to calculate index with 2 domains

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
  
  #health
  data$gpp_dist <- rank(data$gpp_dist,ties.method= "min")
  data$ed_dist <- rank(data$ed_dist,ties.method= "min")
  data$dent_dist <- rank(data$dent_dist,ties.method= "min")
  data$pharm_dist <- rank(data$pharm_dist,ties.method= "min")
  data$leis_dist <- rank(data$leis_dist,ties.method= "min")


# Use of Rankit rank-based normalisation

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
  
  #health
  data$gpp_dist <- exp_default(data$gpp_dist, data)
  data$gpp_dist <- qnorm(data$gpp_dist, mean = 0, sd = 1)
  data$ed_dist <- exp_default(data$ed_dist, data)
  data$ed_dist <- qnorm(data$ed_dist, mean = 0, sd = 1)
  data$dent_dist <- exp_default(data$dent_dist, data)
  data$dent_dist <- qnorm(data$dent_dist, mean = 0, sd = 1)
  data$pharm_dist <- exp_default(data$pharm_dist, data)
  data$pharm_dist <- qnorm(data$pharm_dist, mean = 0, sd = 1)

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

  # rank the domain scores
  # Domain ranks
    data$r_rank <- rank(data$r_domain,ties.method= "min")
    #data$r_rank <- rank(-data$r_rank) # Inverse ranking
    data$h_rank <- rank(data$h_domain,ties.method= "min")
    #data$h_rank <- rank(-data$h_rank) # Inverse ranking

# apply the exponential transformation to the scores
  data$r_exp <- exp_trans(data$r_rank,data)
  data$h_exp <- exp_trans(data$h_rank,data)

#combine (at equal weighting) to make the AHAH score
  # AHAH score
  data$ahah <- (0.5 * data$r_exp + 
                  0.5 * data$h_exp)

# Create Ranks and Deciles
  data$r_ahah <- rank(data$ahah,ties.method= "min")

  data$d_ahah <- ntile(data$ahah, 10)
  data$d5_ahah <- ntile(data$ahah, 5)

  data$r_dec <- ntile(data$r_exp,10)
  data$h_dec <- ntile(data$h_exp,10)

#write out csv file
  write.csv(data,"2020-09-25-r+h.csv",quote = FALSE, row.names = FALSE)

#select out specific columns
  data_ph <- fread("2020-09-25-r+h.csv")
  data_ph <- data_ph[ ,c(2,36:47)]
  write.csv(data_ph,"2020-09-25-summary.csv",quote = FALSE, row.names = FALSE)


