combine_measures_who <- function(weight_measures, 
                                 p_0_13, p_0_5,
                                 age_range, # "0_13" or "0_5"
                                 weight_in, # "g" or "kg"
                                 gender) { # "boy" or "girl"
  
  # if weight in g convert to kg
  if (weight_in == "g") {
    weight_measures <- weight_measures %>%
      mutate(weight = weight / 1000)
  }
  
  # get first and last date in table
  reference_date <- weight_measures$date[[1]]
  end_date <- weight_measures$date[[nrow(weight_measures)]] + 31
  
  # get weight at birth for calculating the reference percentile
  reference_weight <- weight_measures$weight[[1]]
  
  # filter for input age range & sex
  if (age_range == "0_13") {
    if (gender == "boy") {
      who <- p_0_13 %>%
        filter(gender == "boys")
      
    } else if (gender == "girl") {
      who <- p_0_13 %>%
        filter(gender == "girls")
      
    } else {
      stop('"gender" either has to be "boy" or "girl"')
    }
    
    who <- who %>%
      mutate(date = rep(seq(from = reference_date, by = "week", length.out = 14), 15),
             ref = "WHO")
    
    who_f <- who %>%
      filter(Week == 0)
    
  } else if (age_range == "0_5") {
    if (gender == "boy") {
      who <- p_0_5 %>%
        filter(gender == "boys")
      
    } else if (gender == "girl") {
      who <- p_0_5 %>%
        filter(gender == "girls")
      
    } else {
      stop('"gender" either has to be "boy" or "girl"')
    }
    
    who <- who %>%
      mutate(date = rep(seq(from = reference_date, by = "month", length.out = 61), 15),
             ref = "WHO") %>%
      filter(date <= !!paste(end_date))
    
    who_f <- who %>%
      filter(Month == 0)
    
  } else {
    stop('"age_range" either has to be "0_13" or "0_5"')
  }
  
  who <- weight_measures %>%
    mutate(ref = "measurement",
           percentile = "measurement",
           starting_p = who_f[which.min(abs(who_f$weight - reference_weight)), ]$percentile) %>%
    bind_rows(who)
  
  return(who)
}
