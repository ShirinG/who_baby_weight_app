combine_measures_who_2 <- function(weight_measures, 
                                   weight_in, # "g" or "kg"
                                   gender) { # "boy" or "girl"
    
    if (weight_in == "g") {
        weight_measures <- weight_measures %>%
            mutate(weight = weight / 1000)
    }
    
    reference_date <- weight_measures$date[[1]]
    end_date <- weight_measures$date[[nrow(weight_measures)]] + 31
    
    reference_weight <- weight_measures$weight[[1]]
    
    p_0_13_2 <- p_0_13 %>%
        mutate(date = rep(seq(from = reference_date, by = "week", length.out = 14), 30),
               ref = "WHO")
    
    p_0_5_2 <- p_0_5 %>%
        mutate(date = rep(seq(from = reference_date, by = "month", length.out = 61), 30),
               ref = "WHO")
    
    who <- p_0_13_2 %>%
        bind_rows(p_0_5_2) %>%
        filter(date <= !!paste(end_date))
    
    if (gender == "boy") {
        who <- who %>%
            filter(gender == "boys")
        
    } else if (gender == "girl") {
        who <- who %>%
            filter(gender == "girls")
        
    } else {
        stop('"gender" either has to be "boy" or "girl"')
    }
    
    who_f <- who %>%
        filter(Week == 0)
    
    who <- weight_measures %>%
        mutate(ref = "measurement",
               percentile = "measurement",
               starting_p = who_f[which.min(abs(who_f$weight - reference_weight)), ]$percentile) %>%
        bind_rows(who)
    
    return(who)
}