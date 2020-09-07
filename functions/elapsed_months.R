elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  ed_2 <- ed$year * 12 + ed$mon
  
  sd <- as.POSIXlt(start_date)
  sd_2 <- sd$year * 12 + sd$mon
  
  mo <- ed_2 - sd_2
  mo <- ifelse(day(ed) >= day(sd), mo + 1, mo)
  return(mo)
}