# https://stackoverflow.com/a/26640698
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  mo <- (ed$year - sd$year) + (ed$mon - sd$mon)
  ifelse(day(ed) >= day(sd), mo + 1, mo)
}