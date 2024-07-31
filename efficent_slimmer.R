# slimmer how realy cuts down the time

slimmer <- function(data, threshold = 0.001, fraction= 0.01, notqqplot=TRUE){

sig_data <- data %>%
  subset(P < threshold)

if(notqqplot){notsig_data <- data %>%
  subset(P >= threshold) %>%
  group_by(CHROM)%>%
  slice_sample(prop=fraction)%>%
  ungroup()
}else{  notsig_data <- data %>%
  subset(P >= threshold) %>%
  slice_sample(prop=fraction)}

slim_data <-rbind(sig_data,notsig_data)
return(slim_data)
}
