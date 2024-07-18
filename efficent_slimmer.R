# slimmer how realy cuts down the time

slimmer <- function(data, threshold = 0.05, fraction= 0.01){

sig_data = data %>%
  subset(P < threshold)

notsig_data = data %>%
  subset(P >= threshold) %>%
  sample_frac(fraction)

slim_data <-rbind(sig_data,notsig_data)
return(slim_data)
}
