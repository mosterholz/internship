# function to reconize the Input data... is it plink or is it the snake from voldemord??
# get the Data in the same Heading space
plink_regina_unifier <- function(inputfile_path){
library(dplyr)

if (is.data.frame(inputfile_path)){
    original_data <- inputfile_path
}else {original_data <- read.csv(inputfile_path, sep="")}
  

second_head <- colnames(original_data)[2]

if (second_head == "POS"){
  data_plot <- original_data %>%
    select(CHROM="#CHROM", GENPOS=POS,ID, p.value=P)
  data_plot <- data_plot%>%
    filter(p.value <= 1)
  data_plot <- data_plot %>%
    mutate(CHROM = ifelse(CHROM %in% c("X", "Y", "MT", "0"), c("23","24","25","26"), CHROM))
}else if (second_head == "GENPOS"){
  #print("Moin i am a regenie")
  data_plot <- original_data %>%
    select(CHROM, GENPOS,ID, p.value)
  data_plot <- data_plot%>%
    filter(p.value <= 1)
}else{
  print("I dont know,pleas figur out what i am")
}
return(data_plot)}


