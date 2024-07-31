# function to reconize the Input data... is it plink or is it the snake from voldemord??
# get the Data in the same Heading space
plink_regina_unifier <- function(inputfile_path){
library(dplyr)

if (is.data.frame(inputfile_path)){
    original_data <- inputfile_path
}else {original_data <- read.csv(inputfile_path, sep="", check.names = FALSE)}
  

second_head <- colnames(original_data)[2]
#print(colnames(original_data))
if (second_head == "POS"){
  data_plot <- original_data %>%
    select("#CHROM", POS, ID, P)
  data_plot <- rename(data_plot, "CHROM"="#CHROM", "GENPOS"="POS")
  data_plot <- data_plot%>%
    filter(!is.na(P))%>%
    filter(P <= 1)%>%
    mutate(P, logP=-log10(P))
  data_plot <- data_plot %>%
    mutate(CHROM = ifelse(CHROM %in% c("X", "Y", "MT", "0"), c("23","24","25","26"), CHROM))
}else if (second_head == "GENPOS"){
  if("p.value" %in% names(original_data)){
   # print("yes i got some p values with out log")
    data_plot <- original_data %>%
      select(CHROM, GENPOS,ID, p.value, LOG10P) %>%
      rename(P=p.value, logP=LOG10P)
   data_plot <- data_plot%>%
     filter(!is.na(P))%>%
     filter(P <= 1)
  }else{
   # print("WARNING! This Data has only the -log10(P) value!!")
    data_plot <- original_data %>%
      select(CHROM, GENPOS, ID, LOG10P)%>%
      rename(logP=LOG10P)
    #print("i don that")
    data_plot <- data_plot%>%
      mutate(logP, P=10^(-logP))%>%
      filter(!is.na(P))%>%
      filter(P >= 0)

  }
  
}else{
  print("I dont know,pleas figur out what i am")
}
return(data_plot)}


