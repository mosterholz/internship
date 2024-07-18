# function to reconize the Input data... is it plink or is it the snake from voldemord??
# get the Data in the same Heading space und some other spaces
plink_regenie_unifier <- function(inputfile_path){
library(dplyr)

#Is the input a file path or already a data frame?    
if (is.data.frame(inputfile_path)){
    original_data <- inputfile_path
}else {original_data <- read.csv(inputfile_path, sep="", check.names = FALSE)}
  
#The second header name indicates whether the file is Plink or Regina.
second_head <- colnames(original_data)[2]
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
    data_plot <- original_data %>%
      select(CHROM, GENPOS,ID, p.value, LOG10P) %>%
      rename(P=p.value, logP=LOG10P)
   data_plot <- data_plot%>%
     filter(!is.na(P))%>%
     filter(P <= 1)
  }else{
    data_plot <- original_data %>%
      select(CHROM, GENPOS, ID, LOG10P)%>%
      rename(logP=LOG10P)
    data_plot <- data_plot%>%
      mutate(logP, P=10^(-logP))%>%
      filter(!is.na(P))%>%
      filter(P >= 0)
  }
  
}else{
  print("I dont know,pleas figur out what i am")
}
return(data_plot)}
       

