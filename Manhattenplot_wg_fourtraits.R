

paths <- c("~/Desktop/Praxis Modul/playground/learn_enviroment/antiTNF_bioNaive_7pc_sex_regenie_firth_clinicalRemission_UC_plainpvalue.regenie.gz", "~/Desktop/Praxis Modul/playground/learn_enviroment/clinicalRemission_HLA_pc7_diagnose.PHENO1.glm.logistic (1).hybrid")
diseases <- c("trait 01", "trait 02", "trait 03", "trait 04" )


Manhattanplot <- function(paths, diseases){

all_traits_list <- lapply(paths, plink_regina_unifier)
  #2. bind_rows
all_traits <- bind_rows(all_traits_list ,.id="trait")
all_traits <- all_traits %>%
    mutate(trait = ifelse(trait %in% c("1", "2", "3", "4"), diseases, trait))

all_traits_slim <- slimmer(all_traits, threshold = 0.0001, fraction=0.3)

cumCHROM <- all_traits_slim%>%
  group_by(CHROM)%>%
  arrange(CHROM)%>%
  summarise(maxGENPOS = max(GENPOS))%>%
  mutate(addGENPOS = lag(cumsum(as.numeric(maxGENPOS)), default =0))%>%
  select(CHROM, addGENPOS)

datatoplot <- all_traits_slim%>%
  inner_join(cumCHROM, by= "CHROM") %>%
  mutate(cumGENPOS= GENPOS+addGENPOS)

axisX <- datatoplot%>%
  group_by(CHROM)%>%
  arrange(CHROM)%>%
  summarise(maxGENPOS = max(cumGENPOS), minGENPOS= min(cumGENPOS))%>%
  rowwise()%>%
  mutate(medGENPOS= median(c(minGENPOS,maxGENPOS)))

lables_axisX <- cumCHROM%>%
  select(CHROM)%>%
  mutate(CHROM = ifelse(CHROM == "23", "X", CHROM))

sig_all <- datatoplot%>%
  filter(P<threshold)

datatoplot <- datatoplot%>%
  mutate(colorpattern=case_when(
    P < threshold & trait == diseases[1] ~  "diseases1",
    P < threshold & trait == diseases[2] ~  "diseases2",
    P < threshold & trait == diseases[3] ~  "diseases3",
    P < threshold & trait == diseases[4] ~  "diseases4",
    P >= threshold & CHROM %% 2 == 0 ~ "chrom_even",
    P >= threshold & CHROM %% 2 == 1 ~ "chrom_odd"
  ))
ggplot(data = datatoplot, 
       mapping = aes( x=cumGENPOS, y=-log10(P), colour = colorpattern))+
  geom_hline(
    yintercept = -log10(5*10^(-8)),
    linetype="dashed")+
  geom_point()+
  scale_colour_manual(
    values = c(diseases1 = "blue", diseases2 = "red", diseases3="green4", diseases4="orange", chrom_odd="grey60",chrom_even="grey70"),
    breaks = c("diseases1", "diseases2", "diseases3", "diseases4"),
    labels = diseases,
    name="Disease")+
  scale_fill_discrete(name = "Diseases", labels = c(diseases[1], diseases[2], diseases[3], diseases[4]))+
  geom_text_repel(data = sig_all, aes(x=cumGENPOS, y=-log10(P),label = paste(ID)))+
  scale_x_continuous(
    label = as.factor(lables_axisX$CHROM),
    breaks = axisX$medGENPOS)+
  theme_classic()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 8, vjust = 0.5))+
  labs(
    x = "Chromosome",
    y = expression("-log"[10]*"(p)"))
}

ggsave("plot.png", plot = plot, width = 6, height = 4, dpi = 300)


