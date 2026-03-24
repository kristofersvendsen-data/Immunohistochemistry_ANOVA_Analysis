##### Libary #####
library(tidyverse)
library(skimr)
library(car)
library(ggpubr)
library(RColorBrewer)
library(ggsignif)
library(rstatix)

##### Cleaning #####

#import data
cfos.dat <- read.csv(file = "./Gemini_cFos.csv", header = T, sep =",")


#inspect data
summary(cfos.dat)
glimpse(cfos.dat)


#rename headers nad negative control
cfos.dat <- cfos.dat %>% #lowercase headers
  rename_with(tolower)

cfos.dat[cfos.dat == "Vehicle"]<- "Water" #replace "Vehicle" with "Water"


#verify data types
is.factor(cfos.dat$condiion)
cfos.dat$condition = as.factor(cfos.dat$condition)
is.factor(cfos.dat$condition)

is.factor(cfos.dat$mouse)
cfos.dat$mouse = as.factor(cfos.dat$mouse)
is.factor(cfos.dat$mouse)


#reorder data
sort_order <- c("Water", "DSS", "DrugA", "DrugB") #define order

cfos.dat_sorted <- cfos.dat %>% 
  mutate(condition = fct_relevel(condition, sort_order)) %>% 
  arrange(condition)


##### Analysis #####

#compute average cFos count per mouse
mouse.dat <- aggregate(cfos.dat_sorted[,2], list(cfos.dat_sorted$mouse, cfos.dat_sorted$condition), FUN = mean)

names(mouse.dat)[1]<-'mouse'
names(mouse.dat)[2]<-'condition'
names(mouse.dat)[3]<-'count_avg'

n_values <-table(mouse.dat$condition)
print(n_values)


#ANOVA and Tukey's post hoc
anova.cfos <- aov(count_avg ~ condition, data = mouse.dat)
anova <- Anova(anova.cfos, type ="II")
print(anova)  


tukey <- tukey_hsd(anova.cfos)
print(tukey)



##### Visualization ####

#select pairwise comparisons for figure
tukey_figure <- tukey[c(1,3,4,5),] 

#create boxplot
bp <- ggboxplot(mouse.dat, x = "condition", y = "count_avg",
                fill = "condition", 
                add = "dotplot" , add.params = list(size = 1.6, alpha = 1), 
                ylab = "cFos+ Nuclei per Section", xlab = "Condition")+
  coord_cartesian(
    xlim = NULL,
    ylim = c(0, 80),
    expand = TRUE,
    default = FALSE,
    clip = "off")+
  stat_pvalue_manual(tukey_figure, 
                     label = "p.adj.signif", #to show exact p-value use "p.adj"
                     y.position = 45, step.increase = 0.3)+ #p-value bars location
  theme(legend.position = "none") +#remove legend
  scale_fill_brewer(palette ="Set1") #add colors
bp

##### Key info for write-up #####

key_stats <- list("n_values" = n_values,"anova" = anova, "tukey_hsd" = tukey)
print(key_stats)

#print figure
ggsave("immuno_plot.png", plot = bp)

