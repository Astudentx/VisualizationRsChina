############Figure 2c###########
source("00.libs/functions/project_paths.R")
package <- c('readxl','magrittr','LorMe','randomForest',
             'ggtree','reshape2','ggplot2','dplyr','tidyr',
             'readr','purrr','tibble','stringr','forcats',
             'ggplot2','ggsci','ggridges','RColorBrewer','pheatmap',
             'patchwork','ggpubr','zyzPackage','cowplot','ggrepel','networkD3')

lapply(package, function(x){library(x, character.only = T)}) 
library(LorMe)
library(ggbeeswarm)
library(magrittr)

source("~/Documents/01.Work/02.Code/01.R/00.libs/00.script_profile.r")


library(ggplot2)
library(magrittr)
library(LorMe)
ac=read.table("01.rawdata/RsEcoRiskARGTransZYL/dif_position.txt", header=T, sep="\t",dec=".")
ac$log=log(ac$Transfrequency.frequency)
ac$value=ac$Transfrequency.frequency*1e4
# plot_maker()
#Running script(运行脚本)
#Remember to copy or print to consle! (记得复制或打印至控制台喔！)
###color set####
my_col<-c('#f8ac8c','#1F78B4','#005755')
names(my_col)<-c('Rhizosphere','Root','Stem')
inputdata<- ac 
signif_results<- auto_signif_test(data =inputdata,treatment_col =1,value_col =4,prior = T,valuename = "value")
attach(valueaov_results)
valueaov_results$comparison_letters



letters<- data.frame(valueaov_results$comparison_letters,letterp=max(1.3*(valueaov_results$comparison_letters %>% .[,'Mean'])+1.3*(valueaov_results$comparison_letters %>% .[,'std'])))
letters$letterp <- max(letters$Max+0.005)
letters$compare <- rownames(letters)
mean_frame<- aggregate(inputdata[,4],by=list(inputdata[,1]),FUN=mean)
Sd<- aggregate(inputdata[,4],by=list(inputdata[,1]),FUN=sd) %>% .[,'x']
Treatment_Name<- mean_frame$Group.1
N<- table(inputdata[,1]) %>% as.numeric()
Mean<- mean_frame[,'x']
SEM<- Sd/(N^0.5)
input_mean_frame<- data.frame(Treatment_Name,N,Mean,Sd,SEM)
PlotAcquiring <- ggplot(input_mean_frame,aes(x=as.factor(Treatment_Name),y=Mean/100))+
  theme_jgfbw()+
  geom_bar(stat = 'identity',size=0.2,width=0.5,aes(fill=as.factor(Treatment_Name)),color='#000000',alpha=0.8)+
  geom_errorbar(aes(ymin=Mean/100-Sd/100,ymax=Mean/100+Sd/100),size=0.3,width=0.1)+
  scale_y_continuous(expand = c(0.0005,0.0005))+
  scale_color_manual(values=my_col)+
  scale_fill_manual(values=my_col)+
  labs(x = '',y = 'Acquiring frequency(%)',fill = NULL)+
  # geom_text(data=letters,aes(x=as.factor(compare),y=letterp/100,label=Letter),size=6)+
  theme(legend.position = 'right',
  )+
  guides(color='none',fill='none')


########Figure 3a2
information<-read.table("01.rawdata/RsEcoRiskARGTransZYL/125strain_information.txt", header=T, sep="\t",dec=".") 
information_1=as.data.frame(table(information$Genus))
data<-read.table("01.rawdata/RsEcoRiskARGTransZYL/ac40_1.txt", header=T, sep="\t",dec=".") 
information_2=as.data.frame(table(data$Genus))
proportion=left_join(information_2,information_1,"Var1")
proportion$Freq=proportion$Freq.x/proportion$Freq.y
proportion$Freq=proportion$Freq[order(proportion$Freq)]
PlotProportion <- ggplot() + 
  geom_col(data=proportion,aes(x=Var1,y=Freq,fill=Var1),size = 1.2,position="dodge", stat="identity",width = 0.65,show.legend = F,fill="#F5C045")+
  # geom_errorbar(data=error.1,aes(x=species,ymin=Mean-std, ymax=Mean+std),width = 0.55)+
  scale_y_continuous(expand=c(0.001, 0.005),labels = scales::percent)+
  # coord_flip()+
  # scale_fill_manual(values=my_col1) +#
  theme_jgfbw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = 8))+ 
  labs(x = '',y = 'Proportion of transmissible strains',fill = NULL)

  # theme(axis.text = element_text(size = 5))
PlotProportion
save_pdf("03.figure/90.legacy_arg_transfer/S_ARG_transfer_genus_comparison.pdf",width = 3.5,height = 4)

########Figure 3c
library(phyloseq)
library(LorMe)
library(ggsci)
library(ggplot2)
library(magrittr)
load("01.rawdata/RsEcoRiskARGTransZYL/ARGspread.Rdata")
otu=otu_table(physeq1) %>% as.data.frame()
otu_pct=sweep(otu,colSums(otu),"/",MARGIN=2)
design=sample_data(physeq1)%>% as.data.frame()
tax_table=tax_table(physeq1)%>% as.data.frame()
otu_tax=data.frame(otu_pct,tax_table)
# write.table(otu_tax,"otu_tax.txt",row.names = T,sep="\t")

#####relative abundance####
data<-read.table("01.rawdata/RsEcoRiskARGTransZYL/phylum_percent.txt", header=T, sep="\t",dec=".") 
design$tre=rep(c("Sterilization","Unsterilization"),3)
design$rep=rep(c(1,2,3),2)
design$ID <- rownames(design)
design2 <- data.frame(design)
design2 <- design2 %>% select(ID,everything())

data1<-read.table("01.rawdata/RsEcoRiskARGTransZYL/phylum_percent.txt", header=T, sep="\t",dec=".") 
colnames(data1)[1] <- "ID"
data_top <- Top_taxa(input = data1,n = 9,inputformat = 2,outformat = 1)
df_col <- profile2df_dir(prefix = "ZYL",abundance = data_top,group = design2)

color_pa10 <- c(
  # "#e9e9e9",
  "#FA7F6F",
  "#8ECFC9",
  "#FFBE7A",
  "#82B0D2",
  "#BEB8DC",
  "#2878b5",
  "#9ac9db",
  "#f8ac8c",
  "#005755",
  "#F5C045"
)

Bac_pa_host <- ggplot(df_col,aes(x =ID ,y = value,fill=variable))+
  geom_col(width = 1)+
  scale_y_continuous(expand = c(0,0),labels = scales::percent)+
  scale_x_discrete(expand = c(0,0))+
  scale_fill_manual(values = color_pa10)+
  xlab("ARGs spread from Ralstonia to bacteria") + 
  ylab("Relative Abundance") +
  theme_jgf()+
  facet_wrap(~tre,scales = "free_x")+
  # theme(legend.position = c(.8,.8))+
  theme(legend.background = element_rect(fill = "white"))+
  theme(legend.key.size = unit(.1,'cm'))+
  theme(panel.margin.x=unit(0.8, "lines"))+
  guides(fill=guide_legend(ncol =1))+
  # theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(plot.margin = margin())+
  theme(legend.margin = margin())+
  theme(axis.ticks.length.x = unit(0, "pt"))+
  theme(strip.background = element_rect(color = "white"))
Bac_pa_host


save_rdata(PlotAcquiring,PlotProportion,Bac_pa_host,file = "04.datasave/Rdata/V1_JFGNatureZYLPlot_231216.Rdata")
