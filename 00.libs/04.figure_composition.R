package <- c('readxl','magrittr','LorMe','randomForest',
             'ggtree','reshape2','ggplot2','dplyr','tidyr',
             'readr','purrr','tibble','stringr','forcats',
             'ggplot2','ggsci','ggridges','RColorBrewer','pheatmap',
             'patchwork','ggpubr','zyzPackage','cowplot','ggrepel','networkD3')

lapply(package, function(x){library(x, character.only = T)}) 
source("00.libs/functions/project_paths.R")
load("04.datasave/Rdata/V1_JFGNatureBacRs_231216.Rdata")

attach(Group_rs_nck)
my_comparison=list(c("Healthy","Diseased")) #定义比较的分组
P1 <- ggplot(data = Group_rs_nck[which(Group_rs_nck$Source=="Rhizosphere"),],aes(x= State,y = `R abundance`,color=State))+
  theme_jgfbw()+
  scale_color_manual(values = color_zyz1)+
  scale_fill_manual(values = color_zyz1)+
  # geom_violin(width=1,size = .5,alpha=0.9,position = position_dodge(.5)) +
  geom_boxplot(width=0.4,size = .2,alpha=0.7,outlier.size=0,coef=1,# coef control the IQR
               position = position_dodge(.5)) +
  geom_point(alpha=.6)+
  geom_line(aes(group=Rep1), color="gray" ,size=.2,alpha=.4) +
  theme(legend.position = "none")+
  # theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = scales::percent) +
  stat_compare_means(comparisons = my_comparison,paired = T,method = "t.test",bracket.size=0,vjust = .7,size=3,label = "p.signif")+
  xlab("")+ylab("Ralstonia abundance (%)")
P1



load("04.datasave/Rdata/V1_JFGNatureARGsAll_231216.Rdata")
library(ggpubr)
my_comparison=list(c("Healthy","Diseased")) #定义比较的分组
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
P2 <- ggplot(data = Group_type_sum[which(Group_type_sum$Source=="Rhizosphere"),],aes(x= State,y = abundance,color=State))+
  theme_jgfbw()+
  scale_color_manual(values = color_zyz1)+
  scale_fill_manual(values = color_zyz1)+
  # geom_violin(width=.6,size = .5,alpha=0.9,position = position_dodge(.5)) +
  geom_boxplot(width=0.4,size = .2,alpha=0.7,outlier.size=0,coef=1,# coef control the IQR
               position = position_dodge(.5)) +
  geom_point(alpha=.6)+
  geom_line(aes(group=Rep1), color="gray" ,size=.2,alpha=1) +
  # facet_wrap(~Source,scales = "free_x")+
  theme(legend.position = "none")+
  
  stat_compare_means(comparisons = my_comparison,paired = T,method = "t.test",bracket.size=0,vjust = .7,size=3,label = "p.signif")+
  xlab("")+ylab("Total ARG (ppm)")
# scale_y_continuous(labels = scales::percent,trans = "sqrt")
# save_pdf("01.output/fig1/T-pair_ARGMGEboxplot.pdf",height = 4,width = 8)
P2


load("04.datasave/Rdata/V1_JFGNatureARGsRs_231216.Rdata")
library(ggpubr)
my_comparison=list(c("Healthy","Diseased")) #定义比较的分组
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
P3 <- ggplot(data = Group_rs[which(Group_rs$Source=="Rhizosphere"),] ,aes(x= State,y = abundance,color=State))+
  theme_jgfbw()+
  scale_color_manual(values = color_zyz1)+
  scale_fill_manual(values = color_zyz1)+
  # geom_violin(width=.6,size = .5,alpha=0.9,position = position_dodge(.5)) +
  geom_boxplot(width=0.4,size = .2,alpha=0.7,outlier.size=0,coef=1,# coef control the IQR
               position = position_dodge(.5)) +
  geom_point(alpha=.6)+
  geom_line(aes(group=Rep), color="gray" ,size=.2,alpha=1) +
  theme(legend.position = "none")+
  
  stat_compare_means(comparisons = my_comparison,paired = T,method = "t.test",bracket.size=0,vjust = .7,size=3,label = "p.signif")+
  xlab("")+ylab("Rs ARGs (ppm)")
# scale_y_continuous(labels = scales::percent,trans = "sqrt")
# save_pdf("01.output/fig1/T-pair_ARGMGEboxplot.pdf",height = 4,width = 8)
P3

P1/P2/P3

save_pdf("03.figure/04.figure_composition/JGFNature_S_Rs_Ttest.pdf",height = 6.5,width = 2.5)






# ZYL图片输入
load("04.datasave/Rdata/V1_JFGNatureZYLPlot_231216.Rdata")

# The 2023 RData stores ggplot objects created with an older ggplot2 internal
# scale API.  Current ggplot2 cannot clone those serialized objects.  Their
# original plotting data are retained inside the objects, so recreate the same
# three plots from those data rather than altering any underlying analysis.
input_mean_frame <- PlotAcquiring$data
proportion <- PlotProportion$layers[[1]]$data
df_col <- Bac_pa_host$data

my_col <- c('#f8ac8c', '#1F78B4', '#005755')
names(my_col) <- c('Rhizosphere', 'Root', 'Stem')
PlotAcquiring <- ggplot(input_mean_frame, aes(x = as.factor(Treatment_Name), y = Mean / 100)) +
  theme_jgfbw() +
  geom_bar(stat = 'identity', size = 0.2, width = 0.5,
           aes(fill = as.factor(Treatment_Name)), color = '#000000', alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean / 100 - Sd / 100, ymax = Mean / 100 + Sd / 100),
                size = 0.3, width = 0.1) +
  scale_y_continuous(expand = c(0.0005, 0.0005)) +
  scale_color_manual(values = my_col) +
  scale_fill_manual(values = my_col) +
  labs(x = '', y = 'Acquiring frequency(%)', fill = NULL) +
  theme(legend.position = 'right') +
  guides(color = 'none', fill = 'none')

PlotProportion <- ggplot() +
  geom_col(data = proportion, aes(x = Var1, y = Freq, fill = Var1),
           size = 1.2, position = 'dodge', width = 0.65, show.legend = FALSE,
           fill = '#F5C045') +
  scale_y_continuous(expand = c(0.001, 0.005), labels = scales::percent) +
  theme_jgfbw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  labs(x = '', y = 'Proportion of transmissible strains', fill = NULL)

color_pa10 <- c('#FA7F6F', '#8ECFC9', '#FFBE7A', '#82B0D2', '#BEB8DC',
                '#2878b5', '#9ac9db', '#f8ac8c', '#005755', '#F5C045')
Bac_pa_host <- ggplot(df_col, aes(x = ID, y = value, fill = variable)) +
  geom_col(width = 1) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = color_pa10) +
  xlab('ARGs spread from Ralstonia to bacteria') +
  ylab('Relative Abundance') +
  theme_jgf() +
  facet_wrap(~tre, scales = 'free_x') +
  theme(legend.background = element_rect(fill = 'white'),
        legend.key.size = unit(.1, 'cm'),
        panel.margin.x = unit(.8, 'lines'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(),
        legend.margin = margin(),
        axis.ticks.length.x = unit(0, 'pt'),
        strip.background = element_rect(color = 'white')) +
  guides(fill = guide_legend(ncol = 1))


design1 <- c("147
              258
              369")


plot_spacer()+PlotAcquiring+PlotProportion+P1+P2+P3+Bac_pa_host+plot_spacer()+plot_spacer()+
  plot_layout(design = design1)&
  plot_annotation(tag_levels = list(c('b', 'c',"d","e","f","g")))&
  # theme(plot.margin = ggplot2::margin(0, .1, 0, .1, "cm"),)&
  theme(plot.title = element_text(face = "bold"),plot.tag = element_text(face = "bold"))

save_pdf("03.figure/04.figure_composition/JGFNature_fig3.pdf",height = 10,width = 11)


# 基金图片
P1.1 <- P1+
  scale_color_manual(values = color_zyz2)+
  scale_fill_manual(values = color_zyz2)
P2.1 <- P2+
  scale_color_manual(values = color_zyz2)+
  scale_fill_manual(values = color_zyz2)
P3.1 <- P3+
  scale_color_manual(values = color_zyz2)+
  scale_fill_manual(values = color_zyz2)


# load("04.datasave/Rdata/V1_JFGNatureBacComp_231216.Rdata")
load("04.datasave/Rdata/V1_JFGNatureBacComp2_231216.Rdata")
LorMe04::community_plot(plotprefix ="Bac_G_re_rh" ,inputframe = Bac_G_re_rh,n = 9,inputformat = 2,groupframe = Group_rh,treatlocation =8,replocation = 11 ,treatorder = c("Healthy","Diseased"))
attach(Grouped_top9)

Pcom <- Bac_G_re_rh_alluvialplot+scale_x_discrete()+xlab("")+ylab("Relative abundance") +
  scale_fill_npg()+
  scale_y_continuous(labels = scales::percent,expand = c(0,0)) +theme_jgfbw()+
  scale_x_discrete(expand = c(0.3,0.3))+
  # theme(legend.position = "bottom")+
  theme(legend.key.size = unit(.3,'cm'))+theme(panel.margin.x=unit(0.2, "lines"))#+ guides(fill=guide_legend(nrow =3))
Pcom

design1 <- c("1234")
Pcom+P1.1+P2.1+P3.1+plot_layout(design = design1)+ plot_annotation(tag_levels = 'a')
save_pdf("03.figure/04.figure_composition/JGFNature_S_Rs_Ttest2.pdf",height = 2.5,width = 10)



