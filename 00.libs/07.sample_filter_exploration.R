 suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})
source("00.libs/functions/project_paths.R")

sample <- read.table("01.rawdata/Group/RawSampleID.txt",header = T)

Group <- readxl::read_xlsx("01.rawdata/Group/Group_info1.xlsx") %>% as.data.frame()
GroupID <- Group[,c(1:2)]
# 筛选所有根系土壤样品
Group_Ri <- Group[which(Group$Source=="Rhizosphere soil"),]
Group_Ri <- Group_Ri[Group_Ri$City != "\u5b9c\u6625", ]
message("Rhizosphere-soil cities retained: ", paste(sort(unique(Group_Ri$City)), collapse = ", "))



# 一定要设置select，不然会顺序错误！
rename_jgf <- function(df){
  df_sel <- df %>% dplyr::select(ID,all_of(Group$LinuxID))
  colnames(df_sel)[ colnames(df_sel) %in% Group$LinuxID ] <- Group$SampleID
  return(df_sel)
}

Bac_S_ab <- read.delim("01.rawdata/Tax_kraken/Final.Bacteria.S.xls",header = T,check.names = F) %>% rename_jgf()
Bac_G_ab <- read.delim("01.rawdata/Tax_kraken/Final.Bacteria.G.xls",header = T,check.names = F)



Bac_G_ab_Ri <- Bac_G_ab %>% select(ID,all_of(Group_Ri$LinuxID))
Bac_S_ab_Ri <- Bac_S_ab %>% select(ID,all_of(Group_Ri$SampleID))





LorMe04::structure_plot(plotprefix = "Bac_G_ab_Ri",inputframe = Bac_G_ab_Ri,inputformat = 2,groupframe = Group_Ri,treatlocation = 13)
P1 <- Bac_G_ab_Ri_PCA+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
P2 <- Bac_G_ab_Ri_PCOA+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
P3 <- Bac_G_ab_Ri_NMDS+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_G_ab_Ri_PCA.pdf",plot = P1,width = 15,height = 15)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_G_ab_Ri_PCOA.pdf",plot = P2,width = 15,height = 15)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_G_ab_Ri_NMDS.pdf",plot = P3,width = 15,height = 15)

LorMe04::structure_plot(plotprefix = "Bac_S_ab_Ri",inputframe = Bac_S_ab_Ri,inputformat = 2,groupframe = Group_Ri,treatlocation = 13)
P1 <- Bac_S_ab_Ri_PCA+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
P2 <- Bac_S_ab_Ri_PCOA+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
P3 <- Bac_S_ab_Ri_NMDS+ggrepel::geom_text_repel(aes(label = SampleID),show.legend = F)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_S_ab_Ri_PCA.pdf",plot = P1,width = 15,height = 15)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_S_ab_Ri_PCOA.pdf",plot = P2,width = 15,height = 15)
save_pdf(filename = "03.figure/07.sample_filter_exploration/Bac_S_ab_Ri_NMDS.pdf",plot = P3,width = 15,height = 15)

# The former ad-hoc whole-cohort and package-demo commands were interactive
# scratch work.  The six PDFs above are this script's reproducible outputs.
