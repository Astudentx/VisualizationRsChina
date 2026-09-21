suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggsci)
  library(zyzPackage)
  library(ggrepel)
})
source("00.libs/functions/project_paths.R")
Group <- readxl::read_xlsx("01.rawdata/Group/Group_info1.xlsx") %>% as.data.frame()

# Fixed city-centre coordinates replace the retired REmap/Baidu lookup.  They
# make the map reproducible offline and do not affect sample counts or groups.
city_coordinate <- data.frame(
  City = c("\u5357\u5b81", "\u5f25\u52d2", "\u9075\u4e49", "\u5b9c\u6625", "\u957f\u6c99", "\u91cd\u5e86", "\u6210\u90fd"),
  lon = c(108.3669, 103.4360, 106.9274, 114.4168, 112.9388, 106.5516, 104.0665),
  lat = c(22.8170, 24.4090, 27.7257, 27.8153, 28.2282, 29.5630, 30.5723),
  Name = c("Nanning", "Mile", "Zunyi", "Yichun", "Changsha", "Chongqing", "Chengdu"),
  stringsAsFactors = FALSE
)
Group_num <- as.data.frame(table(Group$City), stringsAsFactors = FALSE)
names(Group_num) <- c("City", "Count")
Group_num <- dplyr::left_join(Group_num, city_coordinate, by = "City")
if (anyNA(Group_num$lon) || anyNA(Group_num$lat)) {
  stop("Missing fixed map coordinates for: ", paste(Group_num$City[is.na(Group_num$lon) | is.na(Group_num$lat)], collapse = ", "))
}
Group_num$size <- Group_num$Count / 10


load("04.datasave/Rdata/china_map_data.RData")
attach(china_map_data)
china_map_data$fill <- china_map_data$NAME
china_map_data$fill[which(!china_map_data$fill %in% c("四川省","云南省","湖南省","江西省","广西壮族自治区","重庆市","贵州省") )] <- NA



ggplot()+
  geom_polygon(china_map_data,mapping = aes(x=long,y=lat,group=group,fill=fill),colour="black",alpha=.6)+
  geom_point(data =Group_num,mapping = aes(x=lon,y=lat) ,color="black",size=2,alpha=1)+
  coord_quickmap()+
  scale_fill_npg()+
  geom_text_repel(data = Group_num,aes(x=lon,y=lat,label=Name))+
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    legend.position="none"
  )
save_pdf(filename = "03.figure/05.sample_map/sample_map.pdf",height = 10,width = 10)
