library(VennDiagram)
library(png)

BasePourR_clean <- BasePourR %>% select(drought,drought_1,drought_2,drought_3,drought_4,drought_5)

BasePourR_clean$DIVenn <- seq(1,dim(BasePourR_clean)[1])

lsVar <- c("drought","drought_1","drought_2","drought_3","drought_4","drought_5")

for (i in seq(1,6)) {
  varNam <- paste(lsVar[i],"top", sep = "_")
  BasePourR_clean[varNam] <- ifelse(BasePourR_clean[lsVar[i]] > 0,BasePourR_clean$DIVenn,0)
}

BasePourR_clean <- na.omit(BasePourR_clean)

set0 <- c(BasePourR_clean$drought_top)
set1 <- c(BasePourR_clean$drought_1_top)
set2 <- c(BasePourR_clean$drought_2_top)
set3 <- c(BasePourR_clean$drought_3_top)
set4 <- c(BasePourR_clean$drought_4_top)
set5 <- c(BasePourR_clean$drought_5_top)

set1 <- set1[is.null(set1) == FALSE]
set2 <- set2[is.null(set2) == FALSE]
set3 <- set3[is.null(set3) == FALSE]
set4 <- set4[is.null(set4) == FALSE]
set5 <- set5[is.null(set5) == FALSE]

colors <- c("#6b7fff", "#c3db0f", "#ff4059", "#2cff21","#21ffb5")

# Make Venn diagram from list of groups
venn.diagram(x = list(set1,set2,set3,set4,set5) ,
             category.names = c("N-1", "N-2", "N-3", "N-4","N-5"),
             filename = 'datadaft_venn.png',
             output=TRUE,
             imagetype="png", 
             scaled = FALSE,
             col = "black",
             fill = colors,
             cat.col = colors,
             cat.cex = 2,
             margin = 0.15)

options(repr.plot.height=12, repr.plot.width= 12)

pp <- readPNG("datadaft_venn.png")
plot.new() 
rasterImage(pp,0,0,1.1,1.1)
