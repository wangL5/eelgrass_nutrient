#R v.3.6.1
setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/2018_09_graphs")
library(ggplot2); library(plotrix); library(reshape2); library(tidyr); library(dplyr)

pore_amm_agg <- read.table(file="/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/pore_amm_agg.txt", header=TRUE, sep="\t")
pore_amm_agg
pore_nit_agg <- read.table(file="/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/pore_nit_agg.txt", header=TRUE, sep="\t")
pore_nit_agg
wc_nit_agg <- read.table(file="/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/nitrate_agg.txt", header=TRUE, sep="\t")
wc_nit_agg
wc_amm_agg <- read.table(file="/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/ammonium_agg.txt", header=TRUE, sep="\t")
wc_amm_agg

###setting the theme to be used for all graphs
theme_min <-  theme(panel.background = element_rect(fill="white"),
                    legend.background = element_rect(fill="white"),
                    legend.key = element_rect(fill="white"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text = element_text(size = 10),
                    legend.title = element_blank(),
                    panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
                    axis.title = element_text(size = 8),
                    axis.line = element_line(size=0.5, colour="black"),
                    legend.position = ("none"), #could also write "none"
                    #axis.text.x = element_text(angle = 0, hjust = 0.5),
                    legend.text = element_text(size = 8),
                    plot.title = element_text(hjust = 0.5, size=10))
  
wc_amm1 <- ggplot(data=wc_amm_agg, aes(x=Day, y=Ammonium_um.mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) + 
  ylab("Ammonium (µM)") + theme_min +
  geom_errorbar(aes(ymin=(Ammonium_um.mean - Ammonium_um.se), ymax=(Ammonium_um.mean + Ammonium_um.se)), width=0.2, position=position_dodge(0.25)) + 
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 8),
        axis.line = element_line(size=0.5, colour="black"),
        legend.position = (c("0.90", "0.75")), #could also write "none"
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=10))
wc_amm1
#ggtitle("Water Column Ammonium Concentration")


BS_amm2 <- ggplot(data=subset(pore_amm_agg, Source %in% c("Bulk_Sediment")), aes(x=Day, y=Amm_uM.mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  #geom_boxplot(aes(color=Treatment), alpha=0.5, na.rm=TRUE) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Ammonium (µM)") + theme_min + 
  geom_errorbar(aes(ymin=(Amm_uM.mean - Amm_uM.se), ymax=(Amm_uM.mean + Amm_uM.se)), width=0.2, position=position_dodge(0.25))
BS_amm2
#ggtitle("Bulk Sediment Porewater Ammonium Concentration")

NR_amm3 <- ggplot(data=subset(pore_amm_agg, Source %in% c("Plant_Associated")), aes(x=Day, y=Amm_uM.mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  #geom_boxplot(aes(color=Treatment), alpha=0.5, na.rm=TRUE) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) + 
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Ammonium (µM)") + theme_min + 
  geom_errorbar(aes(ymin=(Amm_uM.mean - Amm_uM.se), ymax=(Amm_uM.mean + Amm_uM.se)), width=0.2, position=position_dodge(0.25))
NR_amm3
#ggtitle("Plant Associated Sediment Porewater\n Ammonium Concentration")

R_amm4 <- ggplot(data=subset(pore_amm_agg, Source %in% c("Root_Rhizosphere")), aes(x=Day, y=Amm_uM.mean, color=Treatment)) + xlab("Day") + 
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Ammonium (µM)") + theme_min + 
  geom_errorbar(aes(ymin=(Amm_uM.mean - Amm_uM.se), ymax=(Amm_uM.mean + Amm_uM.se)), width=0.2, position=position_dodge(0.25))
R_amm4
#ggtitle("Rhizosphere Porewater Ammonium Concentration")

pore_amm_agg
pore_nit_agg

wc_nit5 <- ggplot(data=wc_nit_agg, aes(x=Day, y=Nitrate_um.mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) + 
  ylab("Nitrate (µM)") + theme_min +
  geom_errorbar(aes(ymin=(Nitrate_um.mean - Nitrate_um.se), ymax=(Nitrate_um.mean + Nitrate_um.se)), width=0.2, position=position_dodge(0.25))
wc_nit5
#ggtitle("Water Column Nitrate Concentration") 

BS_nit6 <-ggplot(data=subset(pore_nit_agg, Source %in% c("Bulk Sediment")), aes(x=Day, y=Nitrate_uM_Mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  #geom_boxplot(aes(color=Treatment), alpha=0.5, na.rm=TRUE) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) + theme_min + 
  ylab("Nitrate (µM)") + theme_min + geom_errorbar(aes(ymin=(Nitrate_uM_Mean - Nitrate_uM_SE), ymax=(Nitrate_uM_Mean + Nitrate_uM_SE)), width=0.2, position=position_dodge(0.25))
BS_nit6
#ggtitle("Bulk Sediment Porewater Nitrate Concentration")

NR_nit7 <-ggplot(data=subset(pore_nit_agg, Source %in% c("Plant Associated")), aes(x=Day, y=Nitrate_uM_Mean, color=Treatment)) + xlab("") +
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  #geom_boxplot(aes(color=Treatment), alpha=0.5, na.rm=TRUE) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Nitrate (µM)") + theme_min + 
  geom_errorbar(aes(ymin=(Nitrate_uM_Mean - Nitrate_uM_SE), ymax=(Nitrate_uM_Mean + Nitrate_uM_SE)), width=0.2, position=position_dodge(0.25))
NR_nit7
#ggtitle("Plant Associated Sediment Porewater Nitrate Concentration")

R_nit8 <-ggplot(data=subset(pore_nit_agg, Source %in% c("Rhizosphere")), aes(x=Day, y=Nitrate_uM_Mean, color=Treatment)) + xlab("Day") + 
  geom_line(position=position_dodge(0.25), stat="identity") + 
  geom_point(position=position_dodge(0.25), stat="identity", size=3) + 
  #geom_boxplot(aes(color=Treatment), alpha=0.5, na.rm=TRUE) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_x_continuous(breaks=c(1, 5, 10, 15, 20, 25, 30)) +
  ylab("Nitrate (µM)") + theme_min + 
  geom_errorbar(aes(ymin=(Nitrate_uM_Mean - Nitrate_uM_SE), ymax=(Nitrate_uM_Mean + Nitrate_uM_SE)), width=0.2, position=position_dodge(0.25))
R_nit8
#ggtitle("Rhizosphere Porewater Nitrate Concentration")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(wc_amm1, BS_amm2, NR_amm3, R_amm4, wc_nit5, BS_nit6, NR_nit7, R_nit8, cols=2, plotlist=NULL, layout=NULL)
