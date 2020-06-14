#R v.3.6.1
library(ggplot2); library(plotrix); library(reshape2); library(tidyr); library(dplyr)

#Figure S1: average nitrogen concentrations
setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/")
#average porewater ammonium concentration
pore_amm_agg4 <- read.table(file="pore_amm_agg2.txt", header=TRUE, sep="\t")
head(pore_amm_agg4)
pore_amm_conc_agg <- ggplot(data=pore_amm_agg4, aes(x=Treatment, y=Amm_uM_Mean, color=Treatment, shape=Source)) + xlab("") + 
  geom_point(position=position_dodge(0.9), stat="identity", size=3) + 
  scale_colour_manual(values=c("royalblue3", "#fc8d59")) + 
  scale_shape_manual(values=c(0, 4, 7)) + 
  ylab("") + ggtitle("") +
  geom_errorbar(aes(ymin=(Amm_uM_Mean - Amm_uM_SE), ymax=(Amm_uM_Mean + Amm_uM_SE)), width=0.2, position=position_dodge(0.9)) +
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 12),
        axis.line = element_line(size=0.5, colour="black"),
        axis.title.x=element_blank(),
        legend.position = (c(0.2, 0.75)), #could also write "none"
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))
pore_amm_conc_agg

#average porewater nitrate concentration
pore_nit_agg4 <- read.table(file="pore_nit_agg2.txt", header=TRUE, sep="\t")
head(pore_nit_agg4)
pore_nit_conc_agg <- ggplot(data=pore_nit_agg4, aes(x=Treatment, y=Nitrate_uM_Mean, color=Treatment, shape=Source)) + xlab("") + 
  geom_point(position=position_dodge(0.9), stat="identity", size=3) + 
  scale_shape_manual(values=c(0, 4, 7)) + 
  scale_colour_manual(values=c("royalblue3", "#fc8d59")) + 
  ylab("") + 
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 12),
        axis.line = element_line(size=0.5, colour="black"),
        legend.position = ("none"), #could also write c(num, num)
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) + ggtitle("") +
  geom_errorbar(aes(ymin=(Nitrate_uM_Mean - Nitrate_uM_SE), ymax=(Nitrate_uM_Mean + Nitrate_uM_SE)), width=0.2, position=position_dodge(0.9))
pore_nit_conc_agg

#average water column nitrate concentration
nit_all_agg2 <- read.table(file="nit_all_agg.txt", header=TRUE, sep="\t")
head(nit_all_agg2)
nit_agg <- ggplot(data=nit_all_agg2, aes(x=Treatment, y=Nitrate_um.mean, color=Treatment)) + xlab("") + 
  geom_point(position=position_dodge(0.9), stat="identity", size=3) + 
  scale_colour_manual(values=c("royalblue3", "#fc8d59")) + 
  ylab("Nitrate Concentration (µM)") + 
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 12),
        axis.line = element_line(size=0.5, colour="black"),
        legend.position = (c("none")), #could also write "none"
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x=element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5)) + ggtitle("") +
  geom_errorbar(aes(ymin=(Nitrate_um.mean - Nitrate_um.se), ymax=(Nitrate_um.mean + Nitrate_um.se)), width=0.2, position=position_dodge(0.9))
nit_agg

#average water column ammonium concentration
amm_all_agg2 <- read.table(file="amm_all_agg.txt", header=TRUE, sep="\t")
head(amm_all_agg2)
amm_agg <- ggplot(data=amm_all_agg2, aes(x=Treatment, y=Ammonium_um.mean, color=Treatment)) +
  geom_point(position=position_dodge(0.9), stat="identity", size=3) + 
  scale_colour_manual(values=c("royalblue3", "#fc8d59")) + 
  ylab("Ammonium Concentration (µM)") + 
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 12),
        axis.line = element_line(size=0.5, colour="black"),
        legend.position = (c("none")), #could also write "none"
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x=element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5)) + ggtitle("") +
  geom_errorbar(aes(ymin=(Ammonium_um.mean - Ammonium_um.se), ymax=(Ammonium_um.mean + Ammonium_um.se)), width=0.2, position=position_dodge(0.9))
amm_agg

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

plot(multiplot(amm_agg, nit_agg, pore_amm_conc_agg, pore_nit_conc_agg, cols=2, plotlist=NULL, layout=NULL))

#300x575
setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/ESF_figuresv2") #set this so plots go in there
pdf("fig1.pdf", width=8, height=6)
plot(multiplot(amm_agg, nit_agg, pore_amm_conc_agg, pore_nit_conc_agg, cols=2))
dev.off()


#Figure 3: FN2 effects of fertilizer on microbial communities of a) eelgrass root and b) eelgrass rhizosphere
#files under unifrac_pcoa.R under unifrac_stuff directory
setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/subsamp_tests/root")
####ROOT#### Weighted Unifrac: Sustained effects in root-associated microbiome


FN1_root_weighted <- read.table(file="fn1_root_resub.pick.pick.filter.fasta.midroot.tre1.weighted.ave.pcoa.axes.txt", header=TRUE, sep="\t")
head(FN1_root_weighted)

weighted_FN1_root <- ggplot(data=FN1_root_weighted, aes(x=axis1, y=axis2, color=Treatment)) + 
  geom_point(position="identity", stat="identity", size=3, shape=8) + 
  scale_color_manual(values=c("royalblue3", "#fc8d59")) + xlab("Axis 1 [46.20%]") + ylab ("Axis 2 [9.51%]") + 
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 10),
        axis.line = element_line(size=0.5, colour="black"),
       legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=10))
weighted_FN1_root


FN1_root_unweighted <- read.table(file="FN1_root/root_FN1.unweighted.pcoa.axes.txt", header=TRUE, sep="\t")
head(FN1_root_unweighted)
FN1_root_unweighted$Day <- factor(FN1_root_unweighted$Day, levels=c("D1", "D3", "D7", "D14"))

FN2_root_weighted <- read.table(file="fn2_root_resub.pick.pick.filter.fasta.midroot.tre1.weighted.ave.pcoa.axes.txt", header=TRUE, sep="\t")
head(FN2_root_weighted)
weighted_FN2_root <- ggplot(data=FN2_root_weighted, aes(x=axis1, y=axis2, color=Treatment)) + xlab("Axis 1 [49.42%]") + 
  geom_point(position="identity", stat="identity", size=3, shape=8) + ylab ("Axis 2 [17.44%]") +
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 10),
        axis.line = element_line(size=0.5, colour="black"),
        #legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=10),
        legend.box.background=element_rect(colour="white"))
weighted_FN2_root


setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/ESF_figures") #set this so plots go in there
tiff(filename="fig3a.tiff", width=5, height=3, units='in', res=300)
plot(weighted_FN2_root)
dev.off()


#weighted unifrac: FN1 in the rhiz
FN1_rhiz_weighted <- read.table(file="rhiz/fn1_rhiz_resub.pick.pick.filter.fasta.midroot.tre1.weighted.ave.pcoa.axes.txt", header=TRUE, sep="\t")
head(FN1_leaf_unweighted)
FN1_leaf_unweighted
FN1_leaf_unweighted$Day <- factor(FN1_leaf_unweighted$Day, levels=c("D0", "D1", "D3", "D7", "D14"))

weighted_fn1_rhiz <- ggplot(data=FN1_rhiz_weighted, aes(x=axis1, y=axis2, color=Treatment, fill=Treatment)) +  
  geom_point(position="identity", stat="identity", size=3, shape=7) + 
  xlab("Axis 1 [28.38%]") + ylab ("Axis 2 [20.38%]") +
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_fill_manual(values=c("royalblue3", "#fc8d59")) +
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 10),
        legend.position="none",
        axis.line = element_line(size=0.5, colour="black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=10))
weighted_fn1_rhiz
setwd("/Users/Winni/Desktop/Mueller_Lab/2017_Mesocosm_Expt/ESF_figures") #set this so plots go in there
tiff(filename="test.tiff", width=5, height=3, units='in', res=300)
plot(weighted_fn1_rhiz)
dev.off()

FN2_rhiz_weighted <- read.table(file="rhiz/fn2_rhiz_resub.pick.pick.filter.fasta.midroot.tre1.weighted.ave.pcoa.axes.txt", header=TRUE, sep="\t")
head(FN2_rhiz_weighted)
FN2_rhiz_weighted
weighted_FN2_rhiz <-ggplot(data=FN2_rhiz_weighted, aes(x=axis1, y=axis2, color=Treatment, fill=Treatment)) +  
  geom_point(position="identity", stat="identity", size=3, shape=7) + 
  xlab("Axis 1 [44.01%]") + ylab ("Axis 2 [9.12%]") +
  scale_color_manual(values=c("royalblue3", "#fc8d59")) +
  scale_fill_manual(values=c("royalblue3", "#fc8d59")) +
  theme(panel.background = element_rect(fill="white"),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA, colour="white"), #can also specify colour = "black"
        axis.title = element_text(size = 10),
        axis.line = element_line(size=0.5, colour="black"),
       legend.position = "none", #could also write "c("decimal", "decimal")
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=10))
weighted_FN2_rhiz
