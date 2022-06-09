install.packages("vegan")
install.packages("ggplot2")
library(vegan)
Sig_path=read.csv("~/Desktop/Merged_Pathabundance_Table_CPM_meta.tsv", header= TRUE,sep="\t")

Metadata=read.csv("~/Desktop/metadata.csv",sep = ",")

Table=Sig_path[,2:ncol(Sig_path)]
m_com = as.matrix(Table)
nmds = metaMDS(t(m_com), distance = "bray")
plot(nmds)
data.scores = as.data.frame(scores(nmds)$sites)
data.scores$Treatment_group = Metadata$Treatment_group
data.scores$Samples=Metadata$Sample_ID


library(ggplot2)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(  colour = Treatment_group))+
  geom_text(aes(label=Samples), size=3,nudge_y = 0.02 )

xx
