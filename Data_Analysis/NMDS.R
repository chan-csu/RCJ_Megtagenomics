install.packages("vegan")
install.packages("ggplot2")
library(vegan)
Sig_path=read.csv("Merged_Pathabundance_Table_CPM_meta.tsv", header= TRUE,sep="\t",row.names = 1)

Metadata=read.csv("../output_all/metadata.csv",sep = ",")

Table=Sig_path[,2:ncol(Sig_path)-1]
m_com = as.matrix(Table)
nmds = metaMDS(t(m_com), distance = "bray")
plot(nmds)
Metadata=Metadata[-c(17),]
data.scores = as.data.frame(scores(nmds)$sites)
data.scores$Treatment_group = Metadata$Treatment_group
data.scores$Samples=Metadata$Sample_ID


library(ggplot2)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = Treatment_group))+
  scale_color_manual(values=c("#999999", "#4C9A2A", "#1F75FE","#a000c8"))+
  geom_text(aes(label=Samples), size=3,nudge_y = 0.02 )

xx



##### Taxa

Sig_taxa=read.csv("megan/rel_freqs.csv", header= TRUE,sep=",",row.names = 1)

Metadata=read.csv("../output_all/metadata.csv",sep = ",")
Metadata=Metadata[-c(7,8,9,10,17),]

Table=Sig_taxa[,1:ncol(Sig_taxa)]
m_com = as.matrix(Table)
nmds = metaMDS(t(m_com), distance = "bray")
plot(nmds)
data.scores = as.data.frame(scores(nmds)$sites)
data.scores$Treatment_group = Metadata$Treatment_group
data.scores$Samples=Metadata$Sample_ID


library(ggplot2)

xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes( colour = Treatment_group))+
  scale_color_manual(values=c("#999999", "#4C9A2A", "#1F75FE","#a000c8"))+
  geom_text(aes(label=Samples), size=3,nudge_y = 0.02 )

xx
