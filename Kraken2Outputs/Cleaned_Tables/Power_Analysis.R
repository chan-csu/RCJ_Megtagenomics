library(vegan)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(pwr)

abund_table=read.csv("Species_TSS.csv",row.names=1, check.names=FALSE)
Metadata=read.csv("metadata.csv",row.names=1,sep=',' ,check.names=FALSE)
H=diversity(t(abund_table))
df_H=data.frame(sample=names(H),value=H,measure=rep("Shannon", length(H)))
df_H$Group=as.factor(Metadata$Treatment_group)
df_H_F=select(df_H,Group,value)
p=ggplot(df_H_F, aes(x=value))+
    geom_histogram(color="black", fill="black")+
     facet_grid(Group ~ .)


TOI=read.csv("Filtered_TSS.csv",row.names=1,sep=',' )
RN=rownames(TOI)
TOI = data_frame(subset(TOI, select = c(RCE_plus_DSS_301,RCE_plus_DSS_305,RCE_plus_DSS_313,
                             DSS_334,DSS_339) ))
row.names(TOI)=RN
Treatment_Group=c("RCE_plus_DSS","RCE_plus_DSS","RCE_plus_DSS","DSS","DSS")


power.prop.test(n=10:20, p1=1, p2=.57, sig.level=0.05, power=NULL, alternative=c("one.sided"), strict = FALSE)

for (i in 1:length(RN)){
  Mean_RCE_plus_DSS=rowMeans((TOI[c(i),c(1,2,3)]))/100
  Mean_DSS=rowMeans((TOI[c(i),c(4,5)]))/100
  print(RN[i])
  print(power.prop.test(n=10:40, p1=Mean_RCE_plus_DSS, p2=Mean_DSS, sig.level=0.05, power=NULL, alternative=c("one.sided"), strict = FALSE))
  
}

for (i in 1:length(RN)){
  Mean_RCE_plus_DSS=rowMeans((TOI[c(i),c(1,2,3)]))
  Mean_DSS=rowMeans((TOI[c(i),c(4,5)]))
  print(RN[i])

  print("-----------------------------")

  
  print(pwr.chisq.test(w = abs(Mean_RCE_plus_DSS-Mean_DSS), N = 10:60, df = 1, sig.level = 0.05, power = NULL))
  

  print("-----------------------------")

}


for (i in 1:length(RN)){
  print(RN[i])
  print("-----------------------------")
  p2=rowMeans((TOI[c(i),c(4,5)]))
  p1_l=c(1.1*p2,1.3*p2,1.5*p2,1.7*p2,2*p2,3*p2,4*p2,5*p2,6*p2)
  r=1
  alpha=0.05
  beta=0.20
  n2=20
  for (j in 1:length(p1_l)){
  p1=p1_l[j]
  z=(p1-p2)/sqrt(p1*(1-p1)/n2/r+p2*(1-p2)/n2)
  Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2))

  
  
  print(c("Number Of Samples: ",ceiling(n2),"Factor: ",(p1-p2)/p2+1))
  print(c("Power: ",Power))
  
  }
  print("-----------------------------")
  
}

