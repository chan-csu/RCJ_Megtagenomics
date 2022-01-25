if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("metagenomeSeq",force = TRUE)
library(metagenomeSeq)
library(biomformat)
setwd("/Users/parsa/Desktop/Academics/Projects/RCE_Megtagenomics/Kraken2Outputs/Cleaned_Tables/")

Species = loadMeta("Species.csv",sep=',')
dim(Species$counts)
meta <- loadPhenoData("metadata.csv", sep = ",")
ord <- match(colnames(Species$counts), rownames(meta))
meta <- meta[ord,]
phenotypeData <- AnnotatedDataFrame(meta)
Species_Data <- newMRexperiment(Species$counts, phenoData = phenotypeData)
p <- cumNormStatFast(Species_Data)
Species_Data <- cumNorm(Species_Data, p = p)
Species_Data <- MRcounts(Species_Data, norm = TRUE, log = TRUE)
write.table(Species_Data, "Species_Normalized.txt", sep = "\t")
