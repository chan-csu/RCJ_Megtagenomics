# RCE_Megtagenomics

Parsa Ghadermazi 5/24/2022

Parsa96@colostate.edu

----------------------------------
A repository for tracking the metagenomics data analysis for "Amelioration of DSS induced colitis through supplementation of Red cabbage juice in mice model". 

**./Data_Analysis** includes the analysis of YAMP raw outputs:

* Main.R includes the analysis of the HumaNn outputs using MaAsLin.
* Data_Analysis.ipynb is a jupyter notebook that generates all the plots used in the manuscript.
* The rest of the files arethe files that are generated by these two scripts.

**./Kraken2Outputs** Includes an extra taxonomy analysis that used Kraken2

* Data_Analysis.ipynb is used to analyze Kraken2 outputs and generate the plots in this directory

**./Log_Files** includes the log files of YAMP, and MultiQC outputs of all the samples.

**./output_all** Includes all of the raw outputs of the YAMP pipeline 
