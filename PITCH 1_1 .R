## Important note: The funtions used in this file are created in another file 
## named: Functions Thesis Lotte van de Kamp. 

## Load packages and look up the citatations------------------------------------

library(dplyr)
library(crqa)
library(ggplot2)
library(reshape2)
library(graphics)
library(utils)

citation(package = "base")
citation(package = "dplyr")
citation(package = "crqa")
citation(package = "ggplot2")
citation(package = "reshape2")
citation(package = "graphics")
citation(package = "utils")

## Loading the data ------------------------------------------------------------

# OpenFace datafiles: 

#DEiA III 2019-2020 SM1:

Pitch_S <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/OpenFace/DEiA_1_Ziggurat.csv", sep = ",")
Pitch_J1 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/OpenFace/DEiA_1_Ziggurat_Judge1.csv", sep = ",")
Pitch_J2 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/OpenFace/DEiA_1_Ziggurat_Judge2.csv", sep = ",")
Pitch_J3 <- read.delim("~/Documents/Lotte/M-Data science/Thesis/Data /DEiA/OpenFace/DEiA_1_Ziggurat_Judge3.csv", sep = ",")

## Check the variables of the datasets: ----------------------------------------

variable.names(Pitch_S)

## Feature Engineering - Mimicry (Recurrence Rates) ----------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Subset the data, to include only the action units and check for NAs ----------

myvars <- c("frame","AU01_c", "AU02_c", "AU04_c", "AU05_c", "AU06_c", "AU07_c",
            "AU09_c", "AU10_c", "AU12_c", "AU14_c", "AU15_c", "AU17_c", "AU20_c"
            , "AU23_c", "AU25_c", "AU26_c", "AU45_c")

maxr <- 100

Pitch_S_AU <- Pitch_S %>%
  select(all_of(myvars)) %>%
  filter(frame< maxr)
Pitch_J1_AU <- Pitch_J1 %>%
  select(all_of(myvars)) %>%
  filter(frame< maxr)
Pitch_J2_AU <- Pitch_J2 %>%
  select(all_of(myvars)) %>%
  filter(frame< maxr)
Pitch_J3_AU <- Pitch_J3 %>%
  select(all_of(myvars)) %>%
  filter(frame< maxr)
  
sum(is.na(Pitch_S_AU))
sum(is.na(Pitch_J1_AU))
sum(is.na(Pitch_J2_AU))
sum(is.na(Pitch_J3_AU))

summary(Pitch_S_AU)
summary(Pitch_J1_AU)
summary(Pitch_J2_AU)
summary(Pitch_J3_AU)

# Calculate the recurrece rates per Action Unit: -------------------------------
##------------------------------------------------------------------------------
##-------------AU01-------------------------------------------------------------

# in this case 5125 rows. 

Student_AU1 <- Subset_dataset(Pitch_S,"AU01_c", maxr)
Judge1_AU1 <- Subset_dataset(Pitch_J1, "AU01_c", maxr)
Judge2_AU1 <- Subset_dataset(Pitch_J2, "AU01_c", maxr)
Judge3_AU1 <- Subset_dataset(Pitch_J3, "AU01_c", maxr)


## EDA of new datasets :--------------------------------------------------------

summary(Student_AU1)
sum(Student_AU1$AU01_c == 1)

summary(Judge1_AU1)
sum(Judge1_AU1$AU01_c == 1)

summary(Judge2_AU1)
sum(Judge2_AU1$AU01_c == 1)

summary(Judge3_AU1)
sum(Judge3_AU1$AU01_c == 1)

## Create timeseries --------------------------------------------------------

Student_AU1 <- ts_m(Student_AU1$AU01_c)
Judge1_AU1 <- ts_m(Judge1_AU1$AU01_c)
Judge2_AU1 <- ts_m(Judge2_AU1$AU01_c)
Judge3_AU1 <- ts_m(Judge3_AU1$AU01_c)

## Plot timeseries--------------------------------------------------------------

pdf("Timeseries Pitch 1_1 AU01.pdf")       
plot_ts(Student_AU1, Judge1_AU1, Judge2_AU1, Judge3_AU1)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU01_P_J1= run_crqa(Student_AU1, Judge1_AU1)
CRP_AU01_P_J2= run_crqa(Student_AU1, Judge2_AU1)
CRP_AU01_P_J3= run_crqa(Student_AU1, Judge3_AU1)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU01.pdf") 
Plot_CRP1(CRP_AU01_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU01.pdf") 
Plot_CRP2(CRP_AU01_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU01.pdf") 
Plot_CRP3(CRP_AU01_P_J3$RP)
dev.off()


##CALCULATE RR over window of 25 frames: ---------------------------------------

RR_AU01_J1 <- run_DCRP(Student_AU1, Judge1_AU1, N)
RR_AU01_J2 <- run_DCRP(Student_AU1, Judge2_AU1, N)
RR_AU01_J3 <- run_DCRP(Student_AU1, Judge3_AU1, N)

RR_AU01_P <- c(RR_AU01_J1[[1]], RR_AU01_J2[[1]], RR_AU01_J3[[1]])

## put in dataframe of pitch 1 -------------------------------------------------

#create dataframe: 

my_list <- list(1:3)
df_pitch1 <- data.frame(my_list)  
df_pitch1$RR_AU01_P <- RR_AU01_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU02----------------------------------------------------

#subset only AU02:

Student_AU2 <- Subset_dataset(Pitch_S,"AU02_c", maxr)
Judge1_AU2 <- Subset_dataset(Pitch_J1, "AU02_c", maxr)
Judge2_AU2 <- Subset_dataset(Pitch_J2, "AU02_c", maxr)
Judge3_AU2 <- Subset_dataset(Pitch_J3, "AU02_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU2)
sum(Student_AU2$AU02_c == 1)


summary(Judge1_AU2)
sum(Judge1_AU2$AU02_c == 1)

summary(Judge2_AU2)
sum(Judge2_AU2$AU02_c == 1)

summary(Judge3_AU2)
sum(Judge3_AU2$AU02_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU2 <- ts_m(Student_AU2$AU02_c)
Judge1_AU2 <- ts_m(Judge1_AU2$AU02_c)
Judge2_AU2 <- ts_m(Judge2_AU2$AU02_c)
Judge3_AU2 <- ts_m(Judge3_AU2$AU02_c)

## Plot timeseries: ------------------------------------------------------------


pdf("Timeseries Pitch 1_1 AU02.pdf")      
plot_ts(Student_AU2, Judge1_AU2, Judge2_AU2, Judge3_AU2)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU02_P_J1= run_crqa(Student_AU2, Judge1_AU2)
CRP_AU02_P_J2= run_crqa(Student_AU2, Judge2_AU2)
CRP_AU02_P_J3= run_crqa(Student_AU2, Judge3_AU2)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU02.pdf") 
Plot_CRP1(CRP_AU02_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU02.pdf") 
Plot_CRP2(CRP_AU02_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU02.pdf") 
Plot_CRP3(CRP_AU02_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------
RR_AU02_J1 <- run_DCRP(Student_AU2, Judge1_AU2, N)
RR_AU02_J2 <- run_DCRP(Student_AU2, Judge2_AU2, N) 
RR_AU02_J3 <- run_DCRP(Student_AU2, Judge3_AU2, N)

RR_AU02_P <- c(RR_AU02_J1[[1]], RR_AU02_J2[[1]], RR_AU02_J3[[1]])


## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU02_P <- RR_AU02_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU04----------------------------------------------------

#subset only AU04:

Student_AU4 <- Subset_dataset(Pitch_S,"AU04_c", maxr)
Judge1_AU4 <- Subset_dataset(Pitch_J1, "AU04_c", maxr)
Judge2_AU4 <- Subset_dataset(Pitch_J2, "AU04_c", maxr)
Judge3_AU4 <- Subset_dataset(Pitch_J3, "AU04_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU4)
sum(Student_AU4$AU04_c == 1)


summary(Judge1_AU4)
sum(Judge1_AU4$AU04_c == 1)

summary(Judge2_AU4)
sum(Judge2_AU4$AU04_c == 1)

summary(Judge3_AU4)
sum(Judge3_AU4$AU04_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU4 <- ts_m(Student_AU4$AU04_c)
Judge1_AU4 <- ts_m(Judge1_AU4$AU04_c)
Judge2_AU4 <- ts_m(Judge2_AU4$AU04_c)
Judge3_AU4 <- ts_m(Judge3_AU4$AU04_c)

## Plot timeseries: ------------------------------------------------------------


pdf("Timeseries Pitch 1_1 AU04.pdf")    
plot_ts(Student_AU4, Judge1_AU4, Judge2_AU4, Judge3_AU4)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU04_P_J1= run_crqa(Student_AU4, Judge1_AU4)
CRP_AU04_P_J2= run_crqa(Student_AU4, Judge2_AU4)
CRP_AU04_P_J3= run_crqa(Student_AU4, Judge3_AU4)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU04.pdf") 
Plot_CRP1(CRP_AU04_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU04.pdf") 
Plot_CRP2(CRP_AU04_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU04.pdf") 
Plot_CRP3(CRP_AU04_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------
RR_AU04_J1 <- run_DCRP(Student_AU4, Judge1_AU4, N)
RR_AU04_J2 <- run_DCRP(Student_AU4, Judge2_AU4, N) 
RR_AU04_J3 <- run_DCRP(Student_AU4, Judge3_AU4, N)

RR_AU04_P <- c(RR_AU04_J1[[1]], RR_AU04_J2[[1]], RR_AU04_J3[[1]])


## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU04_P <- RR_AU04_P




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU05----------------------------------------------------

#subset only AU05:

Student_AU5 <- Subset_dataset(Pitch_S,"AU05_c", maxr)
Judge1_AU5 <- Subset_dataset(Pitch_J1, "AU05_c", maxr)
Judge2_AU5 <- Subset_dataset(Pitch_J2, "AU05_c", maxr)
Judge3_AU5 <- Subset_dataset(Pitch_J3, "AU05_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU5)
sum(Student_AU5$AU05_c == 1)


summary(Judge1_AU5)
sum(Judge1_AU5$AU05_c == 1)

summary(Judge2_AU5)
sum(Judge2_AU5$AU05_c == 1)

summary(Judge3_AU5)
sum(Judge3_AU5$AU05_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU5 <- ts_m(Student_AU5$AU05_c)
Judge1_AU5 <- ts_m(Judge1_AU5$AU05_c)
Judge2_AU5 <- ts_m(Judge2_AU5$AU05_c)
Judge3_AU5 <- ts_m(Judge3_AU5$AU05_c)

## Plot timeseries: ------------------------------------------------------------


pdf("Timeseries Pitch 1_1 AU05.pdf")      
plot_ts(Student_AU5, Judge1_AU5, Judge2_AU5, Judge3_AU5)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU05_P_J1= run_crqa(Student_AU5, Judge1_AU5)
CRP_AU05_P_J2= run_crqa(Student_AU5, Judge2_AU5)
CRP_AU05_P_J3= run_crqa(Student_AU5, Judge3_AU5)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU05.pdf") 
Plot_CRP1(CRP_AU05_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU05.pdf") 
Plot_CRP2(CRP_AU05_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU05.pdf") 
Plot_CRP3(CRP_AU05_P_J3$RP)
dev.off()



## Calculate RR over Window of 25 frames ---------------------------------------
RR_AU05_J1 <- run_DCRP(Student_AU5, Judge1_AU5, N)
RR_AU05_J2 <- run_DCRP(Student_AU5, Judge2_AU5, N) 
RR_AU05_J3 <- run_DCRP(Student_AU5, Judge3_AU5, N)

RR_AU05_P <- c(RR_AU05_J1[[1]], RR_AU05_J2[[1]], RR_AU05_J3[[1]])


## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU05_P <- RR_AU05_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU06----------------------------------------------------

Student_AU6 <- Subset_dataset(Pitch_S,"AU06_c", maxr)
Judge1_AU6 <- Subset_dataset(Pitch_J1, "AU06_c", maxr)
Judge2_AU6 <- Subset_dataset(Pitch_J2, "AU06_c", maxr)
Judge3_AU6 <- Subset_dataset(Pitch_J3, "AU06_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU6)
sum(Student_AU6$AU06_c == 1)


summary(Judge1_AU6)
sum(Judge1_AU6$AU06_c == 1)

summary(Judge2_AU6)
sum(Judge2_AU6$AU06_c == 1)

summary(Judge3_AU6)
sum(Judge3_AU6$AU06_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU6 <- ts_m(Student_AU6$AU06_c)
Judge1_AU6 <- ts_m(Judge1_AU6$AU06_c)
Judge2_AU6 <- ts_m(Judge2_AU6$AU06_c)
Judge3_AU6 <- ts_m(Judge3_AU6$AU06_c)

## Plot timeseries: ------------------------------------------------------------

#Chgange timeseries to create a nicer plot: 


pdf("Timeseries Pitch 1_1 AU06.pdf")  
plot_ts(Student_AU6, Judge1_AU6, Judge2_AU6, Judge3_AU6)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU06_P_J1= run_crqa(Student_AU6, Judge1_AU6)
CRP_AU06_P_J2= run_crqa(Student_AU6, Judge2_AU6)
CRP_AU06_P_J3= run_crqa(Student_AU6, Judge3_AU6)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU06.pdf") 
Plot_CRP1(CRP_AU06_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU06.pdf") 
Plot_CRP2(CRP_AU06_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU06.pdf") 
Plot_CRP3(CRP_AU06_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU06_J1 <- run_DCRP(Student_AU6, Judge1_AU6, N)
RR_AU06_J2 <- run_DCRP(Student_AU6, Judge2_AU6, N)
RR_AU06_J3 <- run_DCRP(Student_AU6, Judge3_AU6, N)

RR_AU06_P <- c(RR_AU06_J1[[1]], RR_AU06_J2[[1]], RR_AU06_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU06_P <- RR_AU06_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU07----------------------------------------------------

Student_AU7 <- Subset_dataset(Pitch_S,"AU07_c", maxr)
Judge1_AU7 <- Subset_dataset(Pitch_J1, "AU07_c", maxr)
Judge2_AU7 <- Subset_dataset(Pitch_J2, "AU07_c", maxr)
Judge3_AU7 <- Subset_dataset(Pitch_J3, "AU07_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU7)
sum(Student_AU7$AU07_c == 1)


summary(Judge1_AU7)
sum(Judge1_AU7$AU07_c == 1)

summary(Judge2_AU7)
sum(Judge2_AU7$AU07_c == 1)

summary(Judge3_AU7)
sum(Judge3_AU7$AU07_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU7 <- ts_m(Student_AU7$AU07_c)
Judge1_AU7 <- ts_m(Judge1_AU7$AU07_c)
Judge2_AU7 <- ts_m(Judge2_AU7$AU07_c)
Judge3_AU7 <- ts_m(Judge3_AU7$AU07_c)

## Plot timeseries: ------------------------------------------------------------


pdf("Timeseries Pitch 1_1 AU07.pdf")     
plot_ts(Student_AU7, Judge1_AU7, Judge2_AU7, Judge3_AU7)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU07_P_J1= run_crqa(Student_AU7, Judge1_AU7)
CRP_AU07_P_J2= run_crqa(Student_AU7, Judge2_AU7)
CRP_AU07_P_J3= run_crqa(Student_AU7, Judge3_AU7)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU07.pdf") 
Plot_CRP1(CRP_AU07_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU07.pdf") 
Plot_CRP2(CRP_AU07_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU07.pdf") 
Plot_CRP3(CRP_AU07_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU07_J1 <- run_DCRP(Student_AU7, Judge1_AU7, N)
RR_AU07_J2 <- run_DCRP(Student_AU7, Judge2_AU7, N)
RR_AU07_J3 <- run_DCRP(Student_AU7, Judge3_AU7, N)

RR_AU07_P <- c(RR_AU07_J1[[1]], RR_AU07_J2[[1]], RR_AU07_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU07_P <- RR_AU07_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU09----------------------------------------------------

Student_AU9 <- Subset_dataset(Pitch_S,"AU09_c", maxr)
Judge1_AU9 <- Subset_dataset(Pitch_J1, "AU09_c", maxr)
Judge2_AU9 <- Subset_dataset(Pitch_J2, "AU09_c", maxr)
Judge3_AU9 <- Subset_dataset(Pitch_J3, "AU09_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU9)
sum(Student_AU9$AU09_c == 1)


summary(Judge1_AU9)
sum(Judge1_AU9$AU09_c == 1)

summary(Judge2_AU9)
sum(Judge2_AU9$AU09_c == 1)

summary(Judge3_AU9)
sum(Judge3_AU9$AU09_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU9 <- ts_m(Student_AU9$AU09_c)
Judge1_AU9 <- ts_m(Judge1_AU9$AU09_c)
Judge2_AU9 <- ts_m(Judge2_AU9$AU09_c)
Judge3_AU9 <- ts_m(Judge3_AU9$AU09_c)

## Plot timeseries: ------------------------------------------------------------


pdf("Timeseries Pitch 1_1 AU09.pdf")     
plot_ts(Student_AU9, Judge1_AU9, Judge2_AU9, Judge3_AU9)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU09_P_J1= run_crqa(Student_AU9, Judge1_AU9)
CRP_AU09_P_J2= run_crqa(Student_AU9, Judge2_AU9)
CRP_AU09_P_J3= run_crqa(Student_AU9, Judge3_AU9)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU09.pdf") 
Plot_CRP1(CRP_AU09_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU09.pdf") 
Plot_CRP2(CRP_AU09_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU09.pdf") 
Plot_CRP3(CRP_AU09_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU09_J1 <- run_DCRP(Student_AU9, Judge1_AU9, N)
RR_AU09_J2 <- run_DCRP(Student_AU9, Judge2_AU9, N)
RR_AU09_J3 <- run_DCRP(Student_AU9, Judge3_AU9, N)

RR_AU09_P <- c(RR_AU09_J1[[1]], RR_AU09_J2[[1]], RR_AU09_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU09_P <- RR_AU09_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU10----------------------------------------------------

#subset only AU10:

Student_AU10 <- Subset_dataset(Pitch_S,"AU10_c", maxr)
Judge1_AU10 <- Subset_dataset(Pitch_J1, "AU10_c", maxr)
Judge2_AU10 <- Subset_dataset(Pitch_J2, "AU10_c", maxr)
Judge3_AU10 <- Subset_dataset(Pitch_J3, "AU10_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU10)
sum(Student_AU10$AU10_c == 1)


summary(Judge1_AU10)
sum(Judge1_AU10$AU10_c == 1)

summary(Judge2_AU10)
sum(Judge2_AU10$AU10_c == 1)

summary(Judge3_AU10)
sum(Judge3_AU10$AU10_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU10 <- ts_m(Student_AU10$AU10_c)
Judge1_AU10 <- ts_m(Judge1_AU10$AU10_c)
Judge2_AU10 <- ts_m(Judge2_AU10$AU10_c)
Judge3_AU10 <- ts_m(Judge3_AU10$AU10_c)

## Plot timeseries: ------------------------------------------------------------

pdf("Timeseries Pitch 1_1 AU10.pdf")     
plot_ts(Student_AU10, Judge1_AU10, Judge2_AU10, Judge3_AU10)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU10_P_J1= run_crqa(Student_AU10, Judge1_AU10)
CRP_AU10_P_J2= run_crqa(Student_AU10, Judge2_AU10)
CRP_AU10_P_J3= run_crqa(Student_AU10, Judge3_AU10)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU10.pdf") 
Plot_CRP1(CRP_AU10_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU10.pdf") 
Plot_CRP2(CRP_AU10_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU10.pdf") 
Plot_CRP3(CRP_AU10_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU10_J1 <- run_DCRP(Student_AU10, Judge1_AU10, N)
RR_AU10_J2 <- run_DCRP(Student_AU10, Judge2_AU10, N)
RR_AU10_J3 <- run_DCRP(Student_AU10, Judge3_AU10, N)

RR_AU10_P <- c(RR_AU10_J1[[1]], RR_AU10_J2[[1]], RR_AU10_J3[[1]])
## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU10_P <- RR_AU10_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU12----------------------------------------------------

#subset only AU12:

Student_AU12 <- Subset_dataset(Pitch_S,"AU12_c", maxr)
Judge1_AU12 <- Subset_dataset(Pitch_J1, "AU12_c", maxr)
Judge2_AU12 <- Subset_dataset(Pitch_J2, "AU12_c", maxr)
Judge3_AU12 <- Subset_dataset(Pitch_J3, "AU12_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU12)
sum(Student_AU12$AU12_c == 1)


summary(Judge1_AU12)
sum(Judge1_AU12$AU12_c == 1)

summary(Judge2_AU12)
sum(Judge2_AU12$AU12_c == 1)

summary(Judge3_AU12)
sum(Judge3_AU12$AU12_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU12 <- ts_m(Student_AU12$AU12_c)
Judge1_AU12 <- ts_m(Judge1_AU12$AU12_c)
Judge2_AU12 <- ts_m(Judge2_AU12$AU12_c)
Judge3_AU12 <- ts_m(Judge3_AU12$AU12_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU12.pdf")     
plot_ts(Student_AU12, Judge1_AU12, Judge2_AU12, Judge3_AU12)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU12_P_J1= run_crqa(Student_AU12, Judge1_AU12)
CRP_AU12_P_J2= run_crqa(Student_AU12, Judge2_AU12)
CRP_AU12_P_J3= run_crqa(Student_AU12, Judge3_AU12)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU12.pdf") 
Plot_CRP1(CRP_AU12_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU12.pdf") 
Plot_CRP2(CRP_AU12_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU12.pdf") 
Plot_CRP3(CRP_AU12_P_J3$RP)
dev.off()



## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU12_J1 <- run_DCRP(Student_AU12, Judge1_AU12, N)
RR_AU12_J2 <- run_DCRP(Student_AU12, Judge2_AU12, N)
RR_AU12_J3 <- run_DCRP(Student_AU12, Judge3_AU12, N)

RR_AU12_P <-c(RR_AU12_J1[[1]], RR_AU12_J2[[1]], RR_AU12_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU12_P <-RR_AU12_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU14----------------------------------------------------

#subset only AU14:

Student_AU14 <- Subset_dataset(Pitch_S,"AU14_c", maxr)
Judge1_AU14 <- Subset_dataset(Pitch_J1, "AU14_c", maxr)
Judge2_AU14 <- Subset_dataset(Pitch_J2, "AU14_c", maxr)
Judge3_AU14 <- Subset_dataset(Pitch_J3, "AU14_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU14)
sum(Student_AU14$AU14_c == 1)

summary(Judge1_AU14)
sum(Judge1_AU14$AU14_c == 1)

summary(Judge2_AU14)
sum(Judge2_AU14$AU14_c == 1)

summary(Judge3_AU14)
sum(Judge3_AU14$AU14_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU14 <- ts_m(Student_AU14$AU14_c)
Judge1_AU14 <- ts_m(Judge1_AU14$AU14_c)
Judge2_AU14 <- ts_m(Judge2_AU14$AU14_c)
Judge3_AU14 <- ts_m(Judge3_AU14$AU14_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU14.pdf")     
plot_ts(Student_AU14, Judge1_AU14, Judge2_AU14, Judge3_AU14)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU14_P_J1= run_crqa(Student_AU14, Judge1_AU14)
CRP_AU14_P_J2= run_crqa(Student_AU14, Judge2_AU14)
CRP_AU14_P_J3= run_crqa(Student_AU14, Judge3_AU14)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU14.pdf") 
Plot_CRP1(CRP_AU14_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU14.pdf") 
Plot_CRP2(CRP_AU14_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU14.pdf") 
Plot_CRP3(CRP_AU14_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU14_J1 <- run_DCRP(Student_AU14, Judge1_AU14, N)
RR_AU14_J2 <- run_DCRP(Student_AU14, Judge2_AU14, N)
RR_AU14_J3 <- run_DCRP(Student_AU14, Judge3_AU14, N)

RR_AU14_P <- c(RR_AU14_J1[[1]], RR_AU14_J2[[1]], RR_AU14_J3[[1]])


## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU14_P <- RR_AU14_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU15----------------------------------------------------

#subset only AU15:

Student_AU15 <- Subset_dataset(Pitch_S,"AU15_c", maxr)
Judge1_AU15 <- Subset_dataset(Pitch_J1, "AU15_c", maxr)
Judge2_AU15 <- Subset_dataset(Pitch_J2, "AU15_c", maxr)
Judge3_AU15 <- Subset_dataset(Pitch_J3, "AU15_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU15)
sum(Student_AU15$AU15_c == 1)


summary(Judge1_AU15)
sum(Judge1_AU15$AU15_c == 1)

summary(Judge2_AU15)
sum(Judge2_AU15$AU15_c == 1)

summary(Judge3_AU15)
sum(Judge3_AU15$AU15_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU15 <- ts_m(Student_AU15$AU15_c)
Judge1_AU15 <- ts_m(Judge1_AU15$AU15_c)
Judge2_AU15 <- ts_m(Judge2_AU15$AU15_c)
Judge3_AU15 <- ts_m(Judge3_AU15$AU15_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU15.pdf")      
plot_ts(Student_AU15, Judge1_AU15, Judge2_AU15, Judge3_AU15)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU15_P_J1= run_crqa(Student_AU15, Judge1_AU15)
CRP_AU15_P_J2= run_crqa(Student_AU15, Judge2_AU15)
CRP_AU15_P_J3= run_crqa(Student_AU15, Judge3_AU15)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU15.pdf") 
Plot_CRP1(CRP_AU15_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU15.pdf") 
Plot_CRP2(CRP_AU15_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU15.pdf") 
Plot_CRP3(CRP_AU15_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU15_J1 <- run_DCRP(Student_AU15, Judge1_AU15, N)
RR_AU15_J2 <- run_DCRP(Student_AU15, Judge2_AU15, N)
RR_AU15_J3 <- run_DCRP(Student_AU15, Judge3_AU15, N)

RR_AU15_P <- c(RR_AU15_J1[[1]], RR_AU15_J2[[1]], RR_AU15_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU15_P <- RR_AU15_P



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU17----------------------------------------------------

#subset only AU17:

Student_AU17 <- Subset_dataset(Pitch_S,"AU17_c", maxr)
Judge1_AU17 <- Subset_dataset(Pitch_J1, "AU17_c", maxr)
Judge2_AU17 <- Subset_dataset(Pitch_J2, "AU17_c", maxr)
Judge3_AU17 <- Subset_dataset(Pitch_J3, "AU17_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU17)
sum(Student_AU17$AU17_c == 1)


summary(Judge1_AU17)
sum(Judge1_AU17$AU17_c == 1)

summary(Judge2_AU17)
sum(Judge2_AU17$AU17_c == 1)

summary(Judge3_AU17)
sum(Judge3_AU17$AU17_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU17 <- ts_m(Student_AU17$AU17_c)
Judge1_AU17 <- ts_m(Judge1_AU17$AU17_c)
Judge2_AU17 <- ts_m(Judge2_AU17$AU17_c)
Judge3_AU17 <- ts_m(Judge3_AU17$AU17_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU17.pdf")     
plot_ts(Student_AU17, Judge1_AU17, Judge2_AU17, Judge3_AU17)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU17_P_J1= run_crqa(Student_AU17, Judge1_AU17)
CRP_AU17_P_J2= run_crqa(Student_AU17, Judge2_AU17)
CRP_AU17_P_J3= run_crqa(Student_AU17, Judge3_AU17)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU17.pdf") 
Plot_CRP1(CRP_AU17_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU17.pdf") 
Plot_CRP2(CRP_AU17_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU17.pdf") 
Plot_CRP3(CRP_AU17_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU17_J1 <- run_DCRP(Student_AU17, Judge1_AU17, N)
RR_AU17_J2 <- run_DCRP(Student_AU17, Judge2_AU17, N)
RR_AU17_J3 <- run_DCRP(Student_AU17, Judge3_AU17, N)

RR_AU17_P <- c(RR_AU17_J1[[1]], RR_AU17_J2[[1]], RR_AU17_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU17_P <- RR_AU17_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU20----------------------------------------------------

#subset only AU20:

Student_AU20 <- Subset_dataset(Pitch_S,"AU20_c", maxr)
Judge1_AU20 <- Subset_dataset(Pitch_J1, "AU20_c", maxr)
Judge2_AU20 <- Subset_dataset(Pitch_J2, "AU20_c", maxr)
Judge3_AU20 <- Subset_dataset(Pitch_J3, "AU20_c", maxr)




## EDA of new datasets :--------------------------------------------------------

summary(Student_AU20)
sum(Student_AU20$AU20_c == 1)


summary(Judge1_AU20)
sum(Judge1_AU20$AU20_c == 1)

summary(Judge2_AU20)
sum(Judge2_AU20$AU20_c == 1)

summary(Judge3_AU20)
sum(Judge3_AU20$AU20_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU20 <- ts_m(Student_AU20$AU20_c)
Judge1_AU20 <- ts_m(Judge1_AU20$AU20_c)
Judge2_AU20 <- ts_m(Judge2_AU20$AU20_c)
Judge3_AU20 <- ts_m(Judge3_AU20$AU20_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU20.pdf")     
plot_ts(Student_AU20, Judge1_AU20, Judge2_AU20, Judge3_AU20)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU20_P_J1= run_crqa(Student_AU20, Judge1_AU20)
CRP_AU20_P_J2= run_crqa(Student_AU20, Judge2_AU20)
CRP_AU20_P_J3= run_crqa(Student_AU20, Judge3_AU20)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU20.pdf") 
Plot_CRP1(CRP_AU20_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU20.pdf") 
Plot_CRP2(CRP_AU20_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU20.pdf") 
Plot_CRP3(CRP_AU20_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU20_J1 <- run_DCRP(Student_AU20, Judge1_AU20, N)
RR_AU20_J2 <- run_DCRP(Student_AU20, Judge2_AU20, N)
RR_AU20_J3 <- run_DCRP(Student_AU20, Judge3_AU20, N)

RR_AU20_P <- c(RR_AU20_J1[[1]], RR_AU20_J2[[1]], RR_AU20_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU20_P <- RR_AU20_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU23----------------------------------------------------

#subset only AU23:

Student_AU23 <- Subset_dataset(Pitch_S,"AU23_c", maxr)
Judge1_AU23 <- Subset_dataset(Pitch_J1, "AU23_c", maxr)
Judge2_AU23 <- Subset_dataset(Pitch_J2, "AU23_c", maxr)
Judge3_AU23 <- Subset_dataset(Pitch_J3, "AU23_c", maxr)


## EDA of new datasets :--------------------------------------------------------

summary(Student_AU23)
sum(Student_AU23$AU23_c == 1)


summary(Judge1_AU23)
sum(Judge1_AU23$AU23_c == 1)

summary(Judge2_AU23)
sum(Judge2_AU23$AU23_c == 1)

summary(Judge3_AU23)
sum(Judge3_AU23$AU23_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU23 <- ts_m(Student_AU23$AU23_c)
Judge1_AU23 <- ts_m(Judge1_AU23$AU23_c)
Judge2_AU23 <- ts_m(Judge2_AU23$AU23_c)
Judge3_AU23 <- ts_m(Judge3_AU23$AU23_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU23.pdf")     
plot_ts(Student_AU23, Judge1_AU23, Judge2_AU23, Judge3_AU23)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU23_P_J1= run_crqa(Student_AU23, Judge1_AU23)
CRP_AU23_P_J2= run_crqa(Student_AU23, Judge2_AU23)
CRP_AU23_P_J3= run_crqa(Student_AU23, Judge3_AU23)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU23.pdf") 
Plot_CRP1(CRP_AU23_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU23.pdf") 
Plot_CRP2(CRP_AU23_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU23.pdf") 
Plot_CRP3(CRP_AU23_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU23_J1 <- run_DCRP(Student_AU23, Judge1_AU23, N)
RR_AU23_J2 <- run_DCRP(Student_AU23, Judge2_AU23, N)
RR_AU23_J3 <- run_DCRP(Student_AU23, Judge3_AU23, N)

RR_AU23_P <- c(RR_AU23_J1[[1]], RR_AU23_J2[[1]], RR_AU23_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU23_P <- RR_AU23_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU25----------------------------------------------------

#subset only AU25:

Student_AU25 <- Subset_dataset(Pitch_S,"AU25_c", maxr)
Judge1_AU25 <- Subset_dataset(Pitch_J1, "AU25_c", maxr)
Judge2_AU25 <- Subset_dataset(Pitch_J2, "AU25_c", maxr)
Judge3_AU25 <- Subset_dataset(Pitch_J3, "AU25_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU25)
sum(Student_AU25$AU25_c == 1)


summary(Judge1_AU25)
sum(Judge1_AU25$AU25_c == 1)

summary(Judge2_AU25)
sum(Judge2_AU25$AU25_c == 1)

summary(Judge3_AU25)
sum(Judge3_AU25$AU25_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU25 <- ts_m(Student_AU25$AU25_c)
Judge1_AU25 <- ts_m(Judge1_AU25$AU25_c)
Judge2_AU25 <- ts_m(Judge2_AU25$AU25_c)
Judge3_AU25 <- ts_m(Judge3_AU25$AU25_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU25.pdf")      
plot_ts(Student_AU25, Judge1_AU25, Judge2_AU25, Judge3_AU25)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU25_P_J1= run_crqa(Student_AU25, Judge1_AU25)
CRP_AU25_P_J2= run_crqa(Student_AU25, Judge2_AU25)
CRP_AU25_P_J3= run_crqa(Student_AU25, Judge3_AU25)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU25.pdf") 
Plot_CRP1(CRP_AU25_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU25.pdf") 
Plot_CRP2(CRP_AU25_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU25.pdf") 
Plot_CRP3(CRP_AU25_P_J3$RP)
dev.off()



## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU25_J1 <- run_DCRP(Student_AU25, Judge1_AU25, N)
RR_AU25_J2 <- run_DCRP(Student_AU25, Judge2_AU25, N)
RR_AU25_J3 <- run_DCRP(Student_AU25, Judge3_AU25, N)

RR_AU25_P <- c(RR_AU25_J1[[1]], RR_AU25_J2[[1]], RR_AU25_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU25_P <- RR_AU25_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU26----------------------------------------------------

#subset only AU26:

Student_AU26 <- Subset_dataset(Pitch_S,"AU26_c", maxr)
Judge1_AU26 <- Subset_dataset(Pitch_J1, "AU26_c", maxr)
Judge2_AU26 <- Subset_dataset(Pitch_J2, "AU26_c", maxr)
Judge3_AU26 <- Subset_dataset(Pitch_J3, "AU26_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU26)
sum(Student_AU26$AU26_c == 1)


summary(Judge1_AU26)
sum(Judge1_AU26$AU26_c == 1)

summary(Judge2_AU26)
sum(Judge2_AU26$AU26_c == 1)

summary(Judge3_AU26)
sum(Judge3_AU26$AU26_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU26 <- ts_m(Student_AU26$AU26_c)
Judge1_AU26 <- ts_m(Judge1_AU26$AU26_c)
Judge2_AU26 <- ts_m(Judge2_AU26$AU26_c)
Judge3_AU26 <- ts_m(Judge3_AU26$AU26_c)

## Plot timeseries: ------------------------------------------------------------



pdf("Timeseries Pitch 1_1 AU26.pdf")      
plot_ts(Student_AU26, Judge1_AU26, Judge2_AU26, Judge3_AU26)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU26_P_J1= run_crqa(Student_AU26, Judge1_AU26)
CRP_AU26_P_J2= run_crqa(Student_AU26, Judge2_AU26)
CRP_AU26_P_J3= run_crqa(Student_AU26, Judge3_AU26)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU26.pdf") 
Plot_CRP1(CRP_AU26_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU26.pdf") 
Plot_CRP2(CRP_AU26_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU26.pdf") 
Plot_CRP3(CRP_AU26_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU26_J1 <- run_DCRP(Student_AU26, Judge1_AU26, N)
RR_AU26_J2 <- run_DCRP(Student_AU26, Judge2_AU26, N)
RR_AU26_J3 <- run_DCRP(Student_AU26, Judge3_AU26, N)

RR_AU26_P <- c(RR_AU26_J1[[1]], RR_AU26_J2[[1]], RR_AU26_J3[[1]])
  
## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU26_P <- RR_AU26_P

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##----------------------AU45----------------------------------------------------

#subset only AU45:

Student_AU45 <- Subset_dataset(Pitch_S,"AU45_c", maxr)
Judge1_AU45 <- Subset_dataset(Pitch_J1, "AU45_c", maxr)
Judge2_AU45 <- Subset_dataset(Pitch_J2, "AU45_c", maxr)
Judge3_AU45 <- Subset_dataset(Pitch_J3, "AU45_c", maxr)



## EDA of new datasets :--------------------------------------------------------

summary(Student_AU45)
sum(Student_AU45$AU45_c == 1)


summary(Judge1_AU45)
sum(Judge1_AU45$AU45_c == 1)

summary(Judge2_AU45)
sum(Judge2_AU45$AU45_c == 1)

summary(Judge3_AU45)
sum(Judge3_AU45$AU45_c == 1)

#create timeseries: ------------------------------------------------------------

Student_AU45 <- ts_m(Student_AU45$AU45_c)
Judge1_AU45 <- ts_m(Judge1_AU45$AU45_c)
Judge2_AU45 <- ts_m(Judge2_AU45$AU45_c)
Judge3_AU45 <- ts_m(Judge3_AU45$AU45_c)

## Plot timeseries: ------------------------------------------------------------

pdf("Timeseries Pitch 1_1 AU45.pdf")     
plot_ts(Student_AU45, Judge1_AU45, Judge2_AU45, Judge3_AU45)
dev.off()

## RUN CRQA (for plots)---------------------------------------------------------

CRP_AU45_P_J1= run_crqa(Student_AU45, Judge1_AU45)
CRP_AU45_P_J2= run_crqa(Student_AU45, Judge2_AU45)
CRP_AU45_P_J3= run_crqa(Student_AU45, Judge3_AU45)

### Plot CRP's  ------------------------------------------------------------------

pdf("CRP Student and Judge 1 Pitch 1_1 AU45.pdf") 
Plot_CRP1(CRP_AU45_P_J1$RP)
dev.off()

pdf("CRP Student and Judge 2 Pitch 1_1 AU45.pdf") 
Plot_CRP2(CRP_AU45_P_J2$RP)
dev.off()

pdf("CRP Student and Judge 3 Pitch 1_1 AU45.pdf") 
Plot_CRP3(CRP_AU45_P_J3$RP)
dev.off()


## Calculate RR over Window of 25 frames ---------------------------------------

RR_AU45_J1 <- run_DCRP(Student_AU45, Judge1_AU45, N)
RR_AU45_J2 <- run_DCRP(Student_AU45, Judge2_AU45, N)
RR_AU45_J3 <- run_DCRP(Student_AU45, Judge3_AU45, N)

RR_AU45_P <- c(RR_AU45_J1[[1]], RR_AU45_J2[[1]], RR_AU45_J3[[1]])

## put in dataframe of Pitch 1_1 -------------------------------------------------

df_pitch1$RR_AU45_P <- RR_AU45_P


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
###Save the DF in map on the computer: ----------------------------------------- 


getwd()
write.csv(df_pitch1, file = "~/Documents/Lotte/M-Data science/Thesis/Data /df pitches/Pitch1_1.csv" ,
          row.names = FALSE )
