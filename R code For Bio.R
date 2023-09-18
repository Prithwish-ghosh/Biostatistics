library(readr)
library(readxl)
setwd("~/Downloads/Bio Informatics")
Gujrat = read_csv("Gujrat.csv")
UP = read_csv("UP.csv")
WB = read_csv("WB.csv")
Maharashtra = read_csv("Mha muts datewise.csv")
Kerala = read_csv("Kerala.csv")
Karnataka = read_csv("Karnata.csv")
#Karnataka
Karnataka = Karnataka[,-c(1526:1528)]
Punjab = read_csv("Punjab.csv")
TamilNadu = read_csv("TamilNadu.csv")
dim(WB)
dim(Gujrat)
dim(UP)
Maharashtra = data.frame(Maharashtra)

head(Maharashtra)

dim(Maharashtra)
dim(Punjab)
dim(TamilNadu)
dim(Karnataka)
dim(Kerala)
dim(Kerala)
tail(Punjab)

tail(data.frame(Punjab))

options(max.print = 10000)  # Increase the maximum number of printed lines
options(width = 120) 

########################################## Kerala ####################################################

for (i in 1:386) {
  kerala_freq_Mut = Kerala[,i + 6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                      Maharashtra[,i+3] + 
                                      Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                      TamilNadu[i+6])
}

for (i in 1:386) {
  kerala_freq_Mut[[i]] = Kerala[,i + 6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                      Maharashtra[,i+3] + 
                                      Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                      TamilNadu[i+6])
}

kerala_freq_Mut[[1]]

kerala_freqency = data.frame(kerala_freq_Mut[c(1:386)]*100)
num_columns <- ncol(kerala_freqency)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(kerala_freqency) <- new_column_names

head(kerala_freqency)


kerala_final_D = data.frame(Kerala[,c(7:392)] , kerala_freqency)
head(kerala_final_D)
dim(kerala_final_D)
g = glm(Kerala$`Daily Cases`~ . , data = kerala_final_D , family = poisson)
summary(g)
glm_kerala = capture.output(summary(g))
# Write the model summary to a text file
writeLines(glm_kerala, "kerala_summary.txt")

head(kerala_final_D)
g$coefficients
g1 = glm(Kerala$`Daily Deaths`~ . , data = kerala_final_D , family = poisson)
summary(g1)
library(MASS)
glm_kerala_D = capture.output(summary(g1))
# Write the model summary to a text file
writeLines(glm_kerala_D, "kerala_summary_death.txt")
gg = glm.nb(Kerala$`Daily Cases`~ . , data = kerala_final_D )


############################################## Punjab ################################################


for(i in 1:386){
  Punjab_Freq_Mut = Punjab[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                    Maharashtra[,i+3] + 
                                    Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                    TamilNadu[i+6])
    
}

for(i in 1:386){
  Punjab_Freq_Mut[[i]] = Punjab[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                    Maharashtra[,i+3] + 
                                    Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                    TamilNadu[i+6])
  
}



for (i in 1:81) {
  Punjab_Freq_Mut1 = Punjab[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                            Maharashtra[,i+389] + 
                                            Punjab[,i+389]+ Karnataka[,i+389] + 
                                            TamilNadu[i+392])
}



for (i in 1:81) {
  Punjab_Freq_Mut1[[i]] = Punjab[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                        Maharashtra[,i+389] + 
                                        Punjab[,i+389]+ Karnataka[,i+389] + 
                                          TamilNadu[i+392])
}

Punjab_freq1 = data.frame(Punjab_Freq_Mut[,c(1:386)]*100 )

Punjab_freq2 =data.frame(Punjab_Freq_Mut1[,c(1:81)]*100)
Punjab_freq = data.frame(Punjab_freq1 , Punjab_freq2)



# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(Punjab_freq) <- new_column_names



Punjab_freq_final = data.frame(Punjab[,4:470]  , Punjab_freq)

summary(Punjab_freq_final)
sum(is.na(Punjab_freq_final))

for (col in colnames(Punjab_freq_final)) {
  col_mean <- mean(Punjab_freq_final[[col]], na.rm = TRUE)
  Punjab_freq_final[[col]][is.na(Punjab_freq_final[[col]])] <- col_mean
}


h = glm(Punjab$`Daily Cases`~. , data = Punjab_freq_final , family = poisson)
summary(h)

glm_Punjab = capture.output(summary(h))
# Write the model summary to a text file
writeLines(glm_Punjab, "Punjab_summary.txt")

UP$`Daily Deaths` = abs(UP$`Daily Deaths`)
Punjab$`Daily Deaths` = abs(Punjab$`Daily Deaths`)

length(Punjab$`Daily Deaths`)
dim(Punjab_freq_final)

summary(Punjab$`Daily Deaths`)

Punjab = data.frame(Punjab)
Punjab$Daily.Deaths =  as.numeric(Punjab$Daily.Deaths)



h1 = glm(Punjab$Daily.Deaths ~. , data = Punjab_freq_final , family = poisson)
summary(h1)

glm_Punjab_Death = capture.output(summary(h1))
# Write the model summary to a text file
writeLines(glm_Punjab_Death, "Punjab_summary_Death.txt")

############################################## UP ###################################################

for (i in 1:386) {
  UP_Freq_Mut = UP[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                 Maharashtra[,i+3] + 
                                 Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                 TamilNadu[i+6])
}

for (i in 1:386) {
  UP_Freq_Mut[[i]] = UP[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                        Maharashtra[,i+3] + 
                                        Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                        TamilNadu[i+6])
}


for (i in 1:81) {
  UP1 = UP[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                       Maharashtra[,i+389] + 
                                       Punjab[,i+389]+ Karnataka[,i+389] + 
                                       TamilNadu[i+392])
}

for (i in 1:81) {
  UP1[[i]] = UP[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                            Maharashtra[,i+389] + 
                                            Punjab[,i+389]+ Karnataka[,i+389] + 
                                            TamilNadu[i+392])
}

for (i in 1:118) {
  UP2 = UP[,i+473]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                           Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  UP2[[i]] = UP[,i+473]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                           Maharashtra[,i+470] +  Karnataka[,i+470] + 
                           TamilNadu[i+473])
}
UP_freq1 = data.frame(UP_Freq_Mut[,c(1:386)]*100)
UP_freq2 = data.frame(UP1[,c(1:81)]*100)
UP_freq3 = data.frame(UP2[,c(1:118)]*100)


UP_Freq  = data.frame(UP_freq1 , UP_freq2 , UP_freq3)
head(UP_Freq)
# Get the number of columns in the data frame
num_columns <- ncol(UP_Freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(UP_Freq) <- new_column_names

# Print the modified data frame

UP_Freq_final = na.omit(data.frame(UP$`Daily Cases`, UP$`Daily Deaths`,UP[,7:591] , UP_Freq))
UP_Freqq = UP_Freq_final[,3:591]


u1 = glm(UP_Freq_final$UP..Daily.Cases.~., data = UP_Freqq , family = poisson)
summary(u1)

glm_UP = capture.output(summary(u1))
# Write the model summary to a text file
writeLines(glm_UP, "UP_summary.txt")

u = glm(UP_Freq_final$UP..Daily.Deaths.~., data = UP_Freqq , family = poisson)
summary(u)

glm_UP_D = capture.output(summary(u))
# Write the model summary to a text file
writeLines(glm_UP_D, "UP_summary_Death.txt")

############################################# Tamil Nadu ##############################################

for (i in 1:386) {
  TM_Freq_Mut = TamilNadu[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                            Maharashtra[,i+3] + 
                            Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                            TamilNadu[i+6])
}

for (i in 1:386) {
  TM_Freq_Mut[[i]] = TamilNadu[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                 Maharashtra[,i+3] + 
                                 Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                 TamilNadu[i+6])
}


for (i in 1:81) {
  TM1 = TamilNadu[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                      Maharashtra[,i+389] + 
                      Punjab[,i+389]+ Karnataka[,i+389] + 
                      TamilNadu[i+392])
}

for (i in 1:81) {
  TM1[[i]] = TamilNadu[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                           Maharashtra[,i+389] + 
                           Punjab[,i+389]+ Karnataka[,i+389] + 
                           TamilNadu[i+392])
}

for (i in 1:118) {
  TM2 = TamilNadu[,i+473]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                      Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  TM2[[i]] = TamilNadu[,i+473]/(Gujrat[,i+473] + WB[, i+470] + 
                           Maharashtra[,i+470] +  Karnataka[,i+470] + 
                           TamilNadu[i+473])
}

for (i in 1:302) {
  TM3 = TamilNadu[,i+591]/(Gujrat[,i+591] + WB[, i+588] + 
                             Maharashtra[,i+588] +  Karnataka[,i+588] + TamilNadu[i+591])
}

for (i in 1:302) {
  TM3[[i]] = TamilNadu[,i+591]/(Gujrat[,i+473] + WB[, i+588] + 
                                  Maharashtra[,i+588] +  Karnataka[,i+588] + 
                                  TamilNadu[i+591])
}


TM_freq1 = data.frame(TM_Freq_Mut[,c(1:386)]*100)
TM_freq2 = data.frame(TM1[,c(1:81)]*100)
TM_freq3 = data.frame(TM2[,c(1:118)]*100)
TM_freq4 = data.frame(TM3[,c(1:302)]*100)

TM_freq = data.frame(TM_freq1 , TM_freq2 , TM_freq3 , TM_freq4)
sum(is.na(TM_freq))

# Perform mean imputation
for (col in colnames(TM_freq)) {
  col_mean <- mean(TM_freq[[col]], na.rm = TRUE)
  TM_freq[[col]][is.na(TM_freq[[col]])] <- col_mean
}

num_columns <- ncol(TM_freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(TM_freq) <- new_column_names


TM_freq_final = data.frame(TamilNadu[,7:893], TM_freq)
y = glm(TamilNadu$`Daily Cases` ~ ., data = TM_freq_final , family = poisson)
summary(y)

glm_TN = capture.output(summary(y))
# Write the model summary to a text file
writeLines(glm_TN, "TN_summary.txt")

y1 = glm(TamilNadu$`Daily Deaths` ~ ., data = TM_freq_final , family = poisson)
summary(y1)

glm_TN_D = capture.output(summary(y1))
# Write the model summary to a text file
writeLines(glm_TN_D, "TN_summary_Death.txt")

################################## Maharashtra #####################

for (i in 1:386) {
  MH_Freq_Mut = Maharashtra[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                   Maharashtra[,i+3] + 
                                   Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                   TamilNadu[i+6])
}

for (i in 1:386) {
  MH_Freq_Mut[[i]] = Maharashtra[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                        Maharashtra[,i+3] + 
                                        Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                        TamilNadu[i+6])
}


for (i in 1:81) {
  MH1 = Maharashtra[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                             Maharashtra[,i+389] + 
                             Punjab[,i+389]+ Karnataka[,i+389] + 
                             TamilNadu[i+392])
}

for (i in 1:81) {
  MH1[[i]] = Maharashtra[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                  Maharashtra[,i+389] + 
                                  Punjab[,i+389]+ Karnataka[,i+389] + 
                                  TamilNadu[i+392])
}

for (i in 1:118) {
  MH2 = Maharashtra[,i+470]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                             Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  MH2[[i]] = Maharashtra[,i+470]/(Gujrat[,i+473] + WB[, i+470] + 
                                  Maharashtra[,i+470] +  Karnataka[,i+470] + 
                                  TamilNadu[i+473])
}

for (i in 1:302) {
  MH3 = Maharashtra[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                             Maharashtra[,i+588] +  Karnataka[,i+588] + TamilNadu[i+591])
}

for (i in 1:302) {
  MH3[[i]] = Maharashtra[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                                  Maharashtra[,i+588] +  Karnataka[,i+588] + 
                                  TamilNadu[i+591])
}


for (i in 1:110) {
  MH4 = Maharashtra[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                           Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:110) {
  MH4[[i]] = Maharashtra[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                      Maharashtra[,i+890] +  Karnataka[,i+890])
}

MH_freq1 = data.frame(MH_Freq_Mut[,c(1:386)]*100)
MH_freq2 = data.frame(MH1[,c(1:81)]*100)
MH_freq3 = data.frame(MH2[,c(1:118)]*100)
MH_freq4 = data.frame(MH3[,c(1:302)]*100)
MH_freq5 = data.frame(MH4[,c(1:110)]*100)

MH_freq = data.frame(MH_freq1 , MH_freq2 , MH_freq3 , MH_freq4, MH_freq5)
sum(is.na(MH_freq))

for (col in colnames(MH_freq)) {
  col_mean <- mean(MH_freq[[col]], na.rm = TRUE)
  MH_freq[[col]][is.na(MH_freq[[col]])] <- col_mean
}

num_columns <- ncol(MH_freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(MH_freq) <- new_column_names
head(MH_freq)
MH_freq_final = data.frame(Maharashtra[,4:1000], MH_freq)

t = glm(Maharashtra$Daily.Cases ~., data = MH_freq_final , family = poisson)
summary(t)

glm_MH = capture.output(summary(t))
# Write the model summary to a text file
writeLines(glm_MH, "MH_summary.txt")

t1 = glm(Maharashtra$Daily.Deaths~., data = MH_freq_final , family = poisson)
summary(t1)

glm_MH_D = capture.output(summary(t1))
# Write the model summary to a text file
writeLines(glm_MH_D, "MH_summary_Death.txt")


############################## West Bengal ####################

for (i in 1:386) {
  WB_Freq_Mut = WB[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                     Maharashtra[,i+3] + 
                                     Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                     TamilNadu[i+6])
}

for (i in 1:386) {
  WB_Freq_Mut[[i]] = WB[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                          Maharashtra[,i+3] + 
                                          Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                          TamilNadu[i+6])
}


for (i in 1:81) {
  WB1 = WB[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                               Maharashtra[,i+389] + 
                               Punjab[,i+389]+ Karnataka[,i+389] + 
                               TamilNadu[i+392])
}

for (i in 1:81) {
  WB1[[i]] = WB[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                                    Maharashtra[,i+389] + 
                                    Punjab[,i+389]+ Karnataka[,i+389] + 
                                    TamilNadu[i+392])
}

for (i in 1:118) {
  WB2 = WB[,i+470]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                               Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  WB2[[i]] = WB[,i+470]/(Gujrat[,i+473] + WB[, i+470] + 
                                    Maharashtra[,i+470] +  Karnataka[,i+470] + 
                                    TamilNadu[i+473])
}

for (i in 1:302) {
  WB3 = WB[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                               Maharashtra[,i+588] +  Karnataka[,i+588] + TamilNadu[i+591])
}

for (i in 1:302) {
  WB3[[i]] = WB[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                                    Maharashtra[,i+588] +  Karnataka[,i+588] + 
                                    TamilNadu[i+591])
}


for (i in 1:110) {
  WB4 = WB[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                               Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:110) {
  WB4[[i]] = WB[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                                    Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:391) {
  WB5 = WB[,i+1000]/(Gujrat[,i+1003] + WB[, i+1000] + 
                       Karnataka[,i+1000])
}

for (i in 1:391) {
  WB5[[i]] = WB[,i+1000]/(Gujrat[,i+1003] + WB[, i+1000] + 
                           Karnataka[,i+1000])
}


WB_freq1 = data.frame(WB_Freq_Mut[,c(1:386)]*100)
WB_freq2 = data.frame(WB1[,c(1:81)]*100)
WB_freq3 = data.frame(WB2[,c(1:118)]*100)
WB_freq4 = data.frame(WB3[,c(1:302)]*100)
WB_freq5 = data.frame(WB4[,c(1:110)]*100)
WB_freq6 = data.frame(WB5[,c(1:391)]*100)

WB_freq = data.frame(WB_freq1 , WB_freq2 , WB_freq3 , WB_freq4, WB_freq5 , WB_freq6)
sum(is.na(WB_freq))

for (col in colnames(WB_freq)) {
  col_mean <- mean(WB_freq[[col]], na.rm = TRUE)
  WB_freq[[col]][is.na(WB_freq[[col]])] <- col_mean
}

num_columns <- ncol(WB_freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(WB_freq) <- new_column_names
head(WB_freq)
WB_freq_final = data.frame(WB[,4:1000], WB_freq)

o = glm(WB$`Daily Cases` ~., data = WB_freq_final , family = poisson)
summary(o)

glm_WB = capture.output(summary(o))
# Write the model summary to a text file
writeLines(glm_WB, "WB_summary.txt")

o1 = glm(WB$`Daily Deaths`~., data = WB_freq_final , family = poisson)
summary(o1)

glm_WB_D = capture.output(summary(o1))
# Write the model summary to a text file
writeLines(glm_WB_D, "WB_summary_Death.txt")


############################################## Gujrat ##############################################
dim(Gujrat)

for (i in 1:386) {
  GJ_Freq_Mut = Gujrat[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                            Maharashtra[,i+3] + 
                            Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                            TamilNadu[i+6])
}

for (i in 1:386) {
  GJ_Freq_Mut[[i]] = Gujrat[,i+6]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                 Maharashtra[,i+3] + 
                                 Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                 TamilNadu[i+6])
}


for (i in 1:81) {
  GJ1 = Gujrat[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                      Maharashtra[,i+389] + 
                      Punjab[,i+389]+ Karnataka[,i+389] + 
                      TamilNadu[i+392])
}

for (i in 1:81) {
  GJ1[[i]] = Gujrat[,i+392]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                           Maharashtra[,i+389] + 
                           Punjab[,i+389]+ Karnataka[,i+389] + 
                           TamilNadu[i+392])
}

for (i in 1:118) {
  GJ2 = Gujrat[,i+473]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                      Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  GJ2[[i]] = Gujrat[,i+473]/(Gujrat[,i+473] + WB[, i+470] + 
                           Maharashtra[,i+470] +  Karnataka[,i+470] + 
                           TamilNadu[i+473])
}

for (i in 1:302) {
  GJ3 = Gujrat[,i+591]/(Gujrat[,i+591] + WB[, i+588] + 
                      Maharashtra[,i+588] +  Karnataka[,i+588] + TamilNadu[i+591])
}

for (i in 1:302) {
  GJ3[[i]] = Gujrat[,i+591]/(Gujrat[,i+591] + WB[, i+588] + 
                           Maharashtra[,i+588] +  Karnataka[,i+588] + 
                           TamilNadu[i+591])
}


for (i in 1:110) {
  GJ4 = Gujrat[,i+893]/(Gujrat[,i+893] + WB[, i+890] + 
                      Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:110) {
  GJ4[[i]] = Gujrat[,i+893]/(Gujrat[,i+893] + WB[, i+890] + 
                           Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:391) {
  GJ5 = Gujrat[,i+1003]/(Gujrat[,i+1003] + WB[, i+1000] + 
                       Karnataka[,i+1000])
}

for (i in 1:391) {
  GJ5[[i]] = Gujrat[,i+1003]/(Gujrat[,i+1003] + WB[, i+1000] + 
                            Karnataka[,i+1000])
}


for (i in 1:49) {
  GJ6 = Gujrat[,i+1394]/(Gujrat[,i+1394] + 
                           Karnataka[,i+1391])
}

for (i in 1:49) {
  GJ6[[i]] = Gujrat[,i+1394]/(Gujrat[,i+1394]+  Karnataka[,i+1391])
}

GJ_freq1 = data.frame(GJ_Freq_Mut[,c(1:386)]*100)
GJ_freq2 = data.frame(GJ1[,c(1:81)]*100)
GJ_freq3 = data.frame(GJ2[,c(1:118)]*100)
GJ_freq4 = data.frame(GJ3[,c(1:302)]*100)
GJ_freq5 = data.frame(GJ4[,c(1:110)]*100)
GJ_freq6 = data.frame(GJ5[,c(1:391)]*100)
GJ_freq7 = data.frame(GJ6[,c(1:49)]*100)

GJ_freq = data.frame(GJ_freq1 , GJ_freq2 , GJ_freq3 , GJ_freq4, GJ_freq5, GJ_freq6, GJ_freq7)
sum(is.na(GJ_freq))


# Perform mean imputation
for (col in colnames(GJ_freq)) {
  col_mean <- mean(GJ_freq[[col]], na.rm = TRUE)
  GJ_freq[[col]][is.na(GJ_freq[[col]])] <- col_mean
}

num_columns <- ncol(GJ_freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(GJ_freq) <- new_column_names
head(GJ_freq)
GJ_freq_final = data.frame(Gujrat[,7:1443], GJ_freq)

q = glm(Gujrat$`Daily Cases`~., data = GJ_freq_final , family = poisson)
summary(q)

glm_GJ = capture.output(summary(q))
# Write the model summary to a text file
writeLines(glm_GJ, "GJ_summary.txt")

sum(is.na(Gujrat$`Daily deaths`))
summary(GJ_freq_final)
dim(Gujrat)
sum(is.finite(GJ_freq_final$Mut.1))

q1 = glm(Gujrat$`Daily deaths`~., data = GJ_freq_final , family = poisson)
summary(q1)

glm_GJ_D = capture.output(summary(q1))
# Write the model summary to a text file
writeLines(glm_GJ_D, "GJ_summary_Death.txt")

########################################### Karnataka ###############################################

for (i in 1:386) {
  KR_Freq_Mut = Karnataka[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                Maharashtra[,i+3] + 
                                Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                TamilNadu[i+6])
}

for (i in 1:386) {
  KR_Freq_Mut[[i]] = Karnataka[,i+3]/(Gujrat[,i+6] + WB[, i+3] + UP[,i+6] + 
                                     Maharashtra[,i+3] + 
                                     Punjab[,i+3] + Kerala[,i+6]  + Karnataka[,i+3] + 
                                     TamilNadu[i+6])
}


for (i in 1:81) {
  KR1 = Karnataka[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                          Maharashtra[,i+389] + 
                          Punjab[,i+389]+ Karnataka[,i+389] + 
                          TamilNadu[i+392])
}

for (i in 1:81) {
  KR1[[i]] = Karnataka[,i+389]/(Gujrat[,i+392] + WB[, i+389] + UP[,i+392] + 
                               Maharashtra[,i+389] + 
                               Punjab[,i+389]+ Karnataka[,i+389] + 
                               TamilNadu[i+392])
}

for (i in 1:118) {
  KR2 = Karnataka[,i+470]/(Gujrat[,i+473] + WB[, i+470] + UP[,i+473] + 
                          Maharashtra[,i+470] +  Karnataka[,i+470] + TamilNadu[i+473])
}

for (i in 1:118) {
  KR2[[i]] = Karnataka[,i+470]/(Gujrat[,i+473] + WB[, i+470] + 
                               Maharashtra[,i+470] +  Karnataka[,i+470] + 
                               TamilNadu[i+473])
}

for (i in 1:302) {
  KR3 = Karnataka[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                          Maharashtra[,i+588] +  Karnataka[,i+588] + TamilNadu[i+591])
}

for (i in 1:302) {
  KR3[[i]] = Karnataka[,i+588]/(Gujrat[,i+591] + WB[, i+588] + 
                               Maharashtra[,i+588] +  Karnataka[,i+588] + 
                               TamilNadu[i+591])
}


for (i in 1:110) {
  KR4 = Karnataka[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                          Maharashtra[,i+890] +  Karnataka[,i+890])
}

for (i in 1:110) {
  KR4[[i]] = Karnataka[,i+890]/(Gujrat[,i+893] + WB[, i+890] + 
                               Maharashtra[,i+890] +  Karnataka[,i+890])
}


for (i in 1:391) {
  KR5 = Karnataka[,i+1000]/(Gujrat[,i+1003] + WB[, i+1000] + 
                              Karnataka[,i+1000])
}

for (i in 1:391) {
  KR5[[i]] = Karnataka[,i+1000]/(Gujrat[,i+1003] + WB[, i+1000] + 
                                   Karnataka[,i+1000])
}



for (i in 1:49) {
  KR6 = Karnataka[,i+1391]/(Gujrat[,i+1394]  +  Karnataka[,i+1391])
}

for (i in 1:49) {
  KR6[[i]] = Karnataka[,i+1391]/(Gujrat[,i+1394] + 
                                  Karnataka[,i+1391])
}


for (i in 1:85) {
  KR7 = Karnataka[,i+1440]
}

for (i in 1:85) {
  KR7[[i]] = Karnataka[,i+1440]
}

KR_freq1 = data.frame(KR_Freq_Mut[,c(1:386)]*100)
KR_freq2 = data.frame(KR1[,c(1:81)]*100)
KR_freq3 = data.frame(KR2[,c(1:118)]*100)
KR_freq4 = data.frame(KR3[,c(1:302)]*100)
KR_freq5 = data.frame(KR4[,c(1:110)]*100)
KR_freq6 = data.frame(KR5[,c(1:391)]*100)
KR_freq7 = data.frame(KR6[,c(1:49)]*100)
KR_freq8 = data.frame(KR7[,c(1:85)]*100)

KR_freq = data.frame(KR_freq1 , KR_freq2 , KR_freq3 , KR_freq4, KR_freq5, KR_freq6, KR_freq7, KR_freq8)
sum(is.na(KR_freq))


# Perform mean imputation
for (col in colnames(KR_freq)) {
  col_mean <- mean(KR_freq[[col]], na.rm = TRUE)
  KR_freq[[col]][is.na(KR_freq[[col]])] <- col_mean
}

num_columns <- ncol(KR_freq)

# Create new column names
new_column_names <- paste("Freq_Mut", 1:num_columns, sep = "")

# Rename the columns
colnames(KR_freq) <- new_column_names
head(KR_freq)

#KR_freq$Freq_Mut1465= NULL

KR_freq_final = data.frame(Karnataka[,4:1525], KR_freq)
dim(KR_freq_final)

# Check for missing values in each column
#missing_values <- colSums(is.na(KR_freq_final))

# Identify columns with missing values
#columns_with_missing <- names(missing_values[missing_values > 0])

# Print the columns with missing values
#print(columns_with_missing)

sum(is.na(KR_freq_final))

p = glm(Karnataka$`Daily Cases`~., data = KR_freq_final , family = poisson)
summary(p)

glm_KR = capture.output(summary(p))
# Write the model summary to a text file
writeLines(glm_KR, "KR_summary.txt")

p1 = glm(Karnataka$`Daily Deaths`~., data = KR_freq_final , family = poisson)
summary(p1)

glm_KR_D = capture.output(summary(p1))
# Write the model summary to a text file
writeLines(glm_KR_D, "KR_summary_Death.txt")
