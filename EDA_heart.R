pacman::p_load(
  "ggplot2",
  "knitr",
  "arm",
  "rstanarm",
  "foreign",
  "bayesplot",
  "glmx",
  "reshape2",
  "VGAM",
  "dplyr",
  "magrittr",
  "tidyverse",
  "AER",
  "nnet",
  "lme4",
  "GGally",
  "ggcorrplot"
)

## Data Cleaning
num_heart<- read.csv("heart.csv")

# correlation plot

library(ggcorrplot)
#correlation p value 
num_heart$Sex <- ifelse(num_heart$Sex=="M", 1,0)
num_heart$ExerciseAngina <- ifelse(num_heart$ExerciseAngina=="Y", 1, 0)

for (i in 1:length(num_heart$ChestPainType)){
  if (str_detect(num_heart$ChestPainType[i],"TA")==TRUE){num_heart$ChestPainType[i]=1}
  if (str_detect(num_heart$ChestPainType[i],"NAP")==TRUE){num_heart$ChestPainType[i]=2}
  if (str_detect(num_heart$ChestPainType[i],"ATA")==TRUE){num_heart$ChestPainType[i]=3}
  if (str_detect(num_heart$ChestPainType[i],"ASY")==TRUE){num_heart$ChestPainType[i]=4}
  
}

for (i in 1:length(num_heart$RestingECG)){
  if (str_detect(num_heart$RestingECG[i],"Normal")==TRUE){num_heart$RestingECG[i]=1}
  if (str_detect(num_heart$RestingECG[i],"ST")==TRUE){num_heart$RestingECG[i]=2}
  if (str_detect(num_heart$RestingECG[i],"LVH")==TRUE){num_heart$RestingECG[i]=3}
  
}

for (i in 1: length(num_heart$ST_Slope)){
  if (str_detect(num_heart$ST_Slope[i],"Up")==TRUE){num_heart$ST_Slope[i]=1}
  if (str_detect(num_heart$ST_Slope[i],"Down")==TRUE){num_heart$ST_Slope[i]=2}
  if (str_detect(num_heart$ST_Slope[i],"Flat")==TRUE){num_heart$ST_Slope[i]=3}
  
}
num_heart$ChestPainType <- as.numeric(num_heart$ChestPainType)
num_heart$RestingECG <- as.numeric(num_heart$RestingECG)
num_heart$ST_Slope <- as.numeric(num_heart$ST_Slope)


p.mat = cor_pmat(num_heart)
p.mat
ggcorrplot(cor(num_heart), hc.order = TRUE,lab = TRUE,type = "lower", p.mat=p.mat)


## boxplot

heart<- read.csv("heart.csv")
heart$Oldpeak <- as.numeric(heart$Oldpeak)
heart_box <- pivot_longer(heart, 
                           cols = c(1, 4, 5, 8, 10),
                           names_to = "varibles",
                           values_to = "values"
)
ggplot(heart_box, aes(x=varibles, y=values, fill=factor(HeartDisease))) + 
  geom_boxplot()+
  theme(axis.text=element_text(size=5.5, face = "bold"))







## EDA

# heart<- read.csv("heart.csv")
# GGally::ggpairs(heart)

# does age have a effect on heart disease

g1 <- ggplot(data=heart)+
  geom_bar(aes(x= Age, y=HeartDisease, fill=ChestPainType), stat="identity")
# age seems to follow the normal distribution

# ggplot(data=heart, aes(x = as.numeric(Age), y=HeartDisease, color=ChestPainType))+
#   geom_smooth(method="lm", formula= y ~x, se = F)+
#   geom_point(alpha = 0.5)

# does sex have a effect on heart disease

p1 <- ggplot(data=heart)+
  geom_bar(aes(x=Sex, fill= as.factor(HeartDisease)), stat="count") + ggtitle("Gender vs. Heart Disease")


# will chest pain types have impact on heart disease?
p2<- ggplot(data=heart)+
  geom_bar(aes(x= ChestPainType,  fill= as.factor(HeartDisease)), stat= "count",show.legend = TRUE)+ggtitle("Different Chestpain Type vs. Heart Disease ")

# ggplot(data=heart,aes(x = ChestPainType, y= as.factor(HeartDisease)))+
#   geom_jitter(aes(color=ChestPainType), alpha = 0.5)

# will resting blood pressure have impact on heart disease?

# ggplot(data=heart)+
#   geom_bar(aes(x= RestingBP, y=as.factor(HeartDisease)), stat="identity")

g4<- ggplot(data=heart,aes(x= as.numeric(RestingBP), y=as.factor(HeartDisease)))+
         geom_violin(aes(color=as.factor(HeartDisease)))+
         geom_jitter(aes(color=as.factor(HeartDisease)), alpha=0.3)

# will cholesterol have impact on heart disease?
ggplot(data=heart,aes(x= as.numeric(Cholesterol), y=as.factor(HeartDisease)))+
  geom_point(aes(color=ChestPainType)) + 
  geom_smooth(aes(color=ChestPainType),method="lm", formula=y~x,
              alpha=0.2, size=2, se= FALSE)

ggplot(data=heart)+
  geom_boxplot(aes(x= Cholesterol, y=as.factor(HeartDisease)))

# will fasting blood sugar have impact on heart disease?

p3<- ggplot(data=heart)+
  geom_bar(aes(x= as.factor(FastingBS), fill=as.factor(HeartDisease)), stat="count")

# ggplot(data=heart)+
#   geom_boxplot(aes(x = as.factor(FastingBS), y= as.factor(HeartDisease)))+
#   geom_jitter(aes(x = as.factor(FastingBS), y= as.factor(HeartDisease), color=as.factor(HeartDisease)),alpha=0.3)

# will resting ECG have impact on heart disease?
g2 <- ggplot(data=heart)+
  geom_bar(aes(x= RestingECG,fill=as.factor(HeartDisease)), stat="count")


# will max heart rate have impact on heart disease?

g3 <- ggplot(data=heart)+
  geom_bar(aes(x= MaxHR, y=HeartDisease), stat="identity")

ggplot(data=heart)+
  geom_boxplot(aes(x= MaxHR, y=as.factor(HeartDisease), color=as.factor(HeartDisease)))+ 
  geom_jitter(aes(x= MaxHR, y=as.factor(HeartDisease), color=as.factor(HeartDisease)), alpha=0.3)
# ggplot(data=heart)+
#   geom_jitter(aes(x= MaxHR, y=HeartDisease, color=as.factor(HeartDisease)))
ggplot(data=heart, aes(x = MaxHR, y=HeartDisease))+
  geom_point(aes(color=ChestPainType), alpha = 0.3) +
  stat_smooth(method="lm", formula=y~x,
              alpha=0.2, size=2, aes(color=ChestPainType), se=FALSE)

# will exercise angina have impact on heart disease?

p4 <-ggplot(data=heart)+
  geom_bar(aes(x= ExerciseAngina, fill=as.factor(HeartDisease)), stat="count") + ggtitle("Exercise Angina vs. HeartDisease")

# ggplot(data=heart,aes(x = as.factor(ExerciseAngina), y= as.factor(HeartDisease)))+
#   stat_smooth(method="loess", formula=y~x,
#               alpha=0.2, size=2, aes(fill=ExerciseAngina))+
#   geom_jitter(aes(color=as.factor(ExerciseAngina)),alpha=0.4)


# will old peak have impact on heart disease?
p5 <- ggplot(data=heart)+
  geom_bar(aes(x=Oldpeak, fill=as.factor(HeartDisease)), stat="count")

ggplot(data=heart, aes(x = Oldpeak, y=as.factor(HeartDisease)))+
  geom_boxplot()+
  geom_jitter(aes(color=as.factor(HeartDisease)), alpha = 0.3)

# will ST slope have impact on heart disease?

p5<- ggplot(data=heart)+
  geom_bar(aes(x= ST_Slope, fill=as.factor(HeartDisease)), stat="count")

# ggplot(data=heart,aes(x =ST_Slope , y= as.factor(HeartDisease)))+
#   geom_jitter(aes(color=ST_Slope), alpha = 0.5)+
#   stat_smooth(method="loess", formula=y~x,
#               alpha=0.2, size=2, aes(fill=ST_Slope))

