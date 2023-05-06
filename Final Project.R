library(readr)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(data.table)
library(Hmisc)
library(SmartEDA)
library(GGally)
library(tableone)
library(pastecs)
library(vtable)
library(ggpubr)
library(fastDummies)
library(cowplot)
library(ggiraphExtra)

setwd("/Users/omkarsadekar/Documents/NEU Study Material/NEU Study Material/Probability and Stats/Final Project")

Leads_Data <- read_csv("Leads.csv")
View(Leads_Data)

Leads_df <- data.frame(Leads_Data)
unique(c(Leads_df))
dim(Leads_df)
summary(Leads_df)


#Dropping Unwanted Coloumns Prospect ID and Lead Number since they're unique Coloumns
Leads_df = subset(Leads_df, select = -c(Prospect.ID, Lead.Number)) 

#Dropping Coloumns with Geographical Data
Leads_df = subset(Leads_df, select = -c(City, Country)) 

#Dropping Coloumns having Single Value
Leads_df = subset(Leads_df, select = -c(Magazine,Receive.More.Updates.About.Our.Courses,
                                        Update.me.on.Supply.Chain.Content,Get.updates.on.DM.Content,
                                        I.agree.to.pay.the.amount.through.cheque)) 
#Dropping Columns with less Variety
Leads_df = subset(Leads_df, select = -c(Do.Not.Call,What.matters.most.to.you.in.choosing.a.course,Search,
                                        Newspaper.Article,X.Education.Forums,Newspaper,Digital.Advertisement,Through.Recommendations))
str(Leads_df)
dim(Leads_df)

#Plot for Missing values in the data

Leads_df  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())

#Table for Missing Values in %
Missing_values = (colMeans(is.na(Leads_df)))*100
Missing_values %>%
  kable() %>%
  kable_styling(font_size = 10)

#Dropping Coloumns with nmore than 30% null values
Leads_df = subset(Leads_df, select = -c(Tags,Lead.Quality,Asymmetrique.Activity.Index,Asymmetrique.Profile.Index,Asymmetrique.Activity.Score,Asymmetrique.Profile.Score))

#Omitting rest of the Null Values for better Analysis
Leads_df = Leads_df%>%na.omit()
dim(Leads_df)


#Converting the "Converted" Column to a categorical Variable
Leads_df <- Leads_df %>%
  mutate_at(4,~as.factor(case_when(. =="0"~"No",.=="No"~"No",.=="1"~"Yes")))

glimpse(Leads_df)
#Handling Outliers in the Data
quartiles <- quantile(Leads_df$TotalVisits, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(Leads_df$TotalVisits)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

Leads_df <- subset(Leads_df, Leads_df$TotalVisits > Lower & Leads_df$TotalVisits < Upper)

dim(Leads_df)

  
#Summary Statistics of all Variables
Sum_Stats = ExpData(data=Leads_df,type=2, fun = c("mean", "median", "var"))
Sum_Stats %>%
  kable() %>%
  kable_styling()

#Table for Summary Statistics
st(Leads_df)

#Summary Statistics for all Numerical Variables
Numeric_Summary = data.frame((ExpNumStat(Leads_df,by="A",gp="Converted",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)))
Numeric_Summary %>%
  kable() %>%
  kable_styling()

#Categorical Variable vs target variable
plot_Category <- ExpCatViz(Leads_df,target="Converted",fname=NULL,clim=5,col=c("slateblue4","slateblue1"),margin=2,Page = c(2,1),sample=3)

#Density Plots for Numerical Variables
Density_plot1 = ExpNumViz(Leads_df,type = 2, nlim = 25,
                          col = NULL,Page = c(2,2), sample = 2, scatter = FALSE,
                          gtitle = "Density plot: ")

#Categorical Variables vs Target using Scatterplot & BoxPlot

Plot_Converted <- ExpNumViz(Leads_df,target="Converted",type=1,nlim=3,fname=NULL,col=c("darkgreen","springgreen3","springgreen1"),Page=c(2,2),sample=NULL, scatter = TRUE, theme = "Default")
Plot_Converted_2 <- ExpNumViz(Leads_df,target="Converted",type=1,nlim=3,fname=NULL,col=c("darkgreen","springgreen3","springgreen1"),Page=c(2,2),sample=NULL, scatter = FALSE, theme = "Default")


#Stats for Subgroups and Target Variable
Total_Visits_stat = ExpCustomStat(Leads_df, Cvar = c("Lead.Origin", "Lead.Source"), Nvar = c("TotalVisits"),
                                  stat = c("Count","sum","mean","median"), gpby = TRUE, filt = NULL)
Total_Visits_stat %>%
  kable() %>%
  kable_styling(font_size = 10)


#Summary Stats for Categorical Variables
#CrossTabulation with Categorical Variable "Converted"
Cross_Tab1 = ExpCTable(Leads_df,Target="Converted",margin=1,clim=10,nlim=3,round=2,bin=NULL,per=F)
Cross_Tab1%>%
  kable() %>%
  kable_styling(font_size = 5)


#Statistical Test
Stats_Test <- ExpCatStat(Leads_df,Target="Converted",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)
Stats_Test%>%
  kable() %>%
  kable_styling(font_size = 10)


#Plot for Variable Importance based on Information Value
varimp <- ExpCatStat(Leads_df,Target="Converted",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=TRUE,top=10,Round=2)


#Project Milestone - 2
#Inferential Statistics
#Statistical Test for the Data
Stat_test <- ExpCatStat(Leads_df,Target="Converted",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)
Stat_test %>%
  kable() %>%
  kable_styling(font_size = 10)
names(Leads_df)

Hypo <- subset(Leads_df, select = c("Total.Time.Spent.on.Website", "Converted"))

#One Sample T test
ggplot(Hypo, aes(y=Total.Time.Spent.on.Website)) + geom_boxplot() + theme_classic()

# Steps of hypothesis testing 

# State the null and the alternative hypothesis first

## null : mu <= 250, It states that the average time spent on the website is less than or equal to 250 seconds
#mu <=250
## Alternative : It states that the mean is more than 250 seconds
#One-sided Test

t.test(Leads_df$Total.Time.Spent.on.Website,mu=250,alternative = "less",conf.level = 0.95)

#Two sided Test

t.test(Leads_df$Total.Time.Spent.on.Website,mu=250,alternative = "two.sided",conf.level = 0.95)


# Two-sample t-test for Two Independent Groups
# creating two samples based on Conversion 

Converted <- Leads_df %>% filter(Converted == "Yes") %>% sample_n(size = 1000)
NotConverted <- Leads_df %>% filter(Converted == "No") %>% sample_n(size = 1000)

#Plotting the Data
ggplot(Leads_df, aes(x=Converted, y=Total.Time.Spent.on.Website)) + 
  geom_boxplot()


# We want to prove whether the conversion decision is based on average time spent on the Website?
#H0: mYes ≥ mNo 
t.test(Converted$Total.Time.Spent.on.Website, NotConverted$Total.Time.Spent.on.Website, var.equal = T, alternative = "less")

# Is there any significant difference between Time Spent on the website and the conversion decision?
t.test(Converted$Total.Time.Spent.on.Website, NotConverted$Total.Time.Spent.on.Website, var.equal = T)
#p < 0.05,  We can conclude that the mean Total Time spent on the website by a person is significantly different between the two groups



#We intend to find the traffic from various sources which is the lead origin. We assume 34% of traffic comes from API, 53% from Landing page submission, 9% from Lead add form and 0.4% by importing lead 
#Null: There is no statistically significant difference between the observed frequencies and the expected frequencies (The observed distribution is the same as the expected distribution)
#Alternative: There is a statistically significant difference between the observed frequencies and the expected frequencies (The observed distribution is not the same as the expected distribution)
# Chi-square test of independence
#Null: The two variables are independent (not related)
#Alternative: The variables are dependent (related)
ChiTab <- table(Leads_df$Lead.Origin, Leads_df$Converted)
chi_degpla <- chisq.test(Leads_df$Lead.Origin, Leads_df$Converted)
chi_degpla
#p < 0.05,  We reject the null. Hence, The variables are dependent
chi_degpla$expected

# chi-square goodness of fit test
deg_count <-  table(Leads_df$Lead.Origin)
deg_count

chi_degree <- chisq.test(deg_count, p = c(0.33, 0.53, 0.09, 0.05))
chi_degree

chi_degree$expected

#As p<0.05, we reject the Null hypothesis and accept the alternative hypothesis

#Heatmap for Variables Lead Origin, Converted, Total Time Spent on Website
plot_1 <- ggplot(Leads_df, aes(Lead.Origin , Converted, fill= Total.Time.Spent.on.Website)) + 
  geom_tile(aes(fill = Total.Time.Spent.on.Website)) +
  scale_fill_gradient(low="white", high="blue")

#Regression Analysis 
d<-dummy_cols(Leads_df,select_columns = 'Converted')
d
# Dataset of Non- converted customers with their dependent variables
no<-subset(d,Converted_No>0,select= c('TotalVisits','Total.Time.Spent.on.Website','Page.Views.Per.Visit'))
S_no <-data.frame(no)

# Summary of Non converted customers
summary(S_no)

# Dataset of Converted customers with their dependent variables
yes<-subset(d,Converted_Yes>0,select =c('TotalVisits','Total.Time.Spent.on.Website','Page.Views.Per.Visit'))
S_yes<-data.frame(yes)

# Summary
summary(S_yes)

# Count of Total Time spent on website and Page views for converted and non converted
CountTotalTime<-S_no$Total.Time.Spent.on.Website
CountTotalVisit<-S_no$Page.Views.Per.Visit
CountTotalTimey<-S_yes$Total.Time.Spent.on.Website
CountVisity<-S_yes$Page.Views.Per.Visit

#Simple Linear regression.

plot1<-ggplot(S_no,aes(TotalVisits,Total.Time.Spent.on.Website,color= CountTotalTime))+geom_point()+geom_smooth(method = 'lm')+xlab('Total Visits')+ylab('Total Time Spent on Website')
plot2<-ggplot(S_no,aes(TotalVisits,Page.Views.Per.Visit, color= CountTotalVisit))+geom_point()+geom_smooth(method = 'lm')+xlab('Total Visits')+ylab('Pages Viewed Per visit')

plot_grid(plot1,plot2, align = 'AUTO')


plot3<-ggplot(S_yes,aes(TotalVisits,Total.Time.Spent.on.Website,color= CountTotalTimey))+geom_point()+geom_smooth(method = 'lm')+xlab('Total Visits')+ylab('Total Time Spent on Website')
plot4<-ggplot(S_yes,aes(TotalVisits,Page.Views.Per.Visit, color= CountVisity))+geom_point()+geom_smooth(method = 'lm')+xlab('Total Visits')+ylab('Paged Viewed Per visit')

plot_grid(plot3,plot4,labels = 'AUTO')


#Multiple regression
t<-lm(TotalVisits ~ Total.Time.Spent.on.Website+Page.Views.Per.Visit,data = S_no)
summary(t)

plot1
ggPredict(t,interactive = TRUE)


t2<-lm(TotalVisits ~ Total.Time.Spent.on.Website+Page.Views.Per.Visit,data = S_yes)
summary(t2)

plot3
ggPredict(t2,interactive = TRUE)


