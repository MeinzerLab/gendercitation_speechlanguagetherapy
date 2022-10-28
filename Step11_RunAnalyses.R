setwd("/media/MeinzerStudy/gendercitation_v5") # Change to your project folder path
source("HelperFunctions.R")
library(ggplot2);library(mgcv);library(patchwork)
library(boot);library(pbmcapply)
library(data.table);library(quantreg)

# Load in data from step 10
load("df10_articledata_propdata_netdata.RData")
# Save number of cores on machine
cores=detectCores()

## Find subset of articles for analysis
## I.e., articles in a specific window that contain at least one relevant reference

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Changed the time window of the analyses to 2010 to 2020.
## Original code:
## time_window=article.data$PY%in%c(2009:2019)

time_window=article.data$PY%in%c(2010:2020)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Added a short code snippet to get the number of papers within in the analyses.

article.data_subset = article.data[subset_articles,]
length(unique(unlist(strsplit(article.data_subset$CP, ", "))))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Create gender category vectors
gend_group_4=unlist(lapply(article.data$AG,transform.cat.4))
gend_group_4=factor(gend_group_4,lev=c("MM","WM","MW","WW","NA"))

gend_group_2=unlist(lapply(article.data$AG,transform.cat.2))
gend_group_2=factor(gend_group_2,lev=c("MM","W|W","NA"))


###################################
## Recreate graphs from Figure 1 ##
###################################

# Get overall authorship breakdown by year
timedata=get.timedf(article.data)
p.ov=f1plot(timedata,"Overall authorship by year")

# Get journal-specific graphs
# Change functions to the journal names in your data
td.1=get.timedf(article.data,"AMERICAN JOURNAL OF SPEECH-LANGUAGE PATHOLOGY")
td.2=get.timedf(article.data,"APHASIOLOGY")
td.3=get.timedf(article.data, "AUGMENTATIVE AND ALTERNATIVE COMMUNICATION")
td.4=get.timedf(article.data, "COMMUNICATION SCIENCES AND DISORDERS-CSD")
td.5=get.timedf(article.data, "FOLIA PHONIATRICA ET LOGOPAEDICA")
td.6=get.timedf(article.data, "INTERNATIONAL JOURNAL OF LANGUAGE & COMMUNICATION DISORDERS")
td.7=get.timedf(article.data,"INTERNATIONAL JOURNAL OF SPEECH-LANGUAGE PATHOLOGY")
td.8=get.timedf(article.data,"JOURNAL OF COMMUNICATION DISORDERS")
td.9=get.timedf(article.data, "JOURNAL OF FLUENCY DISORDERS")
td.10=get.timedf(article.data, "JOURNAL OF SPEECH LANGUAGE AND HEARING RESEARCH")
td.11=get.timedf(article.data, "JOURNAL OF VOICE")
td.12=get.timedf(article.data, "LANGUAGE AND SPEECH")
td.13=get.timedf(article.data, "LANGUAGE SPEECH AND HEARING SERVICES IN SCHOOLS")
td.14=get.timedf(article.data, "SEMINARS IN SPEECH AND LANGUAGE")
# ...
p.1=f1plot(td.1,"AMERICAN JOURNAL OF SPEECH-LANGUAGE PATHOLOGY",yl=F,xl=F)
p.2=f1plot(td.2,"APHASIOLOGY",yl=F,xl=F)
p.3=f1plot(td.3,"AUGMENTATIVE AND ALTERNATIVE COMMUNICATION", yl=F,xl=F)
p.4=f1plot(td.4,"COMMUNICATION SCIENCES AND DISORDERS-CSD",yl=F,xl=F)
p.5=f1plot(td.5,"FOLIA PHONIATRICA ET LOGOPAEDICA",yl=F,xl=F)
p.6=f1plot(td.6,"INTERNATIONAL JOURNAL OF LANGUAGE & COMMUNICATION DISORDERS",yl=F,xl=F)
p.7=f1plot(td.7,"INTERNATIONAL JOURNAL OF SPEECH-LANGUAGE PATHOLOGY",yl=F,xl=F)
p.8=f1plot(td.8,"JOURNAL OF COMMUNICATION DISORDERS",yl=F,xl=F)
p.9=f1plot(td.9,"JOURNAL OF FLUENCY DISORDERS", yl=F,xl=F)
p.10=f1plot(td.10,"JOURNAL OF SPEECH LANGUAGE AND HEARING RESEARCH",yl=F,xl=F)
p.11=f1plot(td.11,"JOURNAL OF VOICE",yl=F,xl=F)
p.12=f1plot(td.12,"LANGUAGE AND SPEECH",yl=F,xl=F)
p.13=f1plot(td.13,"LANGUAGE SPEECH AND HEARING SERVICES IN SCHOOLS",yl=F,xl=F)
p.14=f1plot(td.14,"SEMINARS IN SPEECH AND LANGUAGE",yl=F,xl=F)
# ...

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 1 in publication

p.ov=p.ov+xlab("Publication Year")+ylab("Proportion of Papers")
p.ov

## Supplementary figures
p.1
p.2
p.3
p.4
p.5
p.6
p.7
p.8
p.9
p.10
p.11
p.12
p.13
p.14

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#########################################################
## Calculate citation gaps across cited author genders ##
#########################################################

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]


# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

###################################
## Recreate graphs from Figure 2 ##
###################################

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=500,type='randomdraw')
boot.cn=boot(ref_tot_sub,citegap,R=500,type='conditional')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)
plot.df.conditional=get.plotdf(boot.cn)
p.all.rd=f2plot(plot.df.randomdraw,"Gap relative to literature",
                ymin=-0.35,ymax=0.35)
p.all.cn=f2plot(plot.df.conditional,"Gap conditional on characteristics",
                ymin=-0.35,ymax=0.35)

# View plots
p.all.rd
p.all.cn

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 2 in publication 

p.all = p.all.rd + p.all.cn + plot_layout(
  nrow = 1, heights = unit(c(6,10), c("cm","cm"))) +
  plot_annotation(tag_levels="A")
p.all

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Additional plots for every journal

citegap.journal=function(j,rand){
  subset.j=ref_tot_sub[article.data$SO[subset_articles]==j,]
  citegap(subset.j)
  boot.rd=boot(subset.j,citegap,R=500,type=rand)
  plot.df.randomdraw=get.plotdf(boot.rd)
  p.all.rd=f2plot(plot.df.randomdraw,"Gap relative to literature",
                  ymin=-0.7,ymax=0.7)
  p.all.rd=p.all.rd +
    ggtitle(j)
  plot(p.all.rd)
}

plotarray=pbmclapply(unique(article.data$SO),citegap.journal,rand='randomdraw')

(plotarray[[1]]+plotarray[[2]])/(plotarray[[3]]+plotarray[[4]])
(plotarray[[5]]+plotarray[[6]])/(plotarray[[7]]+plotarray[[8]])
(plotarray[[9]]+plotarray[[10]])/(plotarray[[11]]+plotarray[[12]])
(plotarray[[13]]+plotarray[[14]])/(plot_spacer() + plot_spacer())
## ...

cond_pa=pbmclapply(unique(article.data$SO),citegap.journal,rand='conditional')

(cond_pa[[1]]+cond_pa[[2]])/(cond_pa[[3]]+cond_pa[[4]])
(cond_pa[[5]]+cond_pa[[6]])/(cond_pa[[7]]+cond_pa[[8]])
(cond_pa[[9]]+cond_pa[[10]])/(cond_pa[[11]]+cond_pa[[12]])
(cond_pa[[13]]+cond_pa[[14]])/(plot_spacer()+plot_spacer())
## ...

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##################################################
## Get breakdowns based on citing author gender ##
##################################################

# Get subset of group labels

gend2_sub=gend_group_2[subset_articles]
gend4_sub=gend_group_4[subset_articles]

# Gap within reference lists of MM papers
citegap(ref_tot_sub[gend2_sub=="MM",],type='conditional')

# Gap within reference lists of W|W papers
citegap(ref_tot_sub[gend2_sub=="W|W",],type='conditional')

# Gap within subgroups of W|W papers
citegap(ref_tot_sub[gend4_sub=="WM",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="MW",],type='conditional')
citegap(ref_tot_sub[gend4_sub=="WW",],type='conditional')

###################################
## Recreate graphs from Figure 3 ##
###################################

# Get bootstrap standard errors for gap values
boot.mm=boot(ref_tot_sub[gend2_sub=="MM",],citegap,R=500)
boot.worw=boot(ref_tot_sub[gend2_sub=="W|W",],citegap,R=500)
boot.wm=boot(ref_tot_sub[gend4_sub=="WM",],citegap,R=500)
boot.mw=boot(ref_tot_sub[gend4_sub=="MW",],citegap,R=500)
boot.ww=boot(ref_tot_sub[gend4_sub=="WW",],citegap,R=500)

# Create ggplot compatible data frames
plot.df.mm=get.plotdf(boot.mm)
plot.df.worw=get.plotdf(boot.worw)
plot.df.wm=get.plotdf(boot.wm)
plot.df.mw=get.plotdf(boot.mw)
plot.df.ww=get.plotdf(boot.ww)
p.mm=f2plot(plot.df.mm,"Citing: MM",ymin=-0.30,ymax=0.30)
p.worw=f2plot(plot.df.worw,"Citing: WW, WM or MW",ymin=-0.30,ymax=0.30)
p.wm=f2plot(plot.df.wm,"Citing: WM",ymin=-0.30,ymax=0.30)
p.mw=f2plot(plot.df.mw,"Citing: MW",ymin=-0.30,ymax=0.30)
p.ww=f2plot(plot.df.ww,"Citing: WW",ymin=-0.30,ymax=0.30)

# View plots
p.mm
p.worw
p.wm
p.mw
p.ww

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 3 in publication
gender.all = p.worw +
  (((p.mm + theme(axis.title.x=element_blank())) +
    (p.wm + theme(axis.title.x=element_blank(), 
                  axis.title.y=element_blank()))) / 
    (p.mw + 
    (p.ww + theme(axis.title.y=element_blank())))) +
  plot_layout(tag_level="new") + 
  plot_annotation(tag_levels=c("A","I"))

gender.all

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++