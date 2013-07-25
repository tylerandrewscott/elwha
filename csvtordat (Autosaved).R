#this is the file I will use to extract csvs into r data

library(foreign)
tbl <- read.ssd("//Users/TScott/Google Drive/Watershed/WSA_Data/WSAMarch2_2009")

siteinfo.wsa.dat<-read.csv("//Users/TScott/Google Drive/Watershed/WSA_Data/WSAMarch2_2009/wsa_siteinfo_ts_final.csv")

head(siteinfo.dat)
siteinfo.dat$SITENAME


siteinfo.nrsa.dat<-read.csv("//Users/TScott/Google Drive/Watershed/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_SiteInformation_130411.csv")


colnames(siteinfo.nrsa.dat)

merge(siteinfo.wsa.dat$SITE_ID,
siteinfo.nrsa.dat$SITE_ID)