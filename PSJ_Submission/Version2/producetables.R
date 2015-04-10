require(statnet)
require(texreg)
require(stargazer)
require(grid)
rm(list=ls())
setwd("//Users/tscott1/Google Drive/elwha/PSJ_Submission/Version2/")
load('ergm_model_results.RData')
load('NetworkReady.RData')



g <- sum(net %e% "TVAL")/network.dyadcount(net)
init.geo<-log(1 - 1/(g+1))
geo.init = init.geo

as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default
#/homes/tscott1/win

dpx <- dppsp * dpn
spx <- sppsp * spn
spn7 <- spn/7
spx7 <- spx/7
sppsp7 <- sppsp/7
dpn7 <- dpn/7
dpx7 <- dpx/7
dppsp7 <- dppsp/7
ipn_pt<-get.vertex.attribute(net,'IPn7')*as.sociomatrix(net,"PRIOR_TIE")
ippsp_pt<-get.vertex.attribute(net,'IPpsp7')*as.sociomatrix(net,"PRIOR_TIE")
ipx_pt<-get.vertex.attribute(net,'IPx7')*as.sociomatrix(net,"PRIOR_TIE")
dpn_pt<-dpn7*as.sociomatrix(net,"PRIOR_TIE")
dppsp_pt<-dppsp7*as.sociomatrix(net,"PRIOR_TIE")
dpx_pt<-dpx7*as.sociomatrix(net,"PRIOR_TIE")
spn_pt<-spn7*as.sociomatrix(net,"PRIOR_TIE")
sppsp_pt<-sppsp7*as.sociomatrix(net,"PRIOR_TIE")
spx_pt<-spx7*as.sociomatrix(net,"PRIOR_TIE")
all_sp_quad<-((spn7+sppsp7)^2)
all_sp_pt<-((spn7+sppsp7)*as.sociomatrix(net,"PRIOR_TIE"))
all_sp<-(spn7+sppsp7)

sink('basemod.tex')
stargazer(mod_base,mod_allpart,mod_allpartquad, covariate.labels=
              c('Sum','Mutual','Transitivity','Num. Resp.','Num.Groups','Years',
                'Org. Type','Group Partic.', 'Group Partic.$^2$'),
            digits=2, title = 'Baseline Models',label='table:basemods',
          style='jpam',
            column.separate = c(1,1), 
            column.labels=c('Baseline Model','Group Participation','Group Participation$^2$'),
            digits.extra=2,float=TRUE,
         model.numbers=FALSE,
            #header=FALSE, 
            dep.var.labels.include=FALSE)
sink()

# 
# sink('splitpart.tex')
# stargazer(mod_indpart1, mod_indpart2, mod_indpart3, covariate.labels=
#             c('Group Partic. non-PSP', 'Group Partic. PSP', 'Group Partic. non-PSP*PSP'),
#           digits=2, title = 'Existing Groups',label='table:splitpart',
#           style='jpam',
#           dep.var.caption=NULL,
#           dep.var.labels=NULL,
#           omit = 1:7,
#           model.numbers=FALSE,
#           column.separate = c(1,1), 
#           column.labels=c('Non-PSP','PSP + Non-PSP','PSP * Non-PSP'),
#           digits.extra=2,float=TRUE,
#           #header=FALSE, 
#           dep.var.labels.include=FALSE)  
# sink()

sink('multipart.tex')
stargazer(mod_indpart, mod_dirpart, mod_shapart, covariate.labels=
            c('Group Partic. non-PSP', 'Group Partic. PSP', 'Group Partic. non-PSP*PSP'),
          digits=2, title = 'Triangulating Participation',label='table:partmods',
          dep.var.caption=NULL,
          dep.var.labels=NULL,
          omit = 1:7,style='jpam',
          model.numbers=FALSE,
          column.separate = c(1,1), 
          column.labels=c('Group Participation','Direct Participation','Co-Participation'),
          digits.extra=2,float=TRUE,
          #header=FALSE, 
          dep.var.labels.include=FALSE)  
sink()




sink('pasttie.tex')
stargazer(modpast_allpart7, covariate.labels=
            c('Past Tie (PT)', 'All Group Co-Part.', 
'All Group Co-Part.$^2$', 'All Group Co-Part. * PT'),
          digits=2, title = 'Pre-Existing Ties',label='table:pastties',
          dep.var.caption=NULL,
          dep.var.labels=NULL,
          omit = c(1:7),style='jpam',
          model.numbers=FALSE,
          column.separate = c(1,1), 
          column.labels=c('Past Tie'),
          digits.extra=2,float=TRUE,
          dep.var.labels.include=FALSE)  
sink()
rm(list=ls())
#detach('file:model_results.RData')


