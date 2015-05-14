require(statnet)
require(texreg)
require(stargazer)
require(grid)
rm(list=ls())

setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/min_versions/')
load('small_logliks.RData')



#setwd('//Users/TScott/Google Drive/elwha/PSJ_Submission/Version3')
# load('result_base.RData')
# load('result_dirpart.RData')
# load('result_indpart.RData')
# load('result_shapart.RData')
# load('result_allpartmod.RData')
# load('result_allpartmodquad.RData')
# load('result_pasttie.RData')


sink('basemod.tex')
stargazer(mod_base,mod_allpart,mod_allpartquad, covariate.labels=
            c('Sum','Mutual','Transitivity','Num. Resp.','Num.Groups','Years',
              'Org. Type','Group Partic.', 'Group Partic.$^2$'),
          digits=2, title = 'Baseline Models',label='table:basemods',
          style='jpam',
          column.separate = c(1,1), keep.stat=c('bic','aic'),
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
          omit = 1:7,
          style='jpam',
          model.numbers=FALSE,
          column.separate = c(1,1), 
          column.labels=c('Group Participation','Direct Participation','Co-Participation'),
          digits.extra=2,float=TRUE,
          #header=FALSE, 
          dep.var.labels.include=FALSE)  
sink()




sink('pasttie.tex')
stargazer(modpast_pasttie, covariate.labels=
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


