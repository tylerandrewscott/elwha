rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(statnet)
library(knitr)
library(INLA)
remote=FALSE
#read in raw data
if(remote)
{raw.df = fread('../../input/Final_Verbose.csv')}
if(!remote)
{raw.df = fread('input/Final_Verbose.csv')}
#start base reference df with 1 row per respondent
base.row = raw.df[,1:which(names(raw.df)=='What is your name?|Last'),with=F]
base.row$Organization = raw.df$`What organization do you primarily represent (work for, volunteer for, or are otherwise affiliated with) on the collaborative group(s) that you previously identified?|Organization`
base.row = base.row %>% rename(uq.ID = `Response ID`)
base.row$Organization = tolower(base.row$Organization)
base.row$`Last page saved` = as.numeric(as.character(gsub('P','',base.row$`Last page saved`)))

base.row = base.row %>% filter(Organization!='')

base.row$Organization[base.row$Organization=='wria 2 tag'] = "wria 2"
base.row$Organization[base.row$Organization=='south sound lio'] = "south puget sound lio"
base.row$Organization[base.row$Organization=='snohomish county surface water management'] = "snohomish county"
base.row$Organization[base.row$Organization=='wsux island waste wise'] = "wsux waste wise"

#read in reported benefits from participation in a given group (i.e., benefits from access)
access.df = raw.df[,grep('increased',names(raw.df)),with=F] %>% select(-contains('None of the above')) 
access.df = access.df[,grep("\\|.+\\|",colnames(access.df)),with=F]
access.df = access.df %>% mutate(uq.ID = raw.df$`Response ID`) %>% filter(uq.ID %in% base.row$uq.ID)


access.df = access.df %>% gather(Question,Response,-uq.ID)
temp.vec = data.frame(matrix(unlist(str_split(access.df$Question,"\\|")),ncol=3,byrow=T))
access.df = data.frame(temp.vec,access.df %>% select(uq.ID,Response)) %>% rename(Question = X1, Group = X2, Response.Option = X3) %>% filter(!grepl('One or more',Group)) %>%
  mutate(Response.Option = tolower(Response.Option)) %>%
  filter(Response==1) %>% select(-Response) %>% mutate(Increase = ifelse(Response.Option =='agree'|Response.Option =='strongly agree',1,0))

access.df$Short = NA
access.df$Short[grep('face-to-face',access.df$Question)] = 'face.to.face'
access.df$Short[grep('awareness',access.df$Question)] = 'awareness'
access.df$Short[grep('understanding',access.df$Question)] = 'understanding'
access.df$Short[grep('technical',access.df$Question)] = 'technical'
access.df$Short[grep('human',access.df$Question)] = 'human'
access.df$Short[grep('financial',access.df$Question)] = 'financial'


kable(table(access.df$Short,access.df$Increase))


library(knitr)


library(texreg)
library(stargazer)
stargazer(table(access.df$Question,access.df$Increase),type='text')


#read in indicator for group membership
member.df = raw.df[,intersect(grep('have you participated|regularly participated',names(raw.df)),grep('None of the above|other than those|One or more',names(raw.df),invert=T)),with=F]
member.df = member.df %>% mutate(uq.ID = raw.df$`Response ID`)
member.df = member.df %>% filter(uq.ID %in% base.row$uq.ID)
member.df = member.df %>% gather(Group,Member,-uq.ID)
temp.vec = data.frame(matrix(unlist(str_split(member.df$Group,"\\|")),ncol=2,byrow=T))
member.df = data.frame(temp.vec,member.df %>% select(uq.ID,Member)) %>% rename(Q = X1,Group = X2) %>% select(-Q)

# code the number of groups in which respondent reports membership
temp = data.frame(tapply(member.df$Member,member.df$uq.ID,sum))
colnames(temp) = 'Number.Of.Groups'
temp$uq.ID = rownames(temp)
base.row = base.row %>% data.frame(.)

base.row$Number.Of.Groups = as.numeric(temp$Number.Of.Groups[match(base.row$uq.ID,rownames(temp))])


ties.long = gather(base.row,N.slot,Tie.To,-uq.ID,-Date.started,-Date.completed,-IP.address,-Response.time,-Language,-Last.page.saved,-What.is.your.name..First,-What.is.your.name..Last,
              -Organization,-Number.Of.Groups)

# read in implementation ties, reorder into long-file
ties.imp.wide = raw.df[,grep('do you routinely implement',names(raw.df)),with=F]
setnames(ties.imp.wide,paste(names(ties.imp.wide),c(1:5),sep='.'))
ties.long = ties.imp.wide %>% mutate(uq.ID = raw.df$`Response ID`) %>% gather(Type,Tie.To,-uq.ID) %>% data.frame(.) %>% filter(Tie.To != '')
ties.long$Type = 'implement'


# read in consultation ties, reorder into long-file
ties.cons.wide = raw.df[,grep('do you informally consult',names(raw.df)),with=F]
setnames(ties.cons.wide,paste(names(ties.cons.wide),c(1:5),sep='.'))
ties.cons.long = ties.cons.wide %>% mutate(uq.ID = raw.df$`Response ID`) %>% gather(Type,Tie.To,-uq.ID) %>% data.frame(.) %>% filter(Tie.To != '')
ties.cons.long$Type = 'consult'

# read in planning ties, reorder into long-file
ties.plan.wide = raw.df[,grep('routinely coordinate plans',names(raw.df)),with=F]
setnames(ties.plan.wide,paste(names(ties.plan.wide),c(1:5),sep='.'))
ties.plan.long = ties.plan.wide %>% mutate(uq.ID = raw.df$`Response ID`) %>% gather(Type,Tie.To,-uq.ID) %>% data.frame(.) %>% filter(Tie.To != '')
ties.plan.long$Type = 'plan'

ties.long = full_join(full_join(ties.long,ties.plan.long),ties.cons.long)
ties.long = ties.long[ties.long != '',]

##### RELABEL ORG NAMES CONSISTENTLY #####
ties.long$Tie.To = tolower(ties.long$Tie.To)
ties.long$Tie.To = gsub('  ',' ',ties.long$Tie.To)
ties.long$Tie.To = gsub(" (just started)",'',ties.long$Tie.To,fixed=T)
ties.long$Tie.To = gsub("none",'',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("whidby",'whidbey',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("whidbey camano",'whidbey-camano',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("wsu extension--snohomish",'wsu extension snohomish',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("watershad",'watershed',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("washington state university",'wsu',ties.long$Tie.To,fixed=T) 
ties.long$Tie.To = gsub("marine resource committee",'mrc',ties.long$Tie.To,fixed=T) 

ties.long$Tie.To[grep('adopt-a-stream',ties.long$Tie.To)] = 'adopt-a-stream foundation'
ties.long$Tie.To[grep('capital land trust',ties.long$Tie.To)] = 'capitol land trust'
ties.long$Tie.To[grep('bellevue',ties.long$Tie.To)] = 'city of bellevue'
ties.long$Tie.To[intersect(grep('chambers',ties.long$Tie.To),grep('clover',ties.long$Tie.To))] = 'chambers-clover watershed council'
ties.long$Tie.To[grep('wria 9',ties.long$Tie.To)] = 'wria 9'
ties.long$Tie.To[grep('wria 8',ties.long$Tie.To)] = 'wria 8'
ties.long$Tie.To[grep('wsu kitsap extension',ties.long$Tie.To)] = 'wsu extension kitsap'
ties.long$Tie.To[grep('wsu extension kitsap',ties.long$Tie.To)] = 'wsu extension kitsap'
ties.long$Tie.To[grep('wria 1',ties.long$Tie.To)] = 'wria 1'
ties.long$Tie.To[grep('restoration ecology network',ties.long$Tie.To)] = 'uw restoration ecology network'
ties.long$Tie.To[grep('u. of washington|university of washington',ties.long$Tie.To)] = 'uw'
ties.long$Tie.To[grep('wria1',ties.long$Tie.To)] = 'wria 1'
ties.long$Tie.To[grep('wa doe',ties.long$Tie.To)] = 'wdoe'
ties.long$Tie.To[grep('wwin',ties.long$Tie.To)] = 'whatcom watersheds information network'

ties.long$Tie.To[intersect(grep('ecology',ties.long$Tie.To),grep('restoration ecology',ties.long$Tie.To,invert=T))] = 'wdoe'
ties.long$Tie.To[intersect(grep('tacoma',ties.long$Tie.To),grep('pierce health',ties.long$Tie.To,invert=T))] = 'tacoma-pierce health department'


ties.long$Tie.To[ties.long$Tie.To=='wsu cooperative extension, jefferson'] = "wsu extension jefferson"
ties.long$Tie.To[ties.long$Tie.To=='10,000 yera institute'] = "10,000 year institute"
ties.long$Tie.To[ties.long$Tie.To=='whidbey watershed'] = "whidbey watershed stewards"
ties.long$Tie.To[ties.long$Tie.To=='west sound watershed council'] = "west sound watersheds council"
ties.long$Tie.To[ties.long$Tie.To=='birch bay watershed and aquatic'] = "birch bay watershed & aquatic resources management district"
ties.long$Tie.To[ties.long$Tie.To=='birch bay watershed and aquatic resouces management district'] = "birch bay watershed & aquatic resources management district"
ties.long$Tie.To[ties.long$Tie.To=='adopt a stream foundation'] = "adopt-a-stream foundation"
ties.long$Tie.To[ties.long$Tie.To=='bbat'] = "bellingham bay action team"
ties.long$Tie.To[ties.long$Tie.To=='city of seattle parks & office of sustainability'] = "city of seattle"
ties.long$Tie.To[ties.long$Tie.To=='clallam conservation distric'] = "clallam conservation district"
ties.long$Tie.To[ties.long$Tie.To=='clallm conservation district'] = "clallam conservation district"
ties.long$Tie.To[ties.long$Tie.To=='city of port angeles recycling'] = "city of port angeles"
ties.long$Tie.To[ties.long$Tie.To=='department of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='dept. of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='dept. of natural resources'] = "wdnr"
ties.long$Tie.To[ties.long$Tie.To=='department of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='dnr'] = "wdnr"
ties.long$Tie.To[ties.long$Tie.To=='duwamish river cleanup/tag - seattle'] = "duwamish river cleanup coalition"
ties.long$Tie.To[ties.long$Tie.To=='drcc'] = "duwamish river cleanup coalition"
ties.long$Tie.To[ties.long$Tie.To=='drmt'] = "dungeness river management team"
ties.long$Tie.To[ties.long$Tie.To=='wa state dept of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='wsu puyallup extension'] = "wsu extension pierce"
ties.long$Tie.To[ties.long$Tie.To=='wsu - puyallup'] = "wsu extension pierce"
ties.long$Tie.To[ties.long$Tie.To=='wsu extension snohomish county'] = "wsu extension snohomish"
ties.long$Tie.To[ties.long$Tie.To=='wsu mason county'] = "wsu extension mason"

ties.long$Tie.To[ties.long$Tie.To=='wsu extension jefferson county'] = "wsu extension jefferson"
ties.long$Tie.To[ties.long$Tie.To=='wsu cooperative extension, jefferson county'] = "wsu extension jefferson"

ties.long$Tie.To[ties.long$Tie.To=='wsu beach watchers in other counties'] = "wsu beach watchers"
ties.long$Tie.To[ties.long$Tie.To=='wsu extension beach watchers'] = "wsu beach watchers"

ties.long$Tie.To[grep('thurston conservation district',ties.long$Tie.To)] = "thurston conservation district"
ties.long$Tie.To[grep('thurston stormwater outreach group',ties.long$Tie.To)] = "thurston sog"
ties.long$Tie.To[ties.long$Tie.To=='stream team (thurston co.)'] = "thurston stream team"
ties.long$Tie.To[ties.long$Tie.To=='thurston county stream team (cities of olympia, tumwater, lacey & thurston co.)'] = "thurston stream team"
ties.long$Tie.To[ties.long$Tie.To=='thurston county eco net'] = "thurston eco net"
ties.long$Tie.To[ties.long$Tie.To=='thurston county water resources'] = "thurston county"

ties.long$Tie.To[ties.long$Tie.To=='mason county environmental health'] = "mason county"
ties.long$Tie.To[ties.long$Tie.To=='mason county public works'] = "mason county"
ties.long$Tie.To[ties.long$Tie.To=='mason county public health'] = "mason county"
ties.long$Tie.To[ties.long$Tie.To=='mason county dept of community development'] = "mason county"
ties.long$Tie.To[ties.long$Tie.To=='king co'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county dnr'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county (wastewater division)'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county dnrp'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county noxious weeds'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county parks and open space'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king county & local govts'] = "king county"
ties.long$Tie.To[ties.long$Tie.To=='king conservation district'] = "king county conservation district"

ties.long$Tie.To[ties.long$Tie.To=='whatcom county marine resources committee'] = "whatcom county mrc"
ties.long$Tie.To[ties.long$Tie.To=='whatcom marine resources committee'] = "whatcom county mrc"
ties.long$Tie.To[ties.long$Tie.To=='whatcom mrc'] = "whatcom county mrc"
ties.long$Tie.To[ties.long$Tie.To=='wsu whatcom county extension'] = "wsu whatcom extension"
ties.long$Tie.To[ties.long$Tie.To=='whatcom mrc'] = "whatcom county mrc"
ties.long$Tie.To[ties.long$Tie.To=='whatcom county public works/flood control zone district'] = "whatcom county"
ties.long$Tie.To[ties.long$Tie.To=='clallam county environmental health division'] = "clallam county"
ties.long$Tie.To[ties.long$Tie.To=='stream keepers of clallam county'] = "clallam county streamkeepers"
ties.long$Tie.To[ties.long$Tie.To=='clallam conservation district'] = "clallam county conservation district"

ties.long$Tie.To[ties.long$Tie.To=='snohomish marine resource committee'] = "snohomish county mrc"
ties.long$Tie.To[ties.long$Tie.To=='snohomish marine resources committee'] = "snohomish county mrc"
ties.long$Tie.To[ties.long$Tie.To=='snohomish mrc'] = "snohomish county mrc"
ties.long$Tie.To[ties.long$Tie.To=='snohomish conservation district (as a consultant)'] = "snohomish county conservation district"
ties.long$Tie.To[ties.long$Tie.To=='snohomish conservation district'] = "snohomish county conservation district"
ties.long$Tie.To[ties.long$Tie.To=='snohomish county swm'] = "snohomish county"
ties.long$Tie.To[ties.long$Tie.To=='snohomish county surface water management'] = "snohomish county"
ties.long$Tie.To[ties.long$Tie.To=='snohomish county beach watchers'] = "wsu beach watchers"
ties.long$Tie.To[ties.long$Tie.To=='wsu snohomish county beach watchers'] = "wsu beach watchers"
ties.long$Tie.To[ties.long$Tie.To=='snohomish basisn watershed'] = "snohomish river basin salmon recovery forum"
ties.long$Tie.To[ties.long$Tie.To=='snohomish le'] = "snohomish river basin salmon recovery forum"
ties.long$Tie.To[ties.long$Tie.To=='snohomish watershed forum'] = "snohomish river basin salmon recovery forum"
ties.long$Tie.To[ties.long$Tie.To=='snohomish watershed recovery'] = "snohomish river basin salmon recovery forum"
ties.long$Tie.To[ties.long$Tie.To=='snohomish forum'] = "snohomish river basin salmon recovery forum"

ties.long$Tie.To[ties.long$Tie.To=='skagit system cooperative'] = "skagit river system cooperative"
ties.long$Tie.To[ties.long$Tie.To=='skagit cooperative'] = "skagit river system cooperative"
ties.long$Tie.To[ties.long$Tie.To=='skagit river systems cooperative'] = "skagit river system cooperative"
ties.long$Tie.To[ties.long$Tie.To=='skagit marine resources committee'] = "skagit county mrc"
ties.long$Tie.To[ties.long$Tie.To=='skagit mrc'] = "skagit county mrc"
ties.long$Tie.To[ties.long$Tie.To=='skagit county public works'] = "skagit county"
ties.long$Tie.To[ties.long$Tie.To=='skagit fisheries enhancement'] = "skagit fisheries enhancement group"
ties.long$Tie.To[ties.long$Tie.To=='skagit conservation & education alliance'] = "skagit conservation education alliance"
ties.long$Tie.To[ties.long$Tie.To=='skagit watershed'] = "skagit watershed council"
ties.long$Tie.To[ties.long$Tie.To=='skagit inhancement group'] = "skagit fisheries enhancement group"
ties.long$Tie.To[ties.long$Tie.To=='wsu skagit county beach watchers'] = "wsu beach watchers"
ties.long$Tie.To[ties.long$Tie.To=='skagit beachwatchers'] = "wsu beach watchers"


ties.long$Tie.To[ties.long$Tie.To=='pierce county septic repair program'] = "pierce county"
ties.long$Tie.To[ties.long$Tie.To=='pierce county swm'] = "pierce county"
ties.long$Tie.To[ties.long$Tie.To=='pierce county surface water management'] = "pierce county"
ties.long$Tie.To[ties.long$Tie.To=='pierce conservation district'] = "pierce county conservation district"
ties.long$Tie.To[ties.long$Tie.To=='pierce counservation district'] = "pierce county conservation district"
ties.long$Tie.To[ties.long$Tie.To=='pierce conservation district/stream team'] = "pierce county stream team"
ties.long$Tie.To[ties.long$Tie.To=='pierce conservation district stream team'] = "pierce county stream team"
ties.long$Tie.To[ties.long$Tie.To=='pierce stream team'] = "pierce county stream team"

ties.long$Tie.To[ties.long$Tie.To=='san juan co mrc'] = "san juan county mrc"
ties.long$Tie.To[ties.long$Tie.To=='san juan mrc'] = "san juan county mrc"
ties.long$Tie.To[ties.long$Tie.To=='san juan county marine resources committee'] = "san juan county mrc"
ties.long$Tie.To[ties.long$Tie.To=='sj mrc'] = "san juan county mrc"

ties.long$Tie.To[ties.long$Tie.To=='san juan marine resources committee'] = "san juan county mrc"
ties.long$Tie.To[ties.long$Tie.To=='san juan islands conservation district'] = "san juan county conservation district"
ties.long$Tie.To[ties.long$Tie.To=='san juan econet'] = "san juan eco net"
ties.long$Tie.To[ties.long$Tie.To=='san juan lead entity'] = "san juan lio"
ties.long$Tie.To[ties.long$Tie.To=='wria ii lead entity'] = "san juan lio"
ties.long$Tie.To[ties.long$Tie.To=='san juan county - land bank and public works'] = "san juan county land bank"
ties.long$Tie.To[ties.long$Tie.To=='san juan county public works'] = "san juan county"
ties.long$Tie.To[ties.long$Tie.To=='san juan county council'] = "san juan county"
ties.long$Tie.To[ties.long$Tie.To=='san juan island land bank'] = "san juan county land bank"
ties.long$Tie.To[ties.long$Tie.To=='san juan island preservation trust'] = "san juan preservation trust"
ties.long$Tie.To[ties.long$Tie.To=='san juan county park'] = "san juan county"

ties.long$Tie.To[ties.long$Tie.To=='island county departments of planning, health and public works'] = "island county"

ties.long$Tie.To[ties.long$Tie.To=='island county environmental health'] = "island county"
ties.long$Tie.To[ties.long$Tie.To=='island cty m r c'] = "island county mrc"
ties.long$Tie.To[ties.long$Tie.To=='island cty beachwetchers'] = "wsu beachwatchers"


ties.long$Tie.To[ties.long$Tie.To=='washington dep. fish & wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='washington dept fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='washington dept of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='washington dept. of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='washington department of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='washington state department of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='wa dept fish & wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='wa dept of fish & wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='wa dept of fish and wildlife'] = "wdfw"
ties.long$Tie.To[ties.long$Tie.To=='wa department of fish and wildlife'] = "wdfw"


ties.long$Tie.To[ties.long$Tie.To=='washington state department of health office of shellfish and water protection'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='washington state department of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='washington department of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='u. washington'] = "uw"

ties.long$Tie.To[ties.long$Tie.To=='washington state department of natural resources'] = "wdnr"



ties.long$Tie.To[ties.long$Tie.To=='wa sea grant (upcoming)'] = "washington sea grant"
ties.long$Tie.To[ties.long$Tie.To=='washington seagrant'] = "washington sea grant"
ties.long$Tie.To[ties.long$Tie.To=='u. washington'] = "uw"
ties.long$Tie.To[ties.long$Tie.To=='u of wa school of environmental and forest sciences'] = "uw"
ties.long$Tie.To[ties.long$Tie.To=='washington department of natural resources'] = "wdnr"

ties.long$Tie.To[ties.long$Tie.To=='washington department of transportation'] = "wdot"

ties.long$Tie.To[ties.long$Tie.To=='earth corps'] = "earthcorps"
ties.long$Tie.To[ties.long$Tie.To=='sj watershed'] = "wria 2"
ties.long$Tie.To[ties.long$Tie.To=='wria ii'] = "wria 2"

ties.long$Tie.To[ties.long$Tie.To=='stillaguamish clean water district advisory board'] = "stillaguamish clean water advisory board"
ties.long$Tie.To[ties.long$Tie.To=='stillaguamish river clean water district board'] = "stillaguamish clean water advisory board"
ties.long$Tie.To[ties.long$Tie.To=='stillaguamish river clean water district advisory board'] = "stillaguamish clean water advisory board"
ties.long$Tie.To[ties.long$Tie.To=='stillaguamish river cwdab'] = "stillaguamish clean water advisory board"
ties.long$Tie.To[ties.long$Tie.To=='stillaguamish watershed council- technical advisory group'] = "stillaguamish watershed council"
ties.long$Tie.To[ties.long$Tie.To=='stillaguamish le'] = "stillaguamish lio"
ties.long$Tie.To[grep('people for',ties.long$Tie.To)] <- 'washington environmental council'
ties.long$Tie.To[ties.long$Tie.To=='shoreline'] = "city of shoreline"

ties.long$Tie.To[ties.long$Tie.To=='samish indian nation'] = "samish tribe"
ties.long$Tie.To[ties.long$Tie.To=='salmon funding recovery board'] = "salmon recovery funding board"
ties.long$Tie.To[ties.long$Tie.To=='puyallup tribe of indians'] = "puyallup tribe"
ties.long$Tie.To[ties.long$Tie.To=='puyallup river watershed council'] = "puyallup watershed council"


ties.long$Tie.To[ties.long$Tie.To=='south puget sound salmon enhancement'] = "south puget sound salmon enhancement group"
ties.long$Tie.To[ties.long$Tie.To=='south puget sound seg'] = "south puget sound salmon enhancement group"
ties.long$Tie.To[ties.long$Tie.To=='south sound salmon enhancement group'] = "south puget sound salmon enhancement group"
ties.long$Tie.To[ties.long$Tie.To=='nooksack salmon enhancement association.'] = "nooksack salmon enhancement association"
ties.long$Tie.To[ties.long$Tie.To=='us forest service'] = "usfs"
ties.long$Tie.To[ties.long$Tie.To=='united states forest service'] = "usfs"
ties.long$Tie.To[ties.long$Tie.To=='us navy keyport'] = "us navy"
ties.long$Tie.To[ties.long$Tie.To=='us navy (envvest)'] = "us navy"
ties.long$Tie.To[ties.long$Tie.To=='us fish and wildlife service'] = "usfws"

ties.long$Tie.To[ties.long$Tie.To=='u.s. epa'] = "epa"
ties.long$Tie.To[ties.long$Tie.To=='usepa'] = "epa"
ties.long$Tie.To[ties.long$Tie.To=='us epa'] = "epa"
ties.long$Tie.To[ties.long$Tie.To=='epa region 10'] = "epa"

ties.long$Tie.To[ties.long$Tie.To=='kitsap county departments of community development and public works'] = "kitsap county"
ties.long$Tie.To[ties.long$Tie.To=='kitsap department of community development'] = "kitsap county"

ties.long$Tie.To[ties.long$Tie.To=='nooksack indian tribe'] = "nooksack tribe"
ties.long$Tie.To[ties.long$Tie.To=='nooksack natural resources department'] = "nooksack tribe"

ties.long$Tie.To[ties.long$Tie.To=='wa dept of health'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='wa doh beach program'] = "wdoh"
ties.long$Tie.To[ties.long$Tie.To=='wa-dept natural resources'] = "wdnr"
ties.long$Tie.To[ties.long$Tie.To=='us fish & wildlife service'] = "usfws"
ties.long$Tie.To[ties.long$Tie.To=='us forest service pnw research station'] = "usfs"
ties.long$Tie.To[ties.long$Tie.To=='tnc'] = "the nature conservancy"
ties.long$Tie.To[ties.long$Tie.To=='usda natural resources conservation service'] = "nrcs"

ties.long$Tie.To[ties.long$Tie.To=='tulalip tribes'] = "tulalip tribe"
ties.long$Tie.To[ties.long$Tie.To=='suquamish tribe dungeness crab megalops study'] = "suquamish tribe"

ties.long$Tie.To[ties.long$Tie.To=='stream team/ city & county water resources dept'] = "thurston stream steam"
ties.long$Tie.To[ties.long$Tie.To=='stream team'] = "thurston stream steam"
ties.long$Tie.To[ties.long$Tie.To=='stream team: lacey, olympia, tumwater, tc'] = "thurston stream steam"

ties.long$Tie.To[ties.long$Tie.To=='state dept of health-shellfish division'] = "wdoh"

ties.long$Tie.To[ties.long$Tie.To=='sound salmon solutions (as a consultant)'] = "sound salmon solutions"
ties.long$Tie.To[ties.long$Tie.To=='usda natural resources conservation service'] = "nrcs"

ties.long$Tie.To[ties.long$Tie.To=='seattle parks and recreationn'] = "seattle parks & recreation"

ties.long$Tie.To = gsub('wsu extension','wsux',ties.long$Tie.To)
ties.long$Tie.To[ties.long$Tie.To=='wsux -- multiple counties in puget sound'] = "wsux"

#######

ties.long$Organization = base.row$Organization[match(ties.long$uq.ID,base.row$uq.ID)]
ties.long.uq = ties.long[!duplicated(paste(ties.long$Organization,ties.long$Tie.To)),]
ties.long.uq = ties.long.uq %>% filter(Tie.To != Organization)

is.member = filter(member.df,Member==1)
is.member$Organization = base.row$Organization[match(is.member$uq.ID,base.row$uq.ID)]

access.df$Organization = base.row$Organization[match(access.df$uq.ID,base.row$uq.ID)]
access.df$Number.Of.Groups = base.row$Number.Of.Groups[match(access.df$uq.ID,base.row$uq.ID)]

org.member = is.member[!duplicated(paste(is.member$Group,is.member$Organization)),]

org.member$brokerage.score = NA
for (i in 1:nrow(org.member))
{
  temp.membership = org.member %>% filter(Group == org.member$Group[i])
  temp = ties.long.uq %>% filter(Organization == org.member$Organization[i]|Tie.To == org.member$Organization[i])
  alters = union(temp$Tie.To,temp$Organization)[union(temp$Tie.To,temp$Organization)!=org.member$Organization[i]]
  org.member$brokerage.score[i] = sum(alters %in% temp.membership$Organization) * sum(alters %in% temp.membership$Organization == F)
}

org.member$bimodal.brokerage.score = NA

group.combos = expand.grid(unique(org.member$Group),unique(org.member$Group)) %>% filter(Var1 != Var2)

for (i in 1:nrow(group.combos))
{
  g1 = org.member %>% filter(Group == group.combos$Var1[i])
  g2 = org.member %>% filter(Group == group.combos$Var2[i])
  g1.uq = g1 %>% filter(Organization %in% intersect(g1$Organization, g2$Organization)==F)
  g2.uq = g2 %>% filter(Organization %in% intersect(g1$Organization, g2$Organization)==F)
  group.combos$bimodal.brokerage.reach[i] = nrow(g1.uq) * nrow(g2.uq)
}

  for (i in org.member$Organization)
{
   sub.member = org.member %>% filter(Organization == i)
   for (j in unique(sub.member$Group))
   {
   temp.combos = group.combos %>% filter(Var1 == j,Var2 %in% sub.member$Group)
   org.member$bimodal.brokerage.score[org.member$Group==j&org.member$Organization==i] = sum(temp.combos$bimodal.brokerage.reach)
   }
}

org.member$ties.to.other.members = NA
for (i in 1:nrow(org.member))
{
  temp.membership = org.member %>% filter(Group == org.member$Group[i])
   temp.ties = ties.long.uq %>% filter(Organization == org.member$Organization[i]|Tie.To == org.member$Organization[i])
   org.member$ties.to.other.members[i] = sum(union(temp.ties$Tie.To,temp.ties$Organization)[union(temp.ties$Tie.To,temp.ties$Organization)!=org.member$Organization[i]] %in% temp.membership$Organization)
}


group.brokerage.median.df = data.frame(group.brokerage.median = tapply(org.member$brokerage.score,org.member$Group,median),Group = sort(unique(org.member$Group)))

org.member$diff.from.group.brok.median = org.member$brokerage.score - group.brokerage.median.df$group.brokerage.median[match(org.member$Group,group.brokerage.median.df$Group)]

access.df$bimodal.brokerage.score = org.member$bimodal.brokerage.score[match(paste(access.df$Group,access.df$Organization),paste(org.member$Group,org.member$Organization))]
access.df$brokerage.score = org.member$brokerage.score[match(paste(access.df$Group,access.df$Organization),paste(org.member$Group,org.member$Organization))]
access.df$ties.to.other.members = org.member$ties.to.other.members[match(paste(access.df$Group,access.df$Organization),paste(org.member$Group,org.member$Organization))]
access.df$diff.from.group.brok.median = org.member$diff.from.group.brok.median[match(paste(access.df$Group,access.df$Organization),paste(org.member$Group,org.member$Organization))]

part.df = raw.df[,grep('Please describe your regular level of involvement in the following group activities',names(raw.df)),with=F]
part.df = part.df[,!duplicated(names(part.df)),with=F] 
part.df = as.data.frame(part.df)
part.df = part.df[,grep('None of the above|One or more',names(part.df),invert=T)]
part.df = part.df[,grep('^.*\\|.*\\|.*\\|.*$',names(part.df))]
part.df$uq.ID = raw.df$`Response ID`



part = gather(part.df,key = Group.Type,value=Participation,-uq.ID)
part$Group = gsub('Please describe your regular level of involvement in the following group activities for the collaborative groups in which you participate. For each activity and group you participate in, please check the box if you regularly engage in the specified action.\\|','',part$Group.Type)
part$Group = gsub('\\|.*','',part$Group)
part$Action = gsub('.+\\|','',part$Group.Type)
  
part.sum = data.frame(group.part.sum = tapply(part$Participation,paste(part$uq.ID,part$Group),sum))
part.sum$group.id = rownames(part.sum)

access.df$Participation =  as.numeric(as.character(part.sum$group.part.sum[match(paste(access.df$uq.ID,access.df$Group),part.sum$group.id)]))

#access.df$brokerage.score = as.numeric(scale(access.df$brokerage.score,scale=F,center=T))
#access.df$diff.from.group.brok.median = as.numeric(scale(access.df$diff.from.group.brok.median,scale=F,center=T))
#access.df$bimodal.brokerage.score = as.numeric(scale(access.df$bimodal.brokerage.score,scale=F,center=T))/100
#access.df$ties.to.other.members = as.numeric(scale(access.df$ties.to.other.members,scale=F,center=T))

access.df = access.df %>% select(-Question) %>% mutate(group.resource = paste(Group,Short))

access.df = left_join(access.df,member.df)

access.df = left_join(access.df,data.frame(uq.ID = raw.df$`Response ID`,years.at.org = raw.df$`How many years have you worked at or volunteered for the organization that you identified above?|Years`))


reason.df = data.table(uq.ID = raw.df$`Response ID`,raw.df[,grep('Please describe the reason(s)',names(raw.df),fixed=T),with=F])
reason.df = reason.df[,!duplicated(colnames(reason.df)),with=F]
reason.df = gather(reason.df,Option, Answer,-uq.ID)
reason.df$Reason = (gsub('.*\\|','',reason.df$Option))
reason.df$Group = gsub('\\|.*$','',gsub('^.*?\\|','',reason.df$Option))
reason.df = reason.df[grep('One or more',reason.df$Group,invert=T),]
reason.df = reason.df %>% filter(Answer==1) %>% select(-Answer,-Option)
access.df = left_join(access.df,reason.df)

write.csv(access.df,'input/proj3/resource.access.long.file.csv')



