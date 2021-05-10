require(tidyverse)
require(ggplot2)
require(haven)
require(stringr)
require(httr)
require(jsonlite)
require(quanteda)
require(LSX)
require(robustbase)
require(stargazer)

### Data cleaning and reduction

# Get helpers
source("helpers.R")

# Pull databases and merge
scdb <- rbind(read.csv("SCDB_justicecentered.csv"),read.csv("SCDB_legacy_justicecentered.csv"))

jdb <- read_xpt("http://epstein.wustl.edu/research/justicesdata.v8xpt")

merged <- merge(scdb,jdb,by.x="justice",by.y="spaethid")

# Pull president term data and convert strings to Date objects
## This allows us to check under which President a case was argued (done using a helper function in the other file)
presidents <- read.csv("presidential.csv")
presidents$start <-as.Date(presidents$start,"%m/%d/%Y")
presidents$end <-as.Date(presidents$end,"%m/%d/%Y")

# Merge most recent Segal-Cover scores (retrieved here: http://scdb.wustl.edu/data.php?s=5)
segal <- read.csv("measures.csv") 
merged <- merge(merged, subset(segal,select=-c(justiceName)), by=c("justice","term")) 

# Merge most recent DW-NOMINATE scores for Presidents (retrieved from link below)
nominate<-read.csv("https://voteview.com/static/data/out/members/HSall_members.csv")
presidents <- cbind(presidents,nominate %>%  filter(chamber == "President") %>% slice(-(1:67)) %>% select(c(bioname,nominate_dim1)) %>% separate(bioname,c("bioname",NA),",") %>% mutate(bioname = tolower(bioname)) %>% group_by(bioname, nominate_dim1) %>% distinct())

# Import salience measures
salience <- read.csv("scdbSalience.csv")
merged <- merged %>% inner_join(salience, by="caseId")

# Create subset of all cases where federal government was a party, create variables, ensure that only one row is preserved for each vote
## fed_pet: 1 = federal gov. is petitioner, 0 = respondent
## fed_win: 1 = Court rules for federal gov., 0 = Court rules against federal gov.

relevant_parties <- c(1,27,304,316,324,325,326,327,328,329,330,362,363,364,371,408,422)

fed_cases <- merged %>% filter((respondent %in% relevant_parties | petitioner %in% relevant_parties) & !(dateArgument=="")) %>%
  mutate(dateArgument = as.Date(dateArgument,"%m/%d/%Y"),
         fed_pet = as.numeric(petitioner %in% relevant_parties), 
         fed_win = ifelse((fed_pet==1 & partyWinning == 1) | (fed_pet==0 & partyWinning==0), 1, 
                          ifelse((fed_pet==1 & partyWinning == 0) | (fed_pet==0 & partyWinning == 1), 0, NA))) %>% distinct(voteId,.keep_all=T) %>% filter(dateArgument > as.Date("1920", "%Y"))

# Tune ideological measures and calculate absolute value
## exec_nom is the DW-NOMINATE score for the President in office when the case was heard
## The Segal-Cover score is 0 for conservatives and 1 for liberals, while NOMINATE's ideological poles are reversed.
## Thus, I flip the Segal-Cover score so both measures are oriented the same way and then calculate the absolute value between them.
fed_cases <- fed_cases %>% mutate(exec_nom = fed_checker(dateArgument, "nominate_dim1"), scIdeology = (-1*scIdeology), diff = abs(scIdeology - exec_nom))

# Add variables and remove cases where justices did not author/co-author decision
## fed_par: 1 = Democratic executive, 0 = Republican executive
## republican/democrat = 1 = Justice nominated by president of that party, 0 = not
## fed_vote: 1 = Justice sided with federal gov., 0 = Justice sided against federal gov.
fed_cases <- fed_cases %>% mutate(fed_par=fed_checker(dateArgument, "party"),
                                  era = fed_checker(dateArgument, "sec"),
                                  fed_dem=ifelse(fed_par=="Democratic",1,0), 
                                  republican=ifelse(parnom==6,1,0),democrat=ifelse(parnom==1,1,0), 
                                  fed_vote=ifelse((fed_win == 1 & majority == 2) | (fed_win == 0 & majority == 1),1,0)) %>% filter(opinion %in% c(2,3))

# Create executive experience covariate
fed_cases <- fed_cases %>% mutate(exec_exp = ifelse(usasat1 > 999 | usat1 > 999 | ussgo1 > 999 | ussg1 > 999 | usago1 > 999 | usag1 > 999 | uscab1 > 999 | ussec1 > 999 | usage1 > 999 | usagh1 > 999 | uspres1 > 999, 1, 0))

# Subset cases into those where Justice-Executive were of opposing parties + of same party, create respective variables
opp_cases <- fed_cases %>% filter((democrat==1 & fed_dem==0)|(republican==1 & fed_dem == 1)) %>% mutate(opp_case = 1, same_case = 0)
same_cases <- fed_cases %>% filter((democrat==1 & fed_dem==1)|(republican==1 & fed_dem == 0)) %>% mutate(opp_case = 0, same_case = 1)

### Modeling

# Create dictionary

# Original dictionary used to identify words, later dictionary was developed after picking out n-grams or clearly incorrect terms that got picked up (e.g. "great_britain")
## dict <- as.dictionary(dict <- data.frame(word=c("bad*", "awful*", "negativ*", "horribl*", "wrong*", "terribl*", "poor*",  "incorrect", "err*", "fail*", "brilliant*", "great*", "terrific*", "wonderful*", "splendid*", "good*", "fantastic*", "excellent*", "correct*",  "positiv*"), sentiment=c("negative","negative","negative","negative","negative","negative","negative","negative","negative","negative","negative","positive","positive","positive","positive","positive","positive","positive","positive","positive","positive","positive")))
dict <- data.frame(word = c("accurate", "accurately", "brilliant", "brilliantly", "wonderful", "wonderfully", "excellent", "excellently", "good", "great", "greatly", "positive", "positively", "correct", "correctly", "terrific", "fortunate", "fortunately", "superior", "nice", "nicely"), sentiment="positive")
dict <- rbind(dict, data.frame(word=c("erred", "err", "negatively", "fail", "poorly", "negative", "terrible", "failure", "bad", "badly", "poor", "failing", "errors", "negatives", "error", "incorrect", "wrong", "erroneous", "wrongly", "errs", "failures", "fails", "failed", "erroneously"), sentiment = "negative"))
dict <- as.dictionary(dict)

# Combine the two previously subset dataframes back into one for LSS modeling
## LSS rescales fits on the corpus to have mean 0 and std. dev. 1, so we can't just predict each subset separately.
total_cases <- rbind(opp_cases, same_cases)

# Fit model for k-cohesion plot with very high k
lss_k <- fit_model(total_cases, as.seedwords(dict, upper=-1, lower=1), "the_government", 600)

# Fit model using helper function
lss_gov_comb <- fit_model(total_cases, as.seedwords(dict, upper=-1, lower=1), "the_government", 430)

# Predict model using helper function
total_pred <- predict_model(total_cases, lss_gov_comb)

# Create updated dataframe with predictions, smoothed set, and split by exec-justice partisanship for smoothing
fitted_df <- total_pred[[1]]

fitted_smoothed <- total_pred[[2]]

fitted_opp <- smooth_lss(total_pred[[1]] %>% filter(opp_case == 1), lss_var="fit", engine="loess",date="dateArgument")

fitted_same <- smooth_lss(total_pred[[1]] %>% filter(opp_case == 0), lss_var="fit", engine="loess",date="dateArgument")

### Graphics

# Sentiment time-series for all texts
fitted_smoothed %>% ggplot(aes(date,fit)) + geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), fill="grey80") + geom_line(color="black") +  geom_hline(yintercept=0,linetype="dotted") + geom_rect(data=presidents,aes(xmin=start,xmax=end,ymin=-Inf,ymax=Inf,fill=party),alpha=.2,inherit.aes=F) + scale_fill_manual(values=alpha(c("blue","red"),.1),name = "Executive Party") + coord_cartesian(ylim=c(-1,1), xlim=c(as.Date("1946", "%Y"), as.Date("2009","%Y")))

# Sentiment time-series for opposing executive-justice texts
fitted_opp %>% ggplot(aes(date,fit)) + geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), fill="grey80") + geom_line(color="black") +  geom_hline(yintercept=0,linetype="dotted") + geom_rect(data=presidents,aes(xmin=start,xmax=end,ymin=-Inf,ymax=Inf,fill=party),alpha=.2,inherit.aes=F) + scale_fill_manual(values=alpha(c("blue","red"),.1),name = "Executive Party") + coord_cartesian(ylim=c(-1,1), xlim=c(as.Date("1946", "%Y"), as.Date("2009","%Y")))

# Sentiment time-series for same executive-justice texts
fitted_same %>% ggplot(aes(date,fit)) + geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), fill="grey80") + geom_line(color="black") +  geom_hline(yintercept=0,linetype="dotted") + geom_rect(data=presidents,aes(xmin=start,xmax=end,ymin=-Inf,ymax=Inf,fill=party),alpha=.2,inherit.aes=F) + scale_fill_manual(values=alpha(c("blue","red"),.1),name = "Executive Party") + coord_cartesian(ylim=c(-1,1), xlim=c(as.Date("1946", "%Y"), as.Date("2009","%Y")))

# Justice mean sentiment in opposing cases
## Using what I remember from statistics, I take the document standard errors, square them, sum them by justice, divide by the number of documents squared, and then take the square root of the resultant variance to get standard errors for calculating confidence intervals.
## I use the same strategy for the confidence intervals of all grouped means.
fitted_df %>% filter(!is.na(var.fit), opp_case == 1) %>% group_by(name) %>% dplyr::summarize(mean = mean(fit, na.rm=TRUE),var.sum=sum(var.fit),n=n()) %>% filter(n>15) %>% ggplot(aes(reorder(name,-mean,sum),mean,ymin=mean-(1.96*(sqrt(var.sum/(n^2)))),ymax=mean+1.96*(sqrt(var.sum/(n^2)))))+ geom_pointrange() +geom_hline(yintercept=0, lty=2) + xlab("Justice") + ylab("Sentiment") + coord_flip()

# Justice mean fit in same cases
fitted_df %>% filter(!is.na(var.fit), opp_case == 0) %>% group_by(name) %>% dplyr::summarize(mean = mean(fit, na.rm=TRUE),var.sum=sum(var.fit),n=n()) %>% filter(n>15) %>% ggplot(aes(reorder(name,-mean,sum),mean,ymin=mean-(1.96*(sqrt(var.sum/(n^2)))),ymax=mean+1.96*(sqrt(var.sum/(n^2)))))+ geom_pointrange() +geom_hline(yintercept=0, lty=2) + xlab("Justice") + ylab("Sentiment") + coord_flip()

# Justice mean fit in all cases
fitted_df %>% filter(!is.na(var.fit)) %>% group_by(name) %>% dplyr::summarize(mean = mean(fit, na.rm=TRUE),var.sum=sum(var.fit),n=n()) %>% filter(n>50) %>% ggplot(aes(reorder(name,-mean,sum),mean,ymin=mean-(1.96*(sqrt(var.sum/(n^2)))),ymax=mean+1.96*(sqrt(var.sum/(n^2)))))+ geom_pointrange() +geom_hline(yintercept=0, lty=2) + xlab("Justice") + ylab("Sentiment") + coord_flip()

# Executive party "eras"
## This is just the time periods of partisan continuity in the executive
fitted_df %>% filter(!is.na(var.fit)) %>% group_by(era) %>% dplyr::summarize(mean = mean(fit, na.rm=TRUE),var.sum=sum(var.fit),n=n()) %>% filter(n>15) %>% ggplot(aes(reorder(era,-mean,sum),mean,ymin=mean-(1.96*(sqrt(var.sum/(n^2)))),ymax=mean+1.96*(sqrt(var.sum/(n^2)))))+ geom_pointrange() +geom_hline(yintercept=0, lty=2)  + coord_flip()

# Means in opposing cases vs. same cases
fitted_df %>% filter(!is.na(var.fit)) %>% group_by(opp_case) %>% dplyr::summarize(mean = mean(fit, na.rm=TRUE),var.sum=sum(var.fit),n=n()) %>% ggplot(aes(reorder(opp_case,-mean,sum),mean,ymin=mean-(1.96*(sqrt(var.sum/(n^2)))),ymax=mean+1.96*(sqrt(var.sum/(n^2)))))+ geom_pointrange() +geom_hline(yintercept=0, lty=2) + ylab("Sentiment") + xlab("Justice-Executive Partisanship") + scale_x_discrete(labels = c("Same Party", "Opposing Parties")) + coord_flip()

### Regression
# I use robust regressions (with the 'robustbase' package) as, despite LSS doing its best to fit sentiment onto a normal distribution, there are still enough outliers to justify robust regression here.
## One regression uses ideological distance from DW-NOMINATE/Segal-Cover measures, the other just uses the binary indicator for same/opposing exec-justice partisanship
robustreg_diff <- lmrob(fit~diff + fed_pet + nytSalience + exec_exp, family="gaussian", data = fitted_df)

robustreg_opp <- lmrob(fit~opp_case + fed_pet + nytSalience + exec_exp, family="gaussian", data = fitted_df)
