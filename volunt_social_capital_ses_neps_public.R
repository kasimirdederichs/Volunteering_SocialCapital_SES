
rm(list = ls())
setwd("Y:/Documents/")

# 1. LIBRARIES ####
library(haven)
library(plm)
library(flextable) #make nice tables
library(tidyfast)
library(readxl) #loading excel sheets
library(psych)
library(dplyr)
library(lme4) #multilevel models
library(margins)
library(forcats) #change order of factor levels
library(xlsx) #export tables to Excel
library(sjmisc) #rowwise data wrangling
library(patchwork) #wrapping plots
library(gridExtra) #wrapping plots
library(foreign) # for export to stata
library(weights) #for weighted tables (very basic weighted statistics)
library(lmtest) #lr-tests for multinomial models
library(DescTools) #calculate Pseudo R2
library(nnet) #multinomial regression
library(tidyverse)
library(car) #package for recoding and linear hypothesis test for multinomial models
library(lubridate) #package for handling of dates
library(tidyr)
library(effects) #for predicted probabilities and (CIs)
library(ggeffects) # for ggplot of predicted probabilities (and CIs)
library(zoo)
library(sensemakr) #sensitivity analysis
library(gtools) #stars.pval function
library(data.table) #for overview tables
library(ggpubr) #advanced ggplots
library(stargazer) #print regression tables
library(Hmisc) #package for labeling
# 2. DEFINE FUNCTIONS #####
#get legend for arranged plot:
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# 3. PREPARE DATASET ####
dat01 <- read_dta("03_data/03_edit/Stata14/SC6_pTarget_R_11-1-0.dta")
#keep relevant variables:
dat01 <- dat01 %>%
  select(ID_t, wave, t44630c, t44630d, t44630a, t44630b, t44613a, t700001, 
         t70000y, t70000m, inty, intm, t435000, tx20001, tx20002, tx20003,
         t405000, t751001_g4R, inty, intm,
         t321101, t521000, t733001,
         t405090_ha, t405060_ha, t405000_ha, #harmonized variables for father's, mother's, and own place of birth
         t405100_g2, t405070_g2, t405010_g2, #more detailed information on country of birth
         t32600a, t32600b, t32600c, t32600d, t32600e, t32600f, t32600g, #position generator
         t32600h, t32600k, t32600l, t32600m, t32600n, t32600o,
         t32601a_g2, t32601b_g2, t32601c_g2, t32601d_g2, t32601e_g2, t32601f_g2, t32601g_g2, #position generator - country
         t32601h_g2, t32601k_g2, t32601l_g2, t32601m_g2, t32601n_g2, t32601o_g2,
         t751001_g7, t751001_g4R, #residence (moving houses), municipal code, East/west Germany:  t751001_g1, (also comes from municip. data)
         t435000, t66800a_g1, t66800b_g1, t66800c_g1, t66800d_g1, t66800e_g1, # time-constant variables: religiosity, personality (big five)
         t517392, t517390 #volunteering instruments: potential (assessed only at wave 6), earlier volunt
         ) 

#migration background:
dat01 <- dat01 %>%
  mutate(cbirth = case_when(t405000_ha<0 ~ NA_real_, t405000_ha==1 ~ 0, t405000_ha==2 ~ 1), #own country of birth (0=Germany, 1=abroad)
         cbirth_f = case_when(t405090_ha<0 | is.na(t405090_ha) ~ NA_real_, t405090_ha==1 ~ 0, t405090_ha==2 ~ 1), #father's country of birth
         cbirth_m = case_when(t405060_ha<0 | is.na(t405060_ha) ~ NA_real_, t405060_ha==1 ~ 0, t405060_ha==2 ~ 1), #mother's country of birth
         migback = ifelse(cbirth==1 | cbirth_f==1 | cbirth_m==1, 1, 0),
         migback = case_when(cbirth==0 & cbirth_f==0 & cbirth_m==0 ~ 0,
                             cbirth==0 & cbirth_f==0 & is.na(cbirth_m) ~ 0,
                             cbirth==0 & is.na(cbirth_f) & cbirth_m==0 ~ 0,
                             cbirth==0 & is.na(cbirth_f) & is.na(cbirth_m) ~ 0,
                             cbirth==0 & cbirth_f==1 & cbirth_m==0 ~ 1,
                             cbirth==0 & cbirth_f==1 & is.na(cbirth_m) ~ 1,
                             cbirth==0 & cbirth_f==0 & cbirth_m==1 ~ 1,
                             cbirth==0 & is.na(cbirth_f) & cbirth_m==1 ~ 1,
                             cbirth==0 & cbirth_f==1 & cbirth_m==1 ~ 1,
                             cbirth==1 ~ 1,
                             is.na(cbirth) & cbirth_f==0 & cbirth_m==0 ~ 0,
                             is.na(cbirth) & cbirth_f==1 & cbirth_m==0 ~ 1,
                             is.na(cbirth) & cbirth_f==0 & cbirth_m==1 ~ 1,
                             is.na(cbirth) & cbirth_f==0 & is.na(cbirth_m) ~ 0,
                             is.na(cbirth) & cbirth_f==1 & is.na(cbirth_m) ~ 1,
                             is.na(cbirth) & is.na(cbirth_f) & cbirth_m==0 ~ 0,
                             is.na(cbirth) & is.na(cbirth_f) & cbirth_m==1 ~ 1)) #migration background if at either oneself or at least on of one's parents is born abroad, NA if no information
           
#gender:
dat01$woman <- NA
dat01$woman[dat01$t700001==1] <- 0
dat01$woman[dat01$t700001==2] <- 1

# Year of birth:
dat01$t70000y[dat01$t70000y<0] <- NA
#age (using the lubridate package):
dat01 <- dat01 %>%
  mutate(intdate=make_date(inty, intm)) %>% # convert into date format
  mutate(t70000m = case_when(t70000m>0 ~ t70000m, t70000m<0 ~ 6)) %>% #set birth month to June if missing to preserve cases. Will not matter in FE analysis anyways.
  mutate(birdate=make_date(t70000y, t70000m))
dat01$age <- as.period(interval(start=dat01$birdate, end=dat01$intdate))$year #start is date of birth, end is interview date $year keeps only year
dat01$agem <- as.numeric(as.period(interval(start=dat01$birdate, end=dat01$intdate)), "months") #variable that gives the age in months.
dat01$mdate <- dat01$intdate # to merge the occupation spells later on.


dat01 <- dat01 %>% as_tibble() #convert dataframe into tibble

#Potential for volunteering later on:
dat01 <- dat01 %>%
  mutate(volpot6 = case_when(t517392<0 ~ NA_real_,
                             t517392==1 ~ 0,
                             t517392==2 ~ 1/3,
                             t517392==3 ~ 2/3,
                             t517392==4 ~ 1),
         volearlier6 = case_when(t517390<0 ~ NA_real_,
                             t517390==3 ~ 0, # no previous volunteering at all
                             t517390==1 ~ 1, # one previous voluntary involvement
                             t517390==2 ~ 2)) # multiple previous vol involvements

#time-constant variables: religiosity and personality traits.
dat01[, c("religiosity", "b5extra", "b5agree", "b5consc", "b5neuro", "b5openn")] <- lapply(dat01[, c("t435000", "t66800a_g1", "t66800b_g1", "t66800c_g1", "t66800d_g1", "t66800e_g1")], 
                                                   function(x) recode(x, "-54=NA; -55=NA; -97=NA; -98=NA")) #auxillary value: 100

#carry variables forward in time, if still missing, carry backwards, i.e., "downup" (e.g. first fills downwards, if not possible, fills upwards)
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(volpot6, id=ID_t, .direction="downup") -> dat13
dat01$volpot6 <- dat13$volpot6
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(volearlier6, id=ID_t, .direction="downup") -> dat13
dat01$volearlier6 <- dat13$volearlier6
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(migback, id=ID_t, .direction="downup") -> dat13
dat01$migback <- dat13$migback
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(religiosity, id=ID_t, .direction="downup") -> dat13
dat01$religiosity <- dat13$religiosity
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5extra, id=ID_t, .direction="downup") -> dat13
dat01$b5extra <- dat13$b5extra
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5agree, id=ID_t, .direction="downup") -> dat13
dat01$b5agree <- dat13$b5agree
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5consc, id=ID_t, .direction="downup") -> dat13
dat01$b5consc <- dat13$b5consc
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5neuro, id=ID_t, .direction="downup") -> dat13
dat01$b5neuro <- dat13$b5neuro
dat01 %>%
  group_by(ID_t) %>%
  dt_fill(b5openn, id=ID_t, .direction="downup") -> dat13
dat01$b5openn <- dat13$b5openn

#Municipality & Moving
dat01 <- dat01 %>% rename(municip_number = t751001_g4R)
moves <- dat01 %>%
  filter(wave>=6 & wave<=10) %>%
  mutate(move610 = case_when(t751001_g7==1 ~ 1, is.na(t751001_g7) | t751001_g7!=1 ~ 0)) %>% # all missings on move-indicator are those who were interviewed for the first time -> no move!
  select(ID_t, move610) 
moves <- aggregate(move610 ~ ID_t, data = moves, max)
dat01 <- merge(dat01, moves, by=c("ID_t"), all.x = T, all.y = F)

# 4. highest educational degree: ####
educdat <- read_dta("03_data/03_edit/Stata14/SC6_Education_R_11-1-0.dta")
educdat <- educdat %>%
  filter(datey<=2014) %>% # only degrees that were obtained before the first wave of interest (i.e., 2013)
  group_by(ID_t) %>%
  mutate(educ_isced = tx28103) %>%
  top_n(1, tx28103) %>% #select highest educational degree for each individual according to ISCED
  filter(!duplicated(ID_t)) %>% # remove duplicates (e.g., when 2 degrees of same ISCED categories are mentioned)
  select(-tx28103)

dat01 <- merge(dat01, educdat, by="ID_t", all.x = T, all.y = F) #add information on highest educational degree to main data frame

# 5. position generator variables: ####
#overall variables:
#Vector with new position generator variables: (ordered by ISEI score)
pos_gen_var <- c("pg_trasp.29", "pg_mecha.34", "pg_nurse.38", "pg_sales.43", "pg_optic.48", "pg_polic.56",
                 "pg_banke.56", "pg_socwo.65", "pg_teach.66", "pg_trala.68", "pg_engin.73", "pg_docto.80", "pg_legal.85")
#old position generator variables: (ordered by ISEI score)
old_pos_gen_var <- c("t32600c", "t32600k", "t32600a", "t32600e", "t32600m", "t32600f",
                     "t32600h", "t32600d", "t32600o", "t32600n", "t32600b", "t32600g",  "t32600l")
isei_scores <- c(29, 34, 38, 43, 48, 56, 56, 65, 66, 68, 73, 80, 85) # isei scores 
names(isei_scores) <- pos_gen_var
#intuitive names for positions:
setnames(dat01, old = old_pos_gen_var, new = pos_gen_var)

#function that converts variables into position generator variables:
prep_pos_gen_vars <- function(old_var, value_no, value_yes){
  position <- case_when(old_var == value_no ~ 0, 
                       old_var == value_yes ~ 1, 
                       old_var>max(value_no, value_yes) | old_var<min(value_no, value_yes) | is.na(old_var) ~ NA_real_)
  return(position)
}
dat01 <- dat01 %>%
  mutate(across(all_of(pos_gen_var), ~ prep_pos_gen_vars(old_var = ., value_no = 2, value_yes = 1)))

#variables indicating the alter ISEI scores
dat01 <- dat01 %>% #auxiliary value: 0
  mutate(spg29 = case_when(pg_trasp.29==1~29, pg_trasp.29==0~0),
         spg34 = case_when(pg_mecha.34==1~34, pg_mecha.34==0~0),
         spg38 = case_when(pg_nurse.38==1~38, pg_nurse.38==0~0),
         spg43 = case_when(pg_sales.43==1~43, pg_sales.43==0~0),
         spg48 = case_when(pg_optic.48==1~48, pg_optic.48==0~0),
         spg56a = case_when(pg_polic.56==1~56, pg_polic.56==0~0),
         spg56b = case_when(pg_banke.56==1~56, pg_banke.56==0~0),
         spg65 = case_when(pg_socwo.65==1~65, pg_socwo.65==0~0),
         spg66 = case_when(pg_teach.66==1~66, pg_teach.66==0~0),
         spg68 = case_when(pg_trala.68==1~68, pg_trala.68==0~0),
         spg73 = case_when(pg_engin.73==1~73, pg_engin.73==0~0),
         spg80 = case_when(pg_docto.80==1~80, pg_docto.80==0~0),
         spg85 = case_when(pg_legal.85==1~85, pg_legal.85==0~0))

#variable suitable for calculating lower reach: if not known = 100
dat01[, c("spg29n", "spg34n", "spg38n", "spg43n", "spg48n", 
         "spg56an", "spg56bn", "spg65n", "spg66n", "spg68n", 
         "spg73n", "spg80n", "spg85n")] <- lapply(dat01[, c("spg29", "spg34", "spg38", "spg43", "spg48", 
                                                            "spg56a", "spg56b", "spg65", "spg66", "spg68", 
                                                            "spg73", "spg80", "spg85")], 
                                                  function(x) recode(x, "0=100; 1=1")) #auxillary value: 100

#Vectors for PG-variables
spg_vars <- c("spg29", "spg34", "spg38", "spg43", "spg48", "spg56a", "spg56b", "spg65", "spg66", "spg68", "spg73", "spg80", "spg85")

#classical measures of social capital: upper reach and diversity
dat01 <- dat01 %>%
  rowwise() %>%
  mutate(ureach = max(c(spg29, spg34, spg38, spg43, spg48, spg56a, spg56b, spg65, spg66, spg68, spg73, spg80, spg85)),
         lreach = min(c(spg29n, spg34n, spg38n, spg43n, spg48n, spg56an, spg56bn, spg65n, spg66n, spg68n, spg73n, spg80n, spg85n)),
         diversity = sum(pg_trasp.29, pg_mecha.34, pg_nurse.38, pg_sales.43, pg_optic.48, pg_polic.56,
                         pg_banke.56, pg_socwo.65, pg_teach.66, pg_trala.68, pg_engin.73, pg_docto.80, pg_legal.85),
         aprestige = sum(spg29 , spg34, spg38, spg43, spg48, spg56a, spg56b, 
                         spg65, spg66, spg68, spg73, spg80, spg85)/diversity)
dat01$ureach[dat01$ureach==0] <- NA # recode auxillary values to 0
dat01$lreach[dat01$lreach==100] <- NA

#function that reverses the rowwise grouping of the data frame:
ungroup.rowwise_df <- function(x) {
  class(x) <- c("tbl_df", "data.frame")
  x
}
dat01 <- ungroup.rowwise_df(dat01)

dat01$relwave <- 0
dat01$relwave[dat01$wave==6 | dat01$wave==10] <- 1


# 6. reduce dataset to two relevant waves #####################
wdat <- dat01 %>%
  filter(wave==6 | wave==10) %>%
  mutate(row_obs = 1)

#1. extract number of observations per ID_t
nrowsdat <- wdat %>%
  group_by(ID_t) %>%
  summarise(bothwaves = n()) %>%
  as.data.frame()

#2. merge this to existing data
wdat <- merge(wdat, nrowsdat, by="ID_t", all.x = T, all.y = F)

# 7. ALL SPELL DATA (Occupation, housework, unemployment, retirement) #########
wdat <- wdat %>% #prepare wdat: create variable for interview date:
  mutate(mdate = make_date(inty, intm),
         mdaten = as.numeric(mdate))
#dataset with dates of interviews which will be merged to spell data to determine changes between both observations. 
intdatedat <- wdat %>% 
  select(ID_t, wave, mdate) %>%
  reshape(idvar = "ID_t", timevar = "wave", direction = "wide")

spelldata <- read_dta("VOL_SC/03_data/episodensplit_ck_kr.dta")
# adjust time (R-counts with 1970-01-01 as natural zero, dataset counts 1960-01-01 as natural zero)
spelldata <- spelldata %>%
  mutate(startadjn = ymd(as.Date("1960-01-01")) %m+% months(start),
         endadjn = ymd(as.Date("1960-01-01")) %m+% months(end))

spelldata$spellid <- spelldata$ID_t * 100 + spelldata$epinr #Spell-ID variable

spellspanel <- uncount(spelldata, dur, .id = "spellid") #generate one obesrvation for each month of the spell
# now, the spell data has turned into panel data.

# generate identifier for month of each observation
spellspanel$mdate <- spellspanel$startadjn %m+% months(spellspanel$spellid - 1) #numeric identifier for each obs

spellspanel <- spellspanel %>%
  select(c(ID_t, sptype1, miltype1, schooltype1, traintype1, vocpreptype1, 
           alotype1, gaptype1, emptype1, saisonarbeit1, zeitarbeit1, 
           arbeitszeit1, isco081, egp1, isei081, siops081, mdate)) %>%
  mutate(mdaten = as.numeric(mdate)) 

spellspanel %>%
  group_by(ID_t) %>%
  dt_fill(isei081, id=ID_t, .direction="down") -> dat13 #use previous (and later) ISEI as proxy for ISEI
spellspanel$isei081 <- dat13$isei081


#merge:
wdat <- merge(wdat, spellspanel, by=c("ID_t", "mdate"), all.x = T, all.y = F)
wdat$sptype <- factor(wdat$sptype1, labels = c("school", "voc.preparation", 
                                               "voc.training", "military",
                                               "employment", "unemployment",
                                               "parental leave", "gap", "Dateneditionsl|cke"))

wdat <- wdat %>%
  mutate(occst = case_when(sptype=="school" | sptype=="voc.preparation" | sptype=="voc.training" | sptype=="military" ~ "in.educ",
                           sptype=="employment" ~ "employment",
                           sptype=="unemployment" ~ "unemployment",
                           sptype=="parental leave"  | (sptype=="gap" & (gaptype1==1 | gaptype1==3)) ~ "domestic.work",
                           sptype=="gap" & gaptype1==2 ~ "retirement",
                           sptype=="Dateneditionsl|cke" ~ "unknown"))
wdat <- wdat %>%
  mutate(occstfpt = case_when(sptype=="school" | sptype=="voc.preparation" | sptype=="voc.training" | sptype=="military" ~ "in.educ",
                              sptype=="employment" & arbeitszeit1>=30 & !is.na(arbeitszeit1) ~ "employment.fulltime",
                              sptype=="employment" & arbeitszeit1<30 & !is.na(arbeitszeit1) ~ "employment.parttime",
                              sptype=="unemployment" ~ "unemployment",
                              sptype=="parental leave"  | (sptype=="gap" & (gaptype1==1 | gaptype1==3)) ~ "domestic.work",
                              sptype=="gap" & gaptype1==2 ~ "retirement",
                              sptype=="Dateneditionsl|cke" ~ "unknown"))

# 8. CONTROL VARIABLES FROM SPELL DATA ####
#8.1. partner
sp_partner <- read_dta("03_data/03_edit/Stata14/SC6_spPartner_R_11-1-0.dta")
new_partner <- sp_partner %>%
  filter(subspell==0 & wave>=6 & wave<=10) %>% #no subspells
  select(ID_t, partner, wave) %>%
  group_by(ID_t) %>%
  mutate(nrpartners=n(),
         new_partner610 = case_when(nrpartners<=1~0, nrpartners>=2~1)) %>%
  select(ID_t, new_partner610)

new_partner = new_partner[!duplicated(new_partner),] # remove duplicates

wdat <- merge(wdat, new_partner, by=c("ID_t"), all.x = T, all.y = F)
wdat$new_partner610[is.na(wdat$new_partner610)] <- 0 #those who do not appear in sp_partner never had a partner, hence no new partner
quit_partner <- sp_partner %>%
  filter(subspell==0 & wave>=6 & wave<=10) %>%
  select(ID_t, partner, wave, ts31510) %>%
  group_by(ID_t) %>%
  mutate(quit_partner_sp=case_when(is.na(ts31510) | ts31510<0 | ts31510>=4 ~ 0, #on spell-level: did the relationship end?
                                ts31510==1 | ts31510==2 | ts31510==3 ~ 1))
quit_partner = quit_partner %>%
  group_by(ID_t) %>%
  summarise(quit_partner610 = max(quit_partner_sp))

wdat <- merge(wdat, quit_partner, by=c("ID_t"), all.x = T, all.y = F)
wdat$quit_partner610[is.na(wdat$quit_partner610)] <- 0 #those who do not appear in sp_partner never had a partner, hence did not quit a relationship

# 8.2. child birth: 
childdat <- read_dta("03_data/03_edit/Stata14/SC6_Children_R_11-1-0.dta")
childdat <- merge(childdat, intdatedat, by=c("ID_t"), all.x = T, all.y = F) #determine whether job starts and ends occur between the two interviews

childdat <- childdat %>%
  mutate(date_childbirth = make_date(tx2710y, tx2710m),
         childbirth610 = case_when(date_childbirth>mdate.6 & date_childbirth<mdate.10 ~ 1,
                                   date_childbirth<mdate.6 & date_childbirth>mdate.10 ~ 0)) %>%
  filter(childbirth610==1) %>% #extract only those who had a child born between the two interviews. 
  select(ID_t, childbirth610) %>%
  unique() # each ID_t only once

wdat <- merge(wdat, childdat, by = c("ID_t"), all.x = T, all.y = F)
wdat$childbirth610[is.na(wdat$childbirth610)] <- 0 # those who do not appear childbirth data

# 8.3. job changes:
sp_emp <- read_dta("03_data/03_edit/Stata14/SC6_spEmp_R_11-1-0.dta")
sp_emp <- merge(sp_emp, intdatedat, by=c("ID_t"), all.x = T, all.y = F) #determine whether job starts and ends occur between the two interviews
sp_emp <- sp_emp %>%
  filter(subspell==0) %>%
  select(ID_t, wave, ts2311y, ts2311m, ts2312y, ts2312m, mdate.6, mdate.10) %>%
  mutate(emp_start = make_date(ts2311y, ts2311m),
         emp_end = make_date(ts2312y, ts2312m),
         jobsta610 = case_when(emp_start>mdate.6 & emp_start<mdate.10 ~ 1,
                              is.na(emp_start) | emp_start<=mdate.6 | emp_start>=mdate.10 ~ 0),
         jobend610 = case_when(emp_end>mdate.6 & emp_end<mdate.10 ~1,
                               is.na(emp_end) | emp_end<=mdate.6 | emp_end>=mdate.10 ~0))
startend_emp <- sp_emp %>%
  group_by(ID_t) %>%
  summarise(start_emp610 = max(jobsta610), 
            end_emp610 = max(jobend610))

wdat <- merge(wdat, startend_emp, by = c("ID_t"), all.x = T, all.y = F)
wdat$start_emp610[is.na(wdat$start_emp610)] <- 0 # those who do not appear in sp_emp or have missing values on start_emp610/end_emp610 have have not been employed at all between wave 6 and 10, hence, no start/end of job. 
wdat$end_emp610[is.na(wdat$end_emp610)] <- 0

# 9. MUNICIPALITY LEVEL DATA ####
#main dataset on municipalities:
load("03_data/03_edit/DESTATIS/municipality_data_import.Rda")

#create smaller municipality dataset for merge:
mmerge <- municipalities %>%
  select(c(number, populationdensity)) %>%
  mutate(east = case_when(number<11000 ~ 0,
                          number>=11000 ~ 1),
         populationdensity = log(populationdensity))
#merge municipality data on individual data:
wdat <- merge(wdat, mmerge, by. = "municip_number", by.y="number", all.x = T, all.y = F)

# 10. select relevant variables and reshape to wide format ####
ww <- wdat%>% filter(bothwaves==2 & !is.na(isei081) & !is.na(ureach) & !is.na(lreach) &
                       !is.na(diversity))  %>% #keep only individuals observed in both waves and with nonmissing values on rel. variables / omit those with is.na(avdis)??
  select(ID_t, wave, pg_trasp.29, pg_nurse.38, pg_mecha.34, pg_sales.43, pg_optic.48, pg_polic.56,
         pg_banke.56, pg_socwo.65, pg_teach.66, pg_trala.68, pg_engin.73, pg_docto.80, pg_legal.85, 
         isei081, bothwaves, 
         ureach, lreach, diversity, aprestige,
         volpot6, volearlier6,
         populationdensity, east,
         move610, new_partner610, quit_partner610, start_emp610, end_emp610, childbirth610, agem, # time-varying variables
         religiosity, migback, b5extra, b5agree, b5consc, b5neuro, b5openn, woman, age, educ_isced) # time-constant variables

###

ww <- reshape(ww,
              direction = "wide",
              v.names = c("pg_trasp.29", "pg_mecha.34", "pg_nurse.38", "pg_sales.43", "pg_optic.48", "pg_polic.56",
                          "pg_banke.56", "pg_socwo.65", "pg_teach.66", "pg_trala.68", "pg_engin.73", "pg_docto.80", "pg_legal.85",
                          "ureach", "lreach", "diversity", "aprestige", 
                          "isei081", "religiosity", "migback", "b5extra", "b5agree", "b5consc", "b5neuro", "b5openn", "age", "agem", "populationdensity", "east"),
              timevar = "wave",
              idvar = "ID_t") %>%
  mutate(isei081=isei081.6, religiosity=religiosity.6, b5extra=b5extra.6, b5agree=b5agree.6, b5consc=b5consc.6, b5neuro=b5neuro.6, b5openn=b5openn.6) %>%
  select(-c(isei081.6, religiosity.6, b5extra.6, b5agree.6, b5consc.6, b5neuro.6, b5openn.6, isei081.10, religiosity.10, b5extra.10, b5agree.10, b5consc.10, b5neuro.10, b5openn.10))

ww <- ww %>%
  mutate(dpg_trasp.29 = pg_trasp.29.10 - pg_trasp.29.6,
         dpg_mecha.34 = pg_mecha.34.10 - pg_mecha.34.6,
         dpg_nurse.38 = pg_nurse.38.10 - pg_nurse.38.6,
         dpg_sales.43 = pg_sales.43.10 - pg_sales.43.6,
         dpg_optic.48 = pg_optic.48.10 - pg_optic.48.6,
         dpg_polic.56 = pg_polic.56.10 - pg_polic.56.6,
         dpg_banke.56 = pg_banke.56.10 - pg_banke.56.6,
         dpg_socwo.65 = pg_socwo.65.10 - pg_socwo.65.6,
         dpg_teach.66 = pg_teach.66.10 - pg_teach.66.6,
         dpg_trala.68 = pg_trala.68.10 - pg_trala.68.6,
         dpg_engin.73 = pg_engin.73.10 - pg_engin.73.6,
         dpg_docto.80 = pg_docto.80.10 - pg_docto.80.6,
         dpg_legal.85 = pg_legal.85.10 - pg_legal.85.6,
         dureach = ureach.10 - ureach.6,
         dlreach = lreach.10 - lreach.6,
         ddiversity = diversity.10 - diversity.6,
         daprestige = aprestige.10 - aprestige.6)

# 11. MERGE ww with VOLUNTEERING DATA ####
load("03_data/vl.Rda") #load original volunteerin data in long format.
vollong <- vl %>%
  mutate(volsince = case_when(t261902>0 ~t261902, t261902<0 ~ NA_real_),
         voltime = case_when(t261903>0 ~t261903, t261903<0 ~ NA_real_),
         volmember = case_when(t261901<0 ~NA_real_, t261901==1 ~ 1, t261901==2 ~ 0),
         volstillon = case_when(t261904<0 ~NA_real_, t261904==1 ~ 1, t261904==2 ~ 0)) %>%
  select(c(ID_t, volnr, too, orgv, volunt, wave.x, volsince, voltime, 
         volmember, volstillon))
vollong$uiv <- as.numeric(vl$ID_t)*100+as.numeric(vl$orgv) #unique identifier

volwide <- reshape(vollong,
              direction = "wide",
              v.names = c("too", "volunt", "volnr", "volsince", "voltime", "volmember",
                         "volstillon"),
              timevar = "wave.x",
              idvar = "uiv")

volwide <- volwide %>% # trajectories:
  mutate(traj = case_when(is.na(too.6) & !is.na(too.10) ~ 1, #start 
                          !is.na(too.6) & !is.na(too.10) ~ 2, #stay involved
                          !is.na(too.6) & is.na(too.10) ~ 3), #quit 
         invvorg.6 = case_when(is.na(too.6) ~ 0, !is.na(too.6) ~ 1), #involved in particular vol org in wave 6/10?
         invvorg.10 = case_when(is.na(too.10) ~ 0, !is.na(too.10) ~ 1),
         inv.6 = ave(as.numeric(invvorg.6), ID_t, FUN = sum), # number of involvements per person in wave 6/10
         inv.10 = ave(as.numeric(invvorg.10), ID_t, FUN = sum),
         active.6 = case_when(inv.6==0~0, inv.6>0~1), #dummy: active in w6/10: no or yes?
         active.10 = case_when(inv.10==0~0, inv.10>0~1),
         thisstart = case_when(traj==1 ~ 1, traj!=1 | is.na(traj) ~ 0),
         anystart_total = ave(thisstart, ID_t, FUN = sum)) # number of starting transitions

volwide <- volwide %>% #create dataframe with only one starting, sustaining, quitting transition per individual (the most time-consuming of each type)
  group_by(ID_t, traj) %>%
  arrange(desc(voltime.10), desc(voltime.6), .by_group = T) %>% # based on vl data: high values = high time investment, focus is on starting -> voltime.10 more important!
  mutate(volid = row_number()) %>%
  filter(volid==1) %>% #filter the most important (i.e., time-consuming transition per trajectory type), i.e., max. only one starting transition, only one sust, only one quit
  select(-c(inv.6, inv.10, invvorg.6, invvorg.10, uiv, volid, orgv)) %>%
  ungroup()

vww <- volwide %>% # produce dataframe with one observation per individual (wide format)
  pivot_wider(id_cols = c("ID_t", "active.6", "active.10", "anystart_total"), 
              names_from = "traj", 
              values_from=c("too.6", "volunt.6", "volnr.6", "volsince.6", 
                            "voltime.6", "volmember.6", "volstillon.6", "too.10", 
                            "volunt.10", "volnr.10", "volsince.10", "voltime.10", 
                            "volmember.10", "volstillon.10")) %>%
  mutate(i_start = case_when(is.na(too.10_1) ~ 0, !is.na(too.10_1) ~ 1), #indicators for presence of starting, quitting or sustaining transition
         i_start_intensity = case_when(is.na(voltime.10_1) ~ "never",
                                       voltime.10_1 ==1 ~ "seldom",
                                       voltime.10_1 ==2 ~ "once_a_month",
                                       voltime.10_1 ==3 ~ "several_times_a_month",
                                       voltime.10_1 ==4 ~ "once_a_week",
                                       voltime.10_1 ==5 ~ "several_times_a_week",
                                       voltime.10_1 ==6 ~ "daily"),
         i_start_al_monthly = case_when(is.na(too.10_1) ~ 0, 
                                        !is.na(too.10_1) & voltime.10_1>1 ~ 1, 
                                        !is.na(too.10_1) & (voltime.10_1==1 | is.na(voltime.10_1)) ~ 99),
         i_start_al_stmonthly = case_when(is.na(too.10_1) ~ 0, 
                                          !is.na(too.10_1) & voltime.10_1>2 ~ 1, 
                                          !is.na(too.10_1) & (voltime.10_1==1 | voltime.10_1==2 | is.na(voltime.10_1)) ~ 99),
         i_quit = case_when(is.na(too.6_3) ~ 0, !is.na(too.6_3) ~ 1),
         i_sust = case_when(is.na(too.6_2) & is.na(too.10_2) ~ 0, !is.na(too.6_2) & !is.na(too.10_2) ~ 1),
         a6_intensity = case_when(is.na(voltime.6_1) ~ "never", # Intensity of involvement at t1 (needed for cross-sectional robustness-check. )
                                  voltime.6_1 ==1 ~ "seldom",
                                  voltime.6_1 ==2 ~ "once_a_month",
                                  voltime.6_1 ==3 ~ "several_times_a_month",
                                  voltime.6_1 ==4 ~ "once_a_week",
                                  voltime.6_1 ==5 ~ "several_times_a_week",
                                  voltime.6_1 ==6 ~ "daily"),
         too_start = too.10_1)

adat <- merge(ww, vww, by=c("ID_t"), all.x = T, all.y=F)

adat <- adat %>% #add values for those who are not involved at all (stay uninvolved, 0 for active.6/10 variables)
  mutate(active.6 = case_when(!is.na(active.6) ~ active.6, 
                              is.na(active.6) ~ 0),
         active.10 = case_when(!is.na(active.10) ~ active.10, 
                              is.na(active.10) ~ 0),
         i_start = case_when(!is.na(i_start) ~ i_start,
                             is.na(i_start) ~ 0),
         i_start_al_monthly = case_when(!is.na(i_start_al_monthly) ~ i_start_al_monthly,
                             is.na(i_start_al_monthly) ~ 0),
         i_start_al_stmonthly = case_when(!is.na(i_start_al_stmonthly) ~ i_start_al_stmonthly,
                             is.na(i_start_al_stmonthly) ~ 0),
         i_quit = case_when(!is.na(i_quit) ~ i_quit,
                             is.na(i_quit) ~ 0),
         i_sust = case_when(!is.na(i_sust) ~ i_sust,
                             is.na(i_sust) ~ 0),
         anystart_total = case_when(!is.na(anystart_total) ~ anystart_total,
                                    is.na(anystart_total) ~ 0))

adat <- adat %>% mutate(age.6_2 = age.6 * age.6)

classifyvolorgnew <- function(x) {
  case_when(x==1 ~ "Other welfare", #Social/working men
            x==2 ~ "Charity", #Charity (possibly more locally oriented)
            x==3 ~ "Other", #military
            x==4 ~ "Professional",
            x==5 ~ "Unknown",
            x==6 ~ "Youth",
            x==7 ~ "Sociability/Karneval",
            x==8 ~ "Marksmen",
            x==9 ~ "Neighborhood", #village association
            x==10 ~ "Neighborhood", #home/tradition
            x==11 ~ "Gardening",
            x==12 ~ "Neighborhood", #neighborhood
            x==13 ~ "Neighborhood", #Appartment/housing
            x==14 ~ "Neighborhood", #Landfrauen
            x==15 ~ "Parents (School/Kindergarten)", #Parents
            x==16 ~ "Parents (School/Kindergarten)", #Kindergarten
            x==17 ~ "Parents (School/Kindergarten)", #Library
            x==18 ~ "Parents (School/Kindergarten)", #School
            x==19 ~ "Professional", #University
            x==20 ~ "Culture", #Culture
            x==21 ~ "Music (instumental)", 
            x==22 ~ "Choir",
            x==23 ~ "Music (instumental)", #band
            x==24 ~ "Music (instumental)", #orchestra
            x==25 ~ "Theatre", #Theatre
            x==26 ~ "Culture", #Museum
            x==27 ~ "Citizens' initiative",
            x==28 ~ "Party/municipal politics",
            x==29 ~ "Party/municipal politics", #Election volunteer
            x==30 ~ "Court",
            x==31 ~ "Technical assistance", #Technical assistance
            x==32 ~ "Firefighters", #Firefighters
            x==33 ~ "Other welfare", #Migration/Integration
            x==34 ~ "Animal/Environment", #Environment
            x==35 ~ "Animal/Environment", #Animal
            x==36 ~ "Other sports", #Sports
            x==37 ~ "Soccer",
            x==38 ~ "Teamsports, no soccer or volleyball", #Handball
            x==39 ~ "Watersports, no swimming", #rowing
            x==40 ~ "Watersports, no swimming", #Sailing
            x==41 ~ "Teamsports, no soccer or volleyball", #Hockey
            x==42 ~ "Volleyball", #Volleyball
            x==43 ~ "Teamsports, no soccer or volleyball", #Basketball
            x==44 ~ "Other sports", #Hiking
            x==45 ~ "Fishing/Hunting",
            x==46 ~ "Hobby", #chess
            x==47 ~ "Horse-riding",
            x==48 ~ "Gymnastics",
            x==49 ~ "Aerobic", #Yoga
            x==50 ~ "Aerobic", #Fitness
            x==51 ~ "Swimming",
            x==52 ~ "Watersports, no swimming", #Diving
            x==53 ~ "Aerobic", #Aerobic
            x==54 ~ "Watersports, no swimming", #Canoeing
            x==55 ~ "Aerobic", #Rehabilitation
            x==56 ~ "Dance",
            x==57 ~ "Material arts", #Material arts (e.g. Judo)
            x==58 ~ "Racket sports", #Tennis/Tabletennis/Badminton
            x==59 ~ "Other sports", #climbing
            x==60 ~ "Other sports", #Bowling
            x==61 ~ "Cycling/Running/Athletics", #Running
            x==62 ~ "Other sports", #Skiing
            x==63 ~ "Cycling/Running/Athletics", #Athletics
            x==64 ~ "Hobby", #Fanclub
            x==65 ~ "Material arts", #Material arts (e.g. Judo)
            x==66 ~ "Cycling/Running/Athletics", #Cycling
            x==67 ~ "Other sports", #Golf
            x==68 ~ "Union",
            x==69 ~ "Workplace",
            x==70 ~ "Elderly people care", #Elderly people care
            x==71 ~ "Disabled people care", #Disabled people care
            x==72 ~ "Other welfare", #Hospital/Blood donation
            x==73 ~ "Other welfare", #Poverty
            x==74 ~ "Charity", #Development aid (international charity)
            x==75 ~ "Self-help group/Helpline",
            x==76 ~ "Hobby", #Motorsport/car
            x==77 ~ "Animal/Environment", #Dog
            x==78 ~ "Hobby", #Hobby
            x==79 ~ "Religious (esp. Church)", #Church
            x==80 ~ "Religious (esp. Church)", #Faith-based
            x==81 ~ "Religious (esp. Church)", #Sects
            x==82 ~ "Other")
}

adat <- adat %>%
  mutate_at(c("too.6_1", "too.6_2", "too.6_3", "too.10_1", "too.10_2", "too.10_3", "too_start"), classifyvolorgnew)

adat <- adat %>%
  mutate(a_sport.6 = ifelse(too.6_1 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                              "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                              "Other sports", "Racket sports", "Soccer", "Swimming",
                                              "Teamsports, no soccer or volleyball", "Volleyball",
                                              "Watersports, no swimming") |
                            too.6_2 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                             "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                             "Other sports", "Racket sports", "Soccer", "Swimming",
                                             "Teamsports, no soccer or volleyball", "Volleyball",
                                             "Watersports, no swimming") | 
                            too.6_3 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                             "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                             "Other sports", "Racket sports", "Soccer", "Swimming",
                                             "Teamsports, no soccer or volleyball", "Volleyball",
                                             "Watersports, no swimming"), 1, 0),
         a_sport.10 = ifelse(too.10_1 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                           "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                           "Other sports", "Racket sports", "Soccer", "Swimming",
                                           "Teamsports, no soccer or volleyball", "Volleyball",
                                           "Watersports, no swimming") |
                              too.10_2 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                             "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                             "Other sports", "Racket sports", "Soccer", "Swimming",
                                             "Teamsports, no soccer or volleyball", "Volleyball",
                                             "Watersports, no swimming") | 
                              too.10_3 %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                             "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                             "Other sports", "Racket sports", "Soccer", "Swimming",
                                             "Teamsports, no soccer or volleyball", "Volleyball",
                                             "Watersports, no swimming"), 1, 0),
         a_social.6 = ifelse(too.6_1 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                            "Other welfare", "Parents (School/Kindergarten)", 
                                            "Self-help group/Helpline", "Youth") |
                               too.6_2 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                              "Other welfare", "Parents (School/Kindergarten)", 
                                              "Self-help group/Helpline", "Youth") |
                               too.6_3 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                              "Other welfare", "Parents (School/Kindergarten)", 
                                              "Self-help group/Helpline", "Youth"), 1, 0),
         a_social.10 = ifelse(too.10_1 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                            "Other welfare", "Parents (School/Kindergarten)", 
                                            "Self-help group/Helpline", "Youth") |
                               too.10_2 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                              "Other welfare", "Parents (School/Kindergarten)", 
                                              "Self-help group/Helpline", "Youth") |
                               too.10_3 %in% c("Charity", "Disabled people care", "Disabled people care", 
                                              "Other welfare", "Parents (School/Kindergarten)", 
                                              "Self-help group/Helpline", "Youth"), 1, 0),
         a_other.6 = ifelse(too.6_1 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                           "Citizens' initiative", "Court", "Party/municipal politics",
                                           "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                           "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                           "Religious (esp. Church)",
                                           "Professional", "Union", "Workplace") |
                              too.6_2 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                             "Citizens' initiative", "Court", "Party/municipal politics",
                                             "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                             "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                             "Religious (esp. Church)",
                                             "Professional", "Union", "Workplace") |
                              too.6_3 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                             "Citizens' initiative", "Court", "Party/municipal politics",
                                             "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                             "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                             "Religious (esp. Church)",
                                             "Professional", "Union", "Workplace"), 1, 0),
         a_other.10 = ifelse(too.10_1 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                           "Citizens' initiative", "Court", "Party/municipal politics",
                                           "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                           "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                           "Religious (esp. Church)",
                                           "Professional", "Union", "Workplace") |
                              too.10_2 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                             "Citizens' initiative", "Court", "Party/municipal politics",
                                             "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                             "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                             "Religious (esp. Church)",
                                             "Professional", "Union", "Workplace") |
                              too.10_3 %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                             "Citizens' initiative", "Court", "Party/municipal politics",
                                             "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                             "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                             "Religious (esp. Church)",
                                             "Professional", "Union", "Workplace"), 1, 0))
sdat <- adat
sd_isei <- sd(sdat$isei081, na.rm = T)

# 14. construct final dataframe in long format ####
sw <- sdat %>%
  filter(bothwaves==2 & !is.na(diversity.6) & !is.na(diversity.10) & 
           !is.na(i_start)) %>% 
  mutate(pg_trasp_29.6 = pg_trasp.29.6, pg_mecha_34.6 = pg_mecha.34.6, pg_nurse_38.6 = pg_nurse.38.6,
         pg_sales_43.6 = pg_sales.43.6, pg_optic_48.6 = pg_optic.48.6, pg_polic_56.6 = pg_polic.56.6,
         pg_banke_56.6 = pg_banke.56.6, pg_socwo_65.6 = pg_socwo.65.6, pg_teach_66.6 = pg_teach.66.6,
         pg_trala_68.6 = pg_trala.68.6, pg_engin_73.6 = pg_engin.73.6, pg_docto_80.6 = pg_docto.80.6,
         pg_legal_85.6 = pg_legal.85.6,
         pg_trasp_29.10 = pg_trasp.29.10, pg_mecha_34.10 = pg_mecha.34.10, pg_nurse_38.10 = pg_nurse.38.10,
         pg_sales_43.10 = pg_sales.43.10, pg_optic_48.10 = pg_optic.48.10, pg_polic_56.10 = pg_polic.56.10,
         pg_banke_56.10 = pg_banke.56.10, pg_socwo_65.10 = pg_socwo.65.10, pg_teach_66.10 = pg_teach.66.10,
         pg_trala_68.10 = pg_trala.68.10, pg_engin_73.10 = pg_engin.73.10, pg_docto_80.10 = pg_docto.80.10,
         pg_legal_85.10 = pg_legal.85.10) %>%
  select(ID_t, too_start, i_start, i_start_al_monthly, i_start_al_stmonthly, i_start_intensity, a6_intensity,
         diversity.6, diversity.10, aprestige.6, aprestige.10, ureach.6, ureach.10, lreach.6, lreach.10,
         start_emp610, end_emp610, new_partner610, quit_partner610, move610, childbirth610, populationdensity.6, populationdensity.10, east.6, east.10, #time-varying controls
         isei081, migback.6, woman, age.6, age.10, agem.6, agem.10, b5extra, b5openn, b5neuro, b5consc, b5agree, volearlier6, volpot6, religiosity, educ_isced, #time-constant variables
         active.6, active.10, anystart_total,
         a_sport.6, a_sport.10, a_social.6, a_social.10, a_other.6, a_other.10,
         start_org_mean_isei, start_org_median_isei, start_org_N_isei,
         pg_trasp_29.6, pg_mecha_34.6, pg_nurse_38.6, pg_sales_43.6, pg_optic_48.6, pg_polic_56.6, #position generator
         pg_banke_56.6, pg_socwo_65.6, pg_teach_66.6, pg_trala_68.6, pg_engin_73.6, pg_docto_80.6, pg_legal_85.6,
         pg_trasp_29.10, pg_mecha_34.10, pg_nurse_38.10, pg_sales_43.10, pg_optic_48.10, pg_polic_56.10, #position generator
         pg_banke_56.10, pg_socwo_65.10, pg_teach_66.10, pg_trala_68.10, pg_engin_73.10, pg_docto_80.10, pg_legal_85.10) %>%
  mutate(indiversity = diversity.6)
sw$i_start_al_monthly[sw$i_start_al_monthly==99] <- NA
sw$i_start_al_stmonthly[sw$i_start_al_stmonthly==99] <- NA

#mean imputation of missing continuous variables (only few and just so I have valid values, don't really need them later on)
sw$age.6[is.na(sw$age.6)] <- mean(sw$age.6, na.rm=T)
sw$b5extra[is.na(sw$b5extra)] <- mean(sw$b5extra, na.rm=T)
sw$b5openn[is.na(sw$b5openn)] <- mean(sw$b5openn, na.rm=T)
sw$b5neuro[is.na(sw$b5neuro)] <- mean(sw$b5neuro, na.rm=T)
sw$b5consc[is.na(sw$b5consc)] <- mean(sw$b5consc, na.rm=T)
sw$b5agree[is.na(sw$b5agree)] <- mean(sw$b5agree, na.rm=T)
sw$religiosity[is.na(sw$religiosity)] <- mean(sw$religiosity, na.rm=T)
sw$volpot6[is.na(sw$volpot6)] <- mean(sw$volpot6, na.rm=T)
sw$educ_isced[is.na(sw$educ_isced)] <- mean(sw$educ_isced, na.rm=T)
sw$populationdensity.6[is.na(sw$populationdensity.6)] <- mean(sw$populationdensity.6, na.rm=T)
sw$populationdensity.10[is.na(sw$populationdensity.10)] <- mean(sw$populationdensity.10, na.rm=T)


#mode imputation of missing categorical variables:
table(sw$volearlier6)
sw$volearlier6[is.na(sw$volearlier6)] <- 0
sw$east.6[is.na(sw$east.6)] <- 0
sw$east.10[is.na(sw$east.10)] <- 0

sw <- sw %>%
  mutate(sample_starting = case_when(active.6==0 ~ 1, active.6==1 ~ 0)) #variable indicating whether a respondent is part of the sample

#recode education variable:
sw <- sw %>% mutate(isced0_2 = case_when(educ_isced<=2 ~ 1, educ_isced>2 ~ 0),
                    isced3_4 = case_when(educ_isced>=3 & educ_isced<=7 ~ 1, educ_isced<3 | educ_isced>7 ~ 0),
                    isced5_6 = case_when(educ_isced>=8 ~ 1, educ_isced<8 ~ 0))
sw <- as.data.frame(sw)

#Inverse Probability of Treatment weighting:
sw <- sw %>%
  mutate(s_prob = predict(glm(sample_starting ~ diversity.6 + isei081 + age.6 + 
                                woman + b5extra + b5openn + b5neuro +
                                b5consc + b5agree + volearlier6 + volpot6 + religiosity + isced3_4 + isced5_6, 
                              family = binomial(), data = sw), type = "response"), #probability of being in the analytic sample
         s_weight = case_when(sample_starting==0 ~ 1/(1-s_prob), #sample weight for those who are not part of the sample (does not really matter anyways)
                              sample_starting==1 ~ 1/s_prob)) #sample weight for those who are part of the sample

#probability of being treated in subsample of those who are part of the analytic sample:
sw_weights <- sw %>% filter(sample_starting==1)
sw_weights <- sw_weights %>%
  mutate(t_prob = predict(glm(i_start ~ diversity.6 + isei081 + age.6 + 
                                woman + b5extra + b5openn + b5neuro +
                                b5consc + b5agree + volearlier6 + volpot6 + religiosity + isced3_4 + isced5_6,
                              family = binomial(), data = sw_weights), type = "response"), #probability of being treated given one is part of the sample
         t_weight = case_when(i_start==0 ~ 1/(1-t_prob),
                              i_start==1 ~ 1/t_prob)) %>%
  select("ID_t", "t_prob", "t_weight")

sw <- merge(sw, sw_weights, by="ID_t", all.x = T, all.y = F) #merge treatment weights on original data

#calculate final weights by multiplying s_weight and t_weight:
sw <- sw %>%
  mutate(st_weight = s_weight*t_weight) %>%
  select(-c("s_prob", "t_prob"))

#difference scores:
sw <- sw %>% mutate(ddiversity = diversity.10 - diversity.6,
                    daprestige = aprestige.10-aprestige.6, 
                    dureach = ureach.10 - ureach.6,
                    dlreach = lreach.10 - lreach.6)

p33isei <- quantile(sw$isei081, c(0.33), na.rm=T)
p67isei <- quantile(sw$isei081, c(0.67), na.rm=T)
p25isei <- quantile(sw$isei081, c(0.25), na.rm=T)
p50isei <- quantile(sw$isei081, c(0.50), na.rm=T)
p75isei <- quantile(sw$isei081, c(0.75), na.rm=T)
p20isei <- quantile(sw$isei081, c(0.20), na.rm=T)
p40isei <- quantile(sw$isei081, c(0.40), na.rm=T)
p60isei <- quantile(sw$isei081, c(0.60), na.rm=T)
p80isei <- quantile(sw$isei081, c(0.80), na.rm=T)
sw <- sw %>%
  mutate(d_lisei = case_when(isei081<45.5 ~ 1, isei081>=45.5 ~ 0),
         d_misei = case_when(isei081<45.5 | isei081>=67 ~ 0, isei081>=45.5 & isei081<67 ~ 1),
         d_hisei = case_when(isei081<67 ~ 0, isei081>=67 ~ 1),
         d_lisei_n = case_when(isei081<p33isei ~ 1, isei081>=p33isei ~ 0),
         d_misei_n = case_when(isei081<p33isei | isei081>=p67isei ~ 0, isei081>=p33isei & isei081<p67isei ~ 1),
         d_hisei_n = case_when(isei081<p67isei ~ 0, isei081>=p67isei ~ 1),
         f_isei  = as.factor(case_when(isei081<45.5 ~ 1, isei081>=45.5 & isei081<67 ~ 2, isei081>=67 ~ 3)),
         f_isei_n  = as.factor(case_when(isei081<p33isei ~ 1, isei081>=p33isei & isei081<p67isei ~ 2, isei081>=p67isei ~ 3)),
         f_isei_quart = as.factor(case_when(isei081<p25isei ~ 1, isei081>=p25isei & isei081<p50isei ~ 2, 
                                            isei081>=p50isei & isei081<p75isei ~ 3, isei081>=p67isei ~ 4)),
         f_isei_quint = as.factor(case_when(isei081<p20isei ~ 1, isei081>=p20isei & isei081<p40isei ~ 2, 
                                            isei081>=p40isei & isei081<p60isei ~ 3, isei081>=p60isei & isei081<p80isei ~ 4, isei081>=p80isei ~ 5)))


#export wide data to stata to impute for counterfactual analysis /hyp):
sw_hyp <- sw %>% select("ID_t", "too_start", "i_start", "active.6", "diversity.6", "diversity.10", "aprestige.6",
                        "aprestige.10", "ureach.6", "ureach.10", "start_emp610",
                        "end_emp610", "new_partner610", "quit_partner610", "move610",
                        "childbirth610", "isced0_2", "isced3_4", "isced5_6",
                        "isei081", "woman", "migback.6", "agem.6", "b5extra",
                        "populationdensity.6", "populationdensity.10", "east.6", "east.10",
                        "a_sport.6", "a_sport.10", "a_social.6", "a_social.10", "a_other.6", "a_other.10",
                        "f_isei_n", "d_lisei_n", "d_misei_n", "d_hisei_n",
                        "s_weight", "t_weight", "st_weight",
                        "b5openn", "b5neuro", "b5consc", "b5agree", "volearlier6", "religiosity")
write.dta(sw_hyp, "Y:/Documents/VOL_SC/03_data/wide_data_imp_hyp.dta")


# rehape to long-long format:
sl <- reshape(sw, direction = "long",
              varying = c("diversity.6", "diversity.10", "aprestige.6", "aprestige.10", 
                          "ureach.6", "ureach.10", "lreach.6", "lreach.10",
                          "a_sport.6", "a_sport.10", "a_social.6", "a_social.10", "a_other.6", "a_other.10",
                          "age.6", "age.10", "agem.6", "agem.10", "populationdensity.6", "populationdensity.10", "east.6", "east.10",
                          "pg_trasp_29.6", "pg_mecha_34.6", "pg_nurse_38.6", "pg_sales_43.6", "pg_optic_48.6", "pg_polic_56.6", #position generator
                          "pg_banke_56.6", "pg_socwo_65.6", "pg_teach_66.6", "pg_trala_68.6", "pg_engin_73.6", "pg_docto_80.6", "pg_legal_85.6",
                          "pg_trasp_29.10", "pg_mecha_34.10", "pg_nurse_38.10", "pg_sales_43.10", "pg_optic_48.10", "pg_polic_56.10", #position generator
                          "pg_banke_56.10", "pg_socwo_65.10", "pg_teach_66.10", "pg_trala_68.10", "pg_engin_73.10", "pg_docto_80.10", "pg_legal_85.10"),
              sep = ".",
              new.row.names = sequence(prod(46, nrow(sw))), #46 is the number of columns in varying - argument. 
              idvar = c("ID_t", "too_start", "i_start", "i_start_al_monthly", "i_start_al_stmonthly", "i_start_intensity", "a6_intensity",
                        "start_emp610", "end_emp610", "new_partner610", "quit_partner610", "move610", 
                        "isei081", "migback.6", "indiversity", "active.6", "active.10", 
                        "s_weight", "t_weight", "st_weight", "sample_starting",
                        "woman", "b5extra", "b5openn", "b5neuro",
                        "b5consc", "b5agree", "volearlier6", "volpot6", "religiosity",
                        "educ_isced", "isced0_2", "isced3_4", "isced5_6",
                        "f_isei", "f_isei_n", "f_isei_quart", "f_isei_quint",
                        "anystart_total", "ddiversity", "dureach", "dlreach", "daprestige"),
              timevar = "wave",
              times = c(6, 10))
sl <- sl %>% 
  mutate(broadvol2=case_when(too_start %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                             "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                             "Other sports", "Racket sports", "Soccer", "Swimming",
                                             "Teamsports, no soccer or volleyball", "Volleyball",
                                             "Watersports, no swimming") ~ "Sport",
                            too_start %in% c("Choir", "Culture", "Music (instumental)", "Theatre") ~ "Culture/Music",
                            too_start %in% c("Citizens' initiative", "Court", "Party/municipal politics") ~ "Political",
                            too_start %in% c("Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                             "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown") ~ "Coummunity",
                            too_start %in% c("Charity", "Disabled people care", "Disabled people care", 
                                             "Other welfare", "Parents (School/Kindergarten)", 
                                             "Self-help group/Helpline", "Youth") ~ "Social",
                            too_start %in% c("Religious (esp. Church)") ~ "Religion",
                            too_start %in% c("Professional", "Union", "Workplace") ~ "Work-related"),
         broadvol=case_when(too_start %in% c("Aerobic", "Cycling/Running/Athletics", "Dance",
                                              "Gymnastics", "Fishing/Hunting", "Horse-riding", "Material arts",
                                              "Other sports", "Racket sports", "Soccer", "Swimming",
                                              "Teamsports, no soccer or volleyball", "Volleyball",
                                              "Watersports, no swimming") ~ "Sport",
                            too_start %in% c("Charity", "Disabled people care", "Disabled people care", 
                                             "Other welfare", "Parents (School/Kindergarten)", 
                                             "Self-help group/Helpline", "Youth") ~ "Social",
                             too_start %in% c("Choir", "Culture", "Music (instumental)", "Theatre",
                                              "Citizens' initiative", "Court", "Party/municipal politics",
                                              "Animal/Environment", "Firefighters", "Gardening", "Hobby",
                                              "Marksmen", "Neighborhood", "Other", "Technical assistance", "Unknown",
                                              "Religious (esp. Church)",
                                              "Professional", "Union", "Workplace") ~ "Other"),
         i_start_intensity_2catc = case_when(i_start_intensity %in% c("seldom", "once_a_month", "several_times_a_month", "once_a_week") ~ 1,
                                             i_start_intensity %in% c("several_times_a_week", "daily") ~ 2))


#recode indicators of time-varying changes: always: t1=0, t2=1 if change and t2=0 if no change.
sl <- sl %>% mutate(i_start_tv = case_when(wave==6 ~ 0, wave!=6 ~ i_start),
                    i_start_tc = i_start,
                    i_start_al_monthly_tv = case_when(wave==6 ~ 0, wave!=6 ~ i_start_al_monthly),
                    i_start_al_stmonthly_tv = case_when(wave==6 ~ 0, wave!=6 ~ i_start_al_stmonthly),
                    start_emp610 = case_when(wave==6 ~ 0, wave!=6 ~ start_emp610),
                    end_emp610 = case_when(wave==6 ~ 0, wave!=6 ~ end_emp610),
                    new_partner610 = case_when(wave==6 ~ 0, wave!=6 ~ new_partner610),
                    quit_partner610 = case_when(wave==6 ~ 0, wave!=6 ~ quit_partner610),
                    move610 = case_when(wave==6 ~ 0, wave!=6 ~ move610),
                    childbirth610 = case_when(wave==6 ~ 0, wave != 6 ~ childbirth610),
                    volearlier6_none = case_when(volearlier6==0 ~ 1, volearlier6!=0 ~ 0),
                    volearlier6_once = case_when(volearlier6==1 ~ 1, volearlier6!=1 ~ 0),
                    volearlier6_more = case_when(volearlier6==2 ~ 1, volearlier6!=2 ~ 0))

# Export files for analysis in stata ####

slfull <- sl #save version of full sample (all respondents)
write.dta(slfull, "path/analysis_data_ols.dta")
sl <- sl %>% filter(sample_starting==1) #for most models later on: sample of those who are uninvolved at t1.

#function to standardize variables:
stdize <- function(x, na.rm=T) {
  x_std <- (x-mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
  return(x_std)
}
sl <- sl %>%
  mutate(isei081_s = stdize(isei081), diversity_s = stdize(diversity), indiversity_s = stdize(indiversity))
slfull <- slfull %>%
  mutate(isei081_s = stdize(isei081), diversity_s = stdize(diversity), indiversity_s = stdize(indiversity))

sl <- sl %>%
  mutate(diversity_adj = diversity/13)


#save as stata data file
write.dta(sl, "path/analysis_data_fe.dta")
write.dta(slfull, "path/analysis_full_data_for_robust.dta")

# RUN STATA DO-FILE NOW ####
# Table 1 for main text: overview of PG and voluntary involvement ####
#Table 1a:
pos_gen_var2 <- c("pg_trasp_29", "pg_mecha_34", "pg_nurse_38", "pg_sales_43", "pg_optic_48", "pg_polic_56",
                  "pg_banke_56", "pg_socwo_65", "pg_teach_66", "pg_trala_68", "pg_engin_73", "pg_docto_80", "pg_legal_85")
pos_gen_var3 <- rep("_wtd.mean", times=length(pos_gen_var2))
pos_gen_var3b <- rep("_mean", times=length(pos_gen_var2))
pos_gen_var5 <- paste0(pos_gen_var2, pos_gen_var3)
pos_gen_var5b <- paste0(pos_gen_var2, pos_gen_var3b)
pg_sum_vars <- c("diversity", "ureach", "lreach", "aprestige")

## Weighted table (weighted by s_weight) ####
total <- sl %>%
  filter(sample_starting==1 & wave ==6) %>%
  summarize_at(vars(pos_gen_var2), funs(wtd.mean(., st_weight))) 
total <- as.data.frame(t(total)) # transpose data frame
Position <- c("Transport worker (29)", "Mechanic (34)", "Nurse (38)", "Sales assistant (43)", "Optician (48)", "Police officer (56)",
                      "Bank clerk (56)", "Social worker (65)", "Teacher (66)", "Translator (68)", "Engineer (73)", "Doctor (80)", "Legal practicer (85)")
byses <- sl %>% #access to individual positions by SES
  filter(sample_starting==1 & wave ==6) %>%
  group_by(f_isei_n) %>%
  summarize_at(vars(all_of(pos_gen_var2)), funs(wtd.mean(., st_weight))) %>%
  t(.) %>%
  as.data.frame() %>%
  slice(-1) 
`low SES` <- round(as.numeric(byses$V1), digits = 3)
`medium SES` <- round(as.numeric(byses$V2), digits = 3)
`high SES` <- round(as.numeric(byses$V3), digits = 3)
t1a <- cbind(Position, `low SES`, `medium SES`, `high SES`, total)
t1a <- t1a %>% mutate(total = round(V1, digits = 3)) %>% select(-V1)

#calculate total column:
t1btotal <- sl %>%
  filter(sample_starting==1 & wave ==6) %>%
  summarize_at(vars(all_of(pg_sum_vars)), funs(wtd.mean(., s_weight), sqrt(wtd.var(., s_weight)))) %>%
  t(.) %>%
  as.data.frame()
row1 <- c("a", "a")
t1btotal <- rbind(row1, t1btotal)

t1b <- sl %>%
  filter(sample_starting==1 & wave ==6) %>%
  group_by(f_isei_n) %>%
  summarize_at(vars(all_of(pg_sum_vars)), funs(wtd.mean(., s_weight), sqrt(wtd.var(., s_weight)))) %>%
  t(.) %>%
  as.data.frame() %>%
  mutate(V4 = t1btotal$V1) %>%
  rownames_to_column(var = "Statistic") %>%
  mutate(var_order = c(1,2,4,6,8,3,5,7,9)) %>%
  arrange(var_order) %>%
  mutate(Position = c("", "Extensity (Mean)", "Extensity (SD)", "Upper reachability (Mean)", "Upper reachability (SD)", 
                      "Lower reachability (Mean)", "Lower reachability (SD)", "Average Status (Mean)", "Average Status (SD)"),
         Statistic = c("", "Mean", "SD", "Mean", "SD", "Mean", "SD", "Mean", "SD"),
         `low SES` = as.numeric(V1), `medium SES` = as.numeric(V2), `high SES` = as.numeric(V3), total = as.numeric(V4)) %>%
  filter(var_order!=1) %>%
  select(-c("Statistic", "V1", "V2", "V3", "V4", "var_order")) %>%
  mutate(across(-Position, ~ round(., digits = 3)))

i_start_means_weighted <- sl %>%
  filter(wave==6) %>%
  group_by(f_isei_n) %>%
  summarize_at(vars(i_start), funs(wtd.mean(., s_weight)))
i_start_total_mean_weighted <- sl %>%
  filter(wave ==6) %>%
  summarize_at(vars(i_start), funs(wtd.mean(., s_weight)))

i_start_vector_weighted <- as.data.frame(as.vector(c("Getting involved between t1 and t2", 
                                                     round(i_start_means_weighted[1,2],3), round(i_start_means_weighted[2,2], 3), 
                                                     round(i_start_means_weighted[3,2], 3), round(i_start_total_mean_weighted[1,1], 3))))
i_start_vector_weighted <- i_start_vector_weighted %>%
  rename(`Position` = X.Getting.involved.between.t1.and.t2., `low SES`= i_start, 
         `medium SES` = i_start.1, `high SES` = i_start.2, `total` =X0.231)

i_start_sd_weighted <- sl %>%
  filter(wave==6) %>%
  group_by(f_isei_n) %>%
  summarize_at(vars(i_start), funs(wtd.var(., s_weight))) %>%
  mutate(i_start = sqrt(i_start))
i_start_total_sd_weighted <- sl %>%
  filter(wave ==6) %>%
  summarize_at(vars(i_start), funs(wtd.var(., s_weight))) %>%
  mutate(i_start = sqrt(i_start))
i_start_vector_sd_weighted <- as.data.frame(as.vector(c("Getting involved between t1 and t2", 
                                                     round(i_start_sd_weighted[1,2],3), round(i_start_sd_weighted[2,2], 3), 
                                                     round(i_start_sd_weighted[3,2], 3), round(i_start_total_sd_weighted[1,1], 3))))
i_start_vector_sd_weighted <- i_start_vector_sd_weighted %>%
  rename(`Position` = X.Getting.involved.between.t1.and.t2., `low SES`= i_start, 
         `medium SES` = i_start.1, `high SES` = i_start.2, `total` =X0.421)

t1c <- rbind(i_start_vector_weighted, i_start_vector_sd_weighted)

table1 <- rbind(t1a, t1b, t1c)
table1 <- rbind(table1, c("N", 1430, 1330, 1135, 3895))

ft_table1 <- flextable(table1)
ft_table1 <- width(ft_table1, j = 1, width = 2)
ft_table1

write.xlsx(table1, "path/Table1_maintext_weighted.xlsx")


#generate variable that contrasts single SES groups:
sl <- sl %>%
  mutate(isei_low_medium = case_when(f_isei_n==1 ~ 1, f_isei_n==2 ~ 2),
         isei_low_high = case_when(f_isei_n==1 ~ 1, f_isei_n==3 ~ 3),
         isei_medium_high = case_when(f_isei_n==2 ~ 2, f_isei_n==3 ~ 3))

#t-tests for differences across SES groups:
#diversity
t.test(diversity ~ isei_low_medium, data = subset(sl, sample_starting==1 & wave==6))
t.test(diversity ~ isei_low_high, data = subset(sl, sample_starting==1 & wave==6))
t.test(diversity ~ isei_medium_high, data = subset(sl, sample_starting==1 & wave==6))
#ureach
t.test(ureach ~ isei_low_medium, data = subset(sl, sample_starting==1 & wave==6))
t.test(ureach ~ isei_low_high, data = subset(sl, sample_starting==1 & wave==6))
t.test(ureach ~ isei_medium_high, data = subset(sl, sample_starting==1 & wave==6))
#lreach
t.test(lreach ~ isei_low_medium, data = subset(sl, sample_starting==1 & wave==6))
t.test(lreach ~ isei_low_high, data = subset(sl, sample_starting==1 & wave==6))
t.test(lreach ~ isei_medium_high, data = subset(sl, sample_starting==1 & wave==6))
#aprestige
t.test(aprestige ~ isei_low_medium, data = subset(sl, sample_starting==1 & wave==6))
t.test(aprestige ~ isei_low_high, data = subset(sl, sample_starting==1 & wave==6))
t.test(aprestige ~ isei_medium_high, data = subset(sl, sample_starting==1 & wave==6))
#--> all differences are statistically significant with p<0.001

#correlation plot:
sc_vars <- sl %>% select(c("diversity", "ureach", "lreach", "aprestige"))
library(corrplot)
corrplot(
  cor(sc_vars),
  addCoef.col = "white",
  addCoefasPercent = T,
  type = "upper",
  order = "AOE"
)

# Table 2 in main text (regression results from stata models) ####
statares <- read_dta("path/main_regression_results.dta")
statares <- statares %>%
  filter(var %in% c("active_6", "i_start_tc", "i_start_tv", "1.i_start_tv", "1.f_isei_nXc.i_start_tv", "2.f_isei_nXc.i_start_tv")) %>%
  select(c("var", "coef", "stderr", "pval", "N", "model")) %>%
  mutate(coef = round(coef, digits = 3),
         stderr = round(stderr, digits = 3),
         pval = round(pval, digits = 3))

write.xlsx(statares, "path/Table2_maintext.xlsx")


#main effects expressed in within-person standard deviations:
statares[4,2]/sqrt(wtd.var(sl$ddiversity, sl$st_weight)) #socialization effect (H1a)
statares[8,2]/sqrt(wtd.var(sl$dureach, sl$st_weight)) #effect on upper reach (H3)


# Figure 1 in main text: Effects from stata models ####
#social capital:
df_diversity_margins <- read_dta("path/model4_margins.dta")
df_diversity_margins$group <- c("Low", "Medium", "High")
df_diversity_margins$group <- fct_relevel(df_diversity_margins$group, "Low", "Medium", "High")

plot_diversity_margins <- ggplot(df_diversity_margins) +
  geom_pointrange(mapping = aes(x = group, y = coef, ymin = ci_lower, ymax = ci_upper), size = 1, shape = 19) +
  geom_hline(yintercept = 0) +
  ggtitle("A: AME on social capital") +
  xlab("Socioeconomic Status") + ylab("Changes in social capital") +
  theme_bw()
plot_diversity_margins

#upper reach:
df_ureach_margins <- read_dta("path/model8_margins.dta")
df_ureach_margins$group <- c("Low", "Medium", "High")
df_ureach_margins$group <- fct_relevel(df_ureach_margins$group, "Low", "Medium", "High")

plot_ureach_margins <- ggplot(df_ureach_margins) +
  geom_pointrange(mapping = aes(x = group, y = coef, ymin = ci_lower, ymax = ci_upper), size = 1, shape = 19) +
  geom_hline(yintercept = 0) +
  ggtitle("B: AME on upper reachability") +
  xlab("Socioeconomic Status") + ylab("Changes in upper reachability") +
  theme_bw()
plot_ureach_margins

#average prestige:
df_aprestige_margins <- read_dta("path/model10_margins.dta")
df_aprestige_margins$group <- c("Low", "Medium", "High")
df_aprestige_margins$group <- fct_relevel(df_aprestige_margins$group, "Low", "Medium", "High")

plot_aprestige_margins <- ggplot(df_aprestige_margins) +
  geom_pointrange(mapping = aes(x = group, y = coef, ymin = ci_lower, ymax = ci_upper), size = 1, shape = 19) +
  geom_hline(yintercept = 0) +
  ggtitle("C: AME on average status") +
  xlab("Socioeconomic Status") + ylab("Changes in average status") +
  theme_bw()
plot_aprestige_margins

ameplot <- "EGH"
figure_1 <- wrap_plots(list(E=plot_diversity_margins,
                            G=plot_ureach_margins,
                            H=plot_aprestige_margins), design = ameplot)
figure_1

ggsave("path/figure_1_maintext.jpg", dpi = 600, width = 30, height = 15, units = c("cm"))


# Figure 2 in main text: Simulation - plots: ####

diversity_means <- read_xlsx("path/means_diversity.xlsx") %>%
  mutate(SES= as.factor(SES),
         SES= fct_relevel(SES, "Low", "Medium", "High"))
diversity_means_plot <- diversity_means %>%
  ggplot() +
  geom_pointrange(aes(x = SES, y = b, group = Scenario, 
                      ymin = ll, ymax = ul), size = 0.5, shape = c(15, 15, 15, 16, 16, 16),
                  position = position_dodge(width = 0.2)) +
  ggtitle("A: Social capital") +
  xlab("Socioeconomic status") + ylab("Mean") +
  theme_bw() +
  annotate(geom = "point", shape=16, size=2, x=0.6, y=8.4) +
  annotate(geom = "point", shape=15, size=2, x=0.6, y=8.3) +
  annotate(geom = "text", x=0.65, y=8.4, label="Counterfactual", hjust = "left") +  
  annotate(geom = "text", x=0.65, y=8.3, label="Observed", hjust = "left") +
  theme(legend.position = c(0.8, 0.2))
diversity_means_plot

ureach_means <- read_xlsx("path/means_ureach.xlsx") %>%
  mutate(SES= as.factor(SES),
         SES= fct_relevel(SES, "Low", "Medium", "High"))
ureach_means_plot <- ureach_means %>%
  ggplot() +
  geom_pointrange(aes(x = SES, y = b, group = Scenario, 
                      ymin = ll, ymax = ul), size = 0.5, shape = c(15, 15, 15, 16, 16, 16),
                  position = position_dodge(width = 0.2)) +
  ggtitle("B: Upper reachability") +
  xlab("Socioeconomic status") + ylab("Mean") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2))
ureach_means_plot

aprestige_means <- read_xlsx("path/means_aprestige.xlsx") %>%
  mutate(SES= as.factor(SES),
         SES= fct_relevel(SES, "Low", "Medium", "High"))
aprestige_means_plot <- aprestige_means %>%
  ggplot() +
  geom_pointrange(aes(x = SES, y = b, group = Scenario, 
                      ymin = ll, ymax = ul), size = 0.5, shape = c(15, 15, 15, 16, 16, 16),
                  position = position_dodge(width = 0.2)) +
  ggtitle("C: Average status") +
  xlab("Socioeconomic status") + ylab("Mean") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2))
aprestige_means_plot

coef_diffs <- read_xlsx("path/simulation_results_2022_11_15.xlsx")
coef_diffs <- coef_diffs %>%
  select(c(Outcome, Scenario, Coefficient_name, Coefficient_value)) %>%
  filter(Coefficient_name!= "Constant") %>%
  mutate(Coefficient_name = case_when(Coefficient_name=="Low" ~ "High vs. Low",
                                      Coefficient_name=="Medium" ~ "High vs. Medium"))
#add coefficients for low vs medium: (mref = medium as ref category) 
coef_diffs_mref <- read_xlsx("path/simulation_results_mref.xlsx")
coef_diffs_mref <- coef_diffs_mref %>%
  select(c(Outcome, Scenario, Coefficient_name, Coefficient_value)) %>%
  filter(Coefficient_name== "Low") %>%
  mutate(Coefficient_name = case_when(Coefficient_name=="Low" ~ "Medium vs. Low"))
coef_diffs <- rbind(coef_diffs, coef_diffs_mref) #bind both dataframes together.
#reshape:
coef_diffs <- coef_diffs %>%
  pivot_wider(id_cols =  c("Outcome", "Coefficient_name"), names_from = "Scenario", 
              names_sep = "_", values_from = "Coefficient_value") %>%
  mutate(diff_diff = Counterfactual-Observed)

fig_diversity_did <- coef_diffs %>%
  filter(Outcome == "Social Capital") %>%
  ggplot(aes(x=Coefficient_name, y=diff_diff)) +
  geom_bar(stat = "identity", width = 0.4) +
  theme_bw() + 
  ggtitle("D: Social capital") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.028, 0.012)) +
  #scale_y_continuous(limits = c(-0.005, 0.005)) +
  ylab("Inequality change  vis-a-vis counterfactual") +
  xlab("Status group comparison") +
  geom_text(aes(label = c("", "", ""), x = Coefficient_name, y = diff_diff), vjust = c(1.5, -0.5, -0.5))

fig_ureach_did <- coef_diffs %>%
  filter(Outcome == "Upper Reach") %>%
  ggplot(aes(x=Coefficient_name, y=diff_diff)) +
  geom_bar(stat = "identity", width = 0.4) +
  theme_bw() + 
  ggtitle("E: Upper reachability") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.7, 0.3)) +
  ylab("Inequality change vis-a-vis counterfactual") +
  xlab("Status group comparison") +
  geom_text(aes(label = c("***", "***", "***"), x = Coefficient_name, y = diff_diff), vjust = 1.5)

fig_aprestige_did <- coef_diffs %>%
  filter(Outcome == "Average Prestige") %>%
  ggplot(aes(x=Coefficient_name, y=diff_diff)) +
  geom_bar(stat = "identity", width = 0.4) +
  theme_bw() + 
  ggtitle("F: Average status") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-0.7, 0.3)) +
  ylab("Inequality change vis-a-vis counterfactual") +
  xlab("Status group comparison") +
  geom_text(aes(label = c("***", "***", "***"), x = Coefficient_name, y = diff_diff), vjust = c(-0.5, 1.5, 1.5))

sim_plot <- "ABC
             EGH"
figure_2 <- wrap_plots(list(A=diversity_means_plot,
                            B=ureach_means_plot,
                            C=aprestige_means_plot,
                            E=fig_diversity_did,
                            G=fig_ureach_did,
                            H=fig_aprestige_did), design = sim_plot)

ggsave("path/figure_2_maintext.jpg", dpi = 600, width = 30, height = 20, units = c("cm"))


# Table S1a / S1b for online supplement: Summary table ####
#specify variables for summary table (Table 1):
sumvars  <- c("diversity", "ureach", "lreach", "aprestige", "i_start_tv", 
              "start_emp610", "end_emp610", "new_partner610", "quit_partner610", "childbirth610", "move610",
              "migback.6", "woman", "agem", "b5extra", "b5openn", "b5neuro", "b5consc", "b5agree",
              "volearlier6_none", "volearlier6_once", "volearlier6_more", "volpot6", "religiosity", "isced0_2", 
              "isced3_4", "isced5_6", "east", "populationdensity")
sumvars2 <- rep(sumvars, 2)
sumvars3 <- rep("_wtd.mean", times=length(sumvars))
sumvars3b <- rep("_mean", times = length(sumvars))
sumvars4 <- rep("_sqrt", times=length(sumvars))
sumvars5 <- append(sumvars3, sumvars4) # single vector with all suffixes
sumvars5 <- paste0(sumvars2, sumvars5)
sumvars5b <- append(sumvars3b, sumvars4) # single vector with all suffixes
sumvars5b <- paste0(sumvars2, sumvars5b)

#weighted by weights ws: (not included in final online supplement)
sumtabledf <- sl %>%
  filter(sample_starting==1) %>%
  group_by(wave, i_start) %>%
  summarise_at(vars(all_of(sumvars)), funs(wtd.mean(., s_weight), sqrt(wtd.var(., s_weight)))) %>%
  pivot_longer(all_of(sumvars5), names_to = "Variable", values_to = "statisticvalue") %>%
  mutate(statistictype = sub(".*\\_", "", Variable),
         Variable = sub("_[^_]+$", "", Variable)) %>%
  pivot_wider(id_cols = c("wave", "i_start", "Variable"), names_from = "statistictype",
              names_sep = ".", values_from = "statisticvalue") %>%
  pivot_wider(id_cols = "Variable", names_from = c("wave", "i_start"), 
              names_sep = "_", values_from = c("wtd.mean", "sqrt")) %>%
  mutate(across(-Variable, ~ round(., digits = 3))) %>% # round to 3 digits
  mutate(Variable = c("Number of accessed positions", "Upper reachability", "Lower reachability", "Average status", "Voluntary involvement",
                      "Start employment", "Quit employment", "Start rom. relationship", "Stop rom. relationship", "Childbirth", "Residential mobility",
                      "Low SES", "Medium SES", "High SES", "Migration background", "Woman", "Age (in months)", "Extraversion", "Openness", "Neuroticism", "Conscientiousness", "Agreeableness",
                      "No previous involvement", "One previous involvement", "Multiple previous involvements", 
                      "Potential for volunteering", "Religiosity", "ISCED 0-2", "ISCED 3-4", "ISCED 5-6", "Region", "Populationdensity"))
colnames(sumtabledf) <- c("variable", "uninvolved mean t1", "joiners mean t1", "uninvolved mean t2", "joiners mean t2",
                          "uninvolved sd t1", "joiners sd t1", "uninvolved sd t2", "joiners sd t2")

write.xlsx(sumtabledf, "path/TableS1_weighted.xlsx")

#Unweighted:
sumtabledf_unweighted <- sl %>%
  filter(sample_starting==1) %>%
  group_by(wave, i_start) %>%
  summarise_at(vars(sumvars), funs(mean(.), sqrt(var(.)))) %>%
  pivot_longer(sumvars5b, names_to = "Variable", values_to = "statisticvalue") %>%
  mutate(statistictype = sub(".*\\_", "", Variable),
         Variable = sub("_[^_]+$", "", Variable)) %>%
  pivot_wider(id_cols = c("wave", "i_start", "Variable"), names_from = "statistictype",
              names_sep = ".", values_from = "statisticvalue") %>%
  pivot_wider(id_cols = "Variable", names_from = c("wave", "i_start"), 
              names_sep = "_", values_from = c("mean", "sqrt")) %>%
  mutate(across(-Variable, ~ round(., digits = 3))) %>% # round to 3 digits
  mutate(Variable = c("Number of accessed positions", "Upper reachability", "Lower reachability", "Average status", "Voluntary involvement",
                      "Start employment", "Quit employment", "Start rom. relationship", "Stop rom. relationship", "Childbirth", "Residential mobility",
                      "Low SES", "Medium SES", "High SES", "Migration background", "Woman", "Age (in months)", "Extraversion", "Openness", "Neuroticism", "Conscientiousness", "Agreeableness",
                      "No previous involvement", "One previous involvement", "Multiple previous involvements", 
                      "Potential for volunteering", "Religiosity", "ISCED 0-2", "ISCED 3-4", "ISCED 5-6", "Region", "Populationdensity"))
colnames(sumtabledf_unweighted) <- c("variable", "uninvolved mean t1", "joiners mean t1", "uninvolved mean t2", "joiners mean t2",
                                     "uninvolved sd t1", "joiners sd t1", "uninvolved sd t2", "joiners sd t2")

write.xlsx(sumtabledf_unweighted, "path/TableS1_unweighted.xlsx")

#the same for those who are active at t1 (who are not part of the FE sample):
sumtabledf_involved_t1_unweighted <- slfull %>%
  filter(sample_starting==0) %>%
  group_by(wave) %>%
  summarise_at(vars(sumvars), funs(mean(.), sqrt(var(.)))) %>%
  pivot_longer(sumvars5b, names_to = "Variable", values_to = "statisticvalue") %>%
  mutate(statistictype = sub(".*\\_", "", Variable),
         Variable = sub("_[^_]+$", "", Variable)) %>%
  pivot_wider(id_cols = c("wave", "Variable"), names_from = "statistictype",
              names_sep = ".", values_from = "statisticvalue") %>%
  pivot_wider(id_cols = "Variable", names_from = c("wave"), 
              names_sep = "_", values_from = c("mean", "sqrt")) %>%
  mutate(across(-Variable, ~ round(., digits = 3))) %>% # round to 3 digits
  mutate(Variable = c("Number of accessed positions", "Upper reachability", "Lower reachability", "Average status", "Voluntary involvement",
                      "Start employment", "Quit employment", "Start rom. relationship", "Stop rom. relationship", "Childbirth", "Residential mobility",
                      "Low SES", "Medium SES", "High SES", "Migration background", "Woman", "Age (in months)", "Extraversion", "Openness", "Neuroticism", "Conscientiousness", "Agreeableness",
                      "No previous involvement", "One previous involvement", "Multiple previous involvements", 
                      "Potential for volunteering", "Religiosity", "ISCED 0-2", "ISCED 3-4", "ISCED 5-6", "Region", "Populationdensity"))
colnames(sumtabledf_involved_t1_unweighted) <- c("variable", "involved t1 mean t1", "involved t1 mean t2", "involved t1 sd t1", "involved t1 sd t2")

write.xlsx(sumtabledf_involved_t1_unweighted, "path/TableS1_involved_t1_unweighted.xlsx")


# Robustness checks: sensitivity analysis ####
#relevant values from stata-regression (with robust s.e.), model 3
m3_coef <- 0.5860636
m3_se <- 0.0888276
m3_dof <- 3894
ovb_contour_plot(estimate = m3_coef, se = m3_se , dof = m3_dof, lim = 0.3, sensitivity.of = "estimate")
robustness_value(t_statistic = m3_coef/m3_se, dof = m3_dof)
partial_r2(t_statistic = m3_coef/m3_se, dof = m3_dof)
#ovb_extreme_plot(estimate = m3_coef, se = m3_se , dof = m3_dof, lim = 0.1)

#with bounds:
r2yxj.dx <- partial_r2(t_statistic = m3_coef/m3_se, dof = m3_dof)
r2dxjqr.x <- partial_r2(t_statistic =  0.149685/ 0.185714, dof = m3_dof)
boundsqr <- ovb_partial_r2_bound(r2dxj.x = r2dxjqr.x, r2yxj.dx = r2yxj.dx,
                                 kd = c(10,20, 30), ky = c(10,20, 30),
                                 bound_label = paste(c(10,20,30), "x", "quit rel."))
boundqr.values <- adjusted_estimate(estimate = m3_coef, se = m3_se, dof = m3_dof,
                                    r2dz.x = boundsqr$r2dz.x, r2yz.dx = boundsqr$r2yz.dx)

r2dxjage.x <- partial_r2(t_statistic = -0.0080503/ 0.0011225, dof = m3_dof)
boundsage <- ovb_partial_r2_bound(r2dxj.x = r2dxjage.x, r2yxj.dx = r2yxj.dx,
                                  kd = c( 4, 7), ky = c(4, 7),
                                  bound_label = paste(c(4,7), "x", "age"))
boundage.values <- adjusted_estimate(estimate = m3_coef, se = m3_se, dof = m3_dof,
                                     r2dz.x = boundsage$r2dz.x, r2yz.dx = boundsage$r2yz.dx)


ovb_contour_plot(estimate = m3_coef, se = m3_se , dof = m3_dof, lim = 0.4, label.bump.x = 0.015, label.bump.y = 0.015, sensitivity.of = "estimate")
add_bound_to_contour(boundsqr, bound_value = boundqr.values, label.bump.x = 0.015, label.bump.y = 0.015)
add_bound_to_contour(boundsage, bound_value = boundage.values, label.bump.x = 0.015, label.bump.y = 0.015)

