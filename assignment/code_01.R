# this code is for the problem set of topic in macro and Labor course
# code is beased on the following two papers
# 1. Acemoglu, D. and Autor, D. (2011). Handbook of Labor Economics, 4b:1043–1171
# 2. Katz, L. F. and Murphy, K. M. (1992). The Quarterly Journal of Economics
# clear environment
rm(list = ls())
# laad libaries
library(tidyverse)
library(lubridate)
# read the downloaded
data_00 <- read_fwf(file="data_00.dat",                     
                    fwf_cols(year      = c(1, 4),
                             serial    = c(5,9),
                             month     = c(10,11),
                             hwtfinl   = c(12,21),
                             cpsid     = c(22,35),
                             asecflag  = c(36,36),
                             hflag     = c(37,37),
                             asecwth   = c(38,47),
                             pernum    = c(48,49),
                             wtfinl    = c(50,63),
                             cpsidp    = c(64,77),
                             asecwt    = c(78,87),
                             age       = c(88,89),
                             sex       = c(90,90),
                             race      = c(91,93),
                             educ      = c(94,96),
                             schlcoll  = c(97,97),
                             indly     = c(98,101),
                             classwly  = c(102,103),
                             wkswork1  = c(104,105),
                             wkswork2  = c(106,106),
                             fullpart  = c(107,107),
                             incwage   = c(108,114)),
                 col_types = cols(year       = "i",
                                  serial     = "n",
                                  month      = "i",
                                  hwtfinl    = "d",
                                  cpsid      = "d",
                                  asecflag   = "i",
                                  hflag      = "i",
                                  asecwth    = "d",
                                  pernum     = "i",
                                  wtfinl     = "d",
                                  cpsidp     = "d",
                                  asecwt     = "d",                    
                                  age        = "i",
                                  sex        = "i",
                                  race       = "i",
                                  educ       = "i",
                                  schlcoll   = "i",
                                  indly      = "i",
                                  classwly   = "i",
                                  wkswork1   = "i",
                                  wkswork2   = "i",
                                  fullpart   = "i",
                                  incwage    = "n"))
data_00$hwtfinl = data_00$hwtfinl/10000
data_00$wtfinl = data_00$wtfinl/10000
data_00$asecwt = data_00$asecwt/10000
# merge cpi data (see Acemoglu and Autor's Data Appendix)
data_cpi <- read_csv(file = "data_cpi.csv", col_names = c("year","cpi"), col_types=cols(year = "D", cpi = "d"), skip = 1)
data_cpi$year <- year(data_cpi$year)
data_cpi <- data_cpi %>%
  mutate(price_1982 = ifelse(year == 1982, cpi, 0)) %>% # the base year is 1982 (see Acemoglu and Autor's Data Appendix)
  mutate(price_1982 = max(price_1982)) %>%
  mutate(cpi = cpi/price_1982) %>%
  select(year, cpi)
data_00 <- data_00 %>%
  left_join(data_cpi, by = "year")
# replace missing values
data_00 <- data_00 %>%
  mutate(educ = ifelse(educ == 999, NA, educ)) %>%
  mutate(classwly = ifelse(classwly == 99, NA, classwly)) %>%  
  mutate(wkswork2 = ifelse(wkswork2 == 999, NA, wkswork2)) %>%  
  mutate(incwage = ifelse(incwage == 9999999 | incwage == 9999998, NA, incwage)) %>%
  mutate(race = ifelse(race == 999, NA, race))
# create wrkswork variable: worked weeks are in brackets before 1976 see Katz and Murphy (1992)
data_00 <- data_00 %>%
  mutate(wkswork = ifelse(year >= 1976, wkswork1, NA)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 1, 7, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 2, 20, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 3, 33, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 4, 43.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 5, 48.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 6, 51, wkswork))
# handle the top coding issue for income see Katz and Murphy (1992)'s Data section
data_00 <- data_00 %>%
  group_by(year) %>%
  mutate(top_incwage = max(incwage, na.rm = TRUE)) %>%
  mutate(incwage = ifelse(incwage == top_incwage, 1.45*incwage, incwage)) %>%
  ungroup()
# calculate log real wages
data_00 <- data_00 %>%
  mutate(rwage = incwage/cpi/wkswork) %>%
  mutate(lrwage = log(rwage))
# create education duammies
data_00 <- data_00 %>%
  mutate(dfemale = (sex == 2)) # female
data_00 <- data_00 %>%      
  mutate(deduc_1 = ifelse(educ < 70, 1, 0)) %>%                # highshool dropout
  mutate(deduc_2 = ifelse(educ >= 80 & educ < 110, 1, 0)) %>%  # some college
  mutate(deduc_3 = ifelse(educ >= 110 & educ < 123, 1, 0)) %>% # 4 years college 
  mutate(deduc_4 = ifelse(educ >= 123, 1, 0))                  # more than college
data_00 <- data_00 %>%
  mutate(drace_1 = (race == 200)) %>% # black
  mutate(drace_2 = (race > 200)) # nonwhite other
# create experience variable: check the IPUMS website for variable definition
data_00 <- data_00 %>%
  mutate(exp = ifelse(educ == 10, age - 8.5, NA)) %>%
  mutate(exp = ifelse(educ == 11, age - 7, exp)) %>%
  mutate(exp = ifelse(educ == 12, age - 8, exp)) %>%
  mutate(exp = ifelse(educ == 13, age - 9, exp)) %>%
  mutate(exp = ifelse(educ == 14, age - 10, exp)) %>%
  mutate(exp = ifelse(educ == 20, age - 11.5, exp)) %>%
  mutate(exp = ifelse(educ == 21, age - 11, exp)) %>%
  mutate(exp = ifelse(educ == 22, age - 12, exp)) %>%
  mutate(exp = ifelse(educ == 30, age - 13.5, exp)) %>%
  mutate(exp = ifelse(educ == 31, age - 13, exp)) %>%
  mutate(exp = ifelse(educ == 32, age - 14, exp)) %>%
  mutate(exp = ifelse(educ == 40, age - 15, exp)) %>%
  mutate(exp = ifelse(educ == 50, age - 16, exp)) %>%
  mutate(exp = ifelse(educ == 60, age - 17, exp)) %>%
  mutate(exp = ifelse(educ == 70, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 71, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 72, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 73, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 80, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 81, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 90, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 91, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 92, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 100, age - 21, exp)) %>%
  mutate(exp = ifelse(educ == 110, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 111, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 120, age - 23.5, exp)) %>%
  mutate(exp = ifelse(educ == 121, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 122, age - 24, exp)) %>%
  mutate(exp = ifelse(educ == 123, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 124, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 125, age - 27, exp))
# sample selection (see Katz and Murphy (1992) and Acemoglu and Autor (2011)'s Data Appendix)
data_00 <- data_00 %>%
  filter(rwage >= 67) %>%                                                       # real wage more than 67 dollars in the 1987 dollar
  filter(age >= 16 & age <= 64) %>%                                             # age equal or above 16 and equal or less than 64
  filter(fullpart == 1) %>%                                                     # work more than 35 hours
  filter(wkswork >= 40) %>%                                                     # work more than 40 weeks
  filter(classwly != 10 | classwly != 13 | classwly != 14) %>%                  # not self-employed
  filter(!((year >= 1992 & year <= 2002) & (indly >= 940 & indly <= 960))) %>%  # not in military
  filter(!(year >= 2003 & indly == 9890)) %>%
  filter(schlcoll == 5 | year < 1986) %>%                                       # no school attendance
  filter(exp >= 0)                                                              # get rid of negative experience
# graphing figures from HERE


















