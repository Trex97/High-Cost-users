
# 0. install package ------------------------------------------------------
# install.packages("readr")
# install.packages("dplyr")
 # install.packages("tidyverse")
 # install.packages("glue")
 # install.packages("lubridate")
 # install.packages("comorbidity")
library(readr)
library(dplyr)
library(tidyverse)
library(glue)
library(lubridate)
library(writexl)
library(comorbidity)

# 1. import dataset ----------------------------------------------------------

setwd("C:/Users/Administrator/Desktop/HCUs/Data") # set working directory

ipd_eclaim2016 <- read_delim("ipd_eclaim2016.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2017 <- read_delim("ipd_eclaim2017.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2018 <- read_delim("ipd_eclaim2018.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2019 <- read_delim("ipd_eclaim2019.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2020 <- read_delim("ipd_eclaim2020.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2021 <- read_delim("ipd_eclaim2021.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2022 <- read_delim("ipd_eclaim2022.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2023 <- read_delim("ipd_eclaim2023.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)
ipd_eclaim2024 <- read_delim("ipd_eclaim2024.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)


master_dataset <- rbind(ipd_eclaim2016,ipd_eclaim2017,ipd_eclaim2018,ipd_eclaim2019,ipd_eclaim2020,ipd_eclaim2021,ipd_eclaim2022,ipd_eclaim2023,ipd_eclaim2024)
# 2. select variable ------------------------------------------------------

sdx_ <- paste0("sdx", 1:21) 
proc_ <- paste0("proc", 1:21)
master_dataset_73 <- master_dataset %>%
  select(tran_id,hcode,dateadm,datedsc,person_type,pid,sex, age_year,age_month,age_day,dischs,discht,hmain2,prov1,prov2,sums_serviceitem,drg_nhso,rw_nhso,adjrw_nhso,maininscl_af,rgn1_af,rgn2_af,los,nation,totl_amt,act_amt,poor,death_date,death_age_year,pdx,sdx_,proc_,sum_reimbursement_service)

glimpse(master_dataset_73)

# 3. create date variable  -----------------------------------------------------------
# Variable dateadm
master_dataset_73 <- master_dataset_73 %>%
  mutate(dateadm2 = dateadm)

glimpse(master_dataset_73)

master_dataset_73 <- master_dataset_73 %>%
  mutate(y_adm = substr(dateadm,1,4))

master_dataset_73 <- master_dataset_73 %>%
  mutate(m_adm = substr(dateadm,5,6))

master_dataset_73 <- master_dataset_73 %>%
  mutate(d_adm = substr(dateadm,7,8))

master_dataset_73 <- master_dataset_73 %>%
  mutate(new_dateadm = glue("{d_adm}-{m_adm}-{y_adm}"))

master_dataset_73$new_dateadm <- dmy(master_dataset_73$new_dateadm)

master_dataset_73 <- master_dataset_73 %>%
  mutate(day_adm = day(new_dateadm),
         month_adm = months(new_dateadm),
         year_adm = year(new_dateadm),
         full_month_adm = months(new_dateadm,abbr = F),
         full_month2_adm = months(new_dateadm,abbr = T),
         week_day_adm = weekdays(new_dateadm,abbr = FALSE))

# Variable datedsc

glimpse(master_dataset_73)

master_dataset_73 <- master_dataset_73 %>%
  mutate(y_dsc = substr(datedsc,1,4))

master_dataset_73 <- master_dataset_73 %>%
  mutate(m_dsc = substr(datedsc,5,6))

master_dataset_73 <- master_dataset_73 %>%
  mutate(d_dsc = substr(datedsc,7,8))

master_dataset_73 <- master_dataset_73 %>%
  mutate(new_datedsc = glue("{d_dsc}-{m_dsc}-{y_dsc}"))

master_dataset_73$new_datedsc <- dmy(master_dataset_73$new_datedsc)

master_dataset_73 <- master_dataset_73 %>%
  mutate(day_dsc = day(new_datedsc),
         month_dsc = months(new_datedsc),
         year_dsc = year(new_datedsc),
         full_month_dsc = months(new_datedsc,abbr = F),
         full_month2_dsc = months(new_datedsc,abbr = T),
         week_day_dsc = weekdays(new_datedsc,abbr = FALSE))

# 4. create unique_id variable  -------------------------------------------

master_dataset_73 <- master_dataset_73 %>%
  arrange(pid,new_dateadm) %>%
  mutate(unique_id = group_indices(.,pid))

glimpse(master_dataset_73)
max(master_dataset_73$unique_id) #21665649


#write.table(master_dataset_73,"master_dataset_73_test.txt", sep = "|")


# 5. check sex ------------------------------------------------------------
check_sex <- master_dataset_73 %>%
  group_by(pid,sex) %>%
  count()

check_sex <- check_sex %>%
  mutate(unique_pid = if_else(duplicated(pid),1,0)) %>%
  arrange(pid)

check_sex2 <- check_sex %>%
  group_by(unique_pid) %>%
  count()

check_sex %>%
  group_by(unique_pid) %>%
  count()
# unique_pid        n
# <int>
#  0          21665649

# 6. check age ---------------------------------------------------------------

check_age <- master_dataset_73

check_age <- check_age %>%
  select(pid,tran_id,age_year,new_dateadm,year) %>%
  arrange(pid,new_dateadm)

check_age<- check_age %>%
  group_by(pid) %>%
  mutate(age_diff = as.numeric(age_year - lag(age_year, default = first(age_year)))) %>%
  ungroup()
??lag

# Be careful! There is -1 in diff age 
check_age %>%
  group_by(age_diff) %>%
  count()
# age_diff        n
# <dbl>    <int>
# 1       -6        1
# 2       -2        4
# 3       -1     3288
# 4        0 39611230
# 5        1  6820234
# 6        2  2546367
# 7        3  1459987
# 8        4   906885
# 9        5   574277
# 10        6   334367
# 11        7   164852
# 12        8    48021
# 13        9     1433

check_age <- check_age %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

aa <- table(year_diff = check_age$year_diff,age_diff = check_age$age_diff)

#                                                                       age_diff
# year_diff       -6       -2       -1        0        1        2        3        4        5        6        7        8        9
#         0        1        4     3277 37368308  1932996      118       15        2        0        2        0        0        0
#         1        0        0       10  2242816  4272975   456757       33        0        1        0        0        0        0
#         2        0        0        1       94   614230  1765009   246236       14        1        0        0        0        0
#         3        0        0        0       10       30   324470  1018722   152608        7        0        0        0        0
#         4        0        0        0        0        3       12   194970   630798    97826        2        0        0        0
#         5        0        0        0        1        0        1       11   123462   399658    61636        1        0        0
#         6        0        0        0        1        0        0        0        1    76783   232822    32315        0        0
#         7        0        0        0        0        0        0        0        0        1    39904   116732    15838        0
#         8        0        0        0        0        0        0        0        0        0        1    15804    32183     1433
print(aa)
check_age %>%
  filter(age_diff <=-2 ) %>%
  select(pid)
# 1 0446f0d4a94f9d683d96327b63911a81b423ad30f8290a484ebf148c
# 2 b3ab3795ec3f6979614aedf79dda0d6728291c66478846089efad9d2
# 3 cb65098ff6124e756c450c8243629832c586db5c96bfe14d0f728091
# 4 d1c0348c0aadc5806ccf9fce5281f36fec5c8f43b96de4979d0173d8
# 5 eb7c54213db3099989d75af47c6858ea88fc277f50dc6de8532c5a59

glimpse(check_age)

check_age2 <- check_age %>%
  filter(pid %in% c("0446f0d4a94f9d683d96327b63911a81b423ad30f8290a484ebf148c",
                    "b3ab3795ec3f6979614aedf79dda0d6728291c66478846089efad9d2",
                    "cb65098ff6124e756c450c8243629832c586db5c96bfe14d0f728091",
                    "d1c0348c0aadc5806ccf9fce5281f36fec5c8f43b96de4979d0173d8",
                    "eb7c54213db3099989d75af47c6858ea88fc277f50dc6de8532c5a59"))%>%
  arrange(pid,new_dateadm)


#write_xlsx(check_age2,"age_diff_negative.xlsx")

# check age diff = 3 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 3  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff3 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff3 <- check_age_diff3 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff3 %>%
  group_by(year_diff) %>%
  count()
# year_diff       n
# <dbl>   <int>
# 1         0 3365486
# 2         1  850092
# 3         2  508542
# 4         3 1057793
# 5         4  232903
# 6         5   13901
# 7         6     970

table(year_diff = check_age_diff3$year_diff,age_diff = check_age_diff3$age_diff)


# check age diff = 4 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 4  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff4 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff4 <- check_age_diff4 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff4 %>%
  group_by(year_diff) %>%
  count()
# year_diff       n
# <dbl>   <int>
# 1         0 1924958
# 2         1  433237
# 3         2  138722
# 4         3  201513
# 5         4  635785
# 6         5  123615
# 7         6       1

# check age diff = 5 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 5  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff5 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff5 <- check_age_diff5 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff5 %>%
  group_by(year_diff) %>%
  count()
# year_diff       n
# <dbl>   <int>
# 1         0 1108590
# 2         1  211117
# 3         2   59088
# 4         3   14372
# 5         4   98636
# 6         5  399658
# 7         6   76783
# 8         7       1

# check age diff = 6 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 6  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff6 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff6 <- check_age_diff6 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff6 %>%
  group_by(year_diff) %>%
  count()
# year_diff      n
# <dbl>  <int>
# 1         0 599961
# 2         1  91889
# 3         2  16937
# 4         3    983
# 5         4      2
# 6         5  61637
# 7         6 232822
# 8         7  39904
# 9         8      1

# check age diff = 7 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 7  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff7 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff7 <- check_age_diff7 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff7 %>%
  group_by(year_diff) %>%
  count()
# year_diff      n
# <dbl>  <int>
# 1         0 274246
# 2         1  27058
# 3         2   1022
# 4         5      1
# 5         6  32315
# 6         7 116732
# 7         8  15804

# check age diff = 8 ------------------------------------------------------

pid_age_diff <- check_age %>%
  filter(age_diff == 8  ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff8 <- check_age %>%
  filter(pid %in% pid_age_diff) %>%
  arrange(pid,new_dateadm)

check_age_diff8 <- check_age_diff8 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff8 %>%
  group_by(year_diff) %>%
  count()
# year_diff     n
# <dbl> <int>
# 1         0 71438
# 2         1  2377
# 3         7 15838
# 4         8 32183



# check age diff = 9 ------------------------------------------------------

pid_age_diff9 <- check_age %>%
  filter(age_diff >=9 ) %>%
  select(pid) %>%
  pull(pid)  # Extracts the pid column as a vector

check_age_diff9 <- check_age %>%
  filter(pid %in% pid_age_diff9) %>%
  arrange(pid,new_dateadm)

check_age_diff9 <- check_age_diff9 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year,default =first(year)))) %>%
  ungroup()

check_age_diff9 %>%
  group_by(year_diff) %>%
  count()
# year_diff     n
# <dbl> <int>
# 1         0  1848
# 2         8  1433

#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------- CLEAN AGE -------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------


# 1. Import dataset -------------------------------------------------------
setwd("D:/HICOST_USER/HCUs/1. UCS/Data") # set working directory
master_dataset_73 <- read.table("master_dataset_73_test.txt", sep = "|")
glimpse(master_dataset_73)

master_dataset_73$new_dateadm <- as.Date(master_dataset_73$new_dateadm)
glimpse(master_dataset_73)

# 2. check age ---------------------------------------------------------------

# # Create master-dataset_73_test = master_dataset_73
# master_dataset_73_test <- master_dataset_73
# glimpse(master_dataset_73_test) 

# create number == tran_id
master_dataset_73 <- master_dataset_73 %>%
  arrange(new_dateadm) %>%
  mutate(number = row_number())

max(master_dataset_73$number)
# 52,470,946

# separate dataset for clean age 
check_age <- master_dataset_73

# Keep variables for check age 
check_age <- check_age %>%
  select(pid,unique_id,tran_id,number,age_year,new_dateadm,year) %>%
  arrange(number)


# Create age_diff = Present row - Previous_row 
check_age<- check_age %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff = as.numeric(age_year - lag(age_year, default = first(age_year)))) %>%
  ungroup()


# Be careful! There is -1 in diff age 
check_age %>%
  group_by(age_diff) %>%
  count()
# show age_diff 
# age_diff        n
# <dbl>    <int>
# 1       -6        1
# 2       -2        4
# 3       -1     3288
# 4        0 39611230
# 5        1  6820234
# 6        2  2546367
# 7        3  1459987
# 8        4   906885
# 9        5   574277
# 10        6   334367
# 11        7   164852
# 12        8    48021
# 13        9     1433


# Filter age_diff negative 
# Clean negative age first -------------------------------------------------
# Create uniqueid_age_negative = list pid is negative age 
uniqueid_age_negative <- check_age %>%
  filter(age_diff <= -1)  %>%
  select(unique_id) %>%
  pull(unique_id)

glimpse(uniqueid_age_negative)


# Filter all observation in uniqueid_age_negative
age_negative <- check_age %>%
  filter(unique_id %in% uniqueid_age_negative) %>%
  arrange(number)

# Use mode function to revise negative age 
# Which.max select last obs
age_negative <- age_negative %>%
  arrange(number) %>%
  group_by(unique_id,year) %>%
  mutate(new_age = as.numeric(names(which.max(rev(table(age_year)))))) %>%
  ungroup()



# Create age_negative_diff2 for check difference between visits 
# Check after use mode function
age_negative <- age_negative %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_negative_diff2 = as.numeric(new_age - lag(new_age, default = first(new_age)))) %>%
  ungroup()

# data set sitll have negative age negative 
age_negative %>%
  group_by(age_negative_diff2) %>%
  count()

# age_diff2     n
# <dbl> <int>
# 1        -1     6
# 2         0 20772
# 3         1  2455
# 4         2  1010
# 5         3   443
# 6         4   218
# 7         5   136
# 8         6    72
# 9         7    33
# 10         8     8

# uniqueid_age_negative2 = list age negative (mode function doesn't work)
uniqueid_age_negative2 <- age_negative %>%
  filter(age_negative_diff2 <= -1)  %>%
  select(unique_id) %>%
  pull(unique_id)

glimpse(uniqueid_age_negative2)

# filter age_negative have unique_id in uniqueid_age_negative2 
age_negative_mode_not_work  <- age_negative %>%
  filter(unique_id %in% uniqueid_age_negative2)  %>%
  arrange(number)

# create new_age2 = if age_negative_diff2 < 0 , replace newage+1
age_negative_mode_not_work <- age_negative_mode_not_work %>%
  mutate(new_age2 = if_else(age_negative_diff2 < 0, new_age+1,new_age)) # unique id ="12666975" age 53 every year >>problem<< 

# create age_newgative_diff3 for check difference between visits again
# don't have negative age 
age_negative_mode_not_work <- age_negative_mode_not_work %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_negative_diff3 = as.numeric(new_age2 - lag(new_age2, default = first(new_age2)))) %>%
  ungroup()


# age_negative_mode_not_work_tranid  = keep new-age2 follow by tran_id
age_negative_mode_not_work_tranid <- age_negative_mode_not_work %>%
  select(tran_id,new_age2)

# this step to merge  age_negative_mode_not_work_tranid to age_negative 
age_negative <- merge(x= age_negative , y = age_negative_mode_not_work_tranid, by = "tran_id", all.x =  TRUE)
  

# when merge complete, dataset will show two age column 
# new_age = revise negative age with mode 
# new_age2 = revise newgative age with manual 
# create age_negative_revise variable = if new_age2 is NULL replace new_age
age_negative <- age_negative %>%
  mutate(age_negative_revise = if_else(is.na(new_age2),new_age,new_age2)) %>%
  arrange(number)

# check age_diff again when merge dataset complete. 
age_negative <- age_negative %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_negative_diff3 = as.numeric(age_negative_revise - lag(age_negative_revise, default = first(age_negative_revise)))) %>%
  ungroup()

age_negative %>%
  group_by(age_negative_diff3) %>%
  count()
# age_negative_diff3     n
# 1                  0 20779
# 2                  1  2457
# 3                  2  1007
# 4                  3   443
# 5                  4   218
# 6                  5   136
# 7                  6    72
# 8                  7    33
# 9                  8     8

table(age = age_negative$age_negative_diff3,year =age_negative$year)
#   2016 2017 2018 2019 2020 2021 2022 2023 2024
# 0 2663 3004 2739 2543 2403 2224 2136 2211  856
# 1    0  290  359  346  348  303  297  312  202
# 2    0   48  142  158  148  140  179  127   65
# 3    0    0   20   79   81   66   91   75   31
# 4    0    0    0   12   36   55   53   44   18
# 5    0    0    0    0    7   33   33   49   14
# 6    0    0    0    0    0   14   19   30    9
# 7    0    0    0    0    0    0    7   16   10
# 8    0    0    0    0    0    0    0    4    4


# Copy data set from age_negative to age_negative_for_merge 
age_negative_for_merge <- age_negative

# Select variable "tran_id" & "age_negative_revise" for merge
age_negative_for_merge <- age_negative_for_merge %>%
  select(tran_id,age_negative_revise)


# Replace age_negative_for_merge by tran_id in check_age 
# for revise negative age 
glimpse(check_age)
check_age <- merge(x= check_age, y = age_negative_for_merge, by = "tran_id" , all.x = TRUE)

glimpse(check_age)

# When merge complete, dataset will show age two column 
# age_year = original age
# age_negative_revise = age revise from above process
# Create age_negative_revise variable = if age_negative_revise  is NULL replace age_year
check_age <- check_age %>%
  mutate(age_negative_revise = if_else(is.na(age_negative_revise),age_year,age_negative_revise))

# Check age_diff again when merge dataset complete.
check_age <- check_age %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff2 = as.numeric(age_negative_revise - lag(age_negative_revise, default = first(age_negative_revise)))) %>%
  ungroup()

# must have no age_negative 
check_age %>%
  group_by(age_diff2) %>%
  count()

#   age_diff2        n
# 1         0 39617014
# 2         1  6817710
# 3         2  2546408
# 4         3  1460019
# 5         4   906878
# 6         5   574263
# 7         6   334369
# 8         7   164830
# 9         8    48025
# 10       9     1430

# rename variable age_negative_revise to "age_year_new"
names(check_age)[names(check_age) == "age_negative_revise"] <- "age_year_new"

glimpse(check_age)

# select variable prepare to merge
check_age_for_merge <- check_age %>%
  select(number,age_year_new)
# merge age_year_new to master_dataset_73
master_dataset_73 <- merge(x= master_dataset_73 , y = check_age_for_merge , by = "number" , all.x = TRUE)

#write.table(master_dataset_73,"master_dataset_73_clean_age_sex.txt", sep = "|")

glimpse(master_dataset_73)
master_dataset_73<- master_dataset_73 %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff = as.numeric(age_year_new - lag(age_year_new, default = first(age_year_new)))) %>%
  ungroup()

master_dataset_73<- master_dataset_73 %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(year_diff = as.numeric(year - lag(year, default = first(year)))) %>%
  ungroup()

year_age_diff <- table(year_diff = master_dataset_73$year_diff,age_diff = master_dataset_73$age_diff)

print(year_age_diff)
#          0        1        2        3        4        5        6        7        8        9
# 0 37374376  1930217      112       15        2        0        1        0        0        0
# 1  2242529  4273205   456824       33        0        1        0        0        0        0
# 2       96   614255  1764991   246227       15        1        0        0        0        0
# 3       11       30   324468  1018746   152585        7        0        0        0        0
# 4        0        3       12   194987   630792    97815        2        0        0        0
# 5        1        0        1       11   123483   399643    61630        1        0        0
# 6        1        0        0        0        1    76795   232822    32303        0        0
# 7        0        0        0        0        0        1    39913   116725    15836        0
# 8        0        0        0        0        0        0        1    15801    32189     1430

# Run check -----------------------------------------------------------------------
glimpse(master_dataset_73)
list <- master_dataset_73 %>%
  filter(year_diff == 3 & age_diff ==1) %>%
  select(unique_id) %>%
  pull(unique_id)

check_test <- master_dataset_73 %>%
  filter(unique_id == 4129405) %>%
  select(unique_id,number,new_dateadm,age_year_new,age_diff,year_diff)

check_test <- master_dataset_73 %>%
  filter(unique_id %in% c(9819372,433169,2997653)) %>%
  select(unique_id,number,new_dateadm,age_year_new,age_diff,year_diff) %>%
  arrange(unique_id,number)

check_test <- master_dataset_73 %>%
  filter(unique_id %in% c(7941384,2245920)) %>%
  select(unique_id,number,new_dateadm,age_year_new,age_diff,year_diff) %>%
  arrange(unique_id,number)

check_test <- master_dataset_73 %>%
  filter(unique_id %in% list) %>%
  select(unique_id,number,new_dateadm,age_year_new,age_diff,year_diff) %>%
  arrange(unique_id,number)

check_test <- check_test %>%
  mutate(delta_year_age = year_diff-age_diff)

check_test <- check_test %>%
  mutate(age_new1 = age_year_new + delta_year_age)

check_test <- check_test %>%
  group_by(unique_id) %>%
  mutate(age_diff1 = as.numeric(age_new1 - lag(age_new1,default = first(age_new1)))) %>%
  ungroup()

#write.csv(master_dataset_73,"master_dataset_73_clean_age_sex.csv")


#--------------------------------------------------------------------------------------------------

# Part clean Positive age ----------------------------------------------------------------------------
# Import dataset 

glimpse(master_dataset_73)

# copy dataset 
check_age_positive <- master_dataset_73 %>%
  select(unique_id,number,new_dateadm,year,age_year_new,age_diff,year_diff)


check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age = year_diff-age_diff)

glimpse(check_age_positive)
check_age_positive %>%
  group_by(delta_year_age) %>%
  count()

#    delta_year_age        n
#  1             -6        1
#  2             -4        3
#  3             -3       16
#  4             -2      169
#  5             -1  2994793
#  6              0 45843584
#  7              1  3632211
#  8              2      152
#  9              3       15
# 10              5        1
# 11              6        1

# STEP.1 ---------------------------------------------------
# if negative delta_year_age -> age_year_new + delta_year_age 
check_age_positive <- check_age_positive %>%
  mutate(age_year_new1 = if_else(delta_year_age <= -2,age_year_new+ delta_year_age,age_year_new))


# create age_diff1 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff1 = as.numeric(age_year_new1 - lag(age_year_new1,default = first(age_year_new1)))) %>%
  ungroup()

# table for check 
table(year_diff = check_age_positive$year_diff , age_diff1 = check_age_positive$age_diff1)


# create delta_year-age1 = for check 
# shouldn't be delta_year_age -2 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age1 = year_diff-age_diff1)


# tab table delta_year_age1 
check_age_positive %>%
  group_by(delta_year_age1) %>%
  count()

# delta_year_age1        n
# 1              -2        8
# 2              -1  2994863
# 3               0 45843799
# 4               1  3632147
# 5               2      118
# 6               3       10
# 7               6        1

list <- check_age_positive %>%
  filter(delta_year_age1 == -2) %>%
  pull(unique_id)

check <- check_age_positive %>%
  filter(unique_id %in% list) %>%
  arrange(unique_id)
	


# STEP.2 ---------------------------------------------------
# if negative delta_year_age1 -> age_year_new1 + delta_year_age1 
check_age_positive <- check_age_positive %>%
  mutate(age_year_new2 = if_else(delta_year_age1 <= -2,age_year_new1+ delta_year_age1,age_year_new1))


# create age_diff2 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff2 = as.numeric(age_year_new2 - lag(age_year_new2,default = first(age_year_new2)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff2 = check_age_positive$age_diff2)


# create delta_year-age1 = for check 
# shouldn't be delta_year_age -2 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age2 = year_diff-age_diff2)


# tab table delta_year_age1 
check_age_positive %>%
  group_by(delta_year_age2) %>%
  count()
# delta_year_age2        n
# 1              -2        1
# 2              -1  2994866
# 3               0 45843808
# 4               1  3632144
# 5               2      116
# 6               3       10
# 7               6        1

# STEP.3 ---------------------------------------------------

check_age_positive %>%
  filter(delta_year_age2 == -2)

check_7491978 <- check_age_positive %>%
  filter(unique_id == 7491978) ## discuss tmr

check_age_positive <- check_age_positive %>%
  mutate(age_year_new3 = if_else(delta_year_age2 <= -2,age_year_new2+ delta_year_age2,age_year_new2))


# create age_diff3 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff3 = as.numeric(age_year_new3 - lag(age_year_new3,default = first(age_year_new3)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff3 = check_age_positive$age_diff3)


# create delta_year_age3 = for check 
# shouldn't be delta_year_age -2 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age3 = year_diff-age_diff3)


# tab table delta_year_age3
check_age_positive %>%
  group_by(delta_year_age3) %>%
  count()

# delta_year_age3        n
# 1              -1  2994867
# 2               0 45843809
# 3               1  3632143
# 4               2      116
# 5               3       10
# 6               6        1


# STEP.4 ---------------------------------------------------

check_age_positive %>%
  filter(delta_year_age3 == 6)

check <- check_age_positive %>%
  filter(unique_id == 5416640)



check_age_positive <- check_age_positive %>%
  mutate(age_year_new4 = if_else(delta_year_age3 >= 2,age_year_new3+ delta_year_age3,age_year_new3))


# create age_diff4 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff4 = as.numeric(age_year_new4 - lag(age_year_new4,default = first(age_year_new4)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff4 = check_age_positive$age_diff4)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age4 = year_diff-age_diff4)


# tab table delta_year_age4
check_age_positive %>%
  group_by(delta_year_age4) %>%
  count()

# delta_year_age4        n
# 1              -1  2994837
# 2               0 45843893
# 3               1  3632171
# 4               2       37
# 5               3        7
# 6               6        1

# STEP.5 ---------------------------------------------------

check_age_positive <- check_age_positive %>%
  mutate(age_year_new5 = if_else(delta_year_age4 >= 2,age_year_new4+ delta_year_age4,age_year_new4))


# create age_diff5 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff5 = as.numeric(age_year_new5 - lag(age_year_new5,default = first(age_year_new5)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff5 = check_age_positive$age_diff5)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age5 = year_diff-age_diff5)


# tab table delta_year_age5
check_age_positive %>%
  group_by(delta_year_age5) %>%
  count()


# delta_year_age5        n
# 1              -1  2994827
# 2               0 45843917
# 3               1  3632179
# 4               2       20
# 5               3        3

# STEP.6 ---------------------------------------------------

check_age_positive <- check_age_positive %>%
  mutate(age_year_new6 = if_else(delta_year_age5 >= 2,age_year_new5+ delta_year_age5,age_year_new5))


# create age_diff6 for check 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff6 = as.numeric(age_year_new6 - lag(age_year_new6,default = first(age_year_new6)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff6 = check_age_positive$age_diff6)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age6 = year_diff-age_diff6)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age6) %>%
  count()
# delta_year_age6        n
# 1              -1  2994821
# 2               0 45843926
# 3               1  3632182
# 4               2       15
# 5               3        2

# STEP.7 ---------------------------------------------------

check_age_positive %>%
  filter(delta_year_age6 == 3)

check <- check_age_positive %>%
  filter(unique_id %in% c(17132878,12666975)) %>%
  arrange(unique_id,number)


check_age_positive <- check_age_positive %>%
  select(unique_id,number,new_dateadm,year,year_diff,age_year_new6, age_diff6,delta_year_age6) %>%
  mutate(age_year_new7 = if_else(delta_year_age6 >= 2,age_year_new6+ delta_year_age6,age_year_new6))

# create age_diff7 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff7 = as.numeric(age_year_new7 - lag(age_year_new7,default = first(age_year_new7)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff7 = check_age_positive$age_diff7)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age7 = year_diff-age_diff7)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age7) %>%
  count()






# STEP.8 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new8 = if_else(delta_year_age7 >= 2,age_year_new7+ delta_year_age7,age_year_new7))

# create age_diff7 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff8 = as.numeric(age_year_new8 - lag(age_year_new8,default = first(age_year_new8)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff8 = check_age_positive$age_diff8)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age8 = year_diff-age_diff8)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age8) %>%
  count()

# delta_year_age7        n
# 1              -1  2994820
# 2               0 45843932
# 3               1  3632182
# 4               2       12














# STEP.9 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new9 = if_else(delta_year_age8 >= 2,age_year_new8+ delta_year_age8,age_year_new8))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff9 = as.numeric(age_year_new9 - lag(age_year_new9,default = first(age_year_new9)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff9 = check_age_positive$age_diff9)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age9 = year_diff-age_diff9)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age9) %>%
  count()

#   delta_year_age9        n
# 1              -1  2994815
# 2               0 45843942
# 3               1  3632182
# 4               2        7

# STEP.10 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new10 = if_else(delta_year_age9 >= 2,age_year_new9+ delta_year_age9,age_year_new9))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff10 = as.numeric(age_year_new10 - lag(age_year_new10,default = first(age_year_new10)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff10 = check_age_positive$age_diff10)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age10 = year_diff-age_diff10)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age10) %>%
  count()
# delta_year_age10        n
# 1               -1  2994814
# 2                0 45843946
# 3                1  3632182
# 4                2        3
# 5                3        1


# STEP.11 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new11 = if_else(delta_year_age10 >= 2,age_year_new10+ delta_year_age10,age_year_new10))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff11 = as.numeric(age_year_new11 - lag(age_year_new11,default = first(age_year_new11)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff11 = check_age_positive$age_diff11)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age11 = year_diff-age_diff11)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age11) %>%
  count()

lis <- check_age_positive %>%
  filter(delta_year_age11 == 2) %>%
  pull(unique_id)


check <-  check_age_positive %>%
  filter(unique_id %in% lis)

# delta_year_age11        n
# 1               -1  2994813
# 2                0 45843947
# 3                1  3632182
# 4                2        4

check <- check %>%
  arrange(unique_id,number)

# STEP.12 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new12 = if_else(delta_year_age11 >= 2,age_year_new11+ delta_year_age11,age_year_new11))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff12 = as.numeric(age_year_new12 - lag(age_year_new12,default = first(age_year_new12)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff12 = check_age_positive$age_diff12)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age12 = year_diff-age_diff12)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age12) %>%
  count()


# delta_year_age12        n
# 1               -1  2994813
# 2                0 45843950
# 3                1  3632181
# 4                2        1
# 5                3        1

# STEP.13 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new13 = if_else(delta_year_age12 >= 2,age_year_new12+ delta_year_age12,age_year_new12))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff13 = as.numeric(age_year_new13 - lag(age_year_new13,default = first(age_year_new13)))) %>%
  ungroup()


# table for check 
table(year_diff = check_age_positive$year_diff , age_diff13 = check_age_positive$age_diff13)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age13 = year_diff-age_diff13)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age13) %>%
  count()

# delta_year_age13        n
# <dbl>    <int>
# 1               -1  2994811
# 2                0 45843952
# 3                1  3632182
# 4                2        1

# STEP.14 ---------------------------------------------------


check_age_positive <- check_age_positive %>%
  mutate(age_year_new14 = if_else(delta_year_age13 >= 2,age_year_new13+ delta_year_age13,age_year_new13))

# create age_diff9 for check 
glimpse(check_age_positive)

check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff14 = as.numeric(age_year_new14 - lag(age_year_new14,default = first(age_year_new14)))) %>%
  ungroup()


# table for check 
table_gg <- table(year_diff = check_age_positive$year_diff , age_diff14 = check_age_positive$age_diff14)


# shouldn't be delta_year_age 2 - 6 
check_age_positive <- check_age_positive %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(delta_year_age14 = year_diff-age_diff14)


# tab table delta_year_age6
check_age_positive %>%
  group_by(delta_year_age14) %>%
  count()
# delta_year_age14        n
# 1               -1  2994811
# 2                0 45843953
# 3                1  3632182


# table for check 
table_gg <- table(year_diff = check_age_positive$year_diff , age_diff14 = check_age_positive$age_diff14)

lis_age_diff_negative <- check_age_positive %>%
  filter(age_diff14 == -1 ) %>%
  pull(unique_id)


check <- check_age_positive %>%
  filter(unique_id %in% lis_age_diff_negative) %>%
  arrange(unique_id,number)


check_age_positive_test <- check_age_positive %>%
  filter(!(unique_id %in% lis_age_diff_negative))

# table for check 
table_gg <- table(year_diff = check_age_positive_test$year_diff , age_diff14 = check_age_positive_test$age_diff14)
# age_diff14
# year_diff        0        1        2        3        4        5        6        7        8        9
#         0 37374312  1929970        0        0        0        0        0        0        0        0
#         1  2242444  4273180   456897        0        0        0        0        0        0        0
#         2        0   614230  1765048   246259        0        0        0        0        0        0
#         3        0        0   324471  1018774   152595        0        0        0        0        0
#         4        0        0        0   194979   630810    97821        0        0        0        0
#         5        0        0        0        0   123470   399664    61634        0        0        0
#         6        0        0        0        0        0    76794   232822    32306        0        0
#         7        0        0        0        0        0        0    39908   116733    15834        0
#         8        0        0        0        0        0        0        0    15804    32187     1430



glimpse(check_age_positive_test)

table_for_merge  <- check_age_positive_test %>%
  select(number,age_year_new14)

glimpse(master_dataset_73)
glimpse(table_for_merge)
master_dataset_73 <- merge(x= master_dataset_73 , y = table_for_merge, by = "number", all.x = TRUE)

check <- master_dataset_73 %>%
  select(unique_id,number,age_year,age_year_new14)

check <- check %>%
  mutate(test = age_year_new14- age_year) %>%
  arrange(number)

check %>%
  group_by(test) %>%
  count()

check %>%
  filter(test == 3)




master_dataset_73 <- master_dataset_73%>%
  arrange(number) %>%
  group_by(unique_id.x) %>%
  mutate(age_diff_final =  as.numeric(age_year_new14 - lag(age_year_new14,default = first(age_year_new14)))) %>%
  ungroup()

master_dataset_73 %>%
  group_by(age_diff_final) %>%
  count()







#write_csv(master_dataset_73,"master_dataset_73_clean_age_sex.csv")

master_dataset_73_clean_age_sex <- read_csv("master_dataset_73_clean_age_sex.csv")
glimpse(master_dataset_73_clean_age_sex)

# Check master_Dataset 

glimpse(master_dataset_73_clean_age_sex)

master_dataset_73_clean_age_sex <- master_dataset_73_clean_age_sex %>%
  arrange(number) %>%
  group_by(unique_id) %>%
  mutate(age_diff_final =  as.numeric(age_year_new15 - lag(age_year_new15,default = first(age_year_new15)))) %>%
  ungroup()

master_dataset_73_clean_age_sex %>%
  group_by(age_diff_final) %>%
  count()

# age_diff_final        n
# 1              0 39616756
# 2              1  6817380
# 3              2  2546416
# 4              3  1460012
# 5              4   906875
# 6              5   574279
# 7              6   334364
# 8              7   164843
# 9              8    48021
# 10             9     1430
# 11            NA      570











master_dataset_73_clean_age_sex <-  master_dataset_73_clean_age_sex %>%
  mutate(check_na = if_else(is.na(act_amt&totl_amt),1,0))
glimpse(master_dataset_73_clean_age_sex)

master_dataset_73_clean_age_sex %>%
  group_by(check_na) %>%
  count()

tb1 <- table( check_na  = master_dataset_73_clean_age_sex$check_na, year = master_dataset_73_clean_age_sex$y_dsc)
tb1 <- data.frame(tb1)
















  












































