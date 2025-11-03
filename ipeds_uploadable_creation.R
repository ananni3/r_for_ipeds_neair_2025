# ============================================
# 1. Load Libraries and Data
# ============================================
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(IPEDSuploadables)


# Read the in the student data - This uses a sample dataset that can be replaced with your data
student_data <- read_excel("Data/Sample_dataset.xlsx", sheet = "student_data")

# Read in the retention data - This uses a sample dataset that can be replaced with your data
retention_data <- read_excel("Data/Sample_dataset.xlsx", sheet = "retention_data")

# ============================================
# 2. Clean and Recode Variables
# ============================================

# Convert categorical strings to numeric/binary formats
  #For your own data, replace with the values for each category that you have 
cleaned_student_data <- student_data %>%
  mutate(
    # Full-time/Part-time
    IsFullTime = case_when(
      full_part %in% c("FT", "Full time", "full-time") ~ 1,
      full_part %in% c("PT", "Part time", "part-time") ~ 0
    ),
    
    #First time
    IsFirstTime = case_when(
      entry_type == "first_time" ~ 1,
      TRUE ~ 0
    ),

    #Transfer
    IsTransfer = case_when(
      entry_type == "transfer" ~ 1,
      TRUE ~ 0
    ),
    
    # Degree-seeking indicator
    IsDegreeCertSeeking = case_when(
      degree_seeking %in% c("Y", "Yes", "y") ~ 1,
      TRUE ~ 0
    ),
    
    # Distance Education (IPEDS uses 0=None, 1=Some, 2=All)
    DistanceEd = case_when(
      DistanceEd == "none" ~ 0,
      DistanceEd == "some" ~ 1,
      DistanceEd == "all"  ~ 2
    ),
    
    # Sex (1=Male, 2=Female)
    Sex = case_when(
      Sex %in% c("M", "Male") ~ 1,
      Sex %in% c("F", "Female") ~ 2
    ),
    
    #Gender Detail
    GenderDetail = case_when(
      gender %in% c("M", "Male") ~ 1,
      gender %in% c("F", "Female") ~ 2,
      gender %in% c("U", NA) ~ 3,
      TRUE ~ 4
    ),
    
    # RaceEthnicity (IPEDS numeric codes)
    RaceEthnicity = case_when(
      RaceEthnicity == "Nonresident alien" ~ 1,
      RaceEthnicity == "Hispanic/Latino" ~ 2,
      RaceEthnicity == "American Indian or Alaska Native" ~ 3,
      RaceEthnicity == "Asian" ~ 4,
      RaceEthnicity == "Black or African American" ~ 5,
      RaceEthnicity == "Native Hawaiian or Other Pacific Islander" ~ 6,
      RaceEthnicity == "White" ~ 7,
      RaceEthnicity == "Two or more races" ~ 8,
      TRUE ~ 9 # Race and ethnicity unknown
    ),
    
    #Set state values to characters 
    OnlineState = as.character(OnlineState), 
    AdmitState = as.character(AdmitState)
  ) %>%
  
  select(
    Unitid, StudentId, StudentLevel, IsFullTime, IsFirstTime, IsTransfer,
    IsRecentGrad, IsDegreeCertSeeking, DistanceEd, Sex, GenderDetail, RaceEthnicity, Age,
    AdmitState, OnlineState, UnitidState,
    Cip130000:Cip511201
  ) 


# ============================================
# 3. Generate IPEDS Upload File
# ============================================

#This creates the text file that can be used to upload to IPEDS Key values
produce_ef1_report(cleaned_student_data, retention_data)


# ============================================
# 4. Save Cleaned files for your reference
# ============================================

# Make sure you have a folder titled "Data"
library(openxlsx)
wb<- createWorkbook()
addWorksheet(wb, "student_data")
addWorksheet(wb, "retention_data")
writeData(wb, "student_data", cleaned_student_data)
writeData(wb, "retention_data", retention_data)
saveWorkbook(wb, "Data/Cleaned_Sample_dataset.xlsx", overwrite = T)
