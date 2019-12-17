# Do trees on farms improve household welfare? Evidence from national panel data in Uganda

Daniel C. Miller, Juan Carlos Muñoz-Mora, Laura V. Rasmussen, Alberto Zezza

Data and syntaxis for the project on Trees On Farm in Ugada

Folder Structure:
  - *00_RawData*: Here you can find the original data without any transformation as well as the questionnaries for all waves.
  - *01_DataSets*: This folder contains the different datasets that are the result of the cleaning and data construction.
  - *02_Paper*: Here you can find the tables and figures that are exported from the codes.
 
 # syntaxis
  
At the *syntaxis* folder, you can find the following codes in R in Stata. Data preparation (eg. cleaning, creation of variables and so on) were made using R, the final regression analysis was made using STATA 13. Although we don't use  all controls, wyou can download a full set of household controls for all waves. 

The codes are the following:
  
  ## Codes for building the data set
  
 Here a brief description of the codes:
  - **00_TreesOnFarm_Dynamic.R and 0_TreesOnFarm_Dynamic_v1.R**: These two codes create the most important part of our paper because it creates the variable related with trees on farm. It classifies the agricultural crops in the different type of crops. Here you should look on how we create the definition with and without bananas.
  - **01_HH_Controls.R** Households controls
  - **02a_Nutrition_MainCareGiver.R, 02b_Nutrition_Indexes.R** It creates our different definitions for nutrition outcomes as well as the main charactericts of the main caregiver.
  - **03_Agricultural_Controls.R** It creates all controls at agricultural level.
  - **04a_HH_Income_aux.R, 04b_HH_Income_GrossIncome.R** Here we built all variables for income. We follow the methodology of 
  RIGA. Here more information [RIGA - FAO](http://www.fao.org/economic/riga/riga-database/en/)
  - **05_Expenditures.R, 05a_Food Consumption.R** Here all controls for expenditure total and only in food
  - **06_Livestock.R** Here we built the information from Livestock
  - **07_Create_DataSets.R** It creates the final data set for our analysis, which is saved at 01_DataSets

As our result we have two main data sets:
  - Panel_Structure_Data_Uganda.dta
  - TreesOnFarm_Uganda_12012017.Rda

  ## Codes for analysing

The codes for analysis are:
  - **08_Analysis** This is a very preliminar analysis in R. I wouldn't use this, yet have a look just in case.
  - **09_labels.do** Here all the labels for the DATA analysis
  - **09_STATA_Analysis.do** THIS IS IMPORTANT!.. The current tables we have in our paper were made using this codes.
  - **09a_Panel_Structure.do** This is the baseline results yet using a Panel Data Structure
  - **09a_STATA_Analysis_Consumption.do, 09a_STATA_Analysis_Consumption_test_11.do** THIS IS IMPORTANT!.. The current tables we have in our paper were made using this codes.
  - **10_Descriptive.R, 11_Descriptive_General.R, 10_Descriptive_Crops.R** THIS IS IMPORTANT. All figures currently present in our articule were made with this.
  - **Labels.R** Labels for the R analysis.








  
