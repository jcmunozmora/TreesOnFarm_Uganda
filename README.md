# Do trees on farms improve household well-being? Evidence from national panel data in Uganda

*Daniel C. Miller, Juan Carlos Muñoz-Mora, Laura V. Rasmussen, Alberto Zezza*

*[Published in Frontier in Forest and Global Change - 2020](https://www.frontiersin.org/journals/forests-and-global-change)*


This repository contains the main replication file as well as the original data. We provide the entire data processing and analysis repository, so you can either replicate from the rawdata or going directly to the data analysis.

Folder Structure:
  - *00_RawData*: Here you can find the original data without any transformation as well as the questionnaries for all waves.
    -  The source data is [World Bank](https://microdata.worldbank.org/index.php/home)
  - *01_DataSets*: This folder contains the different datasets that are the result of the cleaning and data construction.
  - *02_Paper*: Here you can find the tables and figures that are exported from the codes.

 # syntaxis

At the *syntaxis* folder contains the codes in R and STATA used for our analysis. Data preparation (eg. cleaning, creation of variables and so on) were made using R, the final regression analysis was made using STATA 13.

The codes are the following:

  ## Codes for building the data set

 Here a brief description of the codes:
  - **00_TreesOnFarm_Dynamic.R and 0_TreesOnFarm_Dynamic_v1.R**: These two codes create the most important part of our paper because it creates the variable related with trees on farm. It classifies the agricultural crops in the different type of crops.
  - **01_HH_Controls.R** Households controls
  - **02a_Nutrition_MainCareGiver.R, 02b_Nutrition_Indexes.R** It creates our different definitions for nutrition outcomes as well as the main charactericts of the main caregiver.
  - **03_Agricultural_Controls.R** It creates all controls at agricultural level.
  - **04a_HH_Income_aux.R, 04b_HH_Income_GrossIncome.R** Here we built all variables for income. We follow the methodology of
  RIGA. Here more information [RIGA - FAO](http://www.fao.org/economic/riga/riga-database/en/)
  - **05_Expenditures.R, 05a_Food Consumption.R** Here all controls for expenditure total and only in food
  - **06_Livestock.R** Here we built the information from Livestock
  - **07_Create_DataSets.R** It creates the final data set for our analysis, which is saved at 01_DataSets
  - **08_Panel_Structure.do** It creates the final data set

  ## Codes for analysing

The codes for analysis are:
  - **08_labels.do** Here all the labels for the DATA analysis
  - **09_STATA_FE_Analysis_Tables1_5.do** This is where we perform the consumtion analysis. Table 1 to 4 are produced here.
  - **10_STATA_Nutrition_Tables1-5** This is the baseline results for the nutrition data set. Table 1 and 5 are produced here
  - **11a_Descriptive_Crops.R, 11b_Descriptive_Crops.R 11_Descriptive_General.R**  All figures currently present in our articule were made with this.

If you have any problem replicating our analysis, please do not hestitate to contact us (Dan C. Miller or Juan Carlos Muñoz-Mora)
