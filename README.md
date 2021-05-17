# wq_lcr_shiny_app  
This is the repository for the 	`wq_lcr_shiny_app`. This repository contains the code and data files needed to create the Shiny App.  
  
## To use this repository:  
  
1. Run the script-> `data_script.R` using a computer connected to the Oyster Project MySQL database, (UF VPN). This script will output `wq.csv`, `wq_total.csv`, `lab.csv`, `lakewatch.csv`, and `wind.rds` in the `data` folder, located in the `wq_lcr_shiny_app`-> `data`.  
2. Click on R project `wq_shiny`.  
3. Navigate in R Studio to wq_app -> `app`.  
4. Run shiny `app`.  Publish shiny app.
  
 ### Folder Definitions:  
 `script`- This folder contains the `data_script` for cleaning the continuous and discrete water quality data of the Oyster Restoration Project at the Universtity of Florida. A script for a `hydro_duration_curve` is also located here.  
 `wq_lcr_shiny_app`-  This folder contains the water quality shiny `app.R` file, and the `data` folder. The `data` folder contains continuous and discrete water quality data.  
  
