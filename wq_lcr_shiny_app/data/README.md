This is the `data` folder located in the `wq_lcr_shiny_app` folder. This folder contains the files:  


-`dis.rds` - this .rds file is created using a script located in `salinity_temp.R` to create salinity or temperature specific figures   
- `lab.csv` - a .csv file containing the discrete and YSI water quality measurements, edited and created by the, `data_script` which pulls in the observations from the MySQL database 
- `lakewatch.csv` - this file contains only Lakewatch data, it is pulled directly from the entry in MySQL workbench  
- `wind.rds` - this data is pulled using `rnoaa` package, and the script it located in `data_script.R`, this file is always updated
- `wq.csv` - a .csv file containing the Star-Oddi and Diver sensor data, edited and created by the `data_script`, which pulls in the observations from the MySQL database  
- `wq_location_gps.txt` - this is a .txt file which contaings the GPS coordinates of the water quality sensors, this file is not used in the Shiny App  
- `wq_total.csv` - a .csv file containing the Star-Oddi and Diver sensor data, unedited (not manipultaed) and created by the `data_script`, which pulls in the observations from the MySQL database  
