<base target="_blank">

# Examples of time series analysis

The files contained in this folder were downloaded after the analysis of some real time series.  

Check the <a href="https://sari-gnss.github.io" target="_blank">official SARI website</a> for a description of the time series contents and of the results of their analysis.

Example 1: GNSS & earthquakes  
<a href="SPOTGINS_CONZ00CHL.enu_East.sari" target="_blank" download>SPOTGINS_CONZ00CHL.enu_East.sari</a>

Example 2: GNSS & mass loading  
<a href="SPOTGINS_NAUS00BRA.enu_Up.sari" target="_blank" download>SPOTGINS_NAUS00BRA.enu_Up.sari</a>

Example 3: GNSS & geophysical models (warning: long computation time of the noise analysis)  
<a href="SPOTGINS_YELL00CAN.enu_Up.sari" target="_blank" download>SPOTGINS_YELL00CAN.enu_Up.sari</a>

Example 4: Earth rotation (warning: long computation time)  
<a href="14C04_IAU2000_UT1-UTC_1972.txt.sari" target="_blank" download>14C04_IAU2000_UT1-UTC_1972.txt.sari</a>

Example 5: sea-level change  
<a href="1.rlrdata.sari" target="_blank" download>1.rlrdata.sari</a>

These examples can be used to test and validate some of the analysis options of a local installation of SARI.  
To do so:  

1. upload one of the files
2. select the 1D format
3. select the time units years
4. select the series units m, mm or none, depending on the file uploaded
5. click plot
6. upload the same file in the *load SARI model* option (andillary information block)
7. if the loaded file contains a KF model, click on the *run KF* button
8. compare the estimated values with those indicated in the header of the uploaded file
9. (optional) compare additional values of the analysis that can be found in the corresponding screenshot
