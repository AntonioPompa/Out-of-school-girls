# Out-of-school-girls
Predicting Out-of-school girls in India 

The key task is to help  develop a statistical algorithm that can predict the number of out-of-school girls (OOSGs) for each village in districts where EG will expand to. Note that they are most interested in finding villages with high numbers of OOSGs, rather than high proportions of OOSGs. So a village with 60 OOSGs out of 500 is better than one with 50 OOSGs out of 75. To do this, you will have access to the following data sets:

1.	Door-to-door EG data (d2d_data.csv): This data set has the actual number of OOSGs found during EG’s past visits in 2016 and 2017. Note that the “D2D_year” column specifies the year in which the village was visited by EG. Our main outcome of interest, the number of OOSGs, is in the column, “oos_g_5to14”. It has the number of out of school girls between the ages of 5 and 14.

2.	DISE and Census data (dise_census_training.csv): IDinsight has cleaned and merged DISE and Census data for all villages. Note that DISE data is available from three rounds of the surveys and each column entry corresponds with a specific round of survey.

3.	ASER (aser_data.csv): Collected by ASER in 2018, this dataset has the average OOSGs in villages within a district. Note that unlike datasets 1 and 2 that have village level observations, the ASER dataset has district level observations for OOSGs.
	
