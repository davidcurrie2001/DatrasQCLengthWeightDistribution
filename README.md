# ICES-HACKATHON
A QC Tool for Length-Weight Distribution on board

Open source testing for errors while you sample & have your QC done before you leave the research vessel!

Based on the QCMan Software used in the Marine Institute Fishlab, Github was used to produce a ShinyApp with an example of a quick visual comparison tool to help correct for simple on board mistakes (tarra, length measurement board inaccuracies) in time. This should avoid the loss of data in future.

We looked at comparing the upload of biological survey results on board, comparing them to the previous 5 years average and confidence intervals. 

The main check is: Does the catch weight per haul (sample condition index) reflect the estimated weight of fish of fish based on the lengths of the fish in the haul. 

As an example, the sample condition index for whiting from the 2017 Irish Ground Fish Survey on board the RV Celtic Explorer (IE-IGFS).
The previous 5 years average is based on the Datras LF files for the IE-IGFS.

After uploading the haul LF results on board, the App will show the recorded sample weight for length frequencies compared to the ‘predicted’ weight based on historic (2012) a and b factors derived from Datras. 

The last haul or hauls in the last 24 hrs will be indicated in a different color to facilitate checking the latest input.

When querying any data point, hovering over it will show the length frequency plot within that haul, in relation to the average length frequency in the previous 5 years on the same sample station. 

The app relates directly to the DATRAS input data format. 

In future, the app could be extended to be able to:
* Correct and/or flag outliers with a comment  
* Check the individual condition index compared to the expected
* Check for missing data in the upload (for example sex, maturity) 
* Check stratum allocation for otoliths
* Check for sufficient sample size 

By: Nissa Ferm and Ibrahim Umar with help from Dave Stokes, the ICES Data Centre and Barbara Schoute ;)

Subsequent edits by David Currie
