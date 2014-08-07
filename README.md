#Hydrostats

 hydrostats is a set of tools to compute streamflow indices commonly used in hydrology and ecology, such as:
- high spells (magnitude, frequency, duration, timing etc.)
- low spells
- cease-to-flow spells
- Colwells indices
- partial series
- baseflow components
	
Most of the he functions can be used with plyr to return a dataframe of indices based on an additional splitting factor. This allows the analysis (for example) of data from multiple gauges simultaneously, without the need to write a for loop. 

To install the latest development version run the following code: 

	install.packages(c("plyr", "lubridate", "devtools"))

for Mac and Linux:

	install_github("nickbond/hydrostats")

for Windows:
	install_github("nickbond/hydrostats", repos=NULL)

	unlink("hydrostats.zip")
