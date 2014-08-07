#Hydrostats

 hydrostats is a set of tools to compute streamflow indices commonly used in hydrology and ecology such as:
- high spells (magnitude, frequency, duration, timing etc.)
- low spells
- cease-to-flow spells
- Colwells indices
- partial series
- baseflow components
	
Most of the functions can be used with plyr to return a dataframe of indices based on an additional splitting factor. This allows the analysis (for example) of data from multiple gauges simultaneously, without the need to write a for loop. As well as summary indices, several functions will instead return a dataframe of individual spell charactersistics for use in subsequent analysis and plotting.

To install the latest development version run the following code: 

	#install dependencies (devtools required only to install from Github)
	install.packages(c("plyr", "lubridate", "devtools"))

	#install pacakge
	devtools::install_github("nickbond/hydrostats")

	# Remove the package zip after installation
	unlink("hydrostats.zip")
