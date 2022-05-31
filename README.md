#Hydrostats

 hydrostats is a set of tools to compute streamflow indices commonly used in hydrology and ecology such as:
- basic summary statistics
- high spells (magnitude, frequency, duration, timing etc.)
- low spells
- cease-to-flow spells
- Colwell's indices
- partial series
- baseflow components

	
Most of the functions can be used with the packages plyr and dplyr to return a dataframe of indices based on an additional grouping variable. This allows the analysis (for example) of data from multiple gauges simultaneously, without the need to write a for loop. While most functions return a single row dataframe, several functions will instead return a dataframe of individual spell characteristics for use in subsequent analysis and plotting.

#Installation
To install the latest development version run the following code: 

Current version on github and CRAN is 0.2.9.

	# install dependencies/suggests:
	# devtools required only to install from Github; 
	# plyr required for examples
	install.packages(c("devtools", "plyr"))

	#install pacakge
	devtools::install_github("nickbond/hydrostats")

	# Remove the package zip after installation
	unlink("hydrostats.zip")


#Developer
Nick Bond n.bond@latrobe.edu.au
