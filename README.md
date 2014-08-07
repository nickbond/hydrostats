**Hydrostats pacakge**

The *hydrostats* package provides a set of R functions to compute a suite of flow indices commonly used in hydrology and ecology, such as:
- high spells (magnitude, frequency, duration, timing etc.)
- low spells
- cease-to-flow spells
- Colwells indices
- partial series
- baseflow components

To install the latest development version run the following code: 

	install.packages("plyr", "lubridate", "devtools")

for Mac and Linux:

	install_github("nickbond/hydrostats")

for Windows:
	install_github("nickbond/hydrostats", repos=NULL)

	unlink("hydrostats.zip")
