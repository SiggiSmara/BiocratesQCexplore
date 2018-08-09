# Biocrates QC explore
Setting:
A database is generated with little over 110700 entries that encompass 114 batches of data generated on a mass spectrometer.
Each batch contains several quality controls (QCs) and the bad QCs have been identified. 

This shiny app explores the question: are there batches with higher or lower number of bad QCs compared to the whole lot.
Both the Graph and the top Table are responsive along with the user controls on the left side.

All of the data in this public database are not real as they are only for demonstration purposes. The real data is part of the [CHRIS study](https://de.chris.eurac.edu/) and access to it is governed by the [CHRIS study regulations](http://www.eurac.edu/en/research/health/biomed/projects/Pages/default.aspx).

To see it in action just run the app within the [BiocratesQCexplore](BiocratesQCexplore) folder making sure that the SQLite database found there is also present.

