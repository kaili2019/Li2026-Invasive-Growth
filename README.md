# Li2026-Invasive-Growth

Code for the Invasive Growth manuscript: Understanding the impact of sodium sulfide on the invasive growth of wine yeast

Authors: Kai Li, Jennifer M. Gardner, Lauren Kennedy, Jin Zhang, Joanna F. Sundstrom, Stephen G. Oliver, Alexander K. Y. Tam, J. Edward F. Green1, Vladimir Jiranek, and Benjamin J. Binder.

The MATLAB script converts the MATLAB files to CSV files containing the data extracted from the further processing of binary images. These include the washed area, the unwashed area and the area ratio. Additional description columns include pre-culture SLAD condition, nutrient level, strain (parent or mutants) names, day of the experiment (days 3,4 or 6), type of experiment (A, B, C, D, or E), and finally the original file name containing all the information listed prior. 

matlab-preprocessing-code contains matlab code to convert the data on figshare to CSV file. For example, EXPA_tab2csv.m is for experiment A (E4). 

The Matlab files (.mat) contain the entire dataset is available on FigShare. DOI: TBA.  

To recreate the CSV data:

1. Download the .mat files from figshare into the matlab-preprocessing-code folder. 
2. Open and run the Matlab script for each experiment (A-E). For example, EXPA_tab2csv.m would correspond the experiment A. 

A table between the experiment names and number used in the manuscript are mapped below.

| Experiment Number | Experiment Name   | Matlab file  |
|-------------------|-------------------|--------------|
| E1                | C                 | EXPC_tab2csv |
| E2                | D                 | EXPD_tab2csv |
| E3                | E                 | EXPE_tab2csv |
| E4                | A                 | EXPA_tab2csv |
| E5                | B                 | EXPB_tab2csv |



 


  

