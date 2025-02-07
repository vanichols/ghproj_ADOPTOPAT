# ghproj_ADOPTOPAT
The beginnings of an Opensource Performance Assessment Tool (OPAT) for the ADOPT-IPM project

The dummy_data is an example of a template data input to this code. 

1. First the elicited data is made into binned 'value' distributions (1 is very low value, 5 is very high value) using the uncertainty rating to create a beta-distribution. 

2. Secondly, the binned distributions for each of the six impact categories are sampled according to the user-defined weight to give each category to create a summary distribution. 

3. Third, visualizations are created. Currently both bar plots and dot plots are being explored. 

Ultimately, the goal is to create an R package and Shiny website so users can enter their own data and get output automatically. 

