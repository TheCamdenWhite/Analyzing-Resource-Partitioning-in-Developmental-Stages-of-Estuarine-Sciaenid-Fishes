Project Objective

The objective of this project is to intrepet and visualize the data from my Bachelor's Essay research. That research involved conducting gut contents analysis of two different species of fish from the family Sciaenidae. The main type of data collected was presence/absence data of several different prey items. Since there were multiple prey items, multivariate anlaysis was conducted. Additionally, each prey item at each size class needed to be analyzed independently to see if it differed from the other species at the same size class. By creating these models and running these statistical tests, this code allows the user to make various conclusions and inferences from the data.

Relevant Dependencies

The packages "dplyr", "vegan", "ggplot2", "ggrepel", "tibbler", and "tidyr" are needed to replicate my results.

Data Structure

The data table should be imported as a CSV file. It should include 9 columns. The first two are the columns for species and size range. These are categorical data points as there are two species and three different size ranges. The two different species were Micropognias undulatus (Atlantic croaker) and Cynoscion nebulosus (spotted seatrout). The three size ranges were 5-10mm, 10-15mm, and 15-20mm. Note that the exact size is not listed, just the range the specimen falls under. The remaing 7 columns are the different prey items: Calanoid copepod, harpacticoid copepod, amphipods, shrimp, polycheate, fish, and the empty stomach condition. Data in these columns is displayed as presence or absence, with a "1" indicating that the prey item was present in that samples gut, and a "0" indicating that it was absent. There should be a total of 30 specimens in the data.

Instructions

To recreate my results three figures must be generated and two statistical tests must be conducted. Two RDA plots and six bar graphs must be created. There should be one bar graph for each species and size range combination. The RDAs should be tested for significance using an ANOVA and the bar graphs #' should be tested for significance using Fisher's exact test.
