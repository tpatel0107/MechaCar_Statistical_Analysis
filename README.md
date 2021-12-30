# MechaCar_Statistical_Analysis
Module 15 Challenge
## Deliverable One
## Linear Regression to Predict MPG
## Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset? Vehicle_weight,spoiler_angle and AWD provide non random amount of variance. 
    **The maximum random variance was provided by ground_clearence and vehicle_length
## Is the slope of the linear model considered to be zero? Why or why not? 
    **Since p-value is less than zero (5.35e-11), the slope is not equal to zero
## Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not? 
    **The R squared value is 71.49% which implies that roughly 72% of the time the predictions will be correct using the linear model
![GitHub Graph](https://github.com/tpatel0107/MechaCar_Statistical_Analysis/blob/main/Call%20LM.PNG?raw=true)
## Summary Statistics on Suspension Coils
   **First the Suspicion Coil csv was converted into a dataframe and then created two subset tables with it: Total Summary and Lot Summary. The Total Summary table is looking at PSI statistics across all lots while the Lot Summary shows statistcs of each lot (seen below). As seen, Lot 1 and 2, are very similar while Lot 3 has a smaller mean with a bigger variance and standar deviation (SD).
![GitHub Graph](https://github.com/tpatel0107/MechaCar_Statistical_Analysis/blob/main/Statistics%20on%20Suspension%20Coils.PNG?raw=true)
## T-Tests on Suspension Coils
   **The P-values from single T-Test on PSI values (compared to the standard of 1500 PSI) for suspicion coils were:-

   **All PSI= 0.06028 Lot 1 PSI= 1 Lot 2 PSI= 0.6072 Lot 3 PSI= 0.04168 Assuming that the significant value should be below p = 0.05 (which is standard), then only Lot 3 is significantly different. All other lots perform the same (or are not significantly different) to the standard 1500 PSI. This, combined with the lower mean of Lot 3 and high variance, could indicate that Lot 3 is under-performing.
![GitHub Graph](https://github.com/tpatel0107/MechaCar_Statistical_Analysis/blob/main/deliverable%20test%203.PNG?raw=true)
![GitHub Graph](https://github.com/tpatel0107/MechaCar_Statistical_Analysis/blob/main/DELIVERABLE%203%20TEST%20PART%202.PNG?raw=true)
## Summary
   **Based on the analysis above, the major impacts on MPG are car weight, spoiler angle, and AWD capability. This would mean for any improvement on the MPG of the MechaCar, designers and enigeers to consider these variables. In addition, when looking at suspicion coils, Lot 3 has the most variance and lower PSI average. This could mean that Lot 3 should not be used within the MechaCar.
## Study Design: MechaCar vs Competition
   **When it comes to comparing the MechaCar to its competition, the company should consider using an ANOVA test to compare the MechaCar in various categories that customers would care about: cost, city and highway fuel efficiency, horse power, safety rating, maintenance cost. ANOVA test is able to test to see if the means from multiple different samples are significantly similar or different. This means that the averages of different cars in these categories can be compared to the average of MecaCar. If the p value is greater than 0.05, then MecaCar has the same or similar performance within these categories (the null hypothesis). If the p value is less than 0.05, then MecaCar is significantly different in those categories. If it is significant different, then looking at the average of the MecaCar is the next step. If MecaCar's average is either below or above the other averages would show how it is performing against its competitors (below = worse, above = better).
