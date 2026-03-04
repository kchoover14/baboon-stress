# BaboonStress
This repo contains data and scripts for "Developmental Instability in Wild Nigerian Olive Baboons". A PDF of a revised and resubmitted version (accepted for publication 20 June 2021) is also located here.

The exact methods used are described in detail in a step-by-step guide: Palmer AR, and Strobeck C. 2003. Fluctuating asymmetry analysis: A step-by-step example. In: Polak M, ed. Developmental Instability: Causes and Consequences. Oxford: Oxford University Press, V1-B36.

### Data Cleaning for Outliers
The zero level script, prefixed Script0, generates univariate measurements that have to be entered into the PS worksheet noted at the start of the script.
Unfortunately this work is done by hand to follow the PS method of using the spreadsheet. A goal for the future is to create the evaluation in the PS worksheet in R to avoid the potential for introducing errors via handwork.

Scripts are batched by leading numbers. The first script, prefixed Script1, tests the data for the normal distribution.

The second batch of scripts, prefixed by 2 then followed by a letter, assess measurement error (ME) on each set of measurements noted at the start of the script. There is a note in the scripts, when taking replicate differences, that a function would have worked better. Ultimately, the script works just fine but would be much better as a function for others to use (so a future goal!). The removal of outliers is done by hand. I didn't have a way to automate this but that would be ideal for, again, avoiding the introduction of errors via hand work.

The third script test for trait size asymmetry (TSA) via scatterplots for visual inspection and confirmed by Rosner's ESD test. Again, handwork to remove outliers occurs at this stage to produce the next dataframe.

The fourth batch of scripts examine trait size differences for fluctuating asymmetry.

### Using cleaned data, these scripts generate values for the PS Worksheet, which will be used for data exploration and hypothesis testing
The fifth batch of scripts generate descriptive and univariate values for the ps worksheet.

The sixth batch of scripts is similar to the second batch b/c it assesses measurement error but, this time, on the cleaned dataset with no outliers. The final script in this batch (e) checks the variance of ME between length v breadths and across tooth classes. This is a check to see if the ME variance is not significantly different and pooling of all teeth into one dataset is acceptable.

The seventh script tests for trait size dependency via correlation on the final dataset.

### The data exploration, hypothesis testing, and plots all derive from the final dataset derived from the PS Worksheet based on cleaned data.
The eighth script explores baboon data trends in FA with those reported in humans and tests hypotheses using the Levene test. The data for this test are derived from the PS Worksheet, which is included in this repository.

The ninth batch of scripts generate the plots.

## Data Files
Raw data are suffixed by *EG data*
Additional data files include the first for data processing prefixed by a 0 and then subsequent datasets after removal of bad data identified by processes in the scripts.

