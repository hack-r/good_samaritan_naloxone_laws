# good_samaritan_naloxone_laws
This code replicates of the estimates from  the Journal of Law & Economics article "With a Little Help from My Friends: The Effects of Good Samaritan and Naloxone Access Laws on Opioid-Related Deaths" (Rees, Sabia, Argys, Dave and Latshaw).

The original Stata code and data were replicated in R. Results show that the published OLS results in Table 5 (column 1) and Table 6 (column 2) do not correspond to the published code or model specification described in the paper. I reached out to the lead author.

Code for recovering Stata's clustered standard errors may be found at the end of the R file.

*Update:* I received a response from one of the authors. It seems that the published data set contained 57 NA values which (in the unpublished original data) were 0's. In their published models they transformed the values to 0.1 so that they could take logs, but they were NA's in the published data set by mistake. This will be useful to know for testing their models on new years of data.

## References
Rees, D. I., Sabia, J. J., Argys, L. M., Dave, D., & Latshaw, J. (2019). With a little help from my friends: The effects of good Samaritan and naloxone access laws on opioid-related deaths. The Journal of Law and Economics, 62(1), 1-27.

## Links
https://www.journals.uchicago.edu/doi/suppl/10.1086/700703
