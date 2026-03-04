options(prompt="R>", scipen=100, digits=4)

#This script tests trait size dependency via correlation tests.

#female breadths
library(readxl)
library(coin)

#female breadths
tsafb=read_excel("tsa.xlsx", sheet="femb")
spearman_test(mnm1_sd~mnm1av, tsafb)
spearman_test(mnm2_sd~mnm2av, tsafb)
spearman_test(mnm3_sd~mnm3av, tsafb)
spearman_test(mnp4_sd~mnp4av, tsafb)
spearman_test(mxm1_sd~mxm1av, tsafb)
spearman_test(mxm2_sd~mxm2av, tsafb)
spearman_test(mxm3_sd~mxm3av, tsafb)
spearman_test(mxp3_sd~mxp3av, tsafb)
spearman_test(mxp4_sd~mxp4av, tsafb)

#female lengths
tsafl=read_excel("tsa.xlsx", sheet="feml")
spearman_test(mnm1_sd~mnm1av, tsafl)
spearman_test(mnm2_sd~mnm2av, tsafl)
spearman_test(mnm3_sd~mnm3av, tsafl)
spearman_test(mnp4_sd~mnp4av, tsafl)
spearman_test(mxm1_sd~mxm1av, tsafl)
spearman_test(mxm2_sd~mxm2av, tsafl)
spearman_test(mxm3_sd~mxm3av, tsafl)
spearman_test(mxp3_sd~mxp3av, tsafl)
spearman_test(mxp4_sd~mxp4av, tsafl)

#male breadths
tsamb=read_excel("tsa.xlsx", sheet="maleb")
spearman_test(mnm1_sd~mnm1av, tsamb)
spearman_test(mnm2_sd~mnm2av, tsamb)
spearman_test(mnm3_sd~mnm3av, tsamb)
spearman_test(mnp4_sd~mnp4av, tsamb)
spearman_test(mxm1_sd~mxm1av, tsamb)
spearman_test(mxm2_sd~mxm2av, tsamb)
spearman_test(mxm3_sd~mxm3av, tsamb)
spearman_test(mxp3_sd~mxp3av, tsamb)
spearman_test(mxp4_sd~mxp4av, tsamb)

#male lengths
tsaml=read_excel("tsa.xlsx", sheet="malel")
spearman_test(mnm1_sd~mnm1av, tsaml)
spearman_test(mnm2_sd~mnm2av, tsaml)
spearman_test(mnm3_sd~mnm3av, tsaml)
spearman_test(mnp4_sd~mnp4av, tsaml)
spearman_test(mxm1_sd~mxm1av, tsaml)
spearman_test(mxm2_sd~mxm2av, tsaml)
spearman_test(mxm3_sd~mxm3av, tsaml)
spearman_test(mxp3_sd~mxp3av, tsaml)
spearman_test(mxp4_sd~mxp4av, tsaml)
