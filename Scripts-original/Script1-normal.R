options(prompt="R>", scipen=100, digits=4)

#read file
library(readxl)
norm= read_excel("0-reps-10.xlsx", sheet = "all")
library(car)
tiff("sf1-normal scatters.tiff", units="in", width = 20,
     height = 20, compression= "lzw", res=300)
scatterplotMatrix(norm[5:13], groups=norm$Sex, col=c("red", "black"),
                  legend= list("bottomright"), cex=.5,data=norm,
                  diagonal=list(method="qqplot"),
                  var.labels = c("mnm1","mnm2","mnm3","mnp4",
                                 "mxm1", "mxm2","mxm3",
                                 "mxp3","mxp4"))
dev.off()



