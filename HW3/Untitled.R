library(dplyr)
library(tidyr)
library(readr)
MIE <-read_csv("MIE.zip")
x <- make_LD(MIE)
print(class(x))
print(x)
out <- subject(x, 10)
print(out)
out <- subject(x, 14)
print(out)
out <- subject(x, 54) %>% summary
print(out)
out <- subject(x, 14) %>% summary
print(out)
out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)
out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)

