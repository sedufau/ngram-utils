

# For English 4-grams, compute:
# - POS frequency (4-GRAM POS)
# - mean biword frequency (fpm non positional)
# - mean biword frequency (fpm positional)
#
# Positional computation is based on each bigram column (B1B2, B2B3, B3B4).
# Non-positional computation is based on the vertical concatenation of 
# B1B2, B2B3 and B3B4. So we take into account of the occurrences of all the bigrams.
# 
# Intermediate variables:
# - n (number of times a bigram appeared in the grouping variable, equivalent to a frequency table)
# "n" is not used per se, but can be useful for further token/surface computation.
# - occurrences (number of time a bigram appeared in books)



rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



library(tidyverse)

# read 4GRAM ENGLISH
df <- read_tsv("~/data/NGRAM_all_ENG_4_SFI.csv.gz")




#############################
### POS
# create 4_POS
df <- df %>% mutate(POS = paste0(POS1, " ",POS2, " ",POS3, " ",POS4))

# compute total occurrences (for normalization)
df_sum_POS_OCCURRENCES <- sum(df$OCCURRENCES)

# get POS frequency
df_POSfreq <- df %>% 
  group_by(POS) %>%
  summarise(POS_n = n(),
            POS_sumOCCURRENCES = sum(OCCURRENCES),
            POS_fpm = 1e6 * (POS_sumOCCURRENCES / df_sum_POS_OCCURRENCES))

# check if fpm is really frequency per million
sum(df_POSfreq$POS_fpm) # OK

# distribute POS information
df <- inner_join(df, df_POSfreq, by="POS")





#############################
### BI-WORDS (NON-POSITIONAL)
# create 3 x BIGRAMS
df <- df %>% mutate(GRAM1GRAM2 = paste0(GRAM1, " ", GRAM2),
                    GRAM2GRAM3 = paste0(GRAM2, " ", GRAM3),
                    GRAM3GRAM4 = paste0(GRAM3, " ", GRAM4),)
             
## compute non-positional frequency (all biwords)
# extract and rename column for row binding     
df_biword12 <- df %>% select(GRAM1GRAM2, OCCURRENCES) %>% rename(BIGRAM = GRAM1GRAM2)
df_biword23 <- df %>% select(GRAM2GRAM3, OCCURRENCES) %>% rename(BIGRAM = GRAM2GRAM3)
df_biword34 <- df %>% select(GRAM3GRAM4, OCCURRENCES) %>% rename(BIGRAM = GRAM3GRAM4)
# row bind the 3 columns
df_allBiwords <- rbind(df_biword12, df_biword23, df_biword34)

# compute total occurrences (for normalization)
df_sum_BIGRAM_OCCURRENCES <- sum(df_allBiwords$OCCURRENCES)

# get BIGRAM frequency
df_GRAMfreq <- df_allBiwords %>% 
  group_by(BIGRAM) %>%
  summarise(n_nonpositional_BIGRAM = n(),
            occurrences_nonpositional_BIGRAM = sum(OCCURRENCES),
            fpm_nonpositional_BIGRAM = 1e6 * (occurrences_nonpositional_BIGRAM / df_sum_BIGRAM_OCCURRENCES))

# check if fpm is really frequency per million
sum(df_GRAMfreq$fpm_nonpositional_BIGRAM) # OK

# copy and rename for the 3 datasets
df_GRAMfreq12 <- df_GRAMfreq %>% rename(GRAM1GRAM2 = BIGRAM,
                                        GRAM1GRAM2_n_nonpositional = n_nonpositional_BIGRAM,
                                        GRAM1GRAM2_occurences_nonpositional = occurrences_nonpositional_BIGRAM,
                                        GRAM1GRAM2_fpm_nonpositional = fpm_nonpositional_BIGRAM)
df_GRAMfreq23 <- df_GRAMfreq %>% rename(GRAM2GRAM3 = BIGRAM,
                                        GRAM2GRAM3_n_nonpositional = n_nonpositional_BIGRAM,
                                        GRAM2GRAM3_occurences_nonpositional = occurrences_nonpositional_BIGRAM,
                                        GRAM2GRAM3_fpm_nonpositional = fpm_nonpositional_BIGRAM)
df_GRAMfreq34 <- df_GRAMfreq %>% rename(GRAM3GRAM4 = BIGRAM,
                                        GRAM3GRAM4_n_nonpositional = n_nonpositional_BIGRAM,
                                        GRAM3GRAM4_occurences_nonpositional = occurrences_nonpositional_BIGRAM,
                                        GRAM3GRAM4_fpm_nonpositional = fpm_nonpositional_BIGRAM)

## join datasets on GRAM1GRAM2
df <- inner_join(df, df_GRAMfreq12, by="GRAM1GRAM2") # df_GRAMfreq12 contains all the possible bigram in the initial list
df <- inner_join(df, df_GRAMfreq23, by="GRAM2GRAM3") # df_GRAMfreq12 contains all the possible bigram in the initial list
df <- inner_join(df, df_GRAMfreq34, by="GRAM3GRAM4") # df_GRAMfreq12 contains all the possible bigram in the initial list

# check if OCCURRENCES is consistant # OK
tmp12 <- df %>% group_by(GRAM1GRAM2) %>% summarise(o = first(GRAM1GRAM2_occurences_nonpositional)) %>% rename(GG = GRAM1GRAM2)
tmp23 <- df %>% group_by(GRAM2GRAM3) %>% summarise(o = first(GRAM2GRAM3_occurences_nonpositional)) %>% rename(GG = GRAM2GRAM3)
tmp34 <- df %>% group_by(GRAM3GRAM4) %>% summarise(o = first(GRAM3GRAM4_occurences_nonpositional)) %>% rename(GG = GRAM3GRAM4)
tmp <- inner_join(tmp12, tmp23, by="GG")
tmp <- inner_join(tmp, tmp34, by="GG")
identical(tmp$o.x, tmp$o.y)
identical(tmp$o.x, tmp$o)

# mean bigram computation
df <- df %>% mutate(MEAN_BIGRAM_occurrences_nonpositional = (GRAM1GRAM2_occurences_nonpositional +
                                                        GRAM2GRAM3_occurences_nonpositional +
                                                        GRAM3GRAM4_occurences_nonpositional) / 3,
                    MEAN_BIGRAM_fpm_nonpositional = 1e6 * (MEAN_BIGRAM_occurrences_nonpositional / df_sum_BIGRAM_OCCURRENCES))

hist(log10(df$MEAN_BIGRAM_fpm_nonpositional), 100)  




#############################
### BI-WORDS (POSITIONAL)

# compute sum of occurrences for each of the positions
df_sum_GRAMXGRAMY_OCCURRENCES <- sum(df$OCCURRENCES)

df_GRAMfreq12_positional <- df %>% 
  group_by(GRAM1GRAM2) %>%
  summarise(GRAM1GRAM2_n_positional = n(),
            GRAM1GRAM2_occurrences_positional = sum(OCCURRENCES),
            GRAM1GRAM2_fpm_positional = 1e6 * (GRAM1GRAM2_occurrences_positional / df_sum_GRAMXGRAMY_OCCURRENCES))

df_GRAMfreq23_positional <- df %>% 
  group_by(GRAM2GRAM3) %>%
  summarise(GRAM2GRAM3_n_positional = n(),
            GRAM2GRAM3_occurrences_positional = sum(OCCURRENCES),
            GRAM2GRAM3_fpm_positional = 1e6 * (GRAM2GRAM3_occurrences_positional / df_sum_GRAMXGRAMY_OCCURRENCES))

df_GRAMfreq34_positional <- df %>% 
  group_by(GRAM3GRAM4) %>%
  summarise(GRAM3GRAM4_n_positional = n(),
            GRAM3GRAM4_occurrences_positional = sum(OCCURRENCES),
            GRAM3GRAM4_fpm_positional = 1e6 * (GRAM3GRAM4_occurrences_positional / df_sum_GRAMXGRAMY_OCCURRENCES))

## join datasets on GRAM1GRAM2
df <- inner_join(df, df_GRAMfreq12_positional, by="GRAM1GRAM2") # df_GRAMfreq12 contains all the possible bigram in the initial list
df <- inner_join(df, df_GRAMfreq23_positional, by="GRAM2GRAM3") # df_GRAMfreq12 contains all the possible bigram in the initial list
df <- inner_join(df, df_GRAMfreq34_positional, by="GRAM3GRAM4") # df_GRAMfreq12 contains all the possible bigram in the initial list

# mean bigram computation
df <- df %>% mutate(MEAN_BIGRAM_occurrences_positional = (GRAM1GRAM2_occurrences_positional +
                                                            GRAM2GRAM3_occurrences_positional +
                                                            GRAM3GRAM4_occurrences_positional) / 3,
                    MEAN_BIGRAM_fpm_positional = 1e6 * (MEAN_BIGRAM_occurrences_positional / df_sum_GRAMXGRAMY_OCCURRENCES))


# plot occurrences
par(mfrow=c(1,1))
p1 <- hist((df$MEAN_BIGRAM_occurrences_nonpositional), breaks=100, border=F)  
p2 <- hist((df$MEAN_BIGRAM_occurrences_positional), breaks=100, border=F)  
plot( p1, col=rgb(0,0,1,0.3), xlim=c(0,1e7))  # first histogram
plot( p2, col=rgb(1,0,0,0.3), xlim=c(0,1e7), add=T)  # second
legend("topright", c("Occ NonPositional", "OccPositional"), fill=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)))

# plot log10 occurrences
par(mfrow=c(1,1))
p1 <- hist(log10(df$MEAN_BIGRAM_occurrences_nonpositional), breaks=100, border=F)  
p2 <- hist(log10(df$MEAN_BIGRAM_occurrences_positional), breaks=100, border=F)  
plot( p1, col=rgb(0,0,1,0.3), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,0.3), xlim=c(0,10), add=T)  # second
legend("topright", c("Occ NonPositional", "OccPositional"), fill=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)))


# plot log10 fpm
par(mfrow=c(1,1))
p1 <- hist(log10(df$MEAN_BIGRAM_fpm_nonpositional), breaks=100, border=FALSE)  
p2 <- hist(log10(df$MEAN_BIGRAM_fpm_positional), breaks=100, border=FALSE)  
plot( p1, col=rgb(0,0,1,0.3), xlim=c(-5,6))  # first histogram
plot( p2, col=rgb(1,0,0,0.3), xlim=c(-5,6), add=T)  # second
legend("topright", c("Occ NonPositional", "OccPositional"), fill=c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)))


# all seem OK, write
write.csv(df, file=gzfile("ENG_4_withPOSfreq_withBIGRAMfreq.csv"))






