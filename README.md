# ngram-utils

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
