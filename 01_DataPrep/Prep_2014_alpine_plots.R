library(tidyverse)
library(tabulapdf)
library(pdftools)

# bring in data
table <- pdf_text("Data/alpine_plots_2014.pdf")





# Teddy's attempt -------------------------------

print(df[1,1])

hi <- strsplit(df[1,1],"") # First column, all separated out

print(hi[[1]])

first_char <- hi[[1]][1] #Check if char works
first_num <- hi[[1]][30] # Check if num works
as.double(first_num)

# Check if the string contains any digit from (0-9)
if (grepl("[0-9]", first_char)) {
  cat("'String' contains a digit")
} else {
  cat("'String' does not contain digit")
}

if (grepl("[A-Za-z]", first_char)) {
  #print the statement if alphabets are present in the string.
  cat("Alphabets present\n")
} else {
  #print the statement if alphabets are not present in the given string.
  cat("Alphabets not present\n")
}

### Psuedo Code for creating a new data frame, from a data frame that has one column with everything smushed into it. Need to separate
# Step 1: Split into individual characters
# Step 2: Loop over each row, if the current character is a letter, save it and keep moving.
# Step 3: If you hit a number, time to move to the next column
# Step 4: Keep going, until you hit a space again store that number, at this point move to the next column.
# Step 5: Keep going until done first row, then onto the next. 
# Step : Done
