This package was written to produce hidden Markov models for MLB pitchers using publicaly available statcast data. 

There are 25 functions in the `mkbaseball` package and two small dataframes used for the functions.
When working through the data, it was important to me to have everything stored in one place, so I could easily access multiple pieces of information for each player, like their particular pitch arsenal, their specific model, or a pitch transition matrix.
I wanted as few moving parts as possible, so I ended up storing absolutely everything up to the model evaluation in a large nested list. 
The large list contains a list for each pitcher, and each pitcher is a list containing all the information necessary for each of the six models:


* 'Last, First' character name
* Cleaned dataframe with 13 columns --- this is the rawest data we keep in the list
* List of AB used for training data
* List of AB used for testing data
* Hidden variable used in model (`pitch`, `pitch_red1`, or `pitch_red2`) character
* Observed variable used in model (`ct` or `ct_red`) character
* Pitch arsenal vector corresponding to hidden variable --- States
* Possible counts vector corresponding to observed variable --- Symbols
* Starting probabilities vector --- probability of each pitch being the first pitch thrown at an AB, at an 0-0 count
* Hidden variable transition matrix
* Observed variable emission matrix
* Hidden Markov Model --- list object of length 5 which uses the previous 5 pieces of information
* Dataframe of model evaluated by each metric, at each testing AB.
* 4 additional dataframes of models evaluated by each metric, at certain critical counts, where we're only concerned with the Hamming Distance


Only five of these functions are needed to be run by a user to reproduce the analyses for this project: `read_data_in()` \ `read_data_in_off()` \ ` read_data_in_match()`, `prep_model_list()`, `big_model()`, `eval_in()`, and `assess_models()`.
An example workflow is below:

```{r workflow, eval=FALSE, include=TRUE}
# install package
library(devtools)
install_github("ewstrawbridge/mkbaseballpkg", force=TRUE)
library(mkbaseball)

#1. Create list of filenames
data_files <- list("~\data\pitcher1.csv", "~\data\pitcher2.csv")
#2. Read data in and clean it appropriately - returns a nested list, which we
#   will build upon with each subsequent function
all_pitcher_data <- read_data_in(data_files) (aggregate all pairs of pitcher/batter handedness)
#                <- read_data_in_match(data_files) matching handedness
#                <- read_data_in_off(data_files) mismatching handedness
#3. Prep the data before making our model (pick out transition matrices, etc.)
pitcher_prep <- prep_model_list(all_pitcher_data)
#4. Fit the model on the training data
pitcher_models <- big_model(pitcher_prep)
#5. Evaluate the models based on testing data
pitcher_eval <- eval_in(pitcher_models)
#6. Asses the models based on either an entire AB ("full") or a particular 
#   critical count ("0-2", "3-0", etc.)
pitcher_final <- assess_models(pitcher_eval, "full")
```

It's possible to make this all one or two steps with one large function, but keeping these separate was especially helpful for debugging, as well as working with the smaller functions that made up these large ones along the way.
These larger functions are basically a complicated way to use the smaller functions that were written across an entire list and then store them in a particular way. 
For instance, `big_model()` just fits a HMM based on data from a list, identified with the appropriate index, and puts that model object right back into the list we gave the function. 
There were not usually issues there, but the function `counter_indicesg()`, a function that formats the data appropriately before it becomes an emission matrix (describing the probability of each particular hidden state being associated with each observed symbol), constantly needed debugging, so it was helpful to have it as a separate function from `prep_model_list()` where it lives and does most of its work.
More detailed descriptions are in the details of the package, which was built with help from [Hilary Parker's blog](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/).


The package is flexible to the extent that one can download different data on different pitchers from Baseball Savant on any available year on their website and use any of the six combinations of pitch and count variables. 
The package itself is designed for use with this project, so it can't be used with other sports data as so much of the data formatting depends on the form of the data from Baseball Savant and the AB present.

For `pitch_red1` we collapse `pitch` into four categories: fastballs, sinker/splitter/changeup, curveball/knuckle curve, and sweeper/slider/cutter based on where in the strike zone the pitch might land. 
This categorization is not perfect and should be better sorted by speed or by spin rate, but since `ct` and `ct_red` both rely on where a pitch might land in the strike zone, as the determination of a ball or a strike depends on the location in or out of strike zone, this seems like the most logical categorization. 
It's something I plan on updating and changing so if you are attached to this, sorry. 
Not every pitcher will have a type of pitch in all four categories -- some might only have a fastball and one other or two others since each `pitch_red1` category other than fastball contains multiple pitch types. 
The goal here is just to reduce the number of levels in each model in hopes that the simplicity will make it more accurate.
Finally, `pitch_red2` collapses `pitch` into two types: fastball and offspeed pitch. 

- For `ct_red`, there are only four levels:
  - 0-0, the beginning of an AB
  - Pitcher's counts, when the number strikes is greater than the number of balls
  - Batter's counts, when the number balls is greater than the number of strikes 
  - Even count, when the number balls is equal to the number of strikes
- For `ct`, containing every possible balls-strikes count:
  - 0-0, the beginning of an AB
  - 0-2, when the pitcher is fully ahead in the count 
  - 3-0, when the batter is fully ahead in the count 
  - 3-2, a full count where neither the pitcher nor the batter is ahead
 

*Hamming Distance* (HD) is our simplest metric. 
Hamming distance gives us the minimum number of substitutions required to change the model's output string to the goal string.
The output will be the number of differences, in our case, the number of differently (inaccurately) predicted pitches at each sequential count. 
We will use $1-\frac{HD}{\text{sequence length}}$ as this metric, so that we will deal with a value between 0 and 1 that is greatest when the sequences are similar.  


The *Dice-Sørensen Coefficient* (DS) is still a comparison between two sequences, but is concerned just with common elements. 


* Given sets $X, Y$, the $\displaystyle DS=\frac{2 | X \cap Y |}{|X| + |Y|}$ where $| |$ is the size (length) of a sequence. 


It's not totally capable of judging the entire sequence as an ordered string, but it can evaluate similar comparisons over the number of pairs, called bigrams, in the sequence. 


* Given sets $X, Y$, where $n_t$ is the total number of character bigrams that match between $X$ and $Y$, $n_X$ is the number of bigrams in $X$, and $n_Y$ is the number of total bigrams in $Y$, we have that the $\displaystyle DS=\frac{2n_t}{n_X+n_Y}$.

Using this method will allow us to check pitch and next-pitch pairs which are connected by the Markov property. 


Finally, we will use the *Smith-Waterman Algorithm* to discover what portion of the predicted sequence matches best with the actual sequence.
The Smith-Waterman algorithm was developed to compare genetic sequences and find optimal local alignments, that is, portions of an entire sequence that aligned best out of the entire sequence^[@smith-waterman]. 
It involves a scoring matrix, rewarding matches and penalizing mismatches.
The matrix is size $(N+1) \times (M+1)$ where $N$ is the length of the first sequence and $M$ is the length of the second sequence, with the first row and column filled out with zeros.
At each step, it chooses the maximum point reward of either moving right, down, or diagonally along the sequence, where the points are chosen by a match or a mismatch between the row/column entry of either sequence. 


After the scoring matrix is filled, we walk backwards through the sequence starting at the highest value and picking the maximal point path until we reach zero.
