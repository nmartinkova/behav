
<!-- README.md is generated from README.Rmd. Please edit that file -->

# behav

<!-- badges: start -->
<!-- badges: end -->

*behav* is a simple package that enables the user to score and quantify
recorded behaviours of a test subject. Videos of a known length are
played from a third-party application.

## Installation

To start using *behav*, install it from GitHub:

``` r
# Check whether required package devtools exists, if not install it
if(!require("devtools", character.only = TRUE)){
    install.packages("devtools", dependencies = TRUE)
}

# Install behav from GitHub using devtools. Internet access necessary
install_github("nmartinkova/behav")

# Load the behav package
require(behav)
# Loading required package: behav
```

## Scoring video files

Good practice for organization of work prior to scoring behaviours from
video files is to prepare a folder dedicated to the results. Then set
this folder as the working directory. Check whether the directory is set
to the correct location:

``` r
getwd()
```

If the path to the folder matches (beware of problems with special or
accented characters in folder path!), open the first video file in
another software and pause it. Note the duration of the video file in
minutes. Start the function to score the behaviours is `R`, setting the
video file duration and file name for the result:

``` r
score_events(duration = 10, file = "nameofthevideofile.txt")
```

The function will proceed with printing information and guidelines to
the console. First, it will say
`Usage: choose keys to represent events for scoring. Hit [enter] to record the given symbol`.
This means that the user should think about what behaviours they expect
in their whole experiment, and assign single letter codes to each
behaviour. **Write the codes for behaviours on a paper** and keep it in
a visible place to check during scoring as needed.

Next, the function will ask `What will be the first symbol?`, meaning
that the user should have already seen the start of the video and know
what the animal is doing in the first second. Write that letter and
press Enter.

Last message in the initialisation is `Press [enter] to start scoring`,
when the function waits for the user to start the video and start
scoring in `R`. Each time the animal changes its behaviour, hit the
corresponding single letter code and press Enter. This is very
important. After each letter, the user must press Enter.

After the set time, the function waits for the last keystroke (any
behaviour observed at the end) and the last Enter. The function then
notifies the user about the intended file name for storing the scored
bahaviours. This is a great chance to check, whether one is not
overwritting existing files.

The last message
`Duration of an event is given in seconds, the last scored event lasted until the end of the allocated time`
is purely informative to keep awareness of the scoring units. The
`duration` argument is in minutes, but the results are stored in
seconds.

# Troubleshooting video scoring

1.  **What if I make a mistake when scoring behaviours?**

    Press Esc to stop the scoring and start from the beginning of the
    video file, calling the function again. This can be done by pressing
    an up arrow in the `R` console.

2.  **What if the video ends before the duration set in the function
    call?**

    Wait until the duration of the function call ends, and press Enter
    at the end. Edit the saved file to correspond to the total length of
    the video. Next time, make sure to set the correct duration. For
    example, 2 minutes 30 seconds equals to `duration = 2.5`,
    i.e. minutes + seconds / 60.

3.  **The animal switches between behaviours faster than I can score
    them.**

    Try your best and be as quick as you can. The resolution of the
    function is 1s, so any precission less than that will not be shown
    in the results.

## Summarizing behaviours

Once all video files were scored and all results are included in one
folder, the scored behaviours can be summarized across the files. First,
set the working directory **outside** of the folder with the scores of
the videos. A usefull place is one level up, to the parent directory.
Next, summarize the behaviours.

``` r
sum_events(folder = "folderNameWithScoredBehavioursForAllIndividuals")
```
This is a good place to check for the most coarse errors in scoring the 
behaviours. The column names should only include the symbols selected
by the user prior to scoring. If there are other columns, check the respective
file or score the video again.

## Behaviour latency

To extract latencies of all instances of a specific behaviour from all
the scored files, prepare the working directory as that used for 
summarizing behaviours. 

``` r
latency(folder = "folderNameWithScoredBehavioursForAllIndividuals")
```
The function will search for all available behaviours scored in all files
in the folder and present the user with the options to choose from. 
The choice will be repeated until the user selects from the list.
`R` is case-sensitive, so make sure to input the behaviour exactly. 

