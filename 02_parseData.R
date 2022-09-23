## get per state data, translate and build the ready-to-use dataset
## dhemerson.costa@ipam.org.br

## get libraries

## avoid scientific notation
options(scipen=999)

## list files to be parsed
files <- list.files('./table/lcluc-tenure-state/raw', full.names= TRUE)

## import land tenure dictionary
tenure_dict <- read.csv('./dictionary/tenure-dict.csv', sep= ';')

## import states dictionary
state_dict <- read.csv('./dictionary/state-dict.csv', sep= ';')

## create recipe to receive data
data <- as.data.frame(NULL)
## for each fiel (state)
for (i in 1:length(unique(files))) {
  ## read file i
  x <- read.csv(files[i])
  ## delete undesired columns
  x <- x[!names(x) %in% c("system.index", ".geo")]
  ## parse state id from basename
  x$state <- sapply(strsplit(basename(files[i]), split='_', fixed=TRUE),
                    function(x) (x[1]))
  
  ## create recipe to translate each land tenure
  recipe <- as.data.frame(NULL)
  ## for each tenure id
  for (j in 1:length(unique(x$tenure))) {
    ## for each unique value, get mean in n levels
    y <- subset(tenure_dict, Value == unique(x$tenure)[j])
    ## select matched land tenure 
    z <- subset(x, tenure == unique(x$tenure)[j])
    ## apply tenure translation for each level
    z$tenure_l1 <- gsub(paste0('^',y$Value,'$'), y$tenure.l1, z$tenure)
    z$tenure_l2 <- gsub(paste0('^',y$Value,'$'), y$tenure.l2, z$tenure)
    z$tenure_l3 <- gsub(paste0('^',y$Value,'$'), y$tenure.l3, z$tenure)
    ## bind into recipe
    recipe <- rbind(recipe, z)
    
  }
  data <- rbind(data, recipe)
}










## empty bin
rm(recipe2, data, x, y, z)

## create recipe to translate each state
recipe2 <- as.data.frame(NULL)
## for each tenure id
for (k in 1:length(unique(recipe$state))) {
  ## for each unique value, get mean in n levels
  y <- subset(state_dict, state == unique(recipe$state)[k])
  ## select matched land tenure 
  z <- subset(recipe, state == unique(recipe$state)[k])
  ## apply tenure translation for each level
  z$state_sig <- gsub(paste0('^',y$id,'$'), y$state, z$state)
  ## bind into recipe
  recipe <- rbind(recipe, z)
}



