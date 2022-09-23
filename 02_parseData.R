## get per state data, translate and build the ready-to-use dataset
## dhemerson.costa@ipam.org.br

## get libraries

## avoid scientific notation
options(scipen=999)

## list files to be parsed
files <- list.files('./table/lcluc-tenure-state/raw', full.names= TRUE)

## import land tenure dictionary
tenure_dict <- read.csv('./dictionary/tenure-dict.csv', sep= ';')

## create recipe to receive data
for (i in 1:length(unique(files))) {
  ## read file i
  x <- read.csv(files[i])
  ## delete undesired columns
  x <- x[!names(x) %in% c("system.index", ".geo")]
  ## parse state id from basename
  x$state <- sapply(strsplit(basename(files[i]), split='_', fixed=TRUE),
                    function(x) (x[1]))
  
}


## for each 
for (j in 1:length(unique(x$tenure))) {
  ## for each unique value, get mean in n levels
  y <- subset(tenure_dict, Value == unique(x$tenure)[j])
  ## apply tenure translation 
  
  
}

  
