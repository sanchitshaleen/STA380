library(tm)

# Remember to source in the "reader" wrapper function

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('F:/Predictive Modelling(JScott)/STA380-master/STA380-master/data/ReutersC50/C50train/*')
author_dirs = author_dirs[1:50]
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

names(all_docs)

my_corpus = Corpus(VectorSource(all_docs))
#names(my_corpus) = names(my_corpus)

names(my_corpus)=file_list

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics

class(DTM)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.945)
DTM

# Now a dense matrix
X = as.matrix(DTM)
dim(X)
# Naive Bayes
AP_train = X[1:45,]
AC_train = X[51:95,]

# AP's multinomial probability vector
# Notice the smoothing factor
# Why?
smooth_count = 1/nrow(X)
w_AP = colSums(AP_train + smooth_count)
w_AP = w_AP/sum(w_AP)


prob_list=list()

# AC's multinomial probability vector
for (i in 0:49){
  prob_list[[i+1]] = colSums(X[(i*50 +1):((i+1)*50-1),] + smooth_count)
  prob_list[[i+1]] = prob_list[[i+1]]/sum(prob_list[[i+1]])
}


x_test = X[49,]
head(sort(x_test, decreasing=TRUE), 25)

## We take log of probability vector to overcome the numerical underflow problem
## probability values will be very small so taking the log of fractional values will scale it to bigger values

# Compare log probabilities under the Naive Bayes model
sum(x_test*log(w_AP))



# Another test document
x_test2 = X[99,]
head(sort(x_test2, decreasing=TRUE), 25)
sum(x_test2*log(w_AP))
sum(x_test2*log(w_AC))



#### For Testing Data ####

library(tm)

# Remember to source in the "reader" wrapper function

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('F:/Predictive Modelling(JScott)/STA380-master/STA380-master/data/ReutersC50/C50test/*')
author_dirs = author_dirs[1:50]
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

names(all_docs)

my_corpus = Corpus(VectorSource(all_docs))
#names(my_corpus) = names(my_corpus)

names(my_corpus)=file_list

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics

class(DTM)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.975)
DTM

# Now a dense matrix
X_Test = as.matrix(DTM)


dim(X_Test)
# Naive Bayes
AP_train = X[1:45,]
AC_train = X[51:95,]

# AP's multinomial probability vector
# Notice the smoothing factor
# Why?
smooth_count = 1/nrow(X)
w_AP = colSums(AP_train + smooth_count)
w_AP = w_AP/sum(w_AP)


prob_list=list()

# AC's multinomial probability vector
for (i in 0:49){
  prob_list[[i+1]] = colSums(X[(i*50 +1):((i+1)*50-1),] + smooth_count)
  prob_list[[i+1]] = prob_list[[i+1]]/sum(prob_list[[i+1]])
}
sum(prob_list[[1]])

x_test = X[49,]
head(sort(x_test, decreasing=TRUE), 25)

## We take log of probability vector to overcome the numerical underflow problem
## probability values will be very small so taking the log of fractional values will scale it to bigger values

# Compare log probabilities under the Naive Bayes model
sum(x_test*log(w_AP))



# Another test document
x_test2 = X[99,]
head(sort(x_test2, decreasing=TRUE), 25)
sum(x_test2*log(w_AP))
sum(x_test2*log(w_AC))

















