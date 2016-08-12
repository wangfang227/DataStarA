import nltk


#Resource u'corpora/gutenberg' not found.  Please use the NLTK
#Downloader to obtain the resource:  >>> nltk.download()

#nltk.download()

#nltk.download('all')

#manually install nltk data http://textminingonline.com/tag/nltk-data-install
#go to https://github.com/nltk/nltk_data/tree/gh-pages/packages
#manually download zip file
#unzip it
#copy all the unzipped files under C:\users\usz003g\AppData\Local\Continuum\Anaconda\nltk_data
#done


from nltk.book import *

texts()

text2

text2.concordance("sister")
text2.concordance("love")

text4.dispersion_plot(["sister", "love", "home", "affection", "sense","lust","sex","father"])

text2.common_contexts(["sister", "very"])
text2.similar("monstrous")

len(text2)

sorted(set(text2))

len(set(text2))

text2.count("love")

text2.count("affection")

text2.count("hate")

text2.count("sister")

fdist1 = FreqDist(text2)
vocab=fdist1.keys()
len(vocab)
vocab[:50]

v=set(text2)
v
longwords=[w for w in v if len(w)>15]
longwords
sorted(longwords)

bigrams(['more', 'is', 'said', 'than', 'done'])

import nltk
nltk.corpus.gutenberg.fileids()

emma = nltk.corpus.gutenberg.words('austen-emma.txt')

emma = nltk.Text(nltk.corpus.gutenberg.words('austen-emma.txt'))

macbeth_sentences = gutenberg.sents('shakespeare-macbeth.txt')

from nltk.corpus import webtext


# Natural Language Toolkit: Some texts for exploration in chapter 1 of the book
#
# Copyright (C) 2001-2015 NLTK Project
# Author: Steven Bird <stevenbird1@gmail.com>
#
# URL: <http://nltk.org/>
# For license information, see LICENSE.TXT
from __future__ import print_function

from nltk.corpus import (gutenberg, genesis, inaugural,
                         nps_chat, webtext, treebank, wordnet)
from nltk.text import Text
from nltk.probability import FreqDist
from nltk.util import bigrams
from nltk.misc import babelize_shell

print("*** Introductory Examples for the NLTK Book ***")
print("Loading text1, ..., text9 and sent1, ..., sent9")
print("Type the name of the text or sentence to view it.")
print("Type: 'texts()' or 'sents()' to list the materials.")

text1 = Text(gutenberg.words('melville-moby_dick.txt'))
print("text1:", text1.name)

text2 = Text(gutenberg.words('austen-sense.txt'))
print("text2:", text2.name)

text3 = Text(genesis.words('english-kjv.txt'), name="The Book of Genesis")
print("text3:", text3.name)

text4 = Text(inaugural.words(), name="Inaugural Address Corpus")
print("text4:", text4.name)

text5 = Text(nps_chat.words(), name="Chat Corpus")
print("text5:", text5.name)

text6 = Text(webtext.words('grail.txt'), name="Monty Python and the Holy Grail")
print("text6:", text6.name)

text7 = Text(treebank.words(), name="Wall Street Journal")
print("text7:", text7.name)

text8 = Text(webtext.words('singles.txt'), name="Personals Corpus")
print("text8:", text8.name)

text9 = Text(gutenberg.words('chesterton-thursday.txt'))
print("text9:", text9.name)

def texts():
    print("text1:", text1.name)
    print("text2:", text2.name)
    print("text3:", text3.name)
    print("text4:", text4.name)
    print("text5:", text5.name)
    print("text6:", text6.name)
    print("text7:", text7.name)
    print("text8:", text8.name)
    print("text9:", text9.name)

sent1 = ["Call", "me", "Ishmael", "."]
sent2 = ["The", "family", "of", "Dashwood", "had", "long",
         "been", "settled", "in", "Sussex", "."]
sent3 = ["In", "the", "beginning", "God", "created", "the",
         "heaven", "and", "the", "earth", "."]
sent4 = ["Fellow", "-", "Citizens", "of", "the", "Senate",
         "and", "of", "the", "House", "of", "Representatives", ":"]
sent5 = ["I", "have", "a", "problem", "with", "people",
         "PMing", "me", "to", "lol", "JOIN"]
sent6 = ['SCENE', '1', ':', '[', 'wind', ']', '[', 'clop', 'clop',
         'clop', ']', 'KING', 'ARTHUR', ':', 'Whoa', 'there', '!']
sent7 = ["Pierre", "Vinken", ",", "61", "years", "old", ",",
         "will", "join", "the", "board", "as", "a", "nonexecutive",
         "director", "Nov.", "29", "."]
sent8 = ['25', 'SEXY', 'MALE', ',', 'seeks', 'attrac', 'older',
         'single', 'lady', ',', 'for', 'discreet', 'encounters', '.']
sent9 = ["THE", "suburb", "of", "Saffron", "Park", "lay", "on", "the",
         "sunset", "side", "of", "London", ",", "as", "red", "and",
         "ragged", "as", "a", "cloud", "of", "sunset", "."]

def sents():
    print("sent1:", " ".join(sent1))
    print("sent2:", " ".join(sent2))
    print("sent3:", " ".join(sent3))
    print("sent4:", " ".join(sent4))
    print("sent5:", " ".join(sent5))
    print("sent6:", " ".join(sent6))
    print("sent7:", " ".join(sent7))
    print("sent8:", " ".join(sent8))
    print("sent9:", " ".join(sent9))
    
    
    
from nltk.corpus import brown
brown.categories()
brown.words(categories='news')


from nltk.corpus import inaugural
inaugural.files()


import nltk
nltk.corpus.gutenberg.fileids()

cfd = nltk.ConditionalFreqDist((g,w) for g in brown.categories() for w in brown.words(categories=g))
cfd
genres = ['news', 'religion', 'hobbies', 'science_fiction', 'romance', 'humor']
modals = ['can', 'could', 'may', 'might', 'must', 'will']
cfd.tabulate(conditions=genres, samples=modals)

from nltk.corpus import inaugural
inaugural.fileids()
 
cfd = nltk.ConditionalFreqDist((target, file[:4])
    for file in inaugural.fileids()
    for w in inaugural.words(file)
    for target in ['america', 'citizen']
    if w.lower().startswith(target))

cfd.plot()

#file[:4] is the year label


from nltk.corpus import udhr
languages = ['Chickasaw', 'English', 'German_Deutsch',
'Greenlandic_Inuktikut', 'Hungarian_Magyar', 'Ibibio_Efik']
cfd = nltk.ConditionalFreqDist((lang, len(word))
for lang in languages
for word in udhr.words(lang + '-Latin1'))
cfd.plot(cumulative=True)


from nltk.corpus import udhr
languages = ['Chickasaw', 'English', 'German_Deutsch',
'Greenlandic_Inuktikut', 'Hungarian_Magyar', 'Ibibio_Efik']
cfd = nltk.ConditionalFreqDist((lang, len(word))
for lang in languages
for word in udhr.words(lang + '-Latin1')):
cfd.tabulate(conditions=languages)
cfd.plot(cumulative=True)
cfd.plot(cumulative=False)


cfd = nltk.ConditionalFreqDist((g,w) for g in brown.categories() for w in brown.words(categories=g))

genre_word = [(g,w) for g in ['news', 'romance'] for w in brown.words(categories=g)]
len(genre_word)


def plural(word):
    if word.endswith('y'):
        return word[:-1] + 'ies'
    elif word[-1] in 'sx' or word[-2:] in ['sh', 'ch']:
        return word + 'es'
    elif word.endswith('an'):
        return word[:-2] + 'en'
    elif word.endswith('f'):
        return word[:-1]+'ves'
    return word + 's'

plural('knif')
    
    
#processing raw text 
import nltk
import urllib
import urllib2
from urllib import urlopen
from bs4 import BeautifulSoup as bs

url = "https://www.zurich.com/en/media/news-releases/2015/2015-1105-01"
raw = urllib.urlopen(url).read()
raw
type(raw)
len(raw)
raw[:200]
text = nltk.wordpunct_tokenize(raw)
type(text)
len(text)
text[:10]
text

#raw = nltk.clean_html(text)
#len(raw)
#print([element.get_text() for element in soup.select('h2')])

#import urllib.request
from bs4 import BeautifulSoup
url = "https://www.zurich.com/en/media/news-releases/2015/2015-1105-01"
html = urlopen(url).read().decode('utf-8')
soup = BeautifulSoup(html)
print type(soup)
print soup.prettify()[0:1000]
raw=soup.get_text()
type(raw)
len(raw)
raw[:75]
tokens = nltk.word_tokenize(raw)
tokens #there is punctuation in it

from nltk.tokenize import RegexpTokenizer #without punctuation
tokenizer = RegexpTokenizer(r'\w+')
tokens=tokenizer.tokenize(raw)

tokens=[x.encode('UTF8') for x in tokens] #encode only works for a string, so get the string elements from the list
type(tokens)
len(tokens)
tokens
text = nltk.Text(tokens)
text[:]
len(text)
type(text)



text.concordance('Zurich')
#from IPython.display import Image
#Image('http://www.openbookproject.net/tutorials/getdown/css/images/lesson4/HTMLDOMTree.png')
tokens = [w.lower() for w in tokens]

vocab = sorted(set(words))
type(vocab)
vocab

from nltk.collocations import *
phrases=text.collocations() #idioms 
#print phrases
bigrams=nltk.bigrams(text)
type(bigrams)

from collections import Counter
bigram_measures = nltk.collocations.BigramAssocMeasures()
trigram_measures = nltk.collocations.TrigramAssocMeasures()
finder = BigramCollocationFinder.from_words(text)
finder.nbest(bigram_measures.pmi, 100) 
finder = TrigramCollocationFinder.from_words(text)
finder.nbest(trigram_measures.pmi, 100) 

count = Counter(tokens)
#count = Counter(vocab)
#vocab only shows the sorted words, no count information saved, only a list of words alphabetically sorted
count
print count.most_common(10)


#stop words removal

from nltk.corpus import stopwords

#tokens = get_tokens()
filtered = [w for w in tokens if not w in stopwords.words('english')]
filtered
count = Counter(filtered)
print count.most_common(50)



#Stemming using NLTK

from nltk.stem.porter import *

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

stemmer = PorterStemmer()
stemmer
stemmed = stem_tokens(filtered, stemmer)
stemmed=[x.encode('UTF8') for x in stemmed] 
count = Counter(stemmed)
print count.most_common(100)

nltk.FreqDist(tokens)

#Tf-Idf in Scikit-Learn
'''
import nltk
import string
import os

from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.stem.porter import PorterStemmer

path = 'C:\\Users\\usz003g\\Documents\\Python training\\NLTK'
token_dict = {}
stemmer = PorterStemmer()

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def tokenize(text):
    tokens = nltk.word_tokenize(text)
    stems = stem_tokens(tokens, stemmer)
    return stems

for subdir, dirs, files in os.walk(path):
    for file in files:
        file_path = subdir + os.path.sep + file
        shakes = open(file_path, 'r')
        text = shakes.read()
        lowers = text.lower()
        no_punctuation = lowers.translate(None, string.punctuation)
        token_dict[file] = no_punctuation
        
 #this can take some time
tfidf = TfidfVectorizer(tokenizer=tokenize, stop_words='english')
tfs = tfidf.fit_transform(token_dict.values())
'''
       
#this can take some time
from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.stem.porter import PorterStemmer
tfidf = TfidfVectorizer(sublinear_tf=True, max_df=0.9, analyzer='word', stop_words='english', vocabulary=vocab)
count = Counter(stemmed)
token_dict = {}
for word in vocab:
    token_dict[word] = count[word]
token_dict.values()
token_dict.keys()
#count.values()
#count.keys()
#count and token_dict are the same
from sklearn.feature_extraction.text import CountVectorizer
tfidf = TfidfVectorizer(tokenizer=tokenize, stop_words='english')
tfs = tfidf.fit_transform(token_dict.values())

vec = CountVectorizer(tokenizer=tokens)
vec
data = vec.fit_transform(token_dict.values())
print data




#tfidf is used for multiple documents 

import math
#from textblob import TextBlob as tb

def tf(word, blob):
    return blob.words.count(word) / len(blob.words)

def n_containing(word, bloblist):
    return sum(1 for blob in bloblist if word in blob)

def idf(word, bloblist):
    return math.log(len(bloblist) / (1 + n_containing(word, bloblist)))

def tfidf(word, blob, bloblist):
    return tf(word, blob) * idf(word, bloblist)
    
document1 = "Python is a 2000 made-for-TV horror movie directed by Richard Clabaugh" 

document2 = "Python, from the Greek word, is a genus of nonvenomous pythons"

document3 = "The Colt Python is a .357 Magnum caliber revolver formerly manufactured by Colt's Manufacturing Company of Hartford",

bloblist = [document1, document2, document3]
for i, blob in enumerate(bloblist):
    print("Top words in document {}".format(i + 1))
    scores = {word: tfidf(word, blob, bloblist) for word in blob.words}
    sorted_words = sorted(scores.items(), key=lambda x: x[1], reverse=True)
    for word, score in sorted_words[:3]:
        print("\tWord: {}, TF-IDF: {}".format(word, round(score, 5)))



#term document matrix

import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer 

def fn_tdm_df(docs, xColNames = None, **kwargs):
    ''' create a term document matrix as pandas DataFrame
    with **kwargs you can pass arguments of CountVectorizer
    if xColNames is given the dataframe gets columns Names'''

    #initialize the  vectorizer
    vectorizer = CountVectorizer(**kwargs)
    x1 = vectorizer.fit_transform(docs)
    #create dataFrame
    df = pd.DataFrame(x1.toarray().transpose(), index = vectorizer.get_feature_names())
    if xColNames is not None:
        df.columns = xColNames

    return df


DIR = 'C:/Data/'

def fn_CorpusFromDIR(xDIR):
    ''' functions to create corpus from a Directories
    Input: Directory
    Output: A dictionary with 
             Names of files ['ColNames']
             the text in corpus ['docs']'''
    import os
    Res = dict(docs = [open(os.path.join(xDIR,f)).read() for f in os.listdir(xDIR)],
               ColNames = map(lambda x: 'P_' + x[0:6], os.listdir(xDIR)))
    return Res
    
    
#use text mining package

import textmining as tdm

def termdocumentmatrix_example():
    # Create some very short sample documents
    doc1 = 'John and Bob are brothers.'
    doc2 = 'John went to the store. The store was closed.'
    doc3 = 'Bob went to the store too.'
    # Initialize class to create term-document matrix
    tdm = textmining.TermDocumentMatrix()
    # Add the documents
    tdm.add_doc(doc1)
    tdm.add_doc(doc2)
    tdm.add_doc(doc3)
    # Write out the matrix to a csv file. Note that setting cutoff=1 means
    # that words which appear in 1 or more documents will be included in
    # the output (i.e. every word will appear in the output). The default
    # for cutoff is 2, since we usually aren't interested in words which
    # appear in a single document. For this example we want to see all
    # words however, hence cutoff=1.
    tdm.write_csv('C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\matrix.csv', cutoff=1)
    # Instead of writing out the matrix you can also access its rows directly.
    # Let's print them to the screen.
    for row in tdm.rows(cutoff=1):
        print row
        
termdocumentmatrix_example()

token1=' '.join(tokens)
token1
token2=' '.join(vocab)
token2



def termdocumentmatrix(doc1,doc2):
    # Create some very short sample documents
    #doc1 = 'John and Bob are brothers.'
    #doc2 = 'John went to the store. The store was closed.'
    #doc3 = 'Bob went to the store too.'
    # Initialize class to create term-document matrix
    tdm = textmining.TermDocumentMatrix()
    # Add the documents
    tdm.add_doc(doc1)
    tdm.add_doc(doc2)
    #tdm.add_doc(doc3)
    # Write out the matrix to a csv file. Note that setting cutoff=1 means
    # that words which appear in 1 or more documents will be included in
    # the output (i.e. every word will appear in the output). The default
    # for cutoff is 2, since we usually aren't interested in words which
    # appear in a single document. For this example we want to see all
    # words however, hence cutoff=1.
    tdm.write_csv('C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\matrix.csv', cutoff=1)
    return tdm
    # Instead of writing out the matrix you can also access its rows directly.
    # Let's print them to the screen.
    for row in tdm.rows(cutoff=1):
       row
        
import numpy as np
import pandas as pd

tdm=termdocumentmatrix(token1,token2)
#the text mining uses string

'''
# Write out the matrix to a csv file. Note that setting cutoff=1 means
    # that words which appear in 1 or more documents will be included in
    # the output (i.e. every word will appear in the output). The default
    # for cutoff is 2, since we usually aren't interested in words which
    # appear in a single document. For this example we want to see all
    # words however, hence cutoff=1.
'''

temp = list(tdm.rows(cutoff=1))
temp
type(temp)

temp = list(tdm.rows(cutoff=2))
temp
type(temp)
#temp is a list, a row is an element of the list

# get the vocab from first row
vocab = tuple(temp[0])
vocab

# get document-term matrix from remaining rows
X = np.array(temp[1:])
X.shape
X[0,]



###################### NLTK classfier
import nltk 
nltk.download()

from nltk.corpus import brown

import nltk.classify.util
from nltk.classify import NaiveBayesClassifier
from nltk.corpus import movie_reviews
 
def word_feats(words):
    return dict([(word, True) for word in words])
 
negids = movie_reviews.fileids('neg')
posids = movie_reviews.fileids('pos')
 
negfeats = [(word_feats(movie_reviews.words(fileids=[f])), 'neg') for f in negids]
posfeats = [(word_feats(movie_reviews.words(fileids=[f])), 'pos') for f in posids]
 
negcutoff = len(negfeats)*3/4
poscutoff = len(posfeats)*3/4
 
trainfeats = negfeats[:negcutoff] + posfeats[:poscutoff]
testfeats = negfeats[negcutoff:] + posfeats[poscutoff:]
print 'train on %d instances, test on %d instances' % (len(trainfeats), len(testfeats))
 
classifier = NaiveBayesClassifier.train(trainfeats)
print 'accuracy:', nltk.classify.util.accuracy(classifier, testfeats)
classifier.show_most_informative_features()





















