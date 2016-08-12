import nltk
import sqlite3
import sys
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt
from collections import Counter
from __future__ import print_function
from time import time

from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer,TfidfTransformer
from sklearn.decomposition import NMF
from sklearn.cluster import KMeans, MiniBatchKMeans

os.getcwd()
os.chdir('C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\clinton_email')


''' text analytics starts here '''

sqlite_file = 'C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\clinton_email\\database.sqlite'
table_name = 'Emails'

# Connecting to the database file
#text is the list
con = sqlite3.connect(sqlite_file)
c = con.cursor()
e = pd.read_sql_query("Select ExtractedBodyText From Emails",con)
#e.ExtractedBodyText=[x.encode('utf-8') for x in e.ExtractedBodyText]
e.ExtractedBodyText=[x.encode('ascii', 'ignore') for x in e.ExtractedBodyText]


e.ExtractedBodyText=[x.encode('UTF8') for x in e.ExtractedBodyText]
cs = ""
for i in range(len(e.ExtractedBodyText)):
    cs += str(e.ExtractedBodyText[i])
len(cs)

text=e.ExtractedBodyText
len(text)
type(text)
text=list(text)
len(text)
type(text)
text[1]
text[2]
text[3]
text[4]
text[5]

#Tf-Idf in Scikit-Learn

#text
import nltk
import string
import os

from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.stem.porter import PorterStemmer

#path = 'C:\\Users\\usz003g\\Documents\\Python training\\NLTK'
token_dict = {}
stemmer = PorterStemmer()

def stem_tokens(tokens, stemmer):
    stemmed = []
    for item in tokens:
        stemmed.append(stemmer.stem(item))
    return stemmed

def tokenize(text):
    #lowers = text.lower().encode('utf-8')
    #no_punctuation = lowers.translate(None, string.punctuation)
    tokens = nltk.word_tokenize(text)
    stems = stem_tokens(tokens, stemmer)
    return stems


#for subdir, dirs, files in os.walk(path):
for i in range(len(text)):
    #file_path = subdir + os.path.sep + file
    #shakes = open(file_path, 'r')
    lowers = text[i].lower()
    no_punctuation = lowers.translate(None, string.punctuation)
    token_dict[i] = no_punctuation

#token_dict.values()
len(token_dict.values())
#token_dict.values contains all the records, but some records after removing punctuations are NONE
#text2 filtered the NONE records
text2=filter(None, token_dict.values())
len(text2)
text2[:10]
type(text2)

#help(TfidfVectorizer)
#this can take some time
#ngram_range=(1, 2): phrases
#by default tfidf transform words to lower case

#max_df: max ratio of document frequency. max_df=0.9, exclude words which were used in more than 90% of the documents, exclude stop words
#min_df: min ratio of document frequency. min_df=0.1, exclude words which were used in less than 10% documents
#occurred in too many documents (`max_df`)
#occurred in too few documents (`min_df`)
#to include more words, higher max_df and lower min_df
#stopwords have high df (document frequency)
#rare words have low df (document frequency)
#when lower max_df, more common words were set as stop words

tfidf = TfidfVectorizer(tokenizer=tokenize, stop_words='english',max_df=0.9, min_df=0.01,ngram_range=(1, 2))
tfidf
tfs = tfidf.fit_transform(text2)
tfs
#term document matrix dimension 6726x704
X=tfs

#k mean clustering
###############################################################################
# Do the actual clustering, number of clusterings true_k

true_k=25

#tried two types of models: minibatchkmean and regular kmean
'''
km = MiniBatchKMeans(n_clusters=true_k, init='k-means++', n_init=1, init_size=1000, batch_size=1000)
print("Clustering sparse data with %s" % km)
t0 = time()
km.fit(X)
print("done in %0.3fs" % (time() - t0))
print()
'''
#use k-mean clustering

km = KMeans(n_clusters=true_k, init='k-means++', max_iter=100, n_init=1)
print("Clustering sparse data with %s" % km)
t0 = time()

X
k1=km.fit(X)	#Compute the centroids on X by chunking it into mini-batches.

k2=km.fit_predict(X)	#Compute cluster centers and predict cluster index for each sample.
len(k2)
k2
Counter(k2)


k3=km.fit_transform(X)	#Compute clustering and transform X to cluster-distance space.
k3
k4=km.get_params(k1)	#Get parameters for this estimator.
k4

#k5=km.partial_fit(X)	#Update k means estimate on a single mini-batch X.
k6=km.predict(X)	#Predict the closest cluster each sample in X belongs to.
k6
Counter(k2)
#k2 and k6 are the same

k7=km.score(X)	#Opposite of the value of X on the K-means objective.
k7
k8=km.transform(X) 	#Transform X to a cluster-distance space.
k8

k8predict=[None]*k8.shape[0]
for i in range(k8.shape[0]):
    k8i=list(k8[i,])
    k8predict[i]=k8i.index(min(k8i))
Counter(k8predict)
Counter(k2)
#k8predict is the same as k2
check=(k2==k8predict)
check
Counter(check)
#all true

print("done in %0.3fs" % (time() - t0))
print()      

km.labels_
km.cluster_centers_
km.inertia_

print("Top terms per cluster:"),km.labels_

order_centroids = km.cluster_centers_.argsort()[:, ::-1]
order_centroids #matrix of the sorted words
order_centroids.shape
km.cluster_centers_.shape
# each row is a centroid, for every row each word (variable) has a value showing the counts (frequency) of the word
# then sort the words by the frequency at each row, descending

terms = tfidf.get_feature_names()
terms=[x.encode('utf8') for x in terms]

for i in range(true_k):
    print("Cluster %d:" % i, end='')
    for ind in order_centroids[i, :50]:
        #select the 20 highest frequency words
        print(' %s' % terms[ind], end='')
    print()
    
#k2=km.fit_predict(X)	#Compute cluster centers and predict cluster index for each sample.
#len(k2)
#k2
#from collections import Counter

countk2=Counter(k2)
countk2.keys()
countk2.values()
pos = np.arange(len(countk2.keys()))
width = 1.0  

plt.bar(countk2.keys(), countk2.values(), width, color='g')

#plt.hist(countk2.values())


'''   
#use text mining package
text0=text

import textmining

tdm = textmining.TermDocumentMatrix()

def termdocumentmatrix():
    # Create some very short sample documents
    for i in (range(len(text))):
        # Initialize class to create term-document matrix]
        doc=text[i]
        # Add the documents
        tdm.add_doc(doc)
    
    # Write out the matrix to a csv file. Note that setting cutoff=1 means
    # that words which appear in 1 or more documents will be included in
    # the output (i.e. every word will appear in the output). The default
    # for cutoff is 2, since we usually aren't interested in words which
    # appear in a single document. For this example we want to see all
    # words however, hence cutoff=1.
    tdm.write_csv('C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\hc matrix.csv', cutoff=2)
    # Instead of writing out the matrix you can also access its rows directly.

termdocumentmatrix()

'''




#network analysis
#Creates a weighted, directed graph of all of the Clinton emails of the type
# email_sender ------weight-------> email_recipient
# where "email_sender" and "email_recipient" are nodes and
# weight is the weight of the edge, defined
# as the number of emails sent by email_sender to email_recipient
# for example, .....

#first the imports

import networkx as nx
from collections import Counter, defaultdict
import matplotlib.pylab as pylab
import matplotlib.pyplot as plt

# read the main data source
os.getcwd()
os.chdir('C:\\Users\\usz003g\\Documents\\Python training\\NLTK\\clinton_email')

emails = pd.read_csv("Emails.csv")

#cleanup the names in the From and To fields
with open("Aliases.csv") as f:
    file = f.read().split("\r\n")[1:] #skip the header line
    aliases = {}
    for line in file:
        line = line.split(",")
        aliases[line[1]] = line[2]
#aliases has three columns, so line has three elements: Id	   Alias	PersonId
#aliases is the dictionary, alias is the key, personid is the value


with open("Persons.csv") as f:
    file = f.read().split("\r\n")[1:] #skip header line
    persons = {}
    for line in file:
        line = line.split(",")
        persons[line[0]] = line[1]
#person has 2 columns, so line has 2 elements, line[0] is the id, line[1] is the name
        
        
        
def resolve_person(name):
    name = str(name).lower().replace(",","").split("@")[0]
    #print(name)
    #correct for some of the common people who are resolved to several different
    # names by the given Aliases.csv file:  Cheryl Mills, Huma Abedin, Jake Sullivan
    # and Lauren Jiloty
    # Also convert "h" and variations to Hillary Clinton
    if ("mills" in name) or ("cheryl" in name) or ("nill" in name) or ("miliscd" in name) or ("cdm" in name) or ("aliil" in name) or ("miliscd" in name):
        return "Cheryl Mills"
    elif ("a bed" in name) or ("abed" in name) or ("hume abed" in name) or ("huma" in name) or ("eabed" in name):
        return "Huma Abedin"
    #elif (name == "abedin huma") or (name=="huma abedin") or (name=="abedinh"): 
    #    return "Huma Abedin"
    elif ("sullivan" in name)  or ("sulliv" in name) or ("sulliy" in name) or ("su ii" in name) or ("suili" in name):
        return "Jake Sullivan"
    elif ("iloty" in name) or ("illoty" in name) or ("jilot" in name):
        return "Lauren Jiloty"
    elif "reines" in name: return "Phillip Reines"
    elif (name == "h") or (name == "h2") or ("secretary" in name) or ("hillary" in name) or ("hrod" in name):
        return "Hillary Clinton"
    #fall back to the aliases file
    elif str(name) == "nan": return "Redacted"
    elif name in aliases.keys():
        return persons[aliases[name]]
    else: return name

emails.shape
type(emails)
emails.MetadataFrom = emails.MetadataFrom.apply(resolve_person)
emails.MetadataTo = emails.MetadataTo.apply(resolve_person)

#Extract the to: from: and Raw body text from each record

From_To_RawText = []
temp = zip(emails.MetadataFrom,emails.MetadataTo,emails.RawText)
len(temp)
#temp[1]
#zip returns a list of tuples

for row in temp:
    From_To_RawText.append(((row[0],row[1]),row[2]))
    
type(From_To_RawText)
From_To_RawText[1][0] #(row[0],row[1])
From_To_RawText[1][1] #row[2]


#Create a dictionary of all edges, i.e. (sender, recipient) relationships 
# and store the individual email text in a list for each key
From_To_allText = defaultdict(list)
for people, text in From_To_RawText:
    From_To_allText[people].append(text)
len(From_To_allText.keys()), len(From_To_RawText)
#people is the combination of from to
#text is the raw email body text
#From_To_allText.items()[1][0]
len(From_To_allText.items()[1][1])
#for the first from to combination, there are 12 emails under it

#Set the weights of each directed edge equal to the number of emails 
# (number of raw text documents) associated with that edge
edges_weights = [[key[0], key[1], len(val)] for key, val in From_To_allText.items()]
edges_weights[1]
type(edges_weights)
edge_text = [val for val in From_To_allText.items()[:][1]]
edge_text=[val for key, val in From_To_allText.items()]
#edge_text[1]

#initialize the graph
graph = nx.DiGraph()
#directed graph


#transform the dict with keys (from,to) and vals weight back to a 
# tuple(from, to, weight)
graph.add_weighted_edges_from(edges_weights)
nx.set_edge_attributes(graph, 'text', edge_text)

#Calculate the pagerank of each person (node) and store it with the node.
pagerank = nx.pagerank(graph)
pagerank
sum(pagerank.values())
#rank is the proportions, sum of all the ranks is 1
#pagerank.keys()
#generate a list 
pagerank_list = {node: rank for node, rank in pagerank.items()}
pagerank_list
type(pagerank_list)
nx.set_node_attributes(graph, 'pagerank', pagerank_list)

#draw the graph
positions=nx.spring_layout(graph)

graph.nodes(data=True)
#graph.edges(data=True)
#size of the graphed node proportional to its pagerank
nodesize = [x['pagerank']*30000 for v,x in graph.nodes(data=True)]
edgesize = [np.sqrt(e[2]['weight']) for e in graph.edges(data=True)]

nx.draw_networkx_nodes(graph, positions, node_size=nodesize, alpha=0.4)
nx.draw_networkx_edges(graph, positions, edge_size=edgesize, alpha=0.2)
nx.draw_networkx_labels(graph, positions, font_size=10)

plt.savefig("email_graph.png")
plt.title("Graph of all send/receive relationships in the Clinton email database", fontsize=20)
plt.clf()

#That graph is pretty big. Let's make a smaller one with just the most
# important people.  
#This will plot only the nodes with pagerank greater than
#  pagerank_cutoff

pagerank_cutoff = 0.0045

small_graph = graph.copy()
for n, p_rank in small_graph.nodes(data=True):
    if p_rank['pagerank'] < pagerank_cutoff: small_graph.remove_node(n)
    
spositions=nx.spring_layout(small_graph, weight=None)
snodesize = [x['pagerank']*30000 for v,x in small_graph.nodes(data=True)]
sedgesize = [np.log(e[2]['weight']) for e in small_graph.edges(data=True)]
scolors = np.random.rand(len(small_graph.nodes()))

nx.draw_networkx_nodes(small_graph, spositions, node_size=snodesize, node_color=scolors, alpha=0.3)
nx.draw_networkx_edges(small_graph, spositions, alpha=0.3, arrows=False) #, width=sedgesize)
nx.draw_networkx_labels(small_graph, spositions, font_size=14)
plt.title("Graph of only those people with a pagerank of greater than %s" % pagerank_cutoff, fontsize=20)
plt.savefig("small_graph.png")






