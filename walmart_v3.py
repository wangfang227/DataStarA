#walmart kaggle competition
from __future__ import division
import os
import sys
import csv
import numpy as np
import pandas as pd
import statsmodels.api as sm
#import matplotlib.pyplot as plt
from sklearn import linear_model
from sklearn import svm
import random
import networkx as nx
from sklearn.feature_extraction.text import CountVectorizer
from collections import Counter
from time import time

os.getcwd()
os.chdir('/Users/wangfang/Documents/Work/Python/Walmart/')
os.getcwd()


'''
d=pd.read_csv(f)
y=d['TripType']
type(y)
Counter(y)
y=np.array(y)
p1=plt.hist(y)
p1.show()
'''

#n=2000 #only use the first 1000 cases to test the program
f=open('train.csv')
d=csv.reader(f)
type(d)
train=[]
for row in d:
    train.append(row)
print len(train)
train=np.array(train)
header=train[0,]
header
train=train[1:,]
ntrain=train.shape[0]
print ntrain


ft=open('test.csv')
dt=csv.reader(ft)
type(dt)
test=[]
for row in dt:
    test.append(row)
print len(test)
test.pop(0)
test=np.array(test)
ntest=test.shape[0]
print ntest

#use a subset of cases to test the program
#ntrain=2000
#ntest=1000
#ntrains=np.random.randint(train.shape[0], size=ntrain)
#ntests=np.random.randint(test.shape[0], size=ntest)
#print ntrain, ntest

# '1000' are the test cases
ytest=np.array(['1000']*test.shape[0])
test=np.concatenate((ytest[:,None],test),axis=1)
test.shape

''' test
n=1000 #use 1000 records to test
ntrain=n
ntest=n
'''

#data=np.concatenate((train[:n,],test[:n,]),axis=0)
data=np.concatenate((train,test),axis=0)

#data=train
data.shape 
#Counter(data[:,0])
#data[0,]

y=np.array(data[:,0],dtype='int')
#Counter(y)
#plt.hist(y)
#plt.hist(y[y<999])
#plt.show()

x=data[:,1:]
x.shape
type(x)
x[:3,]
#x

#Counter(x[:,0])
x0=x[:,0].astype(int)
#x0
x1=x[:,1]
#x1
x2=x[:,2]
#x2
x3=x[:,3].astype(int)
#x3
x4=x[:,4]
#x4
x5=x[:,5].astype(int)
#x5
#x[:,5]=np.array(x[:,5],dtype=int)

#x0,daydummy,upcdummy,x3,deptdummy,finedummy
#generate dummy variables 

#Counter(x1) #weekday
len(np.unique(x1))

upc=Counter(x2) #Upc number
sales=upc.values()
products=upc.keys()
len(products)
max(sales) #maximum: one product was sold 7657 times
min(sales) #minimum: product sold for only once
salesper=np.percentile(sales,(99,99.9,99.99))
salesper
theta=np.percentile(sales,(99.99))
print theta
'''
salesper
len(np.unique(x2))
for i in range(len(salesper)):
    per=salesper[i]
    sales2=[x for x in sales if x<=per]
    round(len(sales2)/len(sales),2)
    #sales2
    plt.figure()
    plt.hist(sales2)   
'''

keyproduct=[key for key, value in upc.items() if value>theta]
#keyproduct
len(keyproduct)

x2key=[i for i, j in enumerate(x2) if j in keyproduct]
len(x2key)
len(x2)


x2new=np.array([x if x in x2key else 0 for x in x2],dtype='int')

'''
x2new=x2
for i in range(len(x2)):
    if i not in x2key: 
        x2new[i]='0'       
x2new='0'*len(x2)
x2new=[j for i, j in enumerate(x2) if i in x2key]

#not key index
x2notkey=list(set(range(len(x2)))-set(x2key))
#x2notkey[:1000]

x2new=x2
x2new[x2notkey]='0'
'''


##Counter(x2)
#Counter(x2[x2key])
#Counter(x2new)

header
x0.shape
x3.shape
#np.percentile(x3,(0,25,50,75,90))

upcdummy=pd.get_dummies(x2new)
x2new.shape
upcdummy.shape
type(upcdummy)
#610 upc

#upcdummy=np.array(upcdummy,dtype=int)
#upcdummy1[:,1]

#Counter(x1)
daydummy=pd.get_dummies(x1)
daydummy=np.array(daydummy)
daydummy.shape

#Counter(x4) #department
len(np.unique(x4))
deptdummy=pd.get_dummies(x4)
deptdummy=np.array(deptdummy,dtype=int)
deptdummy.shape
#69 dept

fineline=Counter(x5) #fine line number 
len(np.unique(x5))
#finelinedummy=pd.get_dummies(x5)
np.percentile(fineline.values(),(25,50,75,90,99.9))
theta2=np.percentile(fineline.values(),95)
#fineline.values().index(303)
finelinekey=[i for i, j in fineline.items() if j>theta2]
len(finelinekey) #268 fine line
#54 fine line
x5new=np.array([x if x in finelinekey else 0 for x in x5],dtype='int')
#len(Counter(x5new))
#pd.Series(x5new).value_counts()
finedummy=pd.get_dummies(x5new)
print finedummy.shape


# how many types if things bought
x0df=pd.DataFrame(x0)
x0df.columns=['x0']
x0dfg=x0df.groupby('x0').size().reset_index()
#x0dfg=x0df.groupby('x0').size()
#x0dfg #ordered by id: x0
x0dfg.columns=['x0','x0count']

#select the first day
#add y into the daydummy, as we also need to select the first of y
day=np.concatenate((x0[:,None],y[:,None],daydummy),axis=1)
daydf=pd.DataFrame(day)
ncolumns=daydf.shape[1]
daydf.columns=[range(ncolumns)]
#daydf
#daydfg=daydf.groupby(0).first().reset_index()
daydfg=daydf.groupby(0).first()
#daydfg

#upcdummy, deptdummy, finedummy needs to be summed

#x0count[x0count<0]=0
x3[x3<0]=0

upcdummy2=np.multiply(upcdummy,x3[:,None])
deptdummy2=np.multiply(deptdummy,x3[:,None])
finedummy2=np.multiply(finedummy,x3[:,None])

dummy=np.concatenate((x0[:,None],upcdummy2,deptdummy2,finedummy2),axis=1)
dummydf=pd.DataFrame(dummy)
dummydf.shape[1]
dummydf.columns=[range(dummydf.shape[1])]
#dummydfg=dummydf.groupby(0).sum().reset_index()
dummydfg=dummydf.groupby(0).sum()
dummydfg.shape
#dummydfg

#'VisitNumber', 'Weekday', 'Upc', 'ScanCount','DepartmentDescription', 'FinelineNumber'
#xnew=np.concatenate((x0[:,None],daydummy,upcdummy,x3[:,None],deptdummy,finedummy),axis=1)

dnew=np.concatenate((x0dfg,daydfg,dummydfg),axis=1)
#only x0dfg has id as a column, daydfg and dummydfg has not x0 as column, x0 are just index for them
dnew.shape
dtrain=dnew[dnew[:,2]!=1000,]
dtrain.shape
dtest=dnew[dnew[:,2]==1000,]
dtest.shape
#x0, x0count, y, daydummy,upcdummy,deptdummy,finedummy
# the 0 and 2 column needs to be deleted from the X matrix
# the 0 column is the id column
# the 2 column is the y column


y_train=dtrain[:,2]
#Counter(ytrain)
y_test=dtest[:,2]
#Counter(ytest)

idtrain=dtrain[:,0]
#idtrain
len(idtrain)
idtest=dtest[:,0]
#idtest
len(idtest)

xtrain=np.delete(dtrain, np.s_[0,2], axis=1) 
#xtrain
#dtrain[:,2:]
xtest=np.delete(dtest, np.s_[0,2], axis=1) 
#xtest

from sklearn.feature_selection import SelectKBest, chi2
from sklearn.linear_model import RidgeClassifier
from sklearn.pipeline import Pipeline
from sklearn.svm import LinearSVC
from sklearn.linear_model import SGDClassifier
from sklearn.linear_model import Perceptron
from sklearn.linear_model import PassiveAggressiveClassifier
from sklearn.naive_bayes import BernoulliNB, MultinomialNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors import NearestCentroid
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import Lasso
from sklearn.utils.extmath import density
from sklearn import metrics

ntrain
ntest

xtrain.shape
xtest.shape
select_chi2=100 # use 500 features

#print("Extracting %d best features by a chi-squared test" %
#      select_chi2)

t0 = time()
xtrain.shape
y_train.shape

X_train=xtrain
#X_train.shape
X_test=xtest
#X_test.shape

#ch2 = SelectKBest(chi2, k=select_chi2)
#X_train = ch2.fit_transform(xtrain, y_train)
#X_train.shape
#X_test = ch2.transform(xtest)
#X_test.shape
#

###############################################################################
# Load some categories from the training set

# Benchmark classifiers

ncat=len(np.unique(y_train)) #number of categories to predict


# calculate walmart score
def walmart_score(clf):
    wscore=[]
    print('_' * 80)
    print("Training: ")
    print(clf)
    t0 = time()
    clf.fit(X_train, y_train)
    train_time = time() - t0
    print("train time: %0.3fs" % train_time)   
    t0 = time()
    pred = clf.predict(X_test)
    test_time = time() - t0
    print("test time:  %0.3fs" % test_time)
    classes=map(int,list(clf.fit(X_train, y_train).classes_))
    print classes    
    prob=clf.predict_proba(X_test)
    #print prob
    prob[prob==0]=0.00001
    #calculate score
    clf_descr = str(clf).split('(')[0]    
    return clf_descr, prob, classes
  
  
results = []

results.append(walmart_score((RandomForestClassifier(n_estimators=100))))

results[0][0] #model name
results[0][1].shape #prob
len(results[0][2]) #classes
#Counter(results[0][2])

label=[]
for i in range(len(results[0][2])):
    label.append('TripType_'+str(results[0][2][i]))
label     
 
column_names=['VisitNumber']+label    
probfinal=np.concatenate((map(int,idtest[:,None]),results[0][1]),axis=1)    
probdf=pd.DataFrame(probfinal)
probdf.columns=column_names    

timestr = time.strftime("_%Y%m%d-%H%M%S")
filename='walmart_result_'+ results[0][0] + timestr + '.csv'
probdf.to_csv(filename,sep=',',index=False)

