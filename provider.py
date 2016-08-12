import os
import pandas as pd
import numpy as np
from nltk.metrics import edit_distance
import seaborn as sns
from IPython.display import Image
import matplotlib.pyplot as plt
import nltk
#from util import *


# create lon, lat
os.getcwd()
os.chdir('C:\\Users\\usz003g\\Documents\\WCPF\\Provider')

p=pd.read_csv("prov_id_10ksample.csv")
p.columns


#license and NPI are almost all missing

#use name + Tin as the first round of exact match 

p['addr']=p['prov_addr1'].map(str)+' '+p['prov_addr2'].map(str)

def addr_clean(s0):
    s=str(s0).lower()
    s=s.replace(',', ' ')
    s=s.replace('/', ' ')
    s=s.replace('.', ' ')
    s=s.replace('street', 'st')
    s=s.replace('road', 'rd')
    s=s.replace('avenue', 'av')
    s=s.replace('drive', 'd')
    s=s.replace('boulevard', 'bd')
    s=s.replace('blvd', 'bd')
    s=s.replace('west','w')
    s=s.replace('east','e')
    s=s.replace('north','n')
    s=s.replace('south','s')
    s=s.strip(' \t\n\r')
    s=s.rstrip()
    s=s.lstrip()
    s=' '.join(s.split())
    return s
    
p['prov_addr1']=p['prov_addr1'].fillna(' ')
p['prov_addr1_cleaned']=p['prov_addr1'].map(addr_clean)

len(p['prov_addr1'].unique())
len(p['prov_addr1_cleaned'].unique())

p['prov_addr2']=p['prov_addr2'].fillna(' ')
p['prov_addr2_cleaned']=p['prov_addr2'].map(addr_clean)

len(p['prov_addr2'].unique())
len(p['prov_addr2_cleaned'].unique())

#p['prov_addr2_cleaned']=p['prov_addr2_cleaned'].fillna(' ')
p['addr']=p['prov_addr1_cleaned'].map(str)+' '+p['prov_addr2_cleaned'].map(str)

p['addr_cleaned']=p['addr'].map(addr_clean)
p['addr_cleaned']

len(p['addr_cleaned'].unique())

p['addr_cleaned'][:50]

'''    
010315414-0002	01-0315414			0		0		X-RAY PROFESSIONAL ASSOC			583 MAIN STREET		LEWISTON	ME	4240
010315414-0004	01-0315414			0		0		DX XRAY PROFESSIONAL ASSOC					LEWISTON	ME	4243
010315414-03	01-0315414			1		61		BARRY M KUTZEN MD			287 MAIN ST  SUITE 200		LEWISTON	ME	4240
010315414-05	01-0315414			1		61		XRAY PROFESSIONAL ASSOCIATION			287 MAIN ST  SUITE 200		LEWISTON	ME	4240
'''

def name_clean(s0):
    s=str(s0).lower()
    s=s.replace(',', ' ')
    s=s.replace('.', ' ')
    s=s.replace('&', ' ')
    s=s.replace('#', ' ')
    s=s.replace('/', ' ')
    s=s.replace('x-ray', 'xray')
    s=s.replace('pa-c', 'pa')
    s=s.replace('pac', 'pa')
    s=s.replace(' md', ' ')
    s=s.replace('dr ', ' ')
    s=s.replace(' pa', ' ')
    s=s.replace(' dc', ' ')
    s=s.replace(' php', ' ')
    s=s.replace(' pt', ' ')
    s=s.replace(' phd', ' ')
    s=s.replace(' lmt', ' ')
    s=s.replace('associaton','assoc')
    s=s.replace('service','serv')
    s=s.replace('company','comp')
    s=s.replace('hospital','hosp')
    s=s.replace('cntr','ctr')
    s=s.replace('center','ctr')
    s=s.replace('junior', 'jr')
    s=s.strip(' \t\n\r')
    s=s.rstrip()
    s=s.lstrip()
    s=' '.join(s.split())
    return s
    
p['prov_last_name_cleaned']=p['prov_last_name'].map(name_clean)

len(p['prov_last_name'].unique())
len(p['prov_last_name_cleaned'].unique())

from nltk.stem.lancaster import LancasterStemmer
st = LancasterStemmer()
p['prov_last_name_cleaned']=p['prov_last_name_cleaned'].map(st.stem)

len(p['prov_last_name'].unique())
len(p['prov_last_name_cleaned'].unique())

p['prov_last_name_cleaned'][:50]


def tin_clean(s0):
    s=str(s0).lower()
    s=s.replace(',', '')
    s=s.replace('.', '')
    s=s.replace('&', '')
    s=s.replace('#', '')
    s=s.replace('-', '')
    s=s.strip(' \t\n\r')
    s=s.lstrip('0')
    s=s.rstrip()
    s=s.lstrip()
    s=' '.join(s.split())
    return s

p['prov_tax_id_cleaned']=p['prov_tax_id'].map(tin_clean)

len(p['prov_tax_id'].unique())
len(p['prov_tax_id_cleaned'].unique())

p['prov_tax_id_cleaned'][:60]


#combine name and tin
p['name_tin']=p['prov_last_name_cleaned'].map(str)+' '+p['prov_tax_id_cleaned'].map(str)
p['name_tin_cleaned']=p['name_tin'].map(name_clean)
p['name_tin_cleaned']=p['name_tin_cleaned'].map(addr_clean)
p['name_tin_cleaned']=p['name_tin_cleaned'].map(tin_clean)

p['name_tin_cleaned'][:50]


#combine name and addr
p['addr_name']=+p['addr_cleaned'].map(str)+' '+p['prov_last_name_cleaned'].map(str)
p['addr_name_cleaned']=p['addr_name'].map(name_clean)
p['addr_name_cleaned']=p['addr_name_cleaned'].map(addr_clean)
p['addr_name_cleaned']=p['addr_name_cleaned'].map(tin_clean)

p['addr_name_cleaned'][:50]


### match by name and tin
p=p.sort(['prov_last_name_cleaned','prov_tax_id_cleaned'])

def name_tin_match(bt,v):
    count=0
    table={}
    cluster=[]
    for a in v:
       if a in table: 
           cluster.append(count)
       else: 
           count+=1
           bt['cluster']=count
           table[a]=count
           cluster.append(count)
    bt['cluster']=np.array(cluster)
    return table, count

name_tin_dict, name_tin_count=name_tin_match(p,p.name_tin_cleaned)

name_tin_dict
name_tin_count

len(p['cluster'].unique())


#### further collapsing by name + address
p=p.sort(['prov_last_name_cleaned','addr_cleaned'])

nr=len(p['addr_name_cleaned'])

for i in range(nr):
    if i==0:
        continue
    i0=i-1
    addr_name0=p.ix[i0,'addr_name_cleaned']
    c0=p.loc[i0,'cluster']
    
    addr_name=p.ix[i,'addr_name_cleaned']
    c=p.loc[i,'cluster']
    
    if addr_name0==addr_name:
        p.loc[i,'cluster']=c0
        
len(p['cluster'].unique())       


############## name address fuzzy match ################

def fuzzy_match(x,y):
    x_list=x.split()
    y_list=y.split()
    x_set=set(x_list)
    y_set=set(y_list)
    start_number=(x_list[0].isdigit() and y_list[0].isdigit())
    if start_number:
        #print(x_list[0], y_list[0])
        num_flag=(x_list[0]==y_list[0])
    else: num_flag=False
    com_flag=len(x_set & y_set)>=1
    dis_flag=(edit_distance(x,y)<5)
    #print(start_number,num_flag,com_flag,dis_flag)
    if start_number:
        if num_flag and com_flag and dis_flag:
            return True
    else:
        if com_flag and dis_flag:
            return True
    return False
    
fuzzy_match('5035  balfour','5297  balfour')  

p.columns

p.index=range(nr)
p.iterrows()



def refine_clusters(df):
    clusters = {}
    
    row0 = df.iloc[0, :]
    prev_cluster = row0['cluster']
    prev_addr = row0['addr_name_cleaned']
    row0_id =row0['prov_id']
    clusters[row0_id] = prev_cluster

    for i in range(nr):
        row=p.ix[i,:]
        addr = row['addr_name_cleaned']
        cluster = row['cluster']
        id = row['prov_id']
        
        if id == row0_id:
            continue
        
        if cluster != prev_cluster:
            if fuzzy_match(addr, prev_addr):
                cluster = prev_cluster
            else:
                prev_cluster = cluster
                prev_addr = addr
        
        clusters[id] = cluster
    
    df['refined_cluster'] = df['prov_id'].map(lambda x: clusters[x])
    return df

p=refine_clusters(p)

len(p['cluster'].unique())       

len(p['refined_cluster'].unique())    

#fuzzy match cleaned 90 cases
9442-9352  

p=p.sort('refined_cluster') 

prov_id_count=p.groupby('refined_cluster').count().reset_index()
prov_id_count.columns
prov_id_count=prov_id_count.ix[prov_id_count['prov_id']>=2,['refined_cluster','prov_id']]
prov_id_count.columns=['refined_cluster','prov_id_count']

pc=pd.merge(p,prov_id_count,how='inner',on='refined_cluster')

pc.columns

pc=pc[['refined_cluster','cluster','prov_id','prov_id_count','prov_last_name','prov_addr1', 'prov_addr2',
'prov_tax_id','name_tin_cleaned','addr_name_cleaned']]

pc.shape

pc.to_csv('collapsed.csv',index=False)

#pd.merge()



'''
#name address
p=p.sort(['name_addr_cleaned','prov_last_name_cleaned'])

def name_addr_match(bt,v):
    count=0
    table={}
    cluster=[]
    for a in v:
       if a in table: 
           cluster.append(count)
       else: 
           count+=1
           bt['cluster']=count
           table[a]=count
           cluster.append(count)
    bt['cluster2']=np.array(cluster)
    return table, count

name_addr_dict, name_addr_count=name_addr_match(p,p.name_addr_cleaned)

name_addr_dict
name_addr_count

len(p['cluster2'].unique())

p=p.sort(['cluster2','cluster1'])

#p[:50]

nr=p.shape[0]

for i in range(nr):
    if i==0:
        continue
    i0=i-1
    c20=p.ix[i0,'cluster2']
    c10=p.loc[i0,'cluster1']
    
    c2=p.ix[i,'cluster2']
    c1=p.loc[i,'cluster1']
    
    if c20==c2:
        p.loc[i,'cluster1']=c10
        
len(p['cluster1'].unique())       



p=p.sort(['cluster1','cluster2'])

nr=p.shape[0]

for i in range(nr):
    if i==0:
        continue
    i0=i-1
    c20=p.ix[i0,'cluster2']
    c10=p.loc[i0,'cluster1']
    
    c2=p.ix[i,'cluster2']
    c1=p.loc[i,'cluster1']
    
    if c20==c2:
        p.loc[i,'cluster1']=c10
        
len(p['cluster1'].unique())       

(10000-9442)/10000
'''




