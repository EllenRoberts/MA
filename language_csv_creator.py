# -*- coding: utf-8 -*-
"""
Created on Mon Mar 11 17:07:04 2019

@author: efr587
"""

import os
import ast 
import pandas as pd
import pandas
from collections import defaultdict

x = "U:/Git/final_files/etymology_etymon_langs_clean_spellings.txt"

count = []
final_word = []

with open("U:/Git/final_files/source_year.txt", 'r', encoding = 'utf-8') as f:
     date_ranges = ast.literal_eval(f.read())

with open("U:/Git/final_files/Word_IDs.txt", 'r', encoding = 'utf-8') as f:
     word_IDs = ast.literal_eval(f.read())

with open(x, 'r', encoding='utf-8') as f:
        langs = ast.literal_eval(f.read())
        count.append(str(len(langs)))

t = tuple(zip(word_IDs, langs))

d = defaultdict(list)

for key, value in sorted(t):
    for i in range(len(value)):
        d[key].append(value[i])
        
keys = d.keys()
values = d.values()
list_values = []
for lists in values:
    for value in lists:
        if value not in list_values:
            list_values.append(value)

df= pd.DataFrame.from_dict(d, orient='index') #orient uses dict keys as rows

dfObj = pd.DataFrame(columns= list_values, index= keys)
#x = df.apply(pd.value_counts)

test = []
for index, row in df.iterrows():
    p = row.value_counts()
    print(p)
    test.append(p)

new = pd.concat(test, axis=1)

new2 = new.transpose().fillna(0)