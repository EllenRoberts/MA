# -*- coding: utf-8 -*-
"""
Created on Thu Jun 20 16:17:58 2019

@author: Elli
"""

import ast
import pandas as pd
import pandas

with open("C:/Users/ellie/Documents/MA_repo/ER_R_code_plots/Raw_data_files/word_IDs.txt", 'r') as f:
    Milton_words = ast.literal_eval(f.read())

letters = []

for word in Milton_words:
    letters.append(word[0])
    
f = open("J:/Users/Elli/Documents/Uni/MA/Git_MA_final/letters.txt", "w")
f.write(str(letters))

with open('J:/Users/Elli/Documents/Uni/MA/Git_MA_final/source_year.txt', 'r', encoding = 'utf-8') as f:
    source_year = ast.literal_eval(f.read())

with open('J:/Users/Elli/Documents/Uni/MA/Git_MA_final/word_IDs.txt', 'r', encoding = 'utf-8') as f:
    word_ID = ast.literal_eval(f.read())
    
d = {'Word_ID': word_ID, 'letters': letters}

#df = pd.DataFrame(d)
#df_analysis.to_csv('U:/Git/data_files/data_for_analysis.csv', sep='\t', encoding='utf-8', index=False)
#df.to_csv('J:/Users/Elli/Documents/Uni/MA/Git_MA_final/letter_data.csv', sep='\t', encoding='utf-8', index=False)