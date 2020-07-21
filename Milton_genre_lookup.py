# -*- coding: utf-8 -*-
"""
Created on Fri Mar 15 09:44:32 2019

@author: efr587
"""

import pandas
import pandas as pd
import numpy as np

#read csv into pandas with tab deliniation------
genre_data = pandas.read_csv('U:/Git/MA_Data_Processing/Milton_texts_classifier.csv', sep=',', encoding='utf-8')

Milton_data = pandas.read_csv('U:/Git/data_files/data_files_milton/Milton_data_all.csv', sep='\t', encoding='utf-8')

new_df = pd.merge(Milton_data, genre_data, how='left')

new_df.to_csv('U:/Git/data_files/data_files_milton/Milton_data_all_with_genres.csv', sep='\t', encoding='utf-8')