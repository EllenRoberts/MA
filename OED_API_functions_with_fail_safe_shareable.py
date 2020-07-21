# -*- coding: utf-8 -*-
"""
Created on Tue Feb 19 15:17:16 2019

@author: Ellen Roberts
"""

import requests
import ast
import os
import time 
import datetime
from time import sleep

#main url for the OED API, note: credentials to access the API are needed, and can be requested from OUP at: https://developer.oxforddictionaries.com/

main_api = 'https://oed-api.oxforddictionaries.com/oed/api/v0.1/' 
app_id = 
app_key = 

#function for the progress bar, thanks to https://stackoverflow.com/questions/3173320/text-progress-bar-in-the-console

def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█'):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
    """
    
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end = '\r')
    
    if iteration == total: 
        print()
        
def all_word_data(file_input):
    with open(file_input, 'r') as f:
        ID_file = ast.literal_eval(f.read())
        path=os.path.dirname(file_input) #takes the custom file name from retrieve functions and outputs text files with API data into these folders.
        no_word_IDs =len(ID_file)
        word_id_list = []
        sense_id_list = []
        lemma_list = []
        PoS_list = []
        definit_list = []
        date_range_list = []
        date_list = []
        obsolete_list = []
        ety_type_list = []
        ety_list = []
        languages_list = []
        main_entry_list = []
        updated_list = []
        created_list = []
        firstuse_list = []
        printProgressBar(0, no_word_IDs, prefix = 'Progress:', suffix = 'Complete', length = 50)
        error_list = []
        
        f = open("%s/word_IDs_check.txt" % (path), "w", encoding="utf-8")
        g = open("%s/sense_IDs.txt" % (path), "w", encoding="utf-8")
        h = open("%s/lemma.txt" % (path), "w", encoding="utf-8")
        k = open("%s/part_of_speech.txt" % (path), "w", encoding="utf-8")
        l = open("%s/definitions.txt" % (path), "w", encoding="utf-8")
        m = open("%s/date_ranges.txt" % (path), "w", encoding="utf-8")
        n = open("%s/word_start_dates.txt" % (path), "w", encoding="utf-8")
        p = open("%s/obsoleteness.txt" % (path), "w", encoding="utf-8")
        q = open("%s/etymology_types.txt" % (path), "w", encoding="utf-8")
        r = open("%s/etymology_summaries" % (path), "w", encoding="utf-8")
        s = open("%s/etymon_origin_languages.txt" % (path), "w", encoding="utf-8")
        t = open("%s/main_entries.txt" % (path), "w", encoding="utf-8")
        v = open("%s/OED_update_dates" % (path), "w", encoding="utf-8")
        w = open("%s/entry_creation_dates.txt" % (path), "w", encoding="utf-8")
        a = open("%s/first_usage_authors_check.txt" % (path), "w", encoding="utf-8")


        for word in ID_file:
            
            word_url = main_api + 'word/' + word        
            for x in range(5):  # 5 attempts max before bailing out
                print('attempt ' + str(x))
                try:
                    json_data = requests.get(word_url, headers = {'app_id': app_id, 'app_key': app_key}).json()
                    ts = time.time()
                    dt = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
                except requests.exceptions.RequestException as err:
                    print('\n' + 'Lost connection... retrying in 10 seconds')
                    sleep(10)  # idle for 10 seconds
                    error_list.append(str(dt) + ' ' + str(x + 1) + ': ' + word + ' ' + str(ID_file.index(word)))
                    b = open("%s/ERRORS.txt" % (path), "w", encoding="utf-8")
                    b.write(str(error_list))
                else:
                    break
            if json_data is None:
                print('Error: no Json response!')
            else:
                word_id_list.append(json_data['data']['id'])
                sense_id_list.append(json_data['data']['sense_ids'][0])
                lemma_list.append(json_data['data']['lemma'])
                Px = json_data['data']['parts_of_speech']
                if Px == []:
                    Px = 'None'
                    PoS_list.append(Px)
                else:
                    PoS_list.append(Px[0])
                definit_list.append(json_data['data']['definition'])
                date_range_list.append(str(json_data['data']['daterange']['rangestring']))
                date_list.append(str(json_data['data']['daterange']['start']))
                obsolete_list.append(str(json_data['data']['daterange']['obsolete']))
                ety_type_list.append(json_data['data']['etymology']['etymology_type'])
                ety_list.append(json_data['data']['etymology']['etymology_summary'])
                x = json_data['data']['etymology']['etymons'][0:len('etymons')]
                if x == []:
                    x = ['None']
                    languages_list.append(x)
                else:
                    z = []
                    for y in x:
                        z.append(y['language'])
                        languages_list.append(z)
                main_entry_list.append(str(json_data['data']['main_entry']))
                updated_list.append(json_data['data']['meta']['updated'])
                created_list.append(json_data['data']['meta']['created'])
                firstuse_list.append(json_data['data']['first_use'])
                
                f.write(str(word_id_list))                 
                g.write(str(sense_id_list))            
                h.write(str(lemma_list)) 
                k.write(str(PoS_list))  
                l.write(str(definit_list))
                m.write(str(date_range_list)) 
                n.write(str(date_list))
                p.write(str(obsolete_list))
                q.write(str(ety_type_list))       
                r.write(str(ety_list))
                s.write(str(languages_list))
                t.write(str(main_entry_list))
                v.write(str(updated_list))
                w.write(str(created_list))
                a.write(str(firstuse_list))
            
                iteration = ID_file.index(word)
            
                #sleep(0.1)
            
                printProgressBar(iteration + 1, no_word_IDs, prefix = 'Progress:', suffix = 'Complete ' + dt, length = 50)
            
    return 'Complete'