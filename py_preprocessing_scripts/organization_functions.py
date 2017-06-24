# -*- coding: utf-8 -*-
"""
Created on Fri Apr 17 18:40:03 2015

@author: moramaldonado
"""


import json
import os
#import matplotlib.pyplot as plt
import os
import csv
import pickle
import time
import math
import pygame

def _decode_list(data):
    rv = []
    for item in data:
        if isinstance(item, unicode):
            item = item.encode('utf-8')
        elif isinstance(item, list):
            item = _decode_list(item)
        elif isinstance(item, dict):
            item = _decode_dict(item)
        rv.append(item)
    return rv

def _decode_dict(data):
    rv = {}
    for key, value in data.iteritems():
        if isinstance(key, unicode):
            key = key.encode('utf-8')
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        elif isinstance(value, list):
            value = _decode_list(value)
        elif isinstance(value, dict):
            value = _decode_dict(value)
        rv[key] = value
    return rv

def joining_data(rootDir):

    all_trials=[]
    names = []

    for dirName, subdirList, fileList in os.walk(rootDir):
        print('Found directory: %s' % dirName)
        for fname in fileList:
            if fname.endswith('.json'):
                names.append(fname)
                #subjects.append(fname.replace('.json',''))

                os.chdir(dirName)
                with open(str(fname)) as f:
                    print('\t%s' % fname)
                    for line in f:
                        all_trials.append(json.loads(line, object_hook=_decode_dict))

                #print('\t%s' % fname) 

    return all_trials,names


def exporting_data(path,all_trials, info):
    os.chdir(path)
    name = 'Data'+'.csv'
    info_name = 'Information'+'.csv'
    
    with open(info_name, 'w') as m:
        writer = csv.writer(m)
        writer.writerow( ('Subject', 'File', 'Gender', 'Age', 'Color', 'Change','Total.time', 'Points', 'Clicker',
                         'Handeness','Language','Mobile','Normalized_button','Portrait','Strategy','Touch',
                          'User-agent','Window') )        

        for j in range(len(info)):
            writer.writerow((info[j]['subject'], info[j]['file'], info[j]['gender'],info[j]['age'],info[j]['colorblind'], info[j]['change'], info[j]['total_time'],info[j]['points'], info[j]['clicker'][0],
                                info[j]['handedness'], info[j]['language'], info[j]['mobile'], info[j]['normalized_button_size'], info[j]['portrait'], info[j]['strategy'], info[j]['touch'],info[j]['userAgent'], info[j]['windowWidth']))
        
     
    with open(name, 'w') as f:
        
        writer = csv.writer(f)
        writer.writerow( ('Subject', 'Item.number', 'Type', 'Condition', 'Polarity', 'Expected_response',
                          'PointChange', 'PointChange.Time', 'PointChange.X', 'PointChange.Y', 'PointChange.Time.Raw',
                          'Response','Accuracy','RT','Normalized.positions.X','Normalized.positions.Y',
                          'Acceleration','Acceleration_Smooth', 'Acceleration_Smooth.Time', 'RawTime', 'LogRatio',
                          'MaxDeviation','MaxDeviation.Time','MaxDeviation.Time.Norm',
                          'MaxDeviationBorder','MaxDeviationBorder.Time','MaxDeviationBorder.Time.Norm',
                          'Median.LogRatio', 'MaxRatio','MaxLogRatio','MaxRatio.Time','MaxRatio.Time.Norm',
                          'AccPeak','AccPeak.Time','AccPeak.Time.Norm', 'Local.Maxima.Acc','Len.Local.Maxima.Acc',
                          'X-flips','Acc-flips', 'AUC','Delay'))

        for i in range(len(all_trials)):
            for t in range(len(all_trials[i])):
                    #print i,t
                    writer.writerow((str(i), str(t), all_trials[i][t]['data']['item']['type'], all_trials[i][t]['data']['item']['raw'], all_trials[i][t]['polarity'], all_trials[i][t]['expected_response'],
                                     all_trials[i][t]['change_point'], all_trials[i][t]['PointChange.Time'],  all_trials[i][t]['PointChange.X'],all_trials[i][t]['PointChange.Y'],all_trials[i][t]['PointChange.Time.Raw'],
                                     all_trials[i][t]['value'], all_trials[i][t]['accuracy'], all_trials[i][t]['RT'], ','.join(map(repr, all_trials[i][t]['normalized_positions_x'])),','.join(map(repr, all_trials[i][t]['normalized_positions_y'])),
                                     ','.join(map(repr, all_trials[i][t]['acceleration'])), ','.join(map(repr, all_trials[i][t]['acceleration_window'])), ','.join(map(repr, all_trials[i][t]['acceleration_window.time'])),
                                     ','.join(map(repr,all_trials[i][t]['corresponding_time'])), ','.join(map(repr,all_trials[i][t]['ratio_log'])),
                                     all_trials[i][t]['maxDeviation'][0], all_trials[i][t]['maxDeviation'][1],all_trials[i][t]['maxDeviation'][2],
                                     all_trials[i][t]['maxDeviationBorder'][0], all_trials[i][t]['maxDeviationBorder'][1],all_trials[i][t]['maxDeviationBorder'][2],
                                     all_trials[i][t]['median_ratio_log'], all_trials[i][t]['max_ratio'][0],all_trials[i][t]['max_ratio_log'][0],all_trials[i][t]['max_ratio'][1],all_trials[i][t]['max_ratio'][2],
                                     all_trials[i][t]['max_acceleration'][0],all_trials[i][t]['max_acceleration'][1],all_trials[i][t]['max_acceleration'][2],
                                     all_trials[i][t]['local_maxima'],len(all_trials[i][t]['local_maxima']),
                                     all_trials[i][t]['x_flips'],all_trials[i][t]['acc_flips'],all_trials[i][t]['integral_X_on_fin'], all_trials[i][t]['delay']))
                                        
  
    f.close()
    m.close()


def points_per_trial(all_trials):
    points_per_trial = []
    for s in range(len(all_trials)):
        points = []
        for t in range(len(all_trials[s])-2):
            points.append(len(all_trials[s][t]['mouse_log']))
        mean = sum(points)/len(points)
        points_per_trial.append([mean,max(points),min(points)])
    return points_per_trial


def information(all_trials,names,points_per_trial):
    total_time = []
    info = []
    for i in range(len(all_trials)):
        m = all_trials[i][-2]['timestamp'] - all_trials[i][0]['timestamp']
        m = float(m /1000)
        m = float(m/60)
        total_time.append(m)

        all_trials[i][-2]['subject'] = i
        all_trials[i][-2]['strategy'] = all_trials[i][-1]['strategy']
        all_trials[i][-2]['total_time'] = m
        all_trials[i][-2]['file'] = names[i]
        all_trials[i][-2]['points']= points_per_trial[i]
        del all_trials[i][-1]
        info.append(all_trials[i][-1])
        del all_trials[i][-1]


        #include gender and age

    return info, total_time

def convert_time(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            for p in range(len(all_trials[s][t]['mouse_log'])):
                raw_time = all_trials[s][t]['mouse_log'][p][2] - all_trials[s][t]['data']['start_track']
                all_trials[s][t]['mouse_log'][p].append(raw_time)
    
    return all_trials         

def organization_trials(all_trials):
    for i in range(len(all_trials)):

        acc = 0
        for t in range(len(all_trials[i])):

            all_trials[i][t]['subject'] = i
            all_trials[i][t]['RT'] = all_trials[i][t]['data']['end_track'] - all_trials[i][t]['data']['start_track']
            all_trials[i][t]['delaySTART'] = all_trials[i][t]['data']['start_track'] - all_trials[i][t]['data']['start_time']
            all_trials[i][t]['type'] = all_trials[i][t]['data']['item']['type']


            if "practice" in all_trials[i][t]['data']['item']['raw']:
                all_trials[i][t]['data']['item']['type'] = 'practice'
                if all_trials[i][t]['data']['item']['item_number'] == 0:
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'straight'
                    all_trials[i][t]['change_point'] = 0

                elif all_trials[i][t]['data']['item']['item_number'] == 1:
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'straight'
                    all_trials[i][t]['change_point'] = 0

                elif all_trials[i][t]['data']['item']['item_number'] in [2,4]:
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = 0 #change this

                elif all_trials[i][t]['data']['item']['item_number'] in [3,5]:
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = 0 #change this

            elif "practice" not in all_trials[i][t]['data']['item']['raw']:

                if all_trials[i][t]['data']['item']['raw'] == 'RR1':
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'straight'
                    all_trials[i][t]['change_point'] = 0

                elif all_trials[i][t]['data']['item']['raw'] == 'BB1':
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'straight'
                    all_trials[i][t]['change_point'] = 0

                elif all_trials[i][t]['data']['item']['raw'] == 'RB2':
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'RB3':
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'BR2':
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'BR3':
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'deviated'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'RR2':
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'uncertain'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'RR3':
                    all_trials[i][t]['expected_response'] = 'true'
                    all_trials[i][t]['color'] = 'red'
                    all_trials[i][t]['polarity'] = 'uncertain'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'BB2':
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'uncertain'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

                elif all_trials[i][t]['data']['item']['raw'] == 'BB3':
                    all_trials[i][t]['expected_response'] = 'false'
                    all_trials[i][t]['color'] = 'blue'
                    all_trials[i][t]['polarity'] = 'uncertain'
                    all_trials[i][t]['change_point'] = all_trials[i][t]['data']['ty']

            if all_trials[i][t]['expected_response'] == all_trials[i][t]['value']:
                all_trials[i][t]['accuracy'] = 1
            else:
                all_trials[i][t]['accuracy'] = 0

            acc = acc + all_trials[i][t]['accuracy']

        print 'subject:'+str(i) +',correct:'+ str(acc)

    return all_trials

def delay(all_trials):
    for i in range(len(all_trials)):
        for t in range(len(all_trials[i])):
            if len(all_trials[i][t]['mouse_log']) > 1:
                all_trials[i][t]['delay'] = all_trials[i][t]['mouse_log'][1][3]
            else: 
                all_trials[i][t]['delay'] = 'NA'
    return all_trials

