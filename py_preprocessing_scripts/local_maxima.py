# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 18:01:41 2015

@author: moramaldonado
"""

def local_maxima(all_trials):
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])):
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                
                m = max(all_trials[s][t]['smooth_acceleration'])
                temp = [i for i, j in enumerate(all_trials[s][t]['smooth_acceleration']) if j == m]
                index = temp[0]
               
                der_acc = []
                local_maxima = []
                der_acc.append(all_trials[s][t]['smooth_acceleration'][0])
                for i in range(1,len(all_trials[s][t]['smooth_acceleration'])):
                    a1 = all_trials[s][t]['smooth_acceleration'][i-1]
                    a2 = all_trials[s][t]['smooth_acceleration'][i]
                    t1 = all_trials[s][t]['temp_velocity'][i-1]
                    t2 = all_trials[s][t]['temp_velocity'][i]
                    d = float((a2-a1)/(t2-t1))
                    der_acc.append(d)
                for j in range(1,len(der_acc)):
                    if der_acc[j-1] > 0 and der_acc[j] < 0 and all_trials[s][t]['smooth_acceleration'][j-1] >= m/2:
                        local_maxima.append([all_trials[s][t]['smooth_acceleration'][j-1],j-1])
                    
                    
                if local_maxima == []:
                    local_maxima.append([m, index])
                all_trials[s][t]['local_maxima'] = local_maxima
            else:
                all_trials[s][t]['local_maxima'] = 'NA'
    return all_trials
            
            
                
            
    
                
                
    