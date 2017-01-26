# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 18:14:08 2015

@author: moramaldonado
"""

def integrate(curve,dx,all_trials):
    name = 'integral_'+ curve +'_on_'+dx
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])): 
            auc = 0
            
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                if dx == 'fin':
                    index = 100
                else:
                    index = all_trials[s][t][dx][2] + 1
                for j in range(1,index):
                    yCurrent = all_trials[s][t][curve][j]
                    xCurrent = j
                    yPrev = all_trials[s][t][curve][j-1]
                    xPrev = j-1
                    aucInc = ((yCurrent + yPrev) / 2) * (xCurrent - xPrev)
                    auc = auc + aucInc
                all_trials[s][t][name] = auc
            else:
                all_trials[s][t][name] = 'NA'
                
                  
    return all_trials


def integrate_x(dx,all_trials):
    name = 'integral_X'+'_on_'+dx
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])): 
            auc = 0
            
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                if dx == 'fin':
                    index = 100
                else:
                    index = all_trials[s][t][dx][2] + 1

                for j in range(1,index):
                    yCurrent = all_trials[s][t]['normalized_positions'][j][0]
                    xCurrent = j
                    yPrev = all_trials[s][t]['normalized_positions'][j-1][0]
                    xPrev = j-1
                    aucInc = ((yCurrent + yPrev) / 2) * (xCurrent - xPrev)
                    auc = auc + aucInc
                all_trials[s][t][name] = auc
            else:
                all_trials[s][t][name] = 'NA'
                  
    return all_trials

