# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 18:14:08 2015

@author: moramaldonado
"""

import operator


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


# Take Area Under the Curve of X axis
# Input: Trial Structure (containing X coordinates) and upper-end interval
# Output: AUC on X axis, named 'integral_X'+'_on_'+dx
# NB: Correct response is considered always to be at 1,1. Negative values for AUC == deviation
def integrate_x(dx,all_trials):
    name = 'integral_X'+'_on_'+dx
    for s in range(len(all_trials)):
        for t in range(len(all_trials[s])): 
            auc = 0
           # as long as we have all the mouse tracking data
            if all_trials[s][t]['value'] != '--' and all_trials[s][t]['mouse_log'] != [] and len(all_trials[s][t]['mouse_log']) > 1:
                if dx == 'fin':
                    index = 100
                else:
                    index = all_trials[s][t][dx][2] + 1

                for j in range(1,index):
                    if all_trials[s][t]['value'] == 'false':
                        yCurrent = operator.neg(all_trials[s][t]['normalized_positions'][j][0])
                        yPrev = operator.neg(all_trials[s][t]['normalized_positions'][j - 1][0])
                    else:
                        yCurrent = all_trials[s][t]['normalized_positions'][j][0]
                        yPrev = all_trials[s][t]['normalized_positions'][j-1][0]

                    xCurrent = j
                    xPrev = j-1
                    aucInc = ((yCurrent + yPrev) / 2) * (xCurrent - xPrev)
                    auc = auc + aucInc
                all_trials[s][t][name] = auc
            else:
                all_trials[s][t][name] = 'NA'
                  
    return all_trials

