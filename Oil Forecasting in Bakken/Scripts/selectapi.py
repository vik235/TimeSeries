import os
import sys

import numpy as np
import pandas as pd
import matplotlib as mpl
import rpy2.robjects.numpy2ri

from rpy2.robjects.packages import importr
from rpy2 import robjects as robj
from matplotlib import pyplot as plt
from matplotlib import rc
from datetime import datetime
from scipy.linalg import toeplitz
from scipy.signal import savgol_filter

rc("font", **{"family":"sans-serif", "sans-serif":["Fira Sans"]})
#rc("font", **{"family":"serif", "serif":["CMU Serif"]})
#rc("text", usetex=True)
np.set_printoptions(precision=25)

# initialization of rpy2 libraries
r_base = importr("base")
r_utils = importr("utils")
r_tseries = importr("tseries")
r_tsoutlier = importr("tsoutliers")
r_astsa = importr("astsa")
r_forecast = importr("forecast")
r = robj.r

def process_data(api):
    sdat = bakkendat[bakkendat["API/UWI"]==api].reset_index(drop=True)
    procdat = pd.DataFrame(data=np.array([sdat["Production Date"].apply(datetime.strptime, args=("%Y-%m-%d",)).values]).T, columns=["t_raw"])
    #procdat = procdat.assign(**{"t_days": procdat["t_raw"].apply(lambda x: (x-procdat.loc[procdat.index.min(), "t_raw"]).days),
                                  #"q": 10**(pd.Series(np.log10(sdat["Daily Avg Oil"]).replace(-np.inf, np.nan).dropna().values))}).dropna()
    procdat = procdat.assign(**{'t_days': np.cumsum(sdat['Days']), 
                                'Q': np.cumsum(sdat['Liquid (bbl)'])})
    procdat = procdat.assign(**{'q': np.gradient(procdat['Q'], procdat['t_days'])})
    
    t_start = retrieve_cutoff(procdat)
    
    procdat = procdat[procdat["t_days"]>t_start].reset_index(drop=True)
    procdat = procdat.assign(**{"t": procdat["t_days"]-procdat.loc[0, "t_days"],})
    
    return procdat

def retrieve_cutoff(procdata):
    
    def on_click(event):
        nonlocal flag1
        nonlocal cid2
        if event.button==1:
            line1.set_data([event.xdata, event.xdata], yliminit)
            line1.figure.canvas.draw()            
            if flag1==1:
                fig.canvas.mpl_disconnect(cid2)                
                flag1 = 0
            elif flag1==0:
                cid2 = fig.canvas.mpl_connect('motion_notify_event', on_motion)
                flag1 = 1
        if event.button==3:
            plt.close()        
            
    def on_motion(event):
        line1.set_data([event.xdata, event.xdata], yliminit)
        line1.figure.canvas.draw()
        
    fig, ax = plt.subplots(1, 1)
    fig.set_size_inches(16, 9)
    
    flag1 = 1
    ax.plot(procdata["t_days"], procdata["q"], "ko", mfc="none", markersize=3)
    ax.set_title('{0}: {1}'.format(ind, api))
    ax.set_yscale("log")
    yliminit = ax.get_ylim()
    
    line1, = ax.plot([0, 0], yliminit, "k--", picker=5)
    ax.set_ylim(yliminit)
    cid1 = fig.canvas.mpl_connect('button_press_event', on_click)
    cid2 = fig.canvas.mpl_connect('motion_notify_event', on_motion)    
    xpick = 0
    
    plt.show()
        
    t_start = line1.get_xdata()[0]
    
    return t_start

bakkendat = pd.read_csv(r"data/US_OG_BAKKEN Production Time Series.CSV")
uniqueapi = bakkendat["API/UWI"].unique()

apilist = [1, 8, 9, 11, 32]
procdat = []

#for ind, api in enumerate(uniqueapi):
    ##ind, api = (14, uniqueapi[14])
    #procdat = process_data(api)
    

    
for axind, ind in enumerate(apilist):
    api = uniqueapi[ind]
    procdat.append(process_data(api))
    
#fig, ax = plt.subplots(5, 1)
#fig.set_size_inches(16, 9)

#for axind, dat in zip(range(5), procdat):    
    #ax[axind].plot(dat.t, dat.Q, "k-", marker='o', mfc='none', ms=4)
    #ax[axind].set_ylabel(r'$Q$, bbl')
    #ax[axind].set_xlabel(r'$t$, days')

#plt.tight_layout()
#plt.show()

fig, ax = plt.subplots(1, 1)
fig.set_size_inches(16, 9)
clist = ['black', 'red', 'green', 'blue', 'purple']

for axind, dat, c in zip(range(5), procdat, clist):    
    ax.plot(dat.t, dat.Q, "k-", marker='o', mfc='none', ms=4, c=c, label='Well {0}'.format(axind))
    ax.set_ylabel(r'$Q$, bbl')
    ax.set_xlabel(r'$t$, days')

ax.set_title('Cumulative Oil Production v. Time')
ax.legend()
plt.tight_layout()

print('a')
