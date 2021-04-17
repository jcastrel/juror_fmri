# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import glob
import pandas as pd


file = glob.glob('all_juror_fmri_behavior.csv')

for filename in file:
    df = pd.read_csv(filename)
    df2 = df

#long to wide
l = pd.wide_to_long(df2, stubnames=['q','rt'], i=['uid','trial','scenario','history','witness','physical','TimedOut','group'], j='rating_type')
l = l.reset_index()
#rename some columns
l = l.rename(columns={'q':'rating','rt':'rt'})
l['rating_type'] = l['rating_type'].map({1:'rating', 2:'rate_punishment'})
#add 1 to trial because indexing was originally at 0
l['trial'] = l.trial.astype(int) + int(1)




input = filename
outfile=input.strip('.csv')
output = outfile+('_long.csv')
l.to_csv(output, header=True, sep=",",index=0) 