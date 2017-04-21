################ CPC Class and Month ############################
import pandas as pd
import numpy as np
### Read the data
Data = pd.read_csv("disambig03to05.csv")

Data = pd.DataFrame(Data, columns = ['MatchAsgName', 'CPCClass'])
Data['MainClass'] = 0
df3 = pd.DataFrame()

r = range(len(Data))
################## Patent MainClass #########################
for i in r:
    Data['MainClass'][i] = (Data["CPCClass"][i].split('+')[0]).split(' ')[0]
    print (i)
    if len((Data["CPCClass"][i].split('+'))) > 1:
    	for j in range(1, len((Data["CPCClass"][i].split('+')))):
    		b = ((Data["CPCClass"][i].split('+'))[j]).split(' ')[0]
    		df2 = pd.DataFrame([[Data['MatchAsgName'][i], b]], columns=['MatchAsgName', 'MainClass'])
    		df3 = df3.append(df2)

Data = Data.append(df3)
Data = pd.DataFrame(Data, columns = ['MatchAsgName', 'MainClass'])

dp = pd.pivot_table(Data, index = 'MatchAsgName', columns = 'MainClass', aggfunc = len)

Data.to_csv('disambig03to05Out1.csv', sep=',')

dp.to_csv('disambig03to05Out2.csv', sep=',')
