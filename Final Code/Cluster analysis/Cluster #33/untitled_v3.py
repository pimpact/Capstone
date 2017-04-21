import pandas as pd
import numpy as np
from scipy import stats
import selenium 
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
import os
from selenium.webdriver.chrome.options import Options
import shutil
import datetime


def es(comp,date):
#    before = os.listdir('C:\\Users\\Andy Ding\\Documents\\Final\\')
#    
#     #setting window
#    b = datetime.datetime.strptime(a,'%Y-%m-%d')
#    c = b + datetime.timedelta(days=300)
#    d = b - datetime.timedelta(days=300)
#    window_start =  c.strftime('%Y/%m/%d')
#    window_end = d.strftime('%Y/%m/%d')
#    
#    
#
#     #Setting Download Location to Our Current Directory
#    chromeOptions = webdriver.ChromeOptions()
#    prefs = {"download.default_directory" : "C:\\Users\\Andy Ding\\Documents\\Final\\"}
#    chromeOptions.add_experimental_option("prefs",prefs)
#    
#     #Open Browser
#    browser = webdriver.Chrome(chrome_options=chromeOptions)
#
#     #Reach Website
#    browser.get("http://www.google.com/finance") #website for stockdata
#
#     #waiting for website to load
#    time.sleep(5)
#    
#     #Looks for the search bar to enter company name
#    elemA = browser.find_element_by_name("q")
#    elemA.send_keys(comp) #Will come from Competitors
#    elemA.send_keys(Keys.RETURN)
#
#
#    time.sleep(5)
#    browser.find_element_by_link_text("Historical prices").click()
#    time.sleep(5)
#
#    elemDS = browser.find_element_by_name("startdate")
#    elemDS.clear() #Will come from main patent date -120
#    elemDS.send_keys(window_end)
#    elemDE = browser.find_element_by_name("enddate")
#    elemDE.clear() #Will come from main patent date +120
#    elemDE.send_keys(window_start)
#
#    time.sleep(1)
#    elemDE.send_keys(Keys.RETURN)
#
#
#
#    time.sleep(1)
#    browser.find_element_by_xpath('//*[@id="gf-viewc"]/div/div/div[2]/div[1]/div/div[3]/div/a').click()
#
#
#    time.sleep(3)
#    browser.quit()



#    after = os.listdir('C:\\Users\\Andy Ding\\Documents\\Final\\')
#    change = set(after) - set(before)
#    file_name = change.pop()    
    
    # import S&P500 price data 
    df_sp500 = pd.read_csv('C:\\Users\\Nick\\Desktop\\New folder\\SP500.csv')
    
    # import competitor price data
    df_ibm = pd.read_csv('C:\\Users\\Nick\\Desktop\\New folder\\'+comp+'.csv')#+file_name)
    
    # convert date 
    #df_ibm["Date"] = datetime.datetime.strptime(df_ibm['\ufeffDate'],'%mm-%dd-%YYYY')
    
    df_ibm["Date"] = pd.to_datetime(df_ibm['Date'])
    df_ibm["Date"] = pd.to_datetime(df_ibm["Date"], format = '%m-%d-%Y')    
    
    
    # reorder dataframes by dates
    df_sp500 = df_sp500.sort_values(by = 'Date', ascending = 1).reset_index(drop = True)
    df_ibm = df_ibm.sort_values(by = 'Date', ascending = 1).reset_index(drop = True)
    
    # add new columns for % changes in adjusted closing prices
    df_sp500['AdjChange'] = df_sp500['Adj Close'].pct_change()
    df_ibm['AdjChange'] = df_ibm['Close'].pct_change()
     
    
    # define windows (we will pass issue date later as a variable)
    df_issuedate_index = int(df_ibm.index[df_ibm['Date'] == date].values)
    df_eventwindow = df_ibm.iloc[df_issuedate_index - 3 :df_issuedate_index + 4,:]
    df_estimationwindow = df_ibm.iloc[df_issuedate_index - 123 :df_issuedate_index - 3,:]
    df_posteventwindow = df_ibm.iloc[df_issuedate_index + 4 :df_issuedate_index + 124,:]
    
    df_eventwindow_sp500 = df_sp500.iloc[df_issuedate_index - 3 :df_issuedate_index + 4,:]
    df_estimationwindow_sp500 = df_sp500.iloc[df_issuedate_index - 123 :df_issuedate_index - 3,:]
    df_posteventwindow_sp500 = df_sp500.iloc[df_issuedate_index + 4 :df_issuedate_index + 124,:]
    
    
    # regression for estimation window
    x = pd.concat([df_estimationwindow['AdjChange'],df_posteventwindow['AdjChange']])
    y = pd.concat([df_estimationwindow_sp500['AdjChange'],df_posteventwindow_sp500['AdjChange']])
    
    slope, intercept, r_value, p_value, std_err = stats.linregress(y,x)
    
    
    # calculate expected and abnormal returns 
    expected_returns = slope * df_eventwindow_sp500['AdjChange'] + intercept
    abnormal_returns = df_eventwindow['AdjChange'] - expected_returns 
    cumulative_abnormal_returns = abnormal_returns.cumsum()
    
    return (sum(cumulative_abnormal_returns))

#a = es('CONOCOPHILLIPS COMPANY', '2003-01-07')

df = pd.read_csv('C:\\Users\\Nick\\Desktop\\New folder\\finalfinal.csv')

df['Returns'] = np.vectorize(es)(df['comp'],df['GrantDate'])

df.to_csv('Result1.csv', sep = ',')