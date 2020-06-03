#!/usr/bin/env python
# coding: utf-8

###Sentiment Analysis

#importing libraries
import tweepy as tp
import pprint
import pandas as pd 

import json
import sys
import string
import time
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
import sys,tweepy,csv,re
from textblob import TextBlob

##cleaning data function
def clean_data(file):
    """takes the file name as input and returns a cleaned dataframe from it"""
    f=open(file,'r')
    stream_data=[]
    for x in f.readlines():
        if len(x)==1 :
            None
        else:
            try:
                print(json.loads(x)['id'])  
                stream_data.append(json.loads(x))
            except KeyError:
                None
    data = [(data_line['id'],data_line['user']['screen_name'],data_line['text'],
                data_line['user']['description']) for data_line in stream_data]
    data_df = pd.DataFrame(data, columns=('id','users','tweet texts','bio')).drop_duplicates(['tweet texts']).set_index('id')
    data_df['bio'].fillna("No Bio", inplace=True)
    return data_df


## Analysis
#creating the file where the results will be stored
csvFile = open('results.csv', 'w+')
csvWriter = csv.writer(csvFile)
csvWriter.writerow(["Strongly Positive","Positive","Weakly Positive","Neutral",
    "Weakly Negative","Negative","Strongly Negative","Company Name"])
csvFile.close()

#creating the sentiment analysis class  
class SentimentAnalysis:

    def __init__(self, tweets, company):
        self.tweets = tweets
        self.tweetText = []
        self.company = company
    def return_data(self):

        # input for term to be searched and how many tweets to search
        NoOfTerms = self.tweets.size
        searchTerm = self.company
        # Open/create a file to append data to
        csvFile = open('results.csv', 'a')

        # Use csv writer
        csvWriter = csv.writer(csvFile)

        # creating some variables to store info
        polarity = 0
        positive = 0
        wpositive = 0
        spositive = 0
        negative = 0
        wnegative = 0
        snegative = 0
        neutral = 0

        # iterating through tweets fetched
        for tweet in self.tweets:
            #Append to temp so that we can store in csv later. I use encode UTF-8
            self.tweetText.append(self.cleanTweet(tweet).encode('utf-8'))
            # print (tweet.text.translate(non_bmp_map))    #print tweet's text
            analysis = TextBlob(tweet)
            # print(analysis.sentiment)  # print tweet's polarity
            polarity += analysis.sentiment.polarity  # adding up polarities to find the average later

            if (analysis.sentiment.polarity == 0):  # adding reaction of how people are reacting to find average later
                neutral += 1
            elif (analysis.sentiment.polarity > 0 and analysis.sentiment.polarity <= 0.3):
                wpositive += 1
            elif (analysis.sentiment.polarity > 0.3 and analysis.sentiment.polarity <= 0.6):
                positive += 1
            elif (analysis.sentiment.polarity > 0.6 and analysis.sentiment.polarity <= 1):
                spositive += 1
            elif (analysis.sentiment.polarity > -0.3 and analysis.sentiment.polarity <= 0):
                wnegative += 1
            elif (analysis.sentiment.polarity > -0.6 and analysis.sentiment.polarity <= -0.3):
                negative += 1
            elif (analysis.sentiment.polarity > -1 and analysis.sentiment.polarity <= -0.6):
                snegative += 1

        # finding average of how people are reacting
        positive = self.percentage(positive, NoOfTerms)
        wpositive = self.percentage(wpositive, NoOfTerms)
        spositive = self.percentage(spositive, NoOfTerms)
        negative = self.percentage(negative, NoOfTerms)
        wnegative = self.percentage(wnegative, NoOfTerms)
        snegative = self.percentage(snegative, NoOfTerms)
        neutral = self.percentage(neutral, NoOfTerms)

        # finding average reaction
        polarity = polarity / NoOfTerms

        # printing out data
        print("How people are reacting on " + searchTerm + " by analyzing " + str(NoOfTerms) + " tweets.")
        print()
        print("General Report: ")

        if (polarity == 0):
            print("Neutral")
        elif (polarity > 0 and polarity <= 0.3):
            print("Weakly Positive")
        elif (polarity > 0.3 and polarity <= 0.6):
            print("Positive")
        elif (polarity > 0.6 and polarity <= 1):
            print("Strongly Positive")
        elif (polarity > -0.3 and polarity <= 0):
            print("Weakly Negative")
        elif (polarity > -0.6 and polarity <= -0.3):
            print("Negative")
        elif (polarity > -1 and polarity <= -0.6):
            print("Strongly Negative")

        print("Detailed Report: ")
        print(str(positive) + "% people thought it was positive")
        print(str(wpositive) + "% people thought it was weakly positive")
        print(str(spositive) + "% people thought it was strongly positive")
        print(str(negative) + "% people thought it was negative")
        print(str(wnegative) + "% people thought it was weakly negative")
        print(str(snegative) + "% people thought it was strongly negative")
        print(str(neutral) + "% people thought it was neutral")

        csvWriter.writerow([spositive, positive, wpositive, neutral, wnegative, negative, snegative, searchTerm])
        csvFile.close()
    def cleanTweet(self, tweet):
        mess = [i for i in tweet if i not in string.punctuation]
        mess = "".join(mess)
        a = [i for i in mess.split(" ") if (i.lower() not in stopwords.words('english') and i.lower()!='https' and i.lower()!='rt')]        
        str1 = " "   
        return (str1.join(a)) 


    # function to calculate percentage
    def percentage(self, part, whole):
        temp = 100 * float(part) / float(whole)
        return format(temp, '.2f')

if __name__== "__main__":
    apple_df = clean_data('../datasets/stream__Apple.jsonl')
    google_df = clean_data('../datasets/stream__Google.jsonl')
    microsoft_df = clean_data('../datasets/stream__Microsoft.jsonl')
    SentimentAnalysis(apple_df["tweet texts"],"Apple").return_data()
    SentimentAnalysis(microsoft_df["tweet texts"],"Microsoft").return_data()
    SentimentAnalysis(google_df["tweet texts"],"Google").return_data()

