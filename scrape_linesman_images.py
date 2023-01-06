from bs4 import BeautifulSoup
import requests
import pandas as pd

speed = pd.read_csv('speed_coeff.csv')
power = pd.read_csv('power_coeff.csv')

me = {'User-agent':'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.2 Safari/605.1.15'}

url = 'https://www.pro-football-reference.com/search/'

tackles = set(speed['tackle_name']).intersection(set(power['tackle_name']))

urls = []
for i in tackles:
    sep = i.split()
    r = requests.get(url+'search.fcgi?search='+sep[0]+'+'+sep[1], headers={'User-agent':'Twitterbot'})
    soup = BeautifulSoup(r.content, 'html.parser')
    images = soup.find_all('img')
    urls.append(str(images[1]).split("src=")[1][1:-3])



first = 'Tristan Wirfs'
first = 'Alejandro Villanueva'
sep = first.split()


r = requests.get(url+'search.fcgi?search='+sep[0]+'+'+sep[1], headers={'User-agent':'Twitterbot'})
soup = BeautifulSoup(r.content, 'html.parser')
images = soup.find_all('img')
str(images[1]).split("src=")[1][1:-3]


