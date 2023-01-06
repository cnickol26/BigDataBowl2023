from bs4 import BeautifulSoup
import requests

url = 'https://www.pro-football-reference.com/search/'

r = requests.get(url+'search.fcgi?search=andrew+whitworth')
soup = BeautifulSoup(r.content, 'html.parser')
images = soup.find_all('img')

images
