# -*- coding: utf-8 -*-
"""
Created on Thu Oct 29 08:24:27 2020

@author: Carlos Heredia Pimienta

Explanation: Guide to do WEB Scraping 
    
"""

import requests as rqst
import bs4 

# EJEMPLO 1 - Coger el título de una página web

# Cogemos la información de la página web
res = rqst.get("https://www.example.com")

# Estructuramos la información
soup = bs4.BeautifulSoup(res.text,"lxml")

print(soup)
web_title = soup.select('title')
print(web_title[0])

# EJEMPLO 2 - Coger todos los elemento de la página web

res = rqst.get("https://es.wikipedia.org/wiki/Stephen_Hawking")
soup = bs4.BeautifulSoup(res.text,"lxml")

for item in soup.select('.toctext'):
    print(item.text)

# EJEMPLO 3 - Coger la imagen de la página web
import re
import pandas as pd

# Buscamos todas las imagenes de la web
images = soup.find_all('img', {'src':re.compile('.jpg')})
df_images = []
for image in images:
    df_images.append(image['src'])

# Las guardamos en un dataframe
df_images = pd.DataFrame(df_images, columns=['src'])

# Descargamos la imagen
image_url = 'https:' + df_images['src'][0]
image_link = rqst.get(image_url)

# Lo escribimos en binario (proviene de esta forma)
dimage = open('Stephen_Hawking.jpg','wb')
dimage.write(image_link.content)
dimage.close()

# EJEMPLO 4 -  Múltiples páginas e items

# Como obtener todos los libros que tengan calificaciones
# de más de 3 estrellas.

url = 'http://books.toscrape.com/catalogue/page-{}.html'

three_stars = []

for page in range(1,51):
    url_page = url.format(page)
    res = rqst.get(url_page)
    
    soup = bs4.BeautifulSoup(res.text,'lxml')
    books = soup.select('.product_pod')
        
    for book in books:
        if len(book.select('.star-rating.Three')) != 0:
            three_stars.append(book.select('a')[1]['title'])
        elif len(book.select('.star-rating.Four')) != 0:
            three_stars.append(book.select('a')[1]['title'])
        elif len(book.select('.star-rating.Five')) != 0:
            three_stars.append(book.select('a')[1]['title'])

three_stars = pd.DataFrame(three_stars, columns= ['Title'])


















