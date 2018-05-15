from bs4 import BeautifulSoup
from collections import Counter
 
try:
    from urllib.parse import urlparse
except:
    from urlparse import urlparse
 
def title_string(soup):
    return soup.title.string
 
def title_length(soup):
    return len(soup.title.string)
 
def tag_count(soup):
    return len(soup.findAll()) 
 
def link_count(soup):
    return len(soup.find_all('a'))
 
def images_count(soup):
    return len(soup.find_all('img'))
 
def images_alt_percentage(soup):
    #count number of images with non-empty  alt attribute
    images = soup.find_all('img')
    total = len(images)
    c = 0
    for image in images:
        if 'alt' in image.attrs and image['alt'] != '': c+=1
    return c/total
 
def links_domain_percentage(soup):
    #find all domains in all links
    #find the most frequent one 
    #calculate proportion of the most frequent domain
    domains = []
    for link in soup.find_all('a'):    
        domains.append(urlparse(link.get('href')).netloc)
    c = Counter(domains)
    return c.most_common(1)[0][1]/sum(c.values())
 
def link_images_count(soup):
    #count number of images that appears to be links
    c = 0
    for link in soup.find_all('a'):
        if 'img' in link.attrs: c +=1
    return c
