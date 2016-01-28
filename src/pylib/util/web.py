# set of general utilities to interact with the Web

# @author: rm3086 (at) columbia (dot) edu

import urllib2, json
from log import strd_logger

# logger
log = strd_logger ('web')

# get the html source associated to a URL
def download_web_data (url):
    try:
        req = urllib2.Request (url, headers={'User-Agent':' Mozilla/5.0'}) 
        con = urllib2.urlopen(req)
        html = con.read()
        con.close()
        return html
    except Exception as e:
        log.error ('%s: %s' % (e, url))
        return None

# get json data
def download_json_data (url):
    try:
        con = urllib2.urlopen(url)
        data = con.read()
        con.close()
        return json.loads(data)
    except Exception as e:
        log.error ('%s: %s' % (e, url))
        return None


# clean the html source
def clean_html (html):
	if html is None:
		return None
	return ' '.join(html.replace('\n','').replace('\t','').split()).strip()

