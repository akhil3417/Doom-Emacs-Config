# [[file:configlight.org::*org roam qutebrowser][org roam qutebrowser:2]]
import os
import sys
import urllib.parse

url   = urllib.parse.quote(sys.argv[1])
title = urllib.parse.quote(sys.argv[2])

command = "emacsclient \"org-protocol://roam-ref?template=r&ref="+url+"&title="+title+"\""
os.system(command)
# org roam qutebrowser:2 ends here
