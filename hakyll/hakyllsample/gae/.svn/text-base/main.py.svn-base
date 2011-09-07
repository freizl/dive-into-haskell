import os
import wsgiref.handlers

from google.appengine.ext.webapp import template
from google.appengine.ext import webapp

"""
TODO: how to make a general static handler
"""

class MainPage(webapp.RequestHandler):
  def get(self):
    self.redirect("/index.html")

def main():
  application = webapp.WSGIApplication([('/', MainPage) ], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == "__main__":
  main()

