import Network.CGI
import Text.XHtml
 
main = runCGI . output . renderHtml $ body << h1 << "Hello World!"

