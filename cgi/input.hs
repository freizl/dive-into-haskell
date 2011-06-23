import Network.CGI
import Text.XHtml
 
inputForm = form << [paragraph << ("My name is " +++ textfield "name"),
                     submit "" "Submit"]
 
greet n = paragraph << ("Hello " ++ n ++ "!")
 
page t b = header << thetitle << t +++ body << b
 
cgiMain = do mn <- getInput "name"
             let x = maybe inputForm greet mn
             output . renderHtml $ page "Input example" x
 
main = runCGI $ handleErrors cgiMain

