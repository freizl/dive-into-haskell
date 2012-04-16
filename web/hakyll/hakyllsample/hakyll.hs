{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Control.Monad(forM_)
import Data.Monoid (mempty, mconcat)
import Data.Char(toUpper)
import Data.List(find)

import Hakyll

main :: IO ()
main = hakyll $ do
    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    -- Static directories
    forM_ ["images/*", "sitemaps/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> arr (setField "poststitle" "All posts")
            >>> applyTemplateCompiler "templates/post.html"
            >>> defaultCompiler
            
    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" addPostList
        >>> compilePostsBody

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    --    match "tags/*" $ route $ setExtension ".html"
    match "tags/*" $ do 
      route $ customRoute chineseTagRoute
      
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Pages
    forM_ pages $ \page -> match page $ do
      route   $ setExtension "html"
      compile $ pageCompiler >>> defaultCompiler
    
    -- Footer
    match "footer.markdown" $ compile pageCompiler
   
    -- Templates
    match "templates/*" $ compile templateCompiler
  where
    pages = ["about.markdown","mathhack.rst", "index.markdown", "code.lhs", "products.markdown", "pandoc.markdown"]
    
    --makeTagList :: String -> [Page String] -> Compiler () (Page String)
    makeTagList tag posts = constA (mempty, posts)
                            >>> addPostList
                            >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
                            >>> compilePostsBody
    
    compilePostsBody = requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
                       >>> applyTemplateCompiler "templates/posts.html"
                       >>> defaultCompiler
    
    defaultCompiler = requireA "footer.markdown" (setFieldA "footer" $ arr pageBody)
                      >>> applyTemplateCompiler "templates/default.html"
                      >>> relativizeUrlsCompiler

    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 80 260

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
--   add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

{-- ======================== Chinese Character hack --}
-- | TODO: glue Route together of (setExtension html)
--         a little nicer pattern for replace?
chineseTagRoute :: Identifier a -> FilePath
chineseTagRoute = (++".html") . replaceAll "[^a-zA-Z/]+" cnToPinyin . toFilePath

cnToPinyin :: String -> String -- whole Chinese characters transform rather than single
cnToPinyin s = case find (\y -> (fst y) == s) cnDict of
  Just v -> (snd v)
  Nothing -> s
  
cnDict :: [(String, String)]
cnDict = [("林凡", "linfan")]
