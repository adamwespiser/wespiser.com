{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/*" $ do
        route idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    match "resume.pdf" $ do
        route idRoute
        compile copyFileCompiler

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match (fromList ["404.html", "info.html"]) $ do
        route idRoute
        let f404Ctx = defaultContext `mappend` constField "title" "Wespiser Blog"
                                     `mappend` constField "header" ""
        compile $ pandocCompiler
             >>= loadAndApplyTemplate "templates/default.html" f404Ctx
             >>= relativizeUrls
             
    match (fromList ["writing.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "writings/wyas/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "writings/wyas/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Wespiser Blog: Archives"  `mappend`
                    constField "header" "Archives"           `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Wespiser Blog: My Technical Writings" `mappend`
                    constField "header" ""                   `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateCompiler

    create ["sitemap.xml"] $ do
        route idRoute 
        compile $ do 
            posts <- recentFirst =<< loadAll "posts/*"
            wyas  <- recentFirst =<< loadAll "writings/wyas/*"
            singlePages <- loadAll $ fromList ["archive.html", "writing.markdown"]

            let pages = posts `mappend` wyas `mappend` singlePages
                siteMapCtx = listField "pages" postCtx (return pages) `mappend`
                             constField "root" root
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" siteMapCtx

    create ["feed.atom"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

    create ["feed.rss"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts




--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    constField "root" root       `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

root :: String
root = "http://www.wespiser.com"


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Wespiser Blog: Feed"
    , feedDescription = "This feed provides articles, hot off the presses, on a variety of technical topics."
    , feedAuthorName  = "Adam Wespiser"
    , feedAuthorEmail = "noemail"
    , feedRoot        = root
    }