--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfiguration $ do
  -- Route to the old Drupal archive site
  match "drupal_archive/webpagedeveloper.me/**" $ do
    route   (gsubRoute "^drupal_archive/webpagedeveloper.me/"
              (const "drupal_archive/"))
    compile copyFileCompiler

  -- Copy .htaccess file for redirecting old urls to new
  match ".htaccess" $ do
    route idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList recentFirst) `mappend`
            constField "title" "Archives"              `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls


  match "index.html" $ do
    route idRoute
    compile $ do
    let indexCtx = field "posts" $ \_ ->
                        postList $ fmap (take 3) . recentFirst

    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y - %r" `mappend`
  dateField "shortdate" "%B %e, %Y" `mappend`
  defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list    <- applyTemplateList itemTpl postCtx posts
  return list

--------------------------------------------------------------------------------
myConfiguration :: Configuration
myConfiguration = defaultConfiguration {ignoreFile = ignoreFile'}
  where
    ignoreFile' ".htaccess" = False
    ignoreFile' path        = ignoreFile defaultConfiguration path
