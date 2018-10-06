--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Hakyll
import Hakyll.FileStore.Git.Context (gitModificationTimeField)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfiguration $ do
  -- Route to the old Drupal archive site
  match "drupal_archive/webpagedeveloper.me/**" $ do
    route   (gsubRoute "^drupal_archive/webpagedeveloper.me/"
              (const "drupal_archive/"))
    compile copyFileCompiler

  -- Copy over keybase proof
  match "keybase.txt" $ do
    route   idRoute
    compile copyFileCompiler

  -- Copy .htaccess file for redirecting old urls to new
  match ".htaccess" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList [ "about.markdown"
                  , "contact.markdown"
                  , "projects.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
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
            field "posts" (\_ -> postList recentFirst) <>
            constField "title" "Archives"              <>
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
  dateField "date" "%B %e, %Y"     <>
  gitModificationTimeField "modified" "%B %e, %Y - %r" <>
  defaultContext

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts

--------------------------------------------------------------------------------
myConfiguration :: Configuration
myConfiguration = defaultConfiguration
  { ignoreFile = ignoreFile'
  -- Create DATE and copy current site to DATE directory and then set the latest.
  -- Delete all but the last five builds using the cleanUp.bash script.
  , deployCommand = "DATE=$(date +%Y-%m-%d:%H:%M:%S) && rsync -ave 'ssh -p22002' _site/ builds@hs.daveparrish.net:daveparrish/\"$DATE\" && ssh -p22002 builds@hs.daveparrish.net \"cd daveparrish && rm -f latest && ln -sf \\\"$DATE\\\" latest && ./cleanUp.bash\""
  }
  where
    ignoreFile' ".htaccess" = False
    ignoreFile' path        = ignoreFile defaultConfiguration path
