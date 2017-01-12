--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Control.Applicative
import Data.Time.Format (formatTime, parseTimeM )
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfiguration $ do
  -- Route to the old Drupal archive site
  match "drupal_archive/webpagedeveloper.me/**" $ do
    route   (gsubRoute "^drupal_archive/webpagedeveloper.me/"
              (const "drupal_archive/"))
    compile copyFileCompiler

  -- Copy over keybase proof
  match "keybase/*" $ do
    route   (gsubRoute "^keybase/"
              (const ".well-known/"))
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

  match (fromList ["about.markdown", "contact.markdown"]) $ do
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
  dateField "date" "%B %e, %Y - %r"     `mappend`
  dateField "shortdate" "%B %e, %Y"     `mappend`
  formatDateFieldValue "modified" "%B %e, %Y - %r" `mappend`
  defaultContext

formatDateFieldValue :: String    -- ^ Field name
                     -> String    -- ^ Format string
                     -> Context a -- ^ Resulting context
formatDateFieldValue name fmt = Context $ \k _ i ->
  if (k == name)
  then (do value <- getMetadataField (itemIdentifier i) k
           maybe empty (\v -> do
                           let mSDate = parseAndFormat fmt v
                           case mSDate of
                             (Just sDate) -> (return . StringField) sDate
                             Nothing  -> empty
                       ) value
       )
  else empty
  where
    parseAndFormat :: String -> String -> Maybe String
    parseAndFormat fmt' v' = do
      let timeV = (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" v') :: Maybe UTCTime
      case timeV of
        (Just t) -> Just $ formatTime defaultTimeLocale fmt' t
        Nothing   -> Nothing

--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list    <- applyTemplateList itemTpl postCtx posts
  return list

--------------------------------------------------------------------------------
myConfiguration :: Configuration
myConfiguration = defaultConfiguration
  { ignoreFile = ignoreFile' }
  where
    ignoreFile' ".htaccess" = False
    ignoreFile' path        = ignoreFile defaultConfiguration path
