module NewType where


newtype Html = Html String

mkHtml :: String -> Html
mkHtml = Html

unHtml :: Html -> String
unHtml x = case x of (Html y) -> y
