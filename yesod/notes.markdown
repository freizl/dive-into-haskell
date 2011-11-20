= Yesod =

  * update a existing data via
{{{
oauth = OAuth { ... }
let oauth' = oauth { oauthCallback = ... }
}}}

  * oauth url in hamlet
{{{
-- import Yesod.Auth.OAuth in Handler
@{AuthR $ oauthUrl "weibo"}
}}}

