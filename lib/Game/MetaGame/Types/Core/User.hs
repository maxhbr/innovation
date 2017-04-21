module Game.MetaGame.Types.Core.User
       where

--------------------------------------------------------------------------------
-- * Users and user-related stuff

-- | should remove:
--   - newlines,
--   - whitespace at all,
-- and keep
--   - only alphanumeric + - _ , ...
sanitizeUserId :: String -> String
sanitizeUserId = id -- TODO

data UserId
  = U String -- ^ regular user, determined by its username
  | Admin -- ^ administrative user (is not allowed to play)
  | Guest -- ^ unauthorized user
  deriving (Show,Eq,Read)
mkUserId :: String -> UserId
mkUserId = U . sanitizeUserId

-- | Guest < U * < Admin
instance Ord UserId where
  compare Admin  Admin  = EQ
  compare Admin  (U _)  = GT
  compare (U _)  Admin  = LT
  compare (U u1) (U u2) = compare u1 u2
  compare Guest  Guest  = EQ
  compare Guest  _      = LT
  compare _      Guest  = GT

isAdmin :: UserId -> Bool
isAdmin Admin = True
isAdmin _     = False

getCommonUID :: UserId -> UserId -> UserId
getCommonUID Guest uid = uid
getCommonUID uid Guest = uid
getCommonUID Admin _   = Admin
getCommonUID _   Admin = Admin
getCommonUID uid1 uid2 | uid1 == uid2 = uid1
                       | otherwise    = Admin

isAuthorizationLevel :: UserId -- ^ the asking user
                     -> UserId -- ^ the user who is to be matched
                     -> Bool
isAuthorizationLevel asker level = (asker `getCommonUID` level) == asker
