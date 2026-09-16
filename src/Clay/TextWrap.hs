{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

-- | The CSS Text level 4 wrapping properties: @text-wrap@ and the two
-- longhands it stands for, @text-wrap-mode@ and @text-wrap-style@,
-- together with @white-space-collapse@.  The last one is the other
-- half of @white-space@, whose wrapping half is @text-wrap-mode@.
--
-- This module is not re-exported by "Clay": @wrap@ and @nowrap@ would
-- collide with "Clay.Flexbox", and @collapse@ with "Clay.Display".
-- Import it directly.
module Clay.TextWrap
  (
  -- * text-wrap
    TextWrap
  , textWrap

  -- * text-wrap-mode
  , TextWrapMode
  , textWrapMode
  , wrap
  , nowrap

  -- * text-wrap-style
  , TextWrapStyle
  , textWrapStyle
  , balance
  , pretty
  , stable

  -- * white-space-collapse
  , WhiteSpaceCollapse
  , whiteSpaceCollapse
  , collapse
  , preserve
  , preserveBreaks
  , preserveSpaces
  )
where

import Clay.Common
import Clay.Property
import Clay.Stylesheet

-------------------------------------------------------------------------------

-- | @text-wrap@ is a shorthand for @text-wrap-mode@ and
-- @text-wrap-style@.  Its own values are the union of theirs, so this
-- takes whichever of them you mean rather than a type of its own:
--
-- > textWrap wrap
-- > textWrap pretty
class Val a => TextWrap a
instance TextWrap TextWrapMode
instance TextWrap TextWrapStyle

textWrap :: TextWrap a => a -> Css
textWrap = key "text-wrap"

-------------------------------------------------------------------------------

newtype TextWrapMode = TextWrapMode Value
  deriving (Val, Normal, Inherit, Other, Initial, Revert, RevertLayer, Unset)

textWrapMode :: TextWrapMode -> Css
textWrapMode = key "text-wrap-mode"

wrap, nowrap :: TextWrapMode
wrap = TextWrapMode "wrap"
nowrap = TextWrapMode "nowrap"

-------------------------------------------------------------------------------

newtype TextWrapStyle = TextWrapStyle Value
  deriving (Auto, Val, Normal, Inherit, Other, Initial, Revert, RevertLayer, Unset)

textWrapStyle :: TextWrapStyle -> Css
textWrapStyle = key "text-wrap-style"

balance, pretty, stable :: TextWrapStyle
balance = TextWrapStyle "balance"
pretty = TextWrapStyle "pretty"
stable = TextWrapStyle "stable"

-------------------------------------------------------------------------------

newtype WhiteSpaceCollapse = WhiteSpaceCollapse Value
  deriving (Val, Normal, Inherit, Other, Initial, Revert, RevertLayer, Unset)

whiteSpaceCollapse :: WhiteSpaceCollapse -> Css
whiteSpaceCollapse = key "white-space-collapse"

collapse, preserve, preserveBreaks, preserveSpaces :: WhiteSpaceCollapse
collapse = WhiteSpaceCollapse "collapse"
preserve = WhiteSpaceCollapse "preserve"
preserveBreaks = WhiteSpaceCollapse "preserve-breaks"
preserveSpaces = WhiteSpaceCollapse "preserve-spaces"
