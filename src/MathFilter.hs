module MathFilter
  ( mathFilter,
  )
where

import Data.Text (Text)
import Text.Pandoc.JSON (Inline (Math))

-- | Apply a function to math expressions in a Pandoc AST. Leave non-math
-- content unchanged.
mathFilter :: (Text -> Text) -> Inline -> Inline
mathFilter f (Math t e) = Math t (f e)
mathFilter _ x = x
