-- Defines string literals for module split boundaries.

module SplitBounds where

splitOpen   = "{-- #SPLIT#"

splitClose  = "#SPLIT# --}"

splitBegin  = "-- Split begin"

splitEnd    = "-- Split end"

-- A generic version of `words': breaks list by any predicate.

parts pred s = case dropWhile pred s of
                 [] -> []
                 s' -> w : parts pred s''
                             where (w, s'') = break pred s'

