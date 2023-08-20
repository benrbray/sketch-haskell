-- Orchard & Mycroft 2012, "A Notation for Comonads"
-- https://www.cs.kent.ac.uk/people/staff/dao7/publ/codo-notation-orchard-ifl12.pdf

module Orchard where

-- array with cursor type i, element type a
--data CursorArray i a = CursorArray (Array i a) i