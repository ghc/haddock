{-# LANGUAGE OverloadedRecordFields #-}

-- | Doc for: module OverloadedRecordFields
--
-- Including references to 'foo', 'baz', 'T' and 'MkT'
module OverloadedRecordFields
    ( T(..)
    , U(MkU, foo, (*:*), baz)
    , foo'
    ) where

-- | Doc for: data T
data T a = -- | Doc for: MkT
           MkT { foo :: a -- ^ Doc for: T.foo
               }

-- | Doc for: data U
data U = -- | Doc for: MkU
         MkU { foo   :: Bool -- ^ Doc for: U.foo
             , (*:*) :: Bool -- ^ Doc for: U.(*:*)
             , baz   :: Bool -- ^ Doc for: U.baz
             }

-- | Doc for: foo'
foo' :: r { foo :: t, bar :: U } => r -> t
foo' = foo
