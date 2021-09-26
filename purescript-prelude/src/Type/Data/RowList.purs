module Type.Data.RowList where

import Prim.RowList (RowList)

-- | A proxy to carry information about a rowlist.
-- | **Deprecated as of v0.14.0 PureScript release**: use `Type.Proxy` instead.
data RLProxy :: RowList Type -> Type
data RLProxy rowlist = RLProxy
