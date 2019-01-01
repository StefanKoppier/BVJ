module Phase where

data PhaseError = UnspecifiedPhaseError

type Phase a b = a -> IO (Either PhaseError b)

instance Functor (Phase a) where
    fmap = _