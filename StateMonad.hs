module StateMonad where
    
newtype ST st a = S (st -> (a,st))
newtype StateIO st a = SIO (st -> IO (a,st))

appIO :: StateIO st a -> st -> IO (a,st)
appIO (SIO st) x = st x

lift :: IO a -> StateIO st a
lift m = SIO (\s -> do x <- m
                       return (x,s))


app :: ST st a -> st -> (a,st)
app (S f) = f


instance Functor (ST st) where
  -- fmap :: (a -> b) -> ST st a -> ST st b
  fmap g sta = S(\s ->
    let (x,s1) = app sta s
    in (g x , s1))

instance Applicative (ST st) where
  pure x = S(\s -> (x,s))

  -- (<*>) :: (ST st (a -> b)) -> (ST st a)
  --                           -> (ST st b)
  stf <*> sta = S(\s ->
    let (f,s1) = app stf s
        (x,s2) = app sta s1
    in (f x , s2))

instance Monad (ST st) where
  return = pure

  -- (>>=) :: (ST st a) -> (a -> ST st b) -> ST st b
  sta >>= f = S(\s ->
    let (x,s1) = app sta s
        (y,s2) = app (f x) s1
    in (y,s2))


instance Functor (StateIO st) where
    -- fmap :: (a -> b) -> StateIO st a -> StateIO st b
    fmap g sta = SIO (\s -> do (a,st) <- appIO sta s
                               return (g a, st))

instance Applicative (StateIO st) where
    pure x = SIO (\s -> return (x,s))                                

    -- (<*>) :: (StateIO st (a -> b)) -> (StateIO st a) -> (StateIO st b) 
    stf <*> sta = SIO (\s -> do (f, s1) <- appIO stf s
                                (x, s2) <- appIO sta s1
                                return (f x, s2))


instance Monad (StateIO st) where
    return = pure
    
    -- (>>=) :: (StateIO st a) -> (a -> StateIO st b) -> StateIO st b
    sta >>= f = SIO (\s -> do (x,s1) <- appIO sta s
                              (y,s2) <- appIO (f x) s1
                              return (y,s2))



stUpdate :: st -> ST st ()
stUpdate s = S(\_ -> ((),s))

stGet :: ST st st
stGet = S(\s -> (s,s))

stRevise :: (st -> st) -> ST st ()
stRevise f = stGet >>= stUpdate . f


stIOUpdate :: st -> StateIO st ()
stIOUpdate s = SIO(\_ -> return ((),s))

stIOGet :: StateIO st st
stIOGet = SIO (\s -> return (s,s))

stIORevise :: (st -> st) -> StateIO st ()
stIORevise f = stIOGet >>= stIOUpdate . f


