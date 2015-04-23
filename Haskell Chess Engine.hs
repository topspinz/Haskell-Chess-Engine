
//Haskell ChessBoard Engine
module ChessBoard where
    import Data.Maybe
    data PieceName = King | Queen | Rook | Bishop | Knight | Pawn
    instance Show PieceName where
             show King = "K "
             show Queen = "Q "
             show Rook = "R "
             show Bishop = "B "
             show Knight = "Kn"
             show Pawn = "P "
    data Piece = White PieceName | Black PieceName
    instance Show Piece where
             show (White p) = 'W':show p
             show (Black p) = 'B':show p

    data Direction = Limit Int Direction |
                    N | NNE | NE | ENE | E | ESE | SE | SSE | S | SSW | SW | WSW | W | WNW | NW | NNW
                         deriving Show
    instance Read Direction where
         readsPrec _ s = [(Limit i d,u) | (Just d,t) <- map f $ lex s,
                                          (u,v) <- lex t,
                                          (i,"") <- reads u]
                         ++
                         [(d,t) | (Just d,t) <- map f $ lex s] where
                            f ("N",x) = (Just N,x)
                            f ("NNE",x) = (Just NNE,x)
                            f ("NE",x) = (Just NE,x)
                            f ("ENE",x) = (Just ENE,x)
                            f ("E",x) = (Just E,x)
                            f ("ESE",x) = (Just ESE,x)
                            f ("SE",x) = (Just SE,x)
                            f ("SSE",x) = (Just SSE,x)
                            f ("NNW",x) = (Just NNW,x)
                            f ("NW",x) = (Just NW,x)
                            f ("WNW",x) = (Just WNW,x)
                            f ("W",x) = (Just W,x)
                            f ("WSW",x) = (Just WSW,x)
                            f ("SW",x) = (Just SW,x)
                            f ("SSW",x) = (Just SSW,x)
                            f ("S",x) = (Just S,x)
                            f _ = (Just S,[])
    moveRank n = f where
          f N = n
          f NNE | n==1 = 2
          f NE = n
          f ENE | n==1 = 1
          f ESE | n==1 = -1
          f SE = -n
          f SSE | n==1 = -2
          f NNW | n==1 = 2
          f NW = n
          f WNW | n==1 = 1
          f WSW | n==1 = -1
          f SW = -n
          f SSW | n==1 = -2
          f S = -n
          f (Limit m mv) | n<=m = moveRank n mv
          f _ = 0
    moveFile n = f where
          f NNE | n==1 = 1
          f NE = n
          f ENE | n==1 = 2
          f E = n
          f ESE | n==1 = -2
          f SE = -n
          f SSE | n==1 = -1
          f NNW | n==1 = 1
          f NW = n
          f WNW | n==1 = 2
          f W = -n
          f WSW | n==1 = -2
          f SW = -n
          f SSW | n==1 = -1
          f (Limit m mv) | n<=m = moveFile n mv
          f _ = 0
    
    data MoveResult = Moves | Captures deriving Show
    maybeAnd :: Maybe a -> Maybe a -> Maybe a
    maybeAnd _ Nothing = Nothing
    maybeAnd x _ = x

    data Location = Location Char Int
    instance Show Location where
             show (Location f r) = f:show r
    instance Read Location where
         readsPrec _ s = [(Location f (read rn),t) | (f:rn,t) <- lex s,
                                          elem f "abcdefgh"]
    newLocation (Location f r) df dr | r+dr>=1 && r+dr<=8 && fromEnum f+df>=97 && fromEnum f+df<=104 = Just(Location (toEnum $ fromEnum f+df) (r+dr))
    newLocation _ _ _ = Nothing

    data Board a = Board [[Maybe a]] [a] [a]
    instance Show a => Show (Board a) where
             show (Board b wt bt) = ((endLine ++) $ foldl (++) "" $ map (\(n,r) -> show n ++ showRank r) $ zip [8,7..1] $ reverse b) ++ fileLine ++ showTakes "White" wt  ++ showTakes "Black" bt where
                  endLine = ' ':(foldl (++) "" $ map (\x -> "+---") [1..8])++"+\n"
                  fileLine = ' ':(foldl (++) "" $ map (\x -> "  "++x:" ") "abcdefgh")++"\n"
                  showRank [] = "|\n"++endLine
                  showRank (Nothing:t) = "|   " ++ showRank t
                  showRank (Just h:t) = '|':(show h ++ showRank t)
                  showTakes n [] = ""
                  showTakes n l = n ++ " took " ++ (showList l "\n")
    elementAt :: Board a -> Location -> Maybe a
    elementAt (Board b _ _) (Location f r) = head $ drop (fromEnum f - 97) $ (head $ drop (r-1) b)
    replace :: Location -> Maybe a -> Board a -> Board a
    replace (Location f r) p (Board b wt bt) = Board (replaceAt (r-1) (\r -> replaceAt (fromEnum f-97) (\_ -> p) r) b) wt bt
    replaceAt :: Int -> (a -> a) -> [a] -> [a]
    replaceAt 0 f (h:t) = (f h):t
    replaceAt n f (h:t) = h:(replaceAt (n-1) f t)

    class Rules a where
        movable :: Int -> Int -> a -> Maybe a -> Maybe MoveResult
        isWhite :: a -> Bool

    instance Rules Piece where
        isWhite (White _) = True
        isWhite _         = False
        movable _ _ (White _) (Just (White _)) = Nothing
        movable _ _ (Black _) (Just (Black _)) = Nothing
        movable df dr p1 p2 = maybeAnd (Just $ if isNothing p2 then Moves else Captures) $
                movable df dr (fromPiece p1) Nothing where
                        fromPiece (White x) = x
                        fromPiece (Black x) = x

    instance Rules PieceName where
        isWhite _         = False
        movable df dr piece _ = if f piece then Just Moves else Nothing where
                adf = abs df
                adr = abs dr
                f King = adf<=1 && adr <=1
                f Queen = adf==adr || adf==0 || adr ==0
                f Rook = adf==0 || adr ==0
                f Bishop = adf==adr
                f Knight = adf==2 && adr==1 || adf==1 && adr==2
                f Pawn = adr==1 && adf<=1

    move :: Rules a => Location -> Direction -> Board a -> Board a
    move l d b = f movelist where
         movelist = [(move,piece,newL,newPiece) |
                       Just piece <- [elementAt b l],
                       df <- [1..8],
                       mf <- [moveFile df d],
                       dr <- [1..8],
                       mr <- [moveRank dr d],
                       mr/=0 || mf/=0,
                       Just newL <- [newLocation l mf mr],
                       newPiece <- [elementAt b newL],
                       Just move <- [movable df dr piece newPiece]
                     ]
         f [] = b
         f ((Captures,p,newL,Just newPiece):_) =
           let (Board bd wt bt) = replace newL (Just p) (replace l Nothing b) in
                      if isWhite p
                           then Board bd (newPiece:wt) bt
                           else Board bd wt (newPiece:bt)
         f [(_,p,newL,_)] = replace newL (Just p) (replace l Nothing b)
         f (_:t) = f t

    textMove s = move l d where
                      [(l,r)] = reads s
                      [(d,"")] = reads r

    textMoves l b = foldl (\b -> \s -> textMove s b) b l

    initialBoard = Board (map rank [1 .. 8]) [] [] where
                 initRank = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
                 rank 8 = map Just $ map Black initRank
                 rank 7 = map (\x -> Just $ Black $ Pawn) [1..8]
                 rank 2 = map (\x -> Just $ White $ Pawn) [1..8]
                 rank 1 = map Just $ map White initRank
                 rank _ = map (\x -> Nothing) [1..8]