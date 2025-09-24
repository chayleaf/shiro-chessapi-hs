-- | Bindings for Shiro's [chess bot tournament](https://github.com/shiro-nya/2025-chess-bot-tournament)
module Chess
  ( Bitboard,
    Board,
    Dir (..),
    Piece (..),
    PlayerColor (..),
    State (..),
    Move (..),
    moveDefault,

    -- * IO actions

    -- | For info on how to use IO actions, see [Haskell wiki](https://wiki.haskell.org/index.php?title=Introduction_to_Haskell_IO/Introduction_to_IO_actions).
    getBoard,
    opponentMove,
    submitMove,
    getTimeMillis,
    getOpponentTimeMillis,
    getElapsedTimeMillis,

    -- * Board functions
    legalMoves,
    nextTurnColor,
    isWhiteTurn,
    isBlackTurn,
    skipTurn,
    gameState,
    inCheck,
    inCheckmate,
    inDraw,
    canCastleKingside,
    canCastleQueenside,
    zobristKey,
    pushMove,
    popMove,
    fullMoves,
    halfMoves,
    bitboard,
    pieceFromIndex,
    pieceFromBitboard,
    colorFromIndex,
    colorFromBitboard,

    -- * Bitboard functions
    indexFromBitboard,
    bitboardFromIndex,
    showBitboard,
    bbSlide,
    bbFlood,
    bbBlocker,

    -- ** Bitboard function specializations

    -- | Specializations for individual directions, matching the C API.
    bbSlideN,
    bbSlideS,
    bbSlideE,
    bbSlideW,
    bbSlideNE,
    bbSlideNW,
    bbSlideSE,
    bbSlideSW,
    bbFloodN,
    bbFloodS,
    bbFloodE,
    bbFloodW,
    bbFloodNE,
    bbFloodNW,
    bbFloodSE,
    bbFloodSW,
    bbBlockerN,
    bbBlockerS,
    bbBlockerE,
    bbBlockerW,
    bbBlockerNE,
    bbBlockerNW,
    bbBlockerSE,
    bbBlockerSW,
  )
where

import Chess.Bindings
import Data.Bits (bit, countLeadingZeros, finiteBitSize, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Word (Word64)
import Foreign (ForeignPtr, Ptr, free, fromBool, malloc, newForeignPtr, peek, peekArray, toBool, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

-- | A bitboard - an integer where each of the 64 bits represents a square of the board.
--   The bits usually encode the presence of a particular piece.
--   The least significant bit corresponds to a1, the 8th least significant bit corresponds to a8,
--   the most significant bit corresponds to h8.
type Bitboard = Word64

-- | A game of chess in a particular state.
newtype Board = Board (ForeignPtr C'Board)

-- | Directions, used for bitboard manipulation.
data Dir
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  deriving (Eq, Show, Enum, Bounded)

-- | Chess piece types
data Piece
  = Pawn
  | Bishop
  | Knight
  | Rook
  | Queen
  | King
  deriving (Eq, Show, Enum, Bounded, Ord)

pieceFromC :: Int -> Maybe Piece
pieceFromC 0 = Nothing
pieceFromC x = Just $ toEnum $ x - 1

pieceToC :: Maybe Piece -> Int
pieceToC = maybe 0 ((+ 1) . fromEnum)

-- | Player colors
data PlayerColor
  = White
  | Black
  deriving (Eq, Show, Enum, Bounded, Ord)

colorFromC :: Int -> Maybe PlayerColor
colorFromC (-1) = Nothing
colorFromC x = Just $ toEnum x

colorToC :: PlayerColor -> Int
colorToC = fromEnum

-- | Game state.
data State
  = -- | The game is ongoing.
    Normal
  | -- | The game has ended in a stalemate.
    Stalemate
  | -- | The game has ended in a checkmate.
    Checkmate
  deriving (Eq, Show, Enum, Bounded, Ord)

stateFromC :: Int -> State
stateFromC (-1) = Checkmate
stateFromC 0 = Normal
stateFromC 1 = Stalemate
stateFromC _ = error "invalid game state received from C code"

-- | A chess move, specifying the positions but not the piece to be moved.
data Move = Move
  { -- | The move's start position (a bitboard with a single set bit).
    from :: Bitboard,
    -- | The move's end position (a bitboard with a single set bit).
    to :: Bitboard,
    -- | If this move causes a pawn promotion, specifies the piece to promote it to.
    promotion :: Maybe Piece,
    -- | Whether this move is a capture.
    capture :: Bool,
    -- | Whether this move is a castle.
    castle :: Bool
  }
  deriving (Eq, Show)

-- | A default 'Move' with all of the fields unset.
moveDefault :: Move
moveDefault = Move 0 0 Nothing False False

moveFromC :: C'Move -> Move
moveFromC move =
  Move
    { from = c'Move'from move,
      to = c'Move'to move,
      promotion = pieceFromC $ fromIntegral $ c'Move'promotion move,
      capture = toBool $ c'Move'capture move,
      castle = toBool $ c'Move'castle move
    }

-- Perform unsafe IO on a Board. To ensure safety, the Haskell API must clone boards before mutation.
unsafeBoard :: (Ptr C'Board -> IO a) -> Board -> a
unsafeBoard action (Board board) = unsafePerformIO $ withForeignPtr board action

-- | Get the game's current board. The current board changes after every move.
getBoard :: IO Board
getBoard = Board <$> (c'chess_get_board >>= newForeignPtr p'chess_free_board)

-- | Get the opponent's latest move. Will equal to 'moveDefault' if this is the first move.
opponentMove :: IO Move
opponentMove = moveFromC <$> (c'chess_get_opponent_move_ptr >>= peek)

-- | Submit a move that you are going to play.
submitMove :: Move -> IO ()
submitMove move = do
  c'chess_push_unrolled
    (fromIntegral $ from move)
    (fromIntegral $ to move)
    (fromIntegral $ pieceToC $ promotion move)
    (fromBool $ capture move)
    (fromBool $ castle move)
  c'chess_done

-- | Get the time that was remaining at the start of the turn in milliseconds.
getTimeMillis :: IO Int
getTimeMillis = fromIntegral <$> c'chess_get_time_millis

-- | Get opponent's remaining time in milliseconds.
getOpponentTimeMillis :: IO Int
getOpponentTimeMillis = fromIntegral <$> c'chess_get_time_millis

-- | Get time elapsed since the start of the turn in milliseconds.
getElapsedTimeMillis :: IO Int
getElapsedTimeMillis = fromIntegral <$> c'chess_get_time_millis

-- | List legal moves for a board. Note that it may be either your or your opponent's moves depending on
-- what 'nextTurnColor' returns.
legalMoves :: Board -> [Move]
legalMoves = unsafeBoard $ \board -> do
  n <- malloc
  moves <- c'chess_get_legal_moves board n
  num <- peek n
  free n
  arr <- newForeignPtr p'chess_free_moves_array moves
  withForeignPtr arr $ fmap (map moveFromC) . peekArray (fromIntegral num)

-- | Returns the player whose turn it currently is.
nextTurnColor :: Board -> PlayerColor
nextTurnColor = unsafeBoard (fmap (toEnum . fromIntegral) . c'chess_is_black_turn)

-- | Whether it's white's turn.
isWhiteTurn :: Board -> Bool
isWhiteTurn = unsafeBoard (fmap toBool . c'chess_is_white_turn)

-- | Whether it's black's turn.
isBlackTurn :: Board -> Bool
isBlackTurn = unsafeBoard (fmap toBool . c'chess_is_black_turn)

-- | Skips a player's turn, returning an equivalent board where it's the opposite player's turn.
skipTurn :: Board -> Board
skipTurn = unsafeBoard $ \board -> do
  b <- c'chess_clone_board board
  c'chess_skip_turn b
  Board <$> newForeignPtr p'chess_free_board b

-- | Get a game's current state (whether it's a checkmate, stalemate, or neither).
gameState :: Board -> State
gameState = unsafeBoard $ fmap (stateFromC . fromIntegral) . c'chess_get_game_state

-- | Whether the current player is in check.
inCheck :: Board -> Bool
inCheck = unsafeBoard $ fmap toBool . c'chess_in_check

-- | Whether the current player is in checkmate.
inCheckmate :: Board -> Bool
inCheckmate = unsafeBoard $ fmap toBool . c'chess_in_checkmate

-- | Whether the current player is in a draw.
inDraw :: Board -> Bool
inDraw = unsafeBoard $ fmap toBool . c'chess_in_draw

-- | Whether a particular player is allowed to castle kingside at some point in the game.
canCastleKingside :: PlayerColor -> Board -> Bool
canCastleKingside side = unsafeBoard $ fmap toBool . flip c'chess_can_kingside_castle (fromIntegral $ colorToC side)

-- | Whether a particular player is allowed to castle queenside at some point in the game.
canCastleQueenside :: PlayerColor -> Board -> Bool
canCastleQueenside side = unsafeBoard $ fmap toBool . flip c'chess_can_kingside_castle (fromIntegral $ colorToC side)

-- | Get a board's Zobrist hash.
zobristKey :: Board -> Int
zobristKey = unsafeBoard $ fmap fromIntegral . c'chess_zobrist_key

-- | Make a move on a board, returning a new board with that move made.
pushMove :: Move -> Board -> Board
pushMove move = unsafeBoard $ \board -> do
  b <- c'chess_clone_board board
  c'chess_make_move_unrolled
    b
    (fromIntegral $ from move)
    (fromIntegral $ to move)
    (fromIntegral $ pieceToC $ promotion move)
    (fromBool $ capture move)
    (fromBool $ castle move)
  Board <$> newForeignPtr p'chess_free_board b

-- | Undo a move on a board, returning a board without that move.
popMove :: Board -> Board
popMove = unsafeBoard $ \board -> do
  b <- c'chess_clone_board board
  c'chess_undo_move b
  Board <$> newForeignPtr p'chess_free_board b

-- | Read the full move counter (starts at 1, increments each time black moves)
fullMoves :: Board -> Int
fullMoves = unsafeBoard $ fmap fromIntegral . c'chess_get_full_moves

-- | Read the half move counter (starts at 0, increments after every move, resets to 0 after captures and pawn moves).
-- Used for the 50-move draw rule.
halfMoves :: Board -> Int
halfMoves = unsafeBoard $ fmap fromIntegral . c'chess_get_half_moves

-- | Get a bitboard for a particular player and piece. The bitboard's bits will indicate the presence
-- of that particular piece of that particular color on each square of the board.
bitboard :: PlayerColor -> Piece -> Board -> Bitboard
bitboard color piece = unsafeBoard $ \b ->
  fromIntegral
    <$> c'chess_get_bitboard
      b
      (fromIntegral $ colorToC color)
      (fromIntegral $ pieceToC $ Just piece)

-- | Get the piece located at a particular index.
pieceFromIndex :: Int -> Board -> Maybe Piece
pieceFromIndex i = unsafeBoard $ fmap (pieceFromC . fromIntegral) . flip c'chess_get_piece_from_index (fromIntegral i)

-- | Get the piece located at the position set in the bitboard.
pieceFromBitboard :: Bitboard -> Board -> Maybe Piece
pieceFromBitboard b = unsafeBoard $ fmap (pieceFromC . fromIntegral) . flip c'chess_get_piece_from_bitboard b

-- | Get the player color for the piece located at a particular index.
colorFromIndex :: Int -> Board -> Maybe PlayerColor
colorFromIndex i = unsafeBoard $ fmap (colorFromC . fromIntegral) . flip c'chess_get_color_from_index (fromIntegral i)

-- | Get the player color for the piece located at the position set in the bitboard.
colorFromBitboard :: Bitboard -> Board -> Maybe PlayerColor
colorFromBitboard b = unsafeBoard $ fmap (colorFromC . fromIntegral) . flip c'chess_get_color_from_bitboard b

-- | Convert a bitboard with a single set bit to a cell index.
indexFromBitboard :: Bitboard -> Int
indexFromBitboard x = finiteBitSize x - countLeadingZeros x - 1

-- | Convert a cell index to a bitboard with a single set bit.
bitboardFromIndex :: Int -> Bitboard
bitboardFromIndex = bit

-- | Get a printable string for a bitboard (a grid with 8 columns and 8 lines)
showBitboard :: Bitboard -> String
showBitboard bb =
  addNewlines $ map (\x -> if testBit bb x then 'X' else '-') [0 .. 63]
  where
    addNewlines :: [Char] -> [Char]
    addNewlines (a : b : c : d : e : f : g : h : rest) = a : b : c : d : e : f : g : h : '\n' : addNewlines rest
    addNewlines x = x

-- | Move all elements of the bitboard in a particular direction.
bbSlide :: Dir -> Bitboard -> Bitboard
bbSlide N = (`shiftL` 8)
bbSlide S = (`shiftR` 8)
bbSlide E = (.&. 0xfefefefefefefefe) . (`shiftL` 1)
bbSlide W = (.&. 0x7f7f7f7f7f7f7f7f) . (`shiftR` 1)
bbSlide NE = bbSlide E . bbSlide N
bbSlide SE = bbSlide E . bbSlide S
bbSlide NW = bbSlide W . bbSlide N
bbSlide SW = bbSlide W . bbSlide S

-- | Travel in a particular direction, setting all bits until encountering a nonempty space, returning all marked spaces.
bbFlood ::
  -- | Whether to also include the final nonempty space.
  Bool ->
  -- | Direction to move in.
  Dir ->
  -- | A bitboard representing a set of empty spaces on the board.
  Bitboard ->
  -- | The bitboard to shift (with a single set bit).
  Bitboard ->
  -- | The board with movement squares marked. The original square will not be marked.
  Bitboard
bbFlood captures dir empty board =
  (if captures then bbSlide dir else (.&. empty)) $
    foldr (\_ gen -> gen .|. (bbSlide dir gen .&. empty)) board [0 :: Int .. 6]

-- | Travel in a particular direction until encountering a nonempty space.
bbBlocker ::
  -- | Direction to move in.
  Dir ->
  -- | A bitboard representing a set of empty spaces on the board.
  Bitboard ->
  -- | A bitboard to shift (with a single set bit).
  Bitboard ->
  -- | The board after a shift encountered a nonempty space.
  Bitboard
bbBlocker dir empty board =
  foldr
    ( \_ gen ->
        if gen .&. empty == 0
          then gen
          else bbSlide dir gen
    )
    (bbSlide dir board)
    [0 :: Int .. 5]

bbSlideN :: Bitboard -> Bitboard
bbSlideN = bbSlide N

bbSlideS :: Bitboard -> Bitboard
bbSlideS = bbSlide S

bbSlideE :: Bitboard -> Bitboard
bbSlideE = bbSlide E

bbSlideW :: Bitboard -> Bitboard
bbSlideW = bbSlide W

bbSlideNE :: Bitboard -> Bitboard
bbSlideNE = bbSlide NE

bbSlideSE :: Bitboard -> Bitboard
bbSlideSE = bbSlide SE

bbSlideNW :: Bitboard -> Bitboard
bbSlideNW = bbSlide NW

bbSlideSW :: Bitboard -> Bitboard
bbSlideSW = bbSlide SW

bbFloodN :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodN = flip bbFlood N

bbFloodS :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodS = flip bbFlood S

bbFloodE :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodE = flip bbFlood E

bbFloodW :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodW = flip bbFlood W

bbFloodNE :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodNE = flip bbFlood NE

bbFloodSE :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodSE = flip bbFlood SE

bbFloodNW :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodNW = flip bbFlood NW

bbFloodSW :: Bool -> Bitboard -> Bitboard -> Bitboard
bbFloodSW = flip bbFlood SW

bbBlockerN :: Bitboard -> Bitboard -> Bitboard
bbBlockerN = bbBlocker N

bbBlockerS :: Bitboard -> Bitboard -> Bitboard
bbBlockerS = bbBlocker S

bbBlockerE :: Bitboard -> Bitboard -> Bitboard
bbBlockerE = bbBlocker E

bbBlockerW :: Bitboard -> Bitboard -> Bitboard
bbBlockerW = bbBlocker W

bbBlockerNE :: Bitboard -> Bitboard -> Bitboard
bbBlockerNE = bbBlocker NE

bbBlockerSE :: Bitboard -> Bitboard -> Bitboard
bbBlockerSE = bbBlocker SE

bbBlockerNW :: Bitboard -> Bitboard -> Bitboard
bbBlockerNW = bbBlocker NW

bbBlockerSW :: Bitboard -> Bitboard -> Bitboard
bbBlockerSW = bbBlocker SW
