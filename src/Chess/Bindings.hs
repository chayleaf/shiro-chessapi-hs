module Chess.Bindings where

import Foreign
import Foreign.C.Types

type C'PlayerColor = CUInt

type C'PieceType = CUInt

type C'GameState = CInt

data C'Board = C'Board

type C'Bitboard = Word64

data C'Move = C'Move
  { c'Move'from :: C'Bitboard,
    c'Move'to :: C'Bitboard,
    c'Move'promotion :: CUChar,
    c'Move'capture :: CBool,
    c'Move'castle :: CBool
  }
  deriving (Eq, Show)

p'Move'from p = plusPtr p 0

p'Move'from :: Ptr C'Move -> Ptr Word64

p'Move'to p = plusPtr p 8

p'Move'to :: Ptr C'Move -> Ptr Word64

p'Move'promotion p = plusPtr p 16

p'Move'promotion :: Ptr C'Move -> Ptr CUChar

p'Move'capture p = plusPtr p 17

p'Move'capture :: Ptr C'Move -> Ptr CBool

p'Move'castle p = plusPtr p 18

p'Move'castle :: Ptr C'Move -> Ptr CBool

instance Storable C'Move where
  sizeOf _ = 24
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    v2 <- peekByteOff p 16
    v3 <- peekByteOff p 17
    v4 <- peekByteOff p 18
    return $ C'Move v0 v1 v2 v3 v4
  poke p (C'Move v0 v1 v2 v3 v4) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    pokeByteOff p 16 v2
    pokeByteOff p 17 v3
    pokeByteOff p 18 v4
    return ()

foreign import ccall unsafe "chess_get_board"
  c'chess_get_board ::
    IO (Ptr C'Board)

foreign import ccall unsafe "chess_clone_board"
  c'chess_clone_board ::
    Ptr C'Board -> IO (Ptr C'Board)

foreign import ccall unsafe "chess_get_legal_moves"
  c'chess_get_legal_moves ::
    Ptr C'Board -> Ptr CInt -> IO (Ptr C'Move)

foreign import ccall unsafe "chess_is_white_turn"
  c'chess_is_white_turn ::
    Ptr C'Board -> IO CBool

foreign import ccall unsafe "chess_is_black_turn"
  c'chess_is_black_turn ::
    Ptr C'Board -> IO CBool

foreign import ccall unsafe "chess_skip_turn"
  c'chess_skip_turn ::
    Ptr C'Board -> IO ()

foreign import ccall unsafe "chess_in_check"
  c'chess_in_check ::
    Ptr C'Board -> IO CBool

foreign import ccall unsafe "chess_in_checkmate"
  c'chess_in_checkmate ::
    Ptr C'Board -> IO CBool

foreign import ccall unsafe "chess_in_draw"
  c'chess_in_draw ::
    Ptr C'Board -> IO CBool

foreign import ccall unsafe "chess_can_kingside_castle"
  c'chess_can_kingside_castle ::
    Ptr C'Board -> C'PlayerColor -> IO CBool

foreign import ccall unsafe "chess_can_queenside_castle"
  c'chess_can_queenside_castle ::
    Ptr C'Board -> C'PlayerColor -> IO CBool

foreign import ccall unsafe "chess_get_game_state"
  c'chess_get_game_state ::
    Ptr C'Board -> IO C'GameState

foreign import ccall unsafe "chess_zobrist_key"
  c'chess_zobrist_key ::
    Ptr C'Board -> IO Word64

foreign import ccall unsafe "chess_make_move_unrolled"
  c'chess_make_move_unrolled ::
    Ptr C'Board -> CULong -> CULong -> CUChar -> CBool -> CBool -> IO ()

foreign import ccall unsafe "chess_undo_move"
  c'chess_undo_move ::
    Ptr C'Board -> IO ()

foreign import ccall unsafe "chess_free_board"
  c'chess_free_board ::
    Ptr C'Board -> IO ()

foreign import ccall unsafe "&chess_free_board"
  p'chess_free_board ::
    FinalizerPtr C'Board

foreign import ccall unsafe "chess_get_bitboard"
  c'chess_get_bitboard ::
    Ptr C'Board -> C'PlayerColor -> C'PieceType -> IO CULong

foreign import ccall unsafe "chess_push_unrolled"
  c'chess_push_unrolled ::
    CULong -> CULong -> CUChar -> CBool -> CBool -> IO ()

foreign import ccall unsafe "chess_done"
  c'chess_done ::
    IO ()

foreign import ccall unsafe "chess_get_time_millis"
  c'chess_get_time_millis ::
    IO CLong

foreign import ccall unsafe "chess_get_opponent_time_millis"
  c'chess_get_opponent_time_millis ::
    IO CLong

foreign import ccall unsafe "chess_get_elapsed_time_millis"
  c'chess_get_elapsed_time_millis ::
    IO CLong

foreign import ccall unsafe "chess_get_piece_from_index"
  c'chess_get_piece_from_index ::
    Ptr C'Board -> CInt -> IO C'PieceType

foreign import ccall unsafe "chess_get_piece_from_bitboard"
  c'chess_get_piece_from_bitboard ::
    Ptr C'Board -> Word64 -> IO C'PieceType

foreign import ccall unsafe "chess_get_color_from_index"
  c'chess_get_color_from_index ::
    Ptr C'Board -> CInt -> IO C'PlayerColor

foreign import ccall unsafe "chess_get_color_from_bitboard"
  c'chess_get_color_from_bitboard ::
    Ptr C'Board -> Word64 -> IO C'PlayerColor

foreign import ccall unsafe "chess_get_index_from_bitboard"
  c'chess_get_index_from_bitboard ::
    Word64 -> IO CInt

foreign import ccall unsafe "chess_get_bitboard_from_index"
  c'chess_get_bitboard_from_index ::
    CInt -> IO Word64

foreign import ccall unsafe "chess_free_moves_array"
  c'chess_free_moves_array ::
    Ptr C'Move -> IO ()

foreign import ccall unsafe "&chess_free_moves_array"
  p'chess_free_moves_array ::
    FinalizerPtr C'Move
