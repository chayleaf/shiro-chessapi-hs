#include <stdbool.h>
#include <stdint.h>
#include <threads.h>

typedef uint64_t BitBoard;
typedef struct {
    BitBoard from;
    BitBoard to;
    uint8_t promotion;
    bool capture;
    bool castle;
} Move;
typedef struct Board Board;

void chess_make_move(Board *board, Move move);
void chess_push(Move move);
Move chess_get_opponent_move();

void chess_make_move_unrolled(Board *board, BitBoard from, BitBoard to, uint8_t promotion, bool capture, bool castle) {
    Move move = {from, to, promotion, capture, castle};
    chess_make_move(board, move);
}
void chess_push_unrolled(BitBoard from, BitBoard to, uint8_t promotion, bool capture, bool castle) {
    Move move = {from, to, promotion, capture, castle};
    chess_push(move);
}
Move *chess_get_opponent_move_ptr() {
    static thread_local Move ret;
    ret = chess_get_opponent_move();
    return &ret;
}
