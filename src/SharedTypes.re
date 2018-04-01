type player =
  | Cross
  | Circle;

type field =
  | Empty
  | Marked(player);

type row = list(field);

type board = list(row);

type gameState =
  | Playing(player)
  | Winner(player)
  | Draw;

type state = {
  board,
  gameState,
};