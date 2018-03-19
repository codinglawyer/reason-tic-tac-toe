type player =
  | Cross
  | Circle;

type field =
  | Empty
  | Marked(player);

type gameState =
  | Playing(player)
  | Winner(player)
  | Draw;

type row = list(field);

type board = list(row);

type state = {
  board,
  gameState,
};