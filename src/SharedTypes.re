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

type fields = list(row);

type state = {
  fields,
  gameState,
};