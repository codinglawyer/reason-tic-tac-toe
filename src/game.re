[%bs.raw {|require('./game.css')|}];

open SharedTypes;

type action =
  | ClickSquare(string)
  | Restart;

let isDraw = board =>
  List.for_all(
    field => field == Marked(Circle) || field == Marked(Cross),
    board,
  );

let checkGameState = (board: board, gameState: gameState) => {
  let flattenBoard = List.flatten(board);
  let winningCombs = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6],
  ];
  let rec check = rest => {
    let head = List.hd(rest);
    let tail = List.tl(rest);
    switch (
      tail,
      List.nth(flattenBoard, List.nth(head, 0)),
      List.nth(flattenBoard, List.nth(head, 1)),
      List.nth(flattenBoard, List.nth(head, 2)),
    ) {
    | (_, Marked(Cross), Marked(Cross), Marked(Cross)) => Winner(Cross)
    | (_, Marked(Circle), Marked(Circle), Marked(Circle)) =>
      Winner(Circle)
    | ([], _, _, _) =>
      isDraw(flattenBoard) ?
        Draw :
        (
          switch (gameState) {
          | Playing(Cross) => Playing(Circle)
          | _ => Playing(Cross)
          }
        )
    | _ => check(tail)
    };
  };
  check(winningCombs);
};

let initialState = {
  board: [
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
    [Empty, Empty, Empty],
  ],
  gameState: Playing(Cross),
};

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  initialState: () => initialState,
  reducer: (action: action, state: state) =>
    switch (action) {
    | Restart => ReasonReact.Update(initialState)
    | ClickSquare((id: string)) =>
      let updatedBoard =
        state.board
        |> List.mapi((ind: int, row: row) =>
             row
             |> List.mapi((index: int, value: field) =>
                  string_of_int(ind) ++ string_of_int(index) === id ?
                    switch (state.gameState, value) {
                    | (_, Marked(_)) => value
                    | (Playing(player), Empty) => Marked(player)
                    | (_, Empty) => Empty
                    } :
                    value
                )
           );
      ReasonReact.Update({
        board: updatedBoard,
        gameState:
          state.board == updatedBoard ?
            state.gameState : checkGameState(updatedBoard, state.gameState),
      });
    },
  render: ({state, reduce}) =>
    <div className="game">
      <Board
        state
        onRestart=(reduce(_evt => Restart))
        onMark=(reduce(id => ClickSquare(id)))
      />
    </div>,
};