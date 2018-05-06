[%bs.raw {|require('./game.css')|}];

open SharedTypes;

type action =
  | ClickSquare(string)
  | Restart;

type winner =
  | Cross
  | Circle
  | NoOne;

type winningRows = list(list(int));

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

let gameEnded = board =>
  List.for_all(
    field => field == Marked(Circle) || field == Marked(Cross),
    board,
  );

let whosPlaying = (gameState: gameState) =>
  switch (gameState) {
  | Playing(Cross) => Playing(Circle)
  | _ => Playing(Cross)
  };

let getWinner = (flattenBoard, coords) =>
  switch (
    List.nth(flattenBoard, List.nth(coords, 0)),
    List.nth(flattenBoard, List.nth(coords, 1)),
    List.nth(flattenBoard, List.nth(coords, 2)),
  ) {
  | (Marked(Cross), Marked(Cross), Marked(Cross)) => Cross
  | (Marked(Circle), Marked(Circle), Marked(Circle)) => Circle
  | (_, _, _) => NoOne
  };

let checkGameState =
    (
      winningRows: winningRows,
      updatedBoard: board,
      oldBoard: board,
      gameState: gameState,
    ) =>
  oldBoard == updatedBoard ?
    gameState :
    {
      let flattenBoard = List.flatten(updatedBoard);
      let rec check = (rest: winningRows) => {
        let head = List.hd(rest);
        let tail = List.tl(rest);
        switch (
          getWinner(flattenBoard, head),
          gameEnded(flattenBoard),
          tail,
        ) {
        | (Cross, _, _) => Winner(Cross)
        | (Circle, _, _) => Winner(Circle)
        | (_, true, []) => Draw
        | (_, false, []) => whosPlaying(gameState)
        | _ => check(tail)
        };
      };
      check(winningRows);
    };

let checkGameState3x3 = checkGameState(winningCombs);

let updateBoard = (board: board, gameState: gameState, id) =>
  board
  |> List.mapi((ind: int, row: row) =>
       row
       |> List.mapi((index: int, value: field) =>
            string_of_int(ind) ++ string_of_int(index) === id ?
              switch (gameState, value) {
              | (_, Marked(_)) => value
              | (Playing(player), Empty) => Marked(player)
              | (_, Empty) => Empty
              } :
              value
          )
     );

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
      let updatedBoard = updateBoard(state.board, state.gameState, id);
      ReasonReact.Update({
        board: updatedBoard,
        gameState:
          checkGameState3x3(updatedBoard, state.board, state.gameState),
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