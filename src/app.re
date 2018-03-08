[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let str = ReasonReact.stringToElement;

module Square = {
  let component = ReasonReact.statelessComponent("Square");
  let make = (~value, ~onToggle, _children) => {
    ...component,
    render: _self =>
      /* <div /> */
      <button className="square" onClick=(_evt => onToggle())>
        (str(value))
      </button>
  };
};

module Board = {
  type action =
    | ClickSquare;
  let component = ReasonReact.reducerComponent("Board");
  let status = "Next player: X";
  /* let renderSquare = value => <Square value=(value)/>; */
  let make = _children => {
    ...component,
    initialState: () => ["Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z"],
    reducer: (action, _state) =>
      switch action {
      | ClickSquare => ReasonReact.Update(["Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z", "Z"])
      },
    render: self =>
      <div>
        <div className="status"> (str(status)) </div>
        (
          ReasonReact.arrayToElement(
            Array.of_list(
              List.map((num) =>
                <Square
                  value=(num)
                  onToggle=(self.reduce(() => ClickSquare))
                />, self.state
              )
            )
          )
        )
        <div
          className="board-row"
          /* <Square value=(string_of_int(1)) onToggle=(self.reduce (()=> ClickSquare)) /> */
          /* (renderSquare(string_of_int(1))) */
          /* <Square value=(string_of_int(2)) />
             <Square value=(string_of_int(3)) /> */
        />
        <div
          className="board-row"
          /* <Square value=(string_of_int(4)) />
             <Square value=(string_of_int(5)) />
             <Square value=(string_of_int(6)) /> */
        />
        <div
          className="board-row"
          /* <Square value=(string_of_int(7)) />
             <Square value=(string_of_int(8)) />
             <Square value=(string_of_int(9)) /> */
        />
      </div>
  };
};

module Game = {
  let component = ReasonReact.statelessComponent("Game");
  let make = _children => {
    ...component,
    render: (_) =>
      <div className="game">
        <div className="game-board"> <Board /> </div>
        <div className="game-info"> <div /> <ol /> </div>
      </div>
  };
};

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self => <div className="App"> <Game /> </div>
};