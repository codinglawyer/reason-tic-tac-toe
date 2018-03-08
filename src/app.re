[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

let str = ReasonReact.stringToElement;

module Square = {
  type action =
    | ClickSquare;
  let component = ReasonReact.reducerComponent("Square");
  let make = (~value, _children) => {
    ...component,
    initialState: () => value,
    reducer: (action, _state) =>
      switch action {
      | ClickSquare => ReasonReact.Update("X")
      },
    render: self =>
      <button className="square" onClick=(self.reduce(_evt => ClickSquare))>
        (str(self.state))
      </button>
  };
};

module Board = {
  let component = ReasonReact.statelessComponent("Board");
  let status = "Next player: X";
  /* let renderSquare = value => <Square value=(value)/>; */
  let make = _children => {
    ...component,
    render: _self =>
      <div>
        <div className="status"> (str(status)) </div>
        <div className="board-row">
          <Square value=(string_of_int(1)) />
          <Square value=(string_of_int(2)) />
          <Square value=(string_of_int(3)) />
        </div>
        <div className="board-row">
          <Square value=(string_of_int(4)) />
          <Square value=(string_of_int(5)) />
          <Square value=(string_of_int(6)) />
        </div>
        <div className="board-row">
          <Square value=(string_of_int(7)) />
          <Square value=(string_of_int(8)) />
          <Square value=(string_of_int(9)) />
        </div>
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