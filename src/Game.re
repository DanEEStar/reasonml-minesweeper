let boardWidth = 20;
let boardHeight = 20;
let numBombs = 100;

module Tile = {
  type tileState =
    | Hidden
    | Revealed;

  type t = {
    state: tileState,
    hasBomb: bool,
  };

  let stringOfTile = tile =>
    if (tile.hasBomb) {
      "X";
    } else {
      "";
    };

  let reactOfTile = tile => {
    <span> {React.string(stringOfTile(tile))} </span>;
  };

  let initTiles = numBombs => {
    let bombIndexes =
      Belt.Array.makeByAndShuffle(boardWidth * boardHeight, i => i)
      ->Belt.Array.slice(~offset=0, ~len=numBombs);

    Belt.Array.make(boardHeight * boardWidth, 0)
    ->Belt.Array.mapWithIndex((index, _value) =>
        (
          index,
          {
            state: Hidden,
            hasBomb:
              Belt.Array.some(bombIndexes, bombIndex => bombIndex == index),
          },
        )
      )
    ->Belt.Map.Int.fromArray;
  };

  let defaultTile = () => {
    state: Hidden,
    hasBomb: false
  };

  [@react.component]
  let make = (~onClick, ~tile) => {
    <button className="square" onClick> {reactOfTile(tile)} </button>;
  };
};

type action =
  | NextMove(int)
  | JumpToHistory(int);

type state = {tiles: Belt.Map.Int.t(Tile.t)};

module Board = {
  [@react.component]
  let make = (~onClick, ~tiles) => {
    let renderTile = tile => {
      <Tile onClick={evt => onClick(evt, tile)} tile />;
    };

    ReasonReact.array(
      Belt.Array.map(Belt.Array.makeBy(boardWidth, i => i), row =>
        <div className="board-row" key={string_of_int(row)}>
          {Belt.Array.map(
             Belt.Array.makeBy(boardHeight, i => i),
             col => {
               let tileIndex = boardWidth * row + col;
               renderTile(
                 Belt.Map.Int.getWithDefault(
                   tiles,
                   tileIndex,
                   Tile.defaultTile()
                 ),
               );
             },
           )
           ->ReasonReact.array}
        </div>
      ),
    );
  };
};

let handleClick = (_event, tile) => Js.log(tile);

[@react.component]
let make = (~message) => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | NextMove(squareIndex) => state
        | JumpToHistory(historyIndex) => state
        },
      {tiles: Tile.initTiles(numBombs)},
    );

  <div className="game-board">
    <Board onClick=handleClick tiles={state.tiles} />
  </div>;
};