let boardWidth = 20;
let boardHeight = 20;
let numBombs = 100;

type tileState =
  | Hidden
  | Revealed;

type tile = {
  state: tileState,
  hasBomb: bool,
};

let stringOfTile = (tile) => {
  if(tile.hasBomb) {
    "1"
  } else {
    "0"
  }
}

type action =
  | NextMove(int)
  | JumpToHistory(int);

type state = {tiles: Belt.Map.Int.t(tile)};

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

module Tile = {
  [@react.component]
  let make = (~onClick, ~tile) => {
    <button className="square" onClick>
      {React.string(stringOfTile(tile))}
    </button>;
  };
};

module Board = {
  [@react.component]
  let make = (~onClick, ~tiles) => {
    let renderTile = tile => {
      <Tile onClick={evt => onClick(evt, tile)} tile=tile />;
    };

    ReasonReact.array(
      Belt.Array.map(Belt.Array.makeBy(boardWidth, i => i), row =>
        <div className="board-row" key={string_of_int(row)}>
          {
            Belt.Array.map(Belt.Array.makeBy(boardHeight, i => i), col => {
              let tileIndex = boardWidth * row + col;
              renderTile(Belt.Map.Int.getWithDefault(tiles, tileIndex, {hasBomb: false, state: Hidden}));
            }
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
      {tiles: initTiles(numBombs)},
    );

  <div className="game-board">
    <Board onClick=handleClick tiles=state.tiles />
  </div>;
};