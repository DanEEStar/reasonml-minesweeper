let boardWidth = 20;
let boardHeight = 20;
let numBombs = 10;

module Tile = {
  type tileState =
    | Hidden
    | Revealed;

  type t = {
    index: int,
    numNeighbourBombs: int,
    neighbours: list(t),
    state: tileState,
    hasBomb: bool,
  };

  let stringOfTile = tile =>
    if (tile.hasBomb) {
      "X";
    } else if (tile.numNeighbourBombs > 0) {
      string_of_int(tile.numNeighbourBombs);
    } else {
      "0";
    };

  let reactOfTile = (tile, showHidden) =>
    if (tile.state == Hidden && !showHidden) {
      React.null;
    } else {
      <span> {React.string(stringOfTile(tile))} </span>;
    };

  let noneTile = {
    index: (-1),
    numNeighbourBombs: 0,
    neighbours: [],
    state: Hidden,
    hasBomb: false,
  };

  let getNeighbours = (tiles, tile) => {
    let tileIndex = tile.index;
    let x = tileIndex mod boardWidth;
    let y = tileIndex / boardWidth;
    Belt.List.map([(-1), 0, 1], i =>
      Belt.List.map(
        [(-1), 0, 1],
        j => {
          let x2 = x + i;
          let y2 = y + j;
          if (i == 0
              && j == 0
              || x2 < 0
              || x2 >= boardWidth
              || y2 < 0
              || y2 >= boardHeight) {
            noneTile;
          } else {
            let neighbourIndex = y2 * boardWidth + x2;
            Belt.Map.Int.getWithDefault(tiles, neighbourIndex, noneTile);
          };
        },
      )
    )
    ->Belt.List.flatten
    ->Belt.List.keep(t => t != noneTile);
  };

  let calcNumNeighbourBombs = (tiles, tile) => {
    getNeighbours(tiles, tile)
    ->Belt.List.keep(t => t.hasBomb)
    ->Belt.List.length;
  };

  let initTiles = numBombs => {
    let bombIndexes =
      Belt.Array.makeByAndShuffle(boardWidth * boardHeight, i => i)
      ->Belt.Array.slice(~offset=0, ~len=numBombs);

    let tiles =
      Belt.Array.make(boardHeight * boardWidth, 0)
      ->Belt.Array.mapWithIndex((index, _value) =>
          (
            index,
            {
              index,
              state: Hidden,
              numNeighbourBombs: 0,
              neighbours: [],
              hasBomb:
                Belt.Array.some(bombIndexes, bombIndex => bombIndex == index),
            },
          )
        )
      ->Belt.Map.Int.fromArray;

    let tilesWithCalcBombs = Belt.Map.Int.map(tiles, t => {
      {...t, numNeighbourBombs: calcNumNeighbourBombs(tiles, t)};
    });

    Belt.Map.Int.map(tilesWithCalcBombs, t => {
      {...t, neighbours: getNeighbours(tilesWithCalcBombs, t)};
    })
  };

  [@react.component]
  let make = (~onClick, ~tile, ~showHidden) => {
    <button className="square" onClick> {reactOfTile(tile, showHidden)} </button>;
  };
};

type action =
  | RevealTile(Tile.t)
  | ToggleHidden;

type state = {
  tiles: Belt.Map.Int.t(Tile.t),
  showHidden: bool
};

module Board = {
  [@react.component]
  let make = (~onClick, ~state: state) => {
    let renderTile = tile => {
      <Tile onClick={evt => onClick(evt, tile)} tile showHidden=state.showHidden/>;
    };

    ReasonReact.array(
      Belt.Array.map(Belt.Array.makeBy(boardHeight, i => i), row =>
        <div className="board-row" key={string_of_int(row)}>
          {Belt.Array.map(
             Belt.Array.makeBy(boardWidth, i => i),
             col => {
               let tileIndex = boardWidth * row + col;
               renderTile(
                 Belt.Map.Int.getWithDefault(state.tiles, tileIndex, Tile.noneTile),
               );
             },
           )
           ->ReasonReact.array}
        </div>
      ),
    );
  };
};

let revealTile = (tile: Tile.t, state) => {
  let rec revealNeighbourTiles = (tile: Tile.t, tiles) => {
    let tiles = Belt.Map.Int.set(state.tiles, tile.index, {...tile, state: Tile.Revealed});
    if(tile.numNeighbourBombs == 0) {
      Belt.List.reduce(tile.neighbours, tiles, (tiles, t) => {
        Js.log(t.index);
        Js.log(t.hasBomb);
        Js.log(t.numNeighbourBombs);
        Belt.Map.Int.set(tiles, t.index, {...t, state: Tile.Revealed});
      });
    } else {
      tiles;
    }
  };
  {...state, tiles: revealNeighbourTiles(tile, state.tiles)};
};

[@react.component]
let make = (~message) => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | RevealTile(tile) => revealTile(tile, state)
        | ToggleHidden => {...state, showHidden: !state.showHidden}
        },
      {showHidden: false, tiles: Tile.initTiles(numBombs)},
    );

  let handleClick = (_event, tile) => {
    dispatch(RevealTile(tile));
  };

  let toggleHidden = _evt => {
    dispatch(ToggleHidden);
  };

  <div className="game-board">
    <Board onClick=handleClick state=state />
    <button onClick=toggleHidden>
      {React.string("toggle hidden")}
    </button>
  </div>;
};