/* The new stdlib additions */
open Belt;

type genre = {
  id: int,
  name: string,
};

type listGenre = array(genre);

type movie = {
  title: string,
  genres: array(string),
};

type pageState =
  | Loading
  | Error
  | Loaded(listGenre);

type state = {
  pageState,
  selectedId: int,
};

type action =
  | GenreFetch
  | GenreFetched(listGenre)
  | GenreFailedToFetch
  | HandleId(int);

module Decode = {
  let genre = json: genre =>
    Json.Decode.{
      id: json |> field("id", int),
      name: json |> field("name", string),
    };

  let list = json: listGenre =>
    Json.Decode.(json |> field("genres", array(genre)));
};

let getList = "https://api.themoviedb.org/3/genre/movie/list?api_key=cee134a9d530ca1dc02dd7058c66352f";

let component = ReasonReact.reducerComponent("Genres");

let make = (~getId, _children) => {
  ...component,
  initialState: _state => {pageState: Loading, selectedId: 0},
  reducer: (action, _state) =>
    switch (action) {
    | GenreFetch =>
      ReasonReact.UpdateWithSideEffects(
        {..._state, pageState: Loading},
        (
          self =>
            Js.Promise.(
              Fetch.fetch(getList)
              |> then_(Fetch.Response.json)
              |> then_(json =>
                   json
                   |> Decode.list
                   |> (listGenre => self.send(GenreFetched(listGenre)))
                   |> resolve
                 )
              |> catch(_err => {
                   Js.log(_err);
                   Js.Promise.resolve(self.send(GenreFailedToFetch));
                 })
              |> ignore
            )
        ),
      )
    | GenreFetched(listGenre) =>
      ReasonReact.Update({..._state, pageState: Loaded(listGenre)})
    | GenreFailedToFetch => ReasonReact.Update({..._state, pageState: Error})
    | HandleId(id) =>
      ReasonReact.UpdateWithSideEffects(
        {..._state, selectedId: id},
        (self => getId(self.state.selectedId)),
      )
    },
  didMount: self => self.send(GenreFetch),
  render: self =>
    <div>
      <h1> {ReasonReact.string("Genres")} </h1>
      {
        switch (self.state.pageState) {
        | Error => <div> {ReasonReact.string("An error occurred!")} </div>
        | Loading => <div> {ReasonReact.string("Loading...")} </div>
        | Loaded(listGenre) =>
          <div>
            {
              Array.map(listGenre, g =>
                <button
                  onClick=(_e => self.send(HandleId(g.id)))
                  key={string_of_int(g.id)}>
                  {ReasonReact.string(g.name)}
                </button>
              )
              |> ReasonReact.array
            }
          </div>
        }
      }
    </div>,
};