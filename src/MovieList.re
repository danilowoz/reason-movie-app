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

type state =
  | Loading
  | Error
  | Loaded(movie);

type action =
  | MovieFetch
  | MovieFetched(movie)
  | MovieFailedToFetch;

module Decode = {
  let genre = json: genre =>
    Json.Decode.{
      name: json |> field("name", string),
      id: json |> field("id", int),
    };

  let movie = json: movie =>
    Json.Decode.{
      title: json |> field("original_title", string),
      genres:
        json |> field("genres", array(genre)) |> Array.map(_, g => g.name),
    };
};

let searchUrl = "https://api.themoviedb.org/3/movie/76341?api_key=cee134a9d530ca1dc02dd7058c66352f";

let component = ReasonReact.reducerComponent("MovieList");

let make = _children => {
  ...component,
  initialState: _state => Loading,
  reducer: (action, _state) =>
    switch (action) {
    | MovieFetch =>
      ReasonReact.UpdateWithSideEffects(
        Loading,
        (
          self =>
            Js.Promise.(
              Fetch.fetch(searchUrl)
              |> then_(Fetch.Response.json)
              |> then_(json =>
                   json
                   |> Decode.movie
                   |> (movie => self.send(MovieFetched(movie)))
                   |> resolve
                 )
              |> catch(_err => {
                   Js.log(_err);
                   Js.Promise.resolve(self.send(MovieFailedToFetch));
                 })
              |> ignore
            )
        ),
      )
    | MovieFetched(movie) => ReasonReact.Update(Loaded(movie))
    | MovieFailedToFetch => ReasonReact.Update(Error)
    },
  didMount: self => self.send(MovieFetch),
  render: self =>
    <div>
      <Genres />
      {
        switch (self.state) {
        | Error => <div> {ReasonReact.string("An error occurred!")} </div>
        | Loading => <div> {ReasonReact.string("Loading...")} </div>
        | Loaded(movie) =>
          <div>
            <h1> {ReasonReact.string("Movie")} </h1>
            {ReasonReact.string(movie.title)}
            <ul>
              {
                Array.map(movie.genres, g =>
                  <p key=g> {ReasonReact.string(g)} </p>
                )
                |> ReasonReact.array
              }
            </ul>
          </div>
        }
      }
    </div>,
};