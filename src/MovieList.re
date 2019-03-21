/* The new stdlib additions */
open Belt;

type genre = {
  id: int,
  name: string,
};

type listGenre = array(genre);

type movie = {
  title: string,
  genres: array(int),
  image: string,
};

type listMovie = array(movie);

type pageState =
  | Loading
  | Error
  | Loaded(listMovie);

type state = {
  pageState,
  selectedId: int,
};

type action =
  | MovieFetch
  | MovieFetched(listMovie)
  | MovieFailedToFetch
  | HandleId(int);

module Decode = {
  let movie = json: movie =>
    Json.Decode.{
      title: json |> field("original_title", string),
      genres: json |> field("genre_ids", array(int)) |> Array.map(_, g => g),
      image: json |> field("poster_path", string),
    };

  let listMovie = json: listMovie =>
    Json.Decode.(json |> field("results", array(movie)));
};

let searchUrl = "https://api.themoviedb.org/3/discover/movie?api_key=cee134a9d530ca1dc02dd7058c66352f";

let component = ReasonReact.reducerComponent("MovieList");

let make = _children => {
  ...component,
  initialState: _state => {pageState: Loading, selectedId: 0},
  reducer: (action, _state) =>
    switch (action) {
    | MovieFetch =>
      ReasonReact.UpdateWithSideEffects(
        {..._state, pageState: Loading},
        (
          self => {
            let hasId = self.state.selectedId !== 0;
            let url =
              hasId ?
                searchUrl
                ++ "&with_genres="
                ++ string_of_int(self.state.selectedId) :
                searchUrl;

            Js.Promise.(
              Fetch.fetch(url)
              |> then_(Fetch.Response.json)
              |> then_(json =>
                   json
                   |> Decode.listMovie
                   |> (listMovie => self.send(MovieFetched(listMovie)))
                   |> resolve
                 )
              |> catch(_err => {
                   Js.log(_err);
                   Js.Promise.resolve(self.send(MovieFailedToFetch));
                 })
              |> ignore
            );
          }
        ),
      )
    | MovieFetched(listMovie) =>
      ReasonReact.Update({..._state, pageState: Loaded(listMovie)})
    | MovieFailedToFetch => ReasonReact.Update({..._state, pageState: Error})
    | HandleId(id) =>
      ReasonReact.UpdateWithSideEffects(
        {..._state, selectedId: id},
        (self => self.send(MovieFetch)),
      )
    },
  didMount: self => self.send(MovieFetch),
  render: self =>
    <div>
      <Genres getId={id => self.send(HandleId(id))} />
      {
        switch (self.state.pageState) {
        | Error => <div> {ReasonReact.string("An error occurred!")} </div>
        | Loading => <div> {ReasonReact.string("Loading...")} </div>
        | Loaded(listMovie) =>
          <div>
            <h1> {ReasonReact.string("Movie")} </h1>
            <ul>
              {
                Array.map(listMovie, movie =>
                  <div
                    style={
                      ReactDOMRe.Style.make(
                        ~display="inline-block",
                        ~verticalAlign="top",
                        ~width="200px",
                        (),
                      )
                    }>
                    <img
                      width="150px"
                      src={"https://image.tmdb.org/t/p/w500/" ++ movie.image}
                    />
                    <p key={movie.title}>
                      {ReasonReact.string(movie.title)}
                    </p>
                  </div>
                )
                |> ReasonReact.array
              }
            </ul>
          </div>
        }
      }
    </div>,
};

/* getId={id => self.send(HandleId)} */
/* onClick=(_e => self.send(HandleId(g.id))) */
/* {
     let id = self.state.selectedId;

     id !== 0 ?
       <h1> {ReasonReact.string(string_of_int(id))} </h1> :
       ReasonReact.string(" ");
   } */