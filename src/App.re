open Webapi.Dom;

type state = {
  active: option(Types.colors),
  isPlaying: bool,
  points: int,
  income: int,
};

type action =
  | PlaySound(Types.colors)
  | ResetColor
  | Click(int)
  | BuyBonus(int, int)
  | SetPlaying;

module Styles = {
  open Css;

  global(
    "body",
    [
      fontFamily(
        "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif",
      ),
    ],
  );

  let container =
    style([
      display(`flex),
      justifyContent(`center),
      alignItems(`center),
      minHeight(`vh(100.0)),
      flexDirection(`column),
    ]);

  let boxes =
    style([
      display(`flex),
      flexWrap(`wrap),
      maxWidth(`px(500)),
      maxHeight(`px(500)),
    ]);

  let box = (~bgColor: Types.colors, ~active: option(Types.colors)) => {
    let baseStyle = [
      minHeight(`px(120)),
      minWidth(`px(120)),
      border(`px(2), `none, `transparent),
    ];

    let opacity =
      switch (bgColor, active) {
      | (Green, Some(Green)) => opacity(0.5)
      | (Red, Some(Red)) => opacity(0.5)
      | (Blue, Some(Blue)) => opacity(0.5)
      | (Yellow, Some(Yellow)) => opacity(0.5)
      | (_, None) => opacity(1.0)
      | (_, Some(_)) => opacity(1.0)
      };

    let bgColor =
      switch (bgColor) {
      | Green => backgroundColor(`hex("78ab2a"))
      | Red => backgroundColor(`hex("fd2475"))
      | Blue => backgroundColor(`hex("0283c7"))
      | Yellow => backgroundColor(`hex("fde916"))
      };

    style([bgColor, opacity, ...baseStyle]);
  };
  let controls = style([marginTop(`px(10)), textAlign(`center)]);
  let buttons = style([marginTop(`px(10))]);
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {active: None, isPlaying: false, points: 0, income: 1},
  reducer: (action, state) =>
    switch (action) {
    | PlaySound(color) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, active: Some(color)},
        self => {
          let sound =
            Belt.List.getAssoc(Sounds.map, color, (==))
            ->Belt.Option.getWithDefault(Sounds.green);
          sound##play();
          let _id = Js.Global.setTimeout(() => self.send(ResetColor), 300);
          ();
        },
      )
    | ResetColor => ReasonReact.Update({...state, active: None})
    | Click(bonus) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, points: state.points + bonus},
        self => Sounds.error##play(),
      )
    | BuyBonus(bonus, cost) =>
      let {points} = state;
      cost <= points ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            income: state.income + bonus,
            points: state.points - cost,
          },
          switch (Js.Math.floor(Js.Math.random() *. 4.0 +. 1.0)) {
          | 1 => (_ => Sounds.force##play())
          | 2 => (_ => Sounds.luck##play())
          | 3 => (_ => Sounds.notout##play())
          | 4 => (_ => Sounds.strong##play())
          | _ => (_ => Sounds.badfeeling##play())
          },
        ) :
        ReasonReact.SideEffects(
          switch (Js.Math.floor(Js.Math.random() *. 4.0 +. 1.0)) {
          | 1 => (_ => Sounds.error##play())
          | 2 => (_ => Sounds.error##play())
          | 3 => (_ => Sounds.error##play())
          | 4 => (_ => Sounds.error##play())
          | _ => (_ => Sounds.badfeeling##play())
          },
        );
    | SetPlaying =>
      ReasonReact.Update({...state, isPlaying: !state.isPlaying})
    },
  didMount: self => {
    ();
  },
  render: self => {
    let {active, isPlaying, points, income} = self.state;
    <div className=Styles.container>
      <h1> "ciacho judasza"->ReasonReact.string </h1>
      <h2> {ReasonReact.string(string_of_int(points))} </h2>
      <div className=Styles.boxes>
        <button
          type_="button"
          className={Styles.box(~bgColor=Green, ~active)}
          onClick={_e => self.send(Click(income))}
          disabled=isPlaying
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Red, ~active)}
          onClick={_e => self.send(BuyBonus(1, 10))}
          disabled=isPlaying
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Blue, ~active)}
          //onClick={_e => self.send(Input(Blue))}
          disabled=isPlaying
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Yellow, ~active)}
          //onClick={_e => self.send(Input(Yellow))}
          disabled=isPlaying
        />
      </div>
    </div>;
  },
};

let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make([||]));