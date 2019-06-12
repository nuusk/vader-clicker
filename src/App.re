open Webapi.Dom;

type state = {
  active: option(Types.colors),
  points: int,
  income: int,
  revenue: int,
  incomeBonusCost: int,
  revenueBonusCost: int,
};

type action =
  | PlaySound(Types.colors)
  | Click(int)
  | IncreaseRevenue(int, int)
  | Payment
  | BuyBonus(int, int);

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
  initialState: () => {active: None, points: 0, income: 1, revenue: 0, incomeBonusCost: 10, revenueBonusCost: 20},
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
          ();
        },
      )
    | Click(bonus) =>
      ReasonReact.Update({...state, points: state.points + bonus})
    | Payment =>
      ReasonReact.Update({...state, points: state.points + state.revenue})
    | IncreaseRevenue(bonus, cost) =>
      let {points} = state;
      cost <= points ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            revenue: state.revenue + bonus,
            points: state.points - cost,
            revenueBonusCost: Js.Math.ceil(float_of_int(state.revenueBonusCost) *. 1.1),
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
    | BuyBonus(bonus, cost) =>
      let {points} = state;
      cost <= points ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            income: state.income + bonus,
            points: state.points - cost,
            incomeBonusCost: Js.Math.ceil(float_of_int(state.incomeBonusCost) *. 1.1),
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
    },
  didMount: self => {
    let intervalId = Js.Global.setInterval(() => self.send(Payment), 1000);
    self.onUnmount(() => Js.Global.clearInterval(intervalId));
    ();
  },
  render: self => {
    let {active, points, income, revenue, incomeBonusCost, revenueBonusCost} = self.state;
    <div className=Styles.container>
      <h1> 
        <span> "Points: "->ReasonReact.string </span>
        <span> {ReasonReact.string(string_of_int(points))} </span>
      </h1>
      <h1>
        <span> "revenue: "->ReasonReact.string </span>
        <span> {ReasonReact.string(string_of_int(revenue))} </span>
      </h1>
      <h1> "ciacho judasza"->ReasonReact.string </h1>
      <h2> 
        <span> "income per click: "->ReasonReact.string </span>
        <span> {ReasonReact.string(string_of_int(income))} </span>
      </h2>
      <div className=Styles.boxes>
        <button
          type_="button"
          className={Styles.box(~bgColor=Green, ~active)}
          onClick={_e => self.send(Click(income))}
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Red, ~active)}
          onClick={_e => self.send(BuyBonus(1, incomeBonusCost))}
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Blue, ~active)}
          onClick={_e => self.send(IncreaseRevenue(2, revenueBonusCost))}
        />
        <button
          type_="button"
          className={Styles.box(~bgColor=Yellow, ~active)}
          //onClick={_e => self.send(Input(Yellow))}
        />
      </div>
    </div>;
  },
};

let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make([||]));