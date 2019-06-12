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

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    active: None,
    points: 0,
    income: 1,
    revenue: 0,
    incomeBonusCost: 10,
    revenueBonusCost: 20,
  },
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
            revenueBonusCost:
              Js.Math.ceil(float_of_int(state.revenueBonusCost) *. 1.1),
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
            incomeBonusCost:
              Js.Math.ceil(float_of_int(state.incomeBonusCost) *. 1.1),
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
    let {active, points, income, revenue, incomeBonusCost, revenueBonusCost} =
      self.state;
    <div className="game">
      <div className="game__header">
        <h3>
          <span> "revenue: "->ReasonReact.string </span>
          <span> {ReasonReact.string(string_of_int(revenue))} </span>
        </h3>
        <h3>
          <span> "income per click: "->ReasonReact.string </span>
          <span> {ReasonReact.string(string_of_int(income))} </span>
        </h3>
      </div>
      <div className="game__points">
        <strong> {ReasonReact.string(string_of_int(points))} </strong>
      </div>
      <div className="game__buttons">
        <div
          className="game__button"
          onClick={_e => self.send(Click(income))}
        />
        <div
          className="game__button"
          onClick={_e => self.send(BuyBonus(1, incomeBonusCost))}
        />
        <div
          className="game__button"
          onClick={_e => self.send(IncreaseRevenue(2, revenueBonusCost))}
        />
        <div
          className="game__button"
          //onClick={_e => self.send(Input(Yellow))}
        />
      </div>
    </div>;
  },
};

let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make([||]));