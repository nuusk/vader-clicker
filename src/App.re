type state = {
  points: int,
  income: int,
  revenue: int,
  incomeBonus: int,
  revenueBonus: int,
  incomeBonusCost: int,
  revenueBonusCost: int,
  incomeMultiplierCost: int,
  revenueMultiplierCost: int,
};

type action =
  | Click(int)
  | Reset
  | IncreaseRevenue(int, int)
  | Payment
  | BuyBonus(int, int)
  | MultiplyRevenue(int, int)
  | MultiplyIncome(int, int)
  | PlaySuccessSound
  | PlayErrorSound;

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    points: 0,
    income: 1,
    revenue: 0,
    incomeBonus: 1,
    revenueBonus: 1,
    incomeBonusCost: 10,
    revenueBonusCost: 20,
    incomeMultiplierCost: 100,
    revenueMultiplierCost: 200,
  },
  reducer: (action, state) =>
    switch (action) {
    | Click(bonus) =>
      ReasonReact.Update({...state, points: state.points + bonus})
    | Reset => ReasonReact.UpdateWithSideEffects({ 
          points: 0, 
          income: 1,
          revenue: 0,
          incomeBonus: 1,
          revenueBonus: 1,
          incomeBonusCost: 10,
          revenueBonusCost: 20,
          incomeMultiplierCost: 100,
          revenueMultiplierCost: 200,
    },
      self => {
        Sounds.trap##play()
      }
    )
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
          self => {
            self.send(PlaySuccessSound);
          },
        ) :
        ReasonReact.SideEffects(
        self => {
          self.send(PlayErrorSound);
        });
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
          self => {
            self.send(PlaySuccessSound);
          },
        ) :
        ReasonReact.SideEffects(
        self => {
          self.send(PlayErrorSound);
        });
    | MultiplyIncome(multiplier, cost) =>
      let {points} = state;
      cost <= points ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            income: state.income * multiplier,
            points: state.points - cost,
            incomeMultiplierCost: state.incomeMultiplierCost * 10,
            incomeBonus: state.incomeBonus * multiplier,
          },
          self => {
            self.send(PlaySuccessSound);
          },
        ) :
        ReasonReact.SideEffects(
        self => {
          self.send(PlayErrorSound);
        });
      | MultiplyRevenue(multiplier, cost) =>
      let {points} = state;
      cost <= points ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            revenue: state.revenue * multiplier,
            points: state.points - cost,
            revenueMultiplierCost: state.revenueMultiplierCost * 10,
            revenueBonus: state.revenueBonus * multiplier,
          },
          self => {
            self.send(PlaySuccessSound);
          },
        ) :
        ReasonReact.SideEffects(
        self => {
          self.send(PlayErrorSound);
        });
      | PlaySuccessSound => 
        ReasonReact.SideEffects(
          switch (Js.Math.floor(Js.Math.random() *. 17.0 +. 1.0)) {
          | 1 => (_ => Sounds.force##play())
          | 2 => (_ => Sounds.luck##play())
          | 3 => (_ => Sounds.notout##play())
          | 4 => (_ => Sounds.strong##play())
          | 5 => (_ => Sounds.destiny##play())
          | 6 => (_ => Sounds.evasion##play())
          | 7 => (_ => Sounds.failed##play())
          | 8 => (_ => Sounds.force2##play())
          | 9 => (_ => Sounds.goodshoot##play())
          | 10 => (_ => Sounds.great##play())
          | 11 => (_ => Sounds.intesify##play())
          | 12 => (_ => Sounds.jedi##play())
          | 13 => (_ => Sounds.learn##play())
          | 14 => (_ => Sounds.scruvvy##play())
          | 15 => (_ => Sounds.sfoils##play())
          | 16 => (_ => Sounds.trap##play())
          | 17 => (_ => Sounds.darkside##play())
          | _ => (_ => Sounds.badfeeling##play())
          }
      );
      | PlayErrorSound => 
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
    let {
      points,
      income,
      revenue,
      incomeBonus,
      revenueBonus,
      incomeBonusCost,
      revenueBonusCost,
      incomeMultiplierCost,
      revenueMultiplierCost,
    } =
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
          className="game__button game__button--click"
          onClick={_e => self.send(Click(income))}
        />
        <div
          className="game__button game__button--income"
          onClick={_e => self.send(BuyBonus(incomeBonus, incomeBonusCost))}>
          <span> "cost"->ReasonReact.string </span>
          <span> {ReasonReact.string(string_of_int(incomeBonusCost))} </span>
        </div>
        <div
          className="game__button game__button--income"
          onClick={_e => self.send(MultiplyIncome(2, incomeMultiplierCost))}>
          <span> "cost"->ReasonReact.string </span>
          <span>
            {ReasonReact.string(string_of_int(incomeMultiplierCost))}
          </span>
        </div>
        <div
          className="game__button game__button--income"
          onClick={_e => self.send(IncreaseRevenue(revenueBonus, revenueBonusCost))}>
          <span> "cost"->ReasonReact.string </span>
          <span>
            {ReasonReact.string(string_of_int(revenueBonusCost))}
          </span>
        </div>
        <div
          className="game__button game__button--income"
          onClick={_e =>
            self.send(MultiplyRevenue(2, revenueMultiplierCost))
          }>
          <span> "cost"->ReasonReact.string </span>
          <span>
            {ReasonReact.string(string_of_int(revenueMultiplierCost))}
          </span>
        </div>
        <div
          className="game__button game__button--click"
          onClick={_e => self.send(Reset)}
        />
      </div>
    </div>;
  },
};

let default = ReasonReact.wrapReasonForJs(~component, _jsProps => make([||]));