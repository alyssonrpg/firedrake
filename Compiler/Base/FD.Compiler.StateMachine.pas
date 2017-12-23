unit FD.Compiler.StateMachine;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils;

type
  EStateMachineException = class(Exception);

  TState<T, U> = class;

  TStateTransition<T, U> = record
    StartCondition: T;
    EndCondition: T;
    NextState: TState<T, U>;
  end;

  TState<T, U> = class(TObject)
  protected
    FIsFinal: Boolean;
    FData: U;
    FComparer: IComparer<T>;

    FTransitions: TList<TStateTransition<T, U>>;
    FTransitionsDirty: Boolean;
    FTransitionsArray: TArray<TStateTransition<T, U>>;

    FFallbackTransitionNextState: TState<T, U>;

    procedure SortTransitions;
  public
    constructor Create(Comparer: IComparer<T>);
    destructor Destroy; override;

    procedure AddTransition(const Value: T; const NextState: TState<T, U>); overload;
    procedure AddTransition(const RangeStart, RangeEnd: T; const NextState: TState<T, U>); overload;
    procedure SetFallbackTransition(const FallbackNextState: TState<T, U>);

    function TryLocateNextState(Value: T; out NextState: TState<T, U>): Boolean;

    property IsFinal: Boolean read FIsFinal write FIsFinal;
    property Data: U read FData write FData;
  end;

  TStateMachine<T, U> = class(TObject)
  private
    function GetInitialState: TState<T, U>;
  protected
    FComparer: IComparer<T>;
    FInitialState: TState<T, U>;
    FStates: TList<TState<T, U>>;

    procedure ClearStates;
  public
    constructor Create(const Comparer: IComparer<T>); overload;
    constructor Create; overload;
    destructor Destroy; override;

    function CreateNewState: TState<T, U>; virtual;

    property InitialState: TState<T, U> read GetInitialState;
    property Comparer: IComparer<T> read FComparer;
  end;

implementation

{ TStateMachine<T, U> }

constructor TStateMachine<T, U>.Create(const Comparer: IComparer<T>);
begin
  if Comparer <> nil then
    FComparer := Comparer
  else
    FComparer := TComparer<T>.Default;

  FStates := TList<TState<T, U>>.Create;
  Assert(FComparer <> nil);
end;

procedure TStateMachine<T, U>.ClearStates;
var
  i: Integer;
begin
  FInitialState := nil;

  if FStates = nil then
    Exit;

  for i := FStates.Count - 1 downto 0 do
    FStates[i].DisposeOf;

  FStates.Clear;
end;

constructor TStateMachine<T, U>.Create;
begin
  Self.Create(nil);
end;

destructor TStateMachine<T, U>.Destroy;
begin
  Self.ClearStates();

  FStates.DisposeOf;
  inherited;
end;

function TStateMachine<T, U>.GetInitialState: TState<T, U>;
begin
  if FInitialState = nil then
    FInitialState := Self.CreateNewState();

  Result := FInitialState;
  Assert(Result <> nil);
end;

function TStateMachine<T, U>.CreateNewState: TState<T, U>;
begin
  Assert(FStates <> nil);

  Result := TState<T, U>.Create(FComparer);
  FStates.Add(Result);
end;

{ TState<T, U> }

procedure TState<T, U>.AddTransition(const Value: T;
  const NextState: TState<T, U>);
begin
  Self.AddTransition(Value, Value, NextState);
end;

procedure TState<T, U>.AddTransition(const RangeStart, RangeEnd: T;
  const NextState: TState<T, U>);
var
  i: Integer;
  Transition: TStateTransition<T,U>;
begin
  Assert(FComparer <> nil);
  Assert(FComparer.Compare(RangeStart, RangeEnd) <= 0, 'Invalid Range. Start must be lesser or equal than End');
  Assert(FComparer.Compare(RangeEnd, RangeStart) >= 0, 'Invalid Range. End must be greater or equal than Start');
  Assert(NextState <> nil, 'Next State must be defined');

  for i := 0 to FTransitions.Count - 1 do
  begin
    Transition := FTransitions[i];

    // System.Types

    if (FComparer.Compare(RangeStart, Transition.EndCondition) <= 0) and
       (FComparer.Compare(RangeEnd, Transition.StartCondition) >= 0) then
      raise EStateMachineException.Create('Cant add this State Transition because the condition intersects with another one');
  end;

  Transition.StartCondition := RangeStart;
  Transition.EndCondition := RangeEnd;
  Transition.NextState := NextState;
  FTransitions.Add(Transition);
  FTransitionsDirty := True;

  if Length(FTransitionsArray) <> 0 then
    SetLength(FTransitionsArray, 0);
end;

constructor TState<T, U>.Create(Comparer: IComparer<T>);
begin
  FComparer := Comparer;
  FTransitions := TList<TStateTransition<T, U>>.Create;

  Assert(FComparer <> nil);
end;

destructor TState<T, U>.Destroy;
begin
  if Length(FTransitionsArray) <> 0 then
    SetLength(FTransitionsArray, 0);

  FTransitions.DisposeOf;
  inherited;
end;

procedure TState<T, U>.SetFallbackTransition(
  const FallbackNextState: TState<T, U>);
begin
  FFallbackTransitionNextState := FallbackNextState;
end;

procedure TState<T, U>.SortTransitions;
begin
  if not FTransitionsDirty then
    Exit;

  FTransitionsDirty := False;
  Assert(FTransitions <> nil);
  Assert(FComparer <> nil);

  FTransitions.Sort(
    TComparer<TStateTransition<T,U>>.Construct(
      function(const Left, Right: TStateTransition<T,U>): Integer
      begin
        Result := FComparer.Compare(Left.StartCondition, Right.StartCondition);
      end));

  FTransitionsArray := FTransitions.ToArray;
end;

function TState<T, U>.TryLocateNextState(Value: T; out NextState: TState<T, U>): Boolean;
var
  Transition: TStateTransition<T,U>;
  L, H, Middle: NativeInt;
begin
  if FTransitionsDirty then
    Self.SortTransitions();

  // Binary Search
  L := 0;
  H := Length(FTransitionsArray) - 1;

  while L <= H do
  begin
    Middle := (L + H) div 2;
    Transition := FTransitionsArray[Middle];

    if FComparer.Compare(Value, Transition.StartCondition) < 0 then
      H := Middle - 1
    else
      if FComparer.Compare(Value, Transition.EndCondition) > 0 then
        L := Middle + 1
      else
      begin
        // Found

        NextState := Transition.NextState;
        Assert(NextState <> nil);
        Exit(True);
      end;
  end;

  if FFallbackTransitionNextState <> nil then
  begin
    NextState := FFallbackTransitionNextState;
    Result := True;
  end else
    Result := False;
end;

end.
