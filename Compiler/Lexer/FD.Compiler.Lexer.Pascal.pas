unit FD.Compiler.Lexer.Pascal;

interface

uses
  System.SyncObjs, FD.Compiler.Lexer, FD.Compiler.Lexer.Tokens,
  FD.Compiler.Environment, FD.Compiler.StateMachine, System.SysUtils;

type
  TPascalLexerRules = class(TLexerRules)
  public
    const
      BLOCKTYPE_MAIN = TLexerRules.BLOCKTYPE_MAIN;
      BLOCKTYPE_PROPERTY_DECLARATION = 1;
      BLOCKTYPE_FUNCTION_DECLARATION = 2;
      BLOCKTYPE_TYPE_DECLARATION = 3;
  protected
    // FToken States
    FTokenState_Initial: TTokenState;
    FTokenState_Identifier: TTokenState;
    FTokenState_WhiteSpace: TTokenState;
    FTokenState_Operator: TTokenState;
    FTokenState_DecimalPositiveInteger: TTokenState;
    FTokenState_MalformedWordToken: TTokenState;

    FTokenState_MaybeFloatDotChar: TTokenState;
    FTokenState_MaybeFloatEChar: TTokenState;
    FTokenState_MaybeFloatECharAndMinusSignal: TTokenState;
    FTokenState_FloatAfterDot: TTokenState;
    FTokenState_FloatAfterEMarker: TTokenState;

    FTokenState_UnterminatedBuildingLiteralString: TTokenState;
    FTokenState_UnterminatedAtEndOfLineLiteralString: TTokenState;
    FTokenState_TerminatedLiteralString: TTokenState;
  protected
    procedure EnumerateBlockTypes;
    procedure EnumerateKeywords;
    procedure MountTokenStateMachine;

    procedure StateUtil_AddBreaklineTransition(const FromState, ToState: TTokenState);
    procedure StateUtil_AddWhiteSpaceTransition(const FromState, ToState: TTokenState);

    procedure MountTokenStateMachine_Identifier;
    procedure MountTokenStateMachine_WhiteSpace;
    procedure MountTokenStateMachine_Operator;
    procedure MountTokenStateMachine_DecimalPositiveInteger;
    procedure MountTokenStateMachine_LiteralString;
    procedure MountTokenStateMachine_FloatNumber;
    procedure MountTokenStateMachine_MalformedWordToken;

    function ParsePascalQuotedString(const InputString: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignParsedValueForToken(var Token: TToken); override;
  end;

  (* TPascalLexer is a class that purposes is to interpret correctly
    the "compiler directives"/"Lexer directives" for pascal sintax  *)

  TPascalLexer = class(TLexer)
  private
    class var
      FPascalRules: TPascalLexerRules;
      FCriticalSection: TCriticalSection;

    class procedure InitializePascalLexerClass;
    class procedure FinalizePascalLexerClass;
  protected
    function InstanciateRules: TLexerRules; override;
    procedure DisposeRules(const Rules: TLexerRules); override;
  public
    constructor Create(Environment: TCompilerEnvironment); override;
    destructor Destroy; override;
  end;

const
  PASCAL_TOKEN_MALFORMED_UNTERMINATED_STRING = 'Unterminated String';

implementation

{ TPascalLexer }


{ TPascalLexer }

constructor TPascalLexer.Create(Environment: TCompilerEnvironment);
begin
  inherited Create(Environment);

end;

destructor TPascalLexer.Destroy;
begin

  inherited;
end;

procedure TPascalLexer.DisposeRules(const Rules: TLexerRules);
begin
  // Dont call inherited DisposeRules
end;

class procedure TPascalLexer.InitializePascalLexerClass;
begin
  TPascalLexer.FCriticalSection := TCriticalSection.Create;
end;

class procedure TPascalLexer.FinalizePascalLexerClass;
begin
  TPascalLexer.FCriticalSection.Free;
end;

function TPascalLexer.InstanciateRules: TLexerRules;
begin
  if TPascalLexer.FPascalRules = nil then
  begin
    TPascalLexer.FCriticalSection.Enter;

    try
      if TPascalLexer.FPascalRules = nil then // Because of Race Condition, this may not be nil here
        TPascalLexer.FPascalRules := TPascalLexerRules.Create;
    finally
      TPascalLexer.FCriticalSection.Leave;
    end;
  end;

  Result := TPascalLexer.FPascalRules;
end;

{ TPascalLexerRules }

procedure TPascalLexerRules.AssignParsedValueForToken(var Token: TToken);
var
  ValInt64: Int64;
  ValDouble: Double;
begin
  inherited;

  case Token.TokenType of
    ttLiteralInteger:
    begin
      if not TryStrToInt64(Token.InputString, ValInt64) then
        raise ETokenException.Create(Token, Format('"%s" is not a valid Literal Integer', [Token.InputString]));

      Token.ParsedValueAsInt64 := ValInt64;
    end;

    ttLiteralFloat:
    begin
      if not TryStrToFloat(Token.InputString, ValDouble, FFormatSettings) then
        raise ETokenException.Create(Token, Format('"%s" is not a valid Literal Float', [Token.InputString]));

      Token.ParsedValueAsFloat := ValDouble;
    end;

    ttLiteralString:
      Token.ParsedValueAsString := Self.ParsePascalQuotedString(Token.InputString);
  end;
end;

constructor TPascalLexerRules.Create;
begin
  inherited Create;
  Self.IsCaseSensitive := False;
  Self.EnumerateBlockTypes();
  Self.EnumerateKeywords();
  Self.MountTokenStateMachine();

  FFormatSettings := TFormatSettings.Create('en-US');
  FFormatSettings.ThousandSeparator := ',';
  FFormatSettings.DecimalSeparator := '.';
end;

destructor TPascalLexerRules.Destroy;
begin

  inherited;
end;

procedure TPascalLexerRules.EnumerateBlockTypes;
begin
  Self.AddBlockType(BLOCKTYPE_PROPERTY_DECLARATION);
  Self.AddBlockType(BLOCKTYPE_FUNCTION_DECLARATION);
  Self.AddBlockType(BLOCKTYPE_TYPE_DECLARATION);
end;

procedure TPascalLexerRules.EnumerateKeywords;
begin
  // Pascal
  Self.AddKeyWord('array');
  Self.AddKeyword('asm');
  Self.AddKeyWord('begin');
  Self.AddKeyWord('break');
  Self.AddKeyWord('case');
  Self.AddKeyword('const');
  Self.AddKeyword('constructor');
  Self.AddKeyword('continue');
  Self.AddKeyword('destructor');
  Self.AddKeyword('div');
  Self.AddKeyword('do');
  Self.AddKeyword('downto');
  Self.AddKeyword('else');
  Self.AddKeyword('end');
  Self.AddKeyword('false');
  Self.AddKeyword('file');
  Self.AddKeyword('for');
  Self.AddKeyword('function');
  Self.AddKeyword('goto');
  Self.AddKeyword('if');
  Self.AddKeyword('implementation');
  Self.AddKeyword('in');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'inline');
  Self.AddKeyword(BLOCKTYPE_TYPE_DECLARATION, 'interface');
  Self.AddKeyword('mod');
  Self.AddKeyword('nil');
  Self.AddKeyword('not');
  Self.AddKeyword('object');
  Self.AddKeyword('of');
  Self.AddKeyword('operator');
  Self.AddKeyword('or');
  Self.AddKeyword('packed');
  Self.AddKeyword('procedure');
  Self.AddKeyword('program');
  Self.AddKeyword('record');
  Self.AddKeyword('repeat');
  Self.AddKeyword('set');
  Self.AddKeyword('shl');
  Self.AddKeyword('shr');
  Self.AddKeyword('string');
  Self.AddKeyword('then');
  Self.AddKeyword('to');
  Self.AddKeyword('true');
  Self.AddKeyword('type');
  Self.AddKeyword('unit');
  Self.AddKeyword('until');
  Self.AddKeyword('uses');
  Self.AddKeyword('var');
  Self.AddKeyword('while');
  Self.AddKeyword('with');
  Self.AddKeyword('xor');
  Self.AddKeyword('as');
  Self.AddKeyword('class');
  Self.AddKeyword('dispose');
  Self.AddKeyword('except');
  Self.AddKeyword('exit');
  Self.AddKeyword('exports');
  Self.AddKeyword('finalization');
  Self.AddKeyword('finally');
  Self.AddKeyword('inherited');
  Self.AddKeyword('initialization');
  Self.AddKeyword('is');
  Self.AddKeyword('library');
  Self.AddKeyword('on');
  Self.AddKeyword('out');
  Self.AddKeyword('property');
  Self.AddKeyword('raise');
  Self.AddKeyword('self');
  Self.AddKeyword('threadvar');
  Self.AddKeyword('try');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'abstract');
  Self.AddKeyword('alias');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'assembler');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'cdecl');
  Self.AddKeyword('default');
  Self.AddKeyword('export');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'external');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'forward');
  Self.AddKeyword('generic');
  Self.AddKeyword(BLOCKTYPE_PROPERTY_DECLARATION, 'index');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'name');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'override');
  Self.AddKeyword('pascal');
  Self.AddKeyword(BLOCKTYPE_TYPE_DECLARATION, 'private');
  Self.AddKeyword(BLOCKTYPE_TYPE_DECLARATION, 'protected');
  Self.AddKeyword(BLOCKTYPE_TYPE_DECLARATION, 'public');
  Self.AddKeyword(BLOCKTYPE_TYPE_DECLARATION, 'published');
  Self.AddKeyword(BLOCKTYPE_PROPERTY_DECLARATION, 'read');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'register');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'reintroduce');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'safecall');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'stdcall');
  Self.AddKeyword(BLOCKTYPE_FUNCTION_DECLARATION, 'virtual');
  Self.AddKeyword(BLOCKTYPE_PROPERTY_DECLARATION, 'write');
end;

procedure TPascalLexerRules.MountTokenStateMachine;
begin
  FTokenState_Initial := Self.TokenStateMachine.InitialState;

  FTokenState_Identifier := Self.TokenStateMachine.CreateNewState();
  FTokenState_Identifier.IsFinal := True;
  FTokenState_Identifier.Data := ttIdentifier;

  FTokenState_WhiteSpace := Self.TokenStateMachine.CreateNewState();
  FTokenState_WhiteSpace.IsFinal := True;
  FTokenState_WhiteSpace.Data := ttWhiteSpace;

  FTokenState_Operator := Self.TokenStateMachine.CreateNewState();
  FTokenState_Operator.IsFinal := True;
  FTokenState_Operator.Data := ttOperator;

  FTokenState_DecimalPositiveInteger := Self.TokenStateMachine.CreateNewState();
  FTokenState_DecimalPositiveInteger.IsFinal := True;
  FTokenState_DecimalPositiveInteger.Data := ttLiteralInteger;

  FTokenState_MalformedWordToken := Self.TokenStateMachine.CreateNewState();
  FTokenState_MalformedWordToken.IsFinal := True;
  FTokenState_MalformedWordToken.Data := ttMalformedToken;

  FTokenState_FloatAfterDot := Self.TokenStateMachine.CreateNewState();
  FTokenState_FloatAfterDot.IsFinal := True;
  FTokenState_FloatAfterDot.Data := ttLiteralFloat;

  FTokenState_FloatAfterEMarker := Self.TokenStateMachine.CreateNewState();
  FTokenState_FloatAfterEMarker.IsFinal := True;
  FTokenState_FloatAfterEMarker.Data := ttLiteralFloat;

  FTokenState_MaybeFloatDotChar := Self.TokenStateMachine.CreateNewState();
  FTokenState_MaybeFloatDotChar.IsFinal := False;

  FTokenState_MaybeFloatEChar := Self.TokenStateMachine.CreateNewState();
  FTokenState_MaybeFloatEChar.IsFinal := False;

  FTokenState_MaybeFloatECharAndMinusSignal := Self.TokenStateMachine.CreateNewState();
  FTokenState_MaybeFloatECharAndMinusSignal.IsFinal := False;

  FTokenState_UnterminatedBuildingLiteralString := Self.TokenStateMachine.CreateNewState();
  FTokenState_UnterminatedBuildingLiteralString.IsFinal := True;
  FTokenState_UnterminatedBuildingLiteralString.Data := TTokenStateData.Create(ttMalformedToken, PASCAL_TOKEN_MALFORMED_UNTERMINATED_STRING);

  FTokenState_UnterminatedAtEndOfLineLiteralString := Self.TokenStateMachine.CreateNewState();
  FTokenState_UnterminatedAtEndOfLineLiteralString.IsFinal := False;
  FTokenState_UnterminatedAtEndOfLineLiteralString.Data := TTokenStateData.Create(ttMalformedToken, PASCAL_TOKEN_MALFORMED_UNTERMINATED_STRING);

  FTokenState_TerminatedLiteralString := Self.TokenStateMachine.CreateNewState();
  FTokenState_TerminatedLiteralString.IsFinal := True;
  FTokenState_TerminatedLiteralString.Data := ttLiteralString;

  Self.MountTokenStateMachine_Identifier();
  Self.MountTokenStateMachine_WhiteSpace();
  Self.MountTokenStateMachine_Operator();
  Self.MountTokenStateMachine_DecimalPositiveInteger();
  Self.MountTokenStateMachine_LiteralString();
  Self.MountTokenStateMachine_FloatNumber();
  Self.MountTokenStateMachine_MalformedWordToken();
end;

procedure TPascalLexerRules.MountTokenStateMachine_DecimalPositiveInteger;
begin
  FTokenState_Initial.AddTransition('0', '9', FTokenState_DecimalPositiveInteger);
  FTokenState_DecimalPositiveInteger.AddTransition('0', '9', FTokenState_DecimalPositiveInteger);

  // Malformed Continuation
  FTokenState_DecimalPositiveInteger.AddTransition('A', 'D', FTokenState_MalformedWordToken);
  FTokenState_DecimalPositiveInteger.AddTransition('F', 'Z', FTokenState_MalformedWordToken);
  FTokenState_DecimalPositiveInteger.AddTransition('a', 'd', FTokenState_MalformedWordToken);
  FTokenState_DecimalPositiveInteger.AddTransition('f', 'z', FTokenState_MalformedWordToken);
  FTokenState_DecimalPositiveInteger.AddTransition('_', FTokenState_MalformedWordToken);

  // Maybe a float.

  FTokenState_DecimalPositiveInteger.AddTransition('.', FTokenState_MaybeFloatDotChar);
  FTokenState_DecimalPositiveInteger.AddTransition('e', FTokenState_MaybeFloatEChar);
  FTokenState_DecimalPositiveInteger.AddTransition('E', FTokenState_MaybeFloatEChar);
end;

procedure TPascalLexerRules.MountTokenStateMachine_FloatNumber;
begin
  // Maybefloat after a dot char
  FTokenState_MaybeFloatDotChar.AddTransition('0', '9', FTokenState_FloatAfterDot);

  // Maybefloat after E char
  FTokenState_MaybeFloatEChar.AddTransition('0', '9', FTokenState_FloatAfterEMarker);
  FTokenState_MaybeFloatEChar.AddTransition('-', FTokenState_MaybeFloatECharAndMinusSignal);
  FTokenState_MaybeFloatEChar.SetFallbackTransition(FTokenState_MalformedWordToken);

  FTokenState_MaybeFloatECharAndMinusSignal.AddTransition('0', '9', FTokenState_FloatAfterEMarker);
  FTokenState_MaybeFloatECharAndMinusSignal.SetFallbackTransition(FTokenState_MalformedWordToken);

  // After DOT marker
  FTokenState_FloatAfterDot.AddTransition('0', '9', FTokenState_FloatAfterDot);
  FTokenState_FloatAfterDot.AddTransition('e', FTokenState_MaybeFloatEChar);
  FTokenState_FloatAfterDot.AddTransition('E', FTokenState_MaybeFloatEChar);
  FTokenState_FloatAfterDot.AddTransition('A', 'D', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterDot.AddTransition('F', 'Z', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterDot.AddTransition('a', 'd', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterDot.AddTransition('f', 'z', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterDot.AddTransition('_', FTokenState_MalformedWordToken);

  // After E Char marker
  FTokenState_FloatAfterEMarker.AddTransition('0', '9', FTokenState_FloatAfterEMarker);
  FTokenState_FloatAfterEMarker.AddTransition('A', 'Z', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterEMarker.AddTransition('a', 'z', FTokenState_MalformedWordToken);
  FTokenState_FloatAfterEMarker.AddTransition('_', FTokenState_MalformedWordToken);
end;

procedure TPascalLexerRules.MountTokenStateMachine_Identifier;
begin
  // Initial State
  FTokenState_Initial.AddTransition('A', 'Z', FTokenState_Identifier);
  FTokenState_Initial.AddTransition('a', 'z', FTokenState_Identifier);
  FTokenState_Initial.AddTransition('_', FTokenState_Identifier);

  // Loopback state
  FTokenState_Identifier.AddTransition('A', 'Z', FTokenState_Identifier);
  FTokenState_Identifier.AddTransition('a', 'z', FTokenState_Identifier);
  FTokenState_Identifier.AddTransition('_', FTokenState_Identifier);
  FTokenState_Identifier.AddTransition('0', '9', FTokenState_Identifier);
end;

procedure TPascalLexerRules.MountTokenStateMachine_LiteralString;
begin
  // Initial to Unterminated Building String
  FTokenState_Initial.AddTransition('''', FTokenState_UnterminatedBuildingLiteralString);

  // unterminated building string to ... loopback, terminated string or unterminated at end of line
  FTokenState_UnterminatedBuildingLiteralString.AddTransition('''', FTokenState_TerminatedLiteralString);
  Self.StateUtil_AddBreaklineTransition(FTokenState_UnterminatedBuildingLiteralString, FTokenState_UnterminatedAtEndOfLineLiteralString);
  FTokenState_UnterminatedBuildingLiteralString.SetFallbackTransition(FTokenState_UnterminatedBuildingLiteralString);

  // Terminated String - Sequence of two ' ('') must be parsed as single in-content '
  FTokenState_TerminatedLiteralString.AddTransition('''', FTokenState_UnterminatedBuildingLiteralString);
end;

procedure TPascalLexerRules.MountTokenStateMachine_MalformedWordToken;
begin
  FTokenState_MalformedWordToken.AddTransition('A', 'Z', FTokenState_MalformedWordToken);
  FTokenState_MalformedWordToken.AddTransition('a', 'z', FTokenState_MalformedWordToken);
  FTokenState_MalformedWordToken.AddTransition('_', FTokenState_MalformedWordToken);
  FTokenState_MalformedWordToken.AddTransition('0', '9', FTokenState_MalformedWordToken);
end;

procedure TPascalLexerRules.MountTokenStateMachine_WhiteSpace;
begin
  // Initial State
  Self.StateUtil_AddBreaklineTransition(FTokenState_Initial, FTokenState_WhiteSpace);
  Self.StateUtil_AddWhiteSpaceTransition(FTokenState_Initial, FTokenState_WhiteSpace);

  // Loopback state
  Self.StateUtil_AddBreaklineTransition(FTokenState_WhiteSpace, FTokenState_WhiteSpace);
  Self.StateUtil_AddWhiteSpaceTransition(FTokenState_WhiteSpace, FTokenState_WhiteSpace);
end;

function TPascalLexerRules.ParsePascalQuotedString(const InputString: String): String;
var
  StrB: TStringBuilder;
  i, MaxPosToSeek: NativeInt;
  C: Char;
begin
  Assert(Length(InputString) >= 2);
  Assert(InputString.Chars[0] = '''');
  Assert(InputString.Chars[Length(InputString) - 1] = '''');

  StrB := TStringBuilder.Create;

  try
    i := 1;
    MaxPosToSeek := Length(InputString) - 2;

    while i <= MaxPosToSeek do
    begin
      C := InputString.Chars[i];

      if C = '''' then
      begin
        Assert(i < MaxPosToSeek);
        Assert(InputString.Chars[i + 1] = '''');
        StrB.Append(C);
        Inc(i, 2);
      end else
      begin
        Assert((C <> #13) and (C <> #10) and (C <> #$85));
        StrB.Append(C);
        Inc(i);
      end;
    end;

    Result := StrB.ToString;
  finally
    StrB.DisposeOf;
  end;
end;

procedure TPascalLexerRules.StateUtil_AddBreaklineTransition(const FromState,
  ToState: TTokenState);
begin
  Assert(FromState <>  nil);
  Assert(ToState <> nil);
  FromState.AddTransition(#13, ToState);
  FromState.AddTransition(#10, ToState);
  FromState.AddTransition(#$85, ToState);
  FromState.AddTransition(#13#10, ToState);
end;

procedure TPascalLexerRules.StateUtil_AddWhiteSpaceTransition(const FromState,
  ToState: TTokenState);
begin
  Assert(FromState <>  nil);
  Assert(ToState <> nil);
  FromState.AddTransition(' ', ToState);
  FromState.AddTransition(#160, ToState);
end;

procedure TPascalLexerRules.MountTokenStateMachine_Operator;
begin
  FTokenState_Initial.AddTransition(';', FTokenState_Operator);
  FTokenState_Initial.AddTransition('+', FTokenState_Operator);
  FTokenState_Initial.AddTransition('-', FTokenState_Operator);
  FTokenState_Initial.AddTransition('(', FTokenState_Operator);
  FTokenState_Initial.AddTransition(')', FTokenState_Operator);
  FTokenState_Initial.AddTransition('[', FTokenState_Operator);
  FTokenState_Initial.AddTransition(']', FTokenState_Operator);
  FTokenState_Initial.AddTransition('@', FTokenState_Operator);
  FTokenState_Initial.AddTransition('*', FTokenState_Operator);
  FTokenState_Initial.AddTransition('/', FTokenState_Operator);
  FTokenState_Initial.AddTransition('.', FTokenState_Operator);
  FTokenState_Initial.AddTransition('=', FTokenState_Operator);
end;

initialization
  TPascalLexer.InitializePascalLexerClass();

finalization
  TPascalLexer.FinalizePascalLexerClass();

end.
