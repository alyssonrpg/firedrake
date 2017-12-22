unit FD.Compiler.Lexer.Tokens;

interface

uses
  System.RTTI, RRPG.Unicode, FD.Compiler.Exceptions;

type
  TTokenType = (ttUnknown,           // Token with unidentified type
                ttKeyword,            // Reserved word, like "unit", "begin", "if" on Pascal
                ttIdentifier,        // Something that is not a Literal value, like a variable name
                ttOperator,          // A separator or operator char/string, like "(", ";", ",", "->", "+", "-"
                ttLiteralInteger,    // A integer value, like "25", "$256", "0x656"
                ttLiteralString,     // A string value, like "Test"
                ttLiteralFloat, // A float point value, like "25.56" or "10e-5";
                ttLiteralBoolean,    // A literal boolean value. "True" or "False"
                ttLiteralChar,        // A literal char value
                ttCommentary,         // A commentary block
                ttPreprocessorDirective, // Something like {$I ....} or {$R ...} {$M+}, etc..
                ttWhiteSpace,
                ttMalformedToken,
                ttEOF                 // End of Content
                );

  TFileRange = record
    FileName: String;

    LineIndex: NativeInt;
    ColumnIndex: NativeInt;  // In Glyph Index

    UTF16CharStartIndex: NativeInt;
    UTF16CharCount: Integer;
    GlyphStartIndex: NativeInt;
    GlyphCount: Integer;

    procedure Initialize;
  end;

  TCodeLocation = record
    RealLocation: TFileRange;
    StackLocation: TArray<TFileRange>;  // Used mostly when the code use a #include like operation and the token is located within that included file.

    procedure Initialize;
  end;

  TTokenFlag = (tfCaseSensitive);
  TTokenFlags = set of TTokenFlag;

  TToken = record
  private
    function GetParsedValueAsInt64: Int64;
    procedure SetParsedValueAsInt64(const V: Int64);

    function GetParsedValueAsFloat: Double;
    procedure SetParsedValueAsFloat(const Value: Double);
  public
    TokenType: TTokenType;
    Location: TCodeLocation;
    InputString: String;  // String containing complete token. Eg: In a commentary, Value will contain the text and Complete Token will contain the opening/closing characters of the commentary
    ParsedValue: TValue;
    Flags: TTokenFlags;

    procedure Initialize;

    function IsKeyword(const Keyword: String): Boolean;

    function IsIdentifier: Boolean; overload; inline;
    function IsIdentifier(const IdentifierName: String): Boolean; overload;

    function IsOperator: Boolean; overload; inline;
    function IsOperator(const OperatorSymbol: String): Boolean; overload;

    function IsLiteralInteger: Boolean; overload; inline;
    function IsLiteralInteger(const Value: Int64): Boolean; overload;

    function IsLiteralFloat: Boolean; overload; inline;
    function IsLiteralFloat(const Value: Double): Boolean; overload;

    function IsMalformedToken: Boolean; overload; inline;
    function IsMalformedToken(const InputString: String): Boolean; overload;

    function IsWhiteSpace: Boolean; inline;
    function IsEOF: Boolean; inline;

    class function CreateEOF(const CodeLocation: TCodeLocation): TToken; static;

    property ParsedValueAsInt64: Int64 read GetParsedValueAsInt64 write SetParsedValueAsInt64;
    property ParsedValueAsFloat: Double read GetParsedValueAsFloat write SetParsedValueAsFloat;
  end;

  ECodeLocationException = class(EFDException)
  protected
    FCodeLocation: TCodeLocation;
  public
    constructor Create(CodeLocation: TCodeLocation; const Msg: String); reintroduce;

    property CodeLocation: TCodeLocation read FCodeLocation;
  end;

  ETokenException = class(ECodeLocationException)
  protected
    FToken: TToken;
  public
    constructor Create(Token: TToken; const Msg: String); reintroduce;

    property Token: TToken read FToken;
  end;

implementation

uses
  System.Math;

{ TToken }

class function TToken.CreateEOF(const CodeLocation: TCodeLocation): TToken;
begin
  Result.Initialize;
  Result.TokenType := ttEOF;
  Result.Location := CodeLocation;
end;

function TToken.GetParsedValueAsFloat: Double;
begin
  Assert(not Self.ParsedValue.IsEmpty);
  Assert(Self.ParsedValue.IsType<Double>);
  Result := Self.ParsedValue.AsType<Double>;
end;

function TToken.GetParsedValueAsInt64: Int64;
begin
  Assert(not Self.ParsedValue.IsEmpty);
  Assert(Self.ParsedValue.IsType<Int64>);
  Result := Self.ParsedValue.AsType<Int64>;
end;

procedure TToken.Initialize;
begin
  Self.TokenType := TTokenType.ttUnknown;
  Self.InputString := '';
  Self.Location.Initialize;
  Self.ParsedValue := TValue.Empty;
  Self.Flags := [];
end;

function TToken.IsIdentifier: Boolean;
begin
  Result := Self.TokenType = ttIdentifier;
end;

function TToken.IsEOF: Boolean;
begin
  Result := Self.TokenType = ttEOF;
end;

function TToken.IsIdentifier(const IdentifierName: String): Boolean;
begin
  if Self.TokenType = ttIdentifier then
  begin
    if TTokenFlag.tfCaseSensitive in Self.Flags then
      Result := Self.InputString = IdentifierName
    else
      Result := TUnicode.Compare(Self.InputString, IdentifierName, [TUnicodeCollateOption.ucoIgnoreCase]) = 0;
  end else
    Result := False;
end;

function TToken.IsKeyword(const Keyword: String): Boolean;
begin
  if Self.TokenType = ttKeyword then
  begin
    if TTokenFlag.tfCaseSensitive in Self.Flags then
      Result := Self.InputString = Keyword
    else
      Result := TUnicode.Compare(Self.InputString, Keyword, [TUnicodeCollateOption.ucoIgnoreCase]) = 0;
  end else
    Result := False;
end;

function TToken.IsLiteralInteger: Boolean;
begin
  Result := Self.TokenType = ttLiteralInteger;
end;

function TToken.IsLiteralFloat: Boolean;
begin
  Result := Self.TokenType = ttLiteralFloat;
end;

function TToken.IsLiteralFloat(const Value: Double): Boolean;
begin
  if Self.TokenType = ttLiteralFloat then
    Result := SameValue(Self.ParsedValueAsFloat, Value)
  else
    Result := False;
end;

function TToken.IsLiteralInteger(const Value: Int64): Boolean;
begin
  if Self.TokenType = ttLiteralInteger then
    Result := Self.GetParsedValueAsInt64() = Value
  else
    Result := False;
end;

function TToken.IsMalformedToken: Boolean;
begin
  Result := Self.TokenType = ttMalformedToken;
end;

function TToken.IsMalformedToken(const InputString: String): Boolean;
begin
  if Self.TokenType = ttMalformedToken then
  begin
    if TTokenFlag.tfCaseSensitive in Self.Flags then
      Result := Self.InputString = InputString
    else
      Result := TUnicode.Compare(Self.InputString, InputString, [TUnicodeCollateOption.ucoIgnoreCase]) = 0;
  end else
    Result := False;
end;

function TToken.IsOperator: Boolean;
begin
  Result := Self.TokenType = ttOperator;
end;

function TToken.IsOperator(const OperatorSymbol: String): Boolean;
begin
  if Self.TokenType = ttOperator then
  begin
    if TTokenFlag.tfCaseSensitive in Self.Flags then
      Result := Self.InputString = OperatorSymbol
    else
      Result := TUnicode.Compare(Self.InputString, OperatorSymbol, [TUnicodeCollateOption.ucoIgnoreCase]) = 0;
  end else
    Result := False;
end;

function TToken.IsWhiteSpace: Boolean;
begin
  Result := Self.TokenType = ttWhiteSpace;
end;

procedure TToken.SetParsedValueAsFloat(const Value: Double);
begin
  Self.ParsedValue := TValue.From<Double>(Value);
end;

procedure TToken.SetParsedValueAsInt64(const V: Int64);
begin
  Self.ParsedValue := TValue.From<Int64>(V);
end;

{ TCodeLocation }

procedure TCodeLocation.Initialize;
begin
  Self.RealLocation.Initialize;
  SetLength(Self.StackLocation, 0);
end;

{ TFileRange }

procedure TFileRange.Initialize;
begin
  Self.FileName := '';
  Self.UTF16CharStartIndex := -1;
  Self.UTF16CharCount := 0;
  Self.GlyphStartIndex := -1;
  Self.GlyphCount := 0;
  Self.LineIndex := -1;
  Self.ColumnIndex := -1;
end;

{ ECodeLocationException }

constructor ECodeLocationException.Create(CodeLocation: TCodeLocation;
  const Msg: String);
begin
  inherited Create(Msg);
  FCodeLocation := CodeLocation;
end;

{ ETokenException }

constructor ETokenException.Create(Token: TToken; const Msg: String);
begin
  inherited Create(Token.Location, Msg);
  FToken := Token;
end;

end.
