unit FD.Compiler.Lexer;

interface

uses
  // Refactor Units
  RRPG.Unicode, RRPG.UTF16Glyphs, RRPG.UTF16Glyphs.UnicodeData,
  System.SysUtils, System.Classes, System.Generics.Collections, System.BTree,
  FD.Compiler.Lexer.Tokens, FD.Compiler.Environment, FD.Compiler.Exceptions,
  System.Comparers, FD.Compiler.StateMachine;

type
  TLexerCharPosInfo = packed record
    LineIndex: NativeInt;
    ColumnIndex: NativeInt;
  end;

  PLexerCharPosInfo = ^TLexerCharPosInfo;

  TLexerContent = class(TObject)
  private
  protected
    FEnvironment: TCompilerEnvironment;
    FUnfoldName: String;
    FAbsoluteFileName: String;
    FContent: String;
    FContentFirstCharPtr: PChar;

    FCharsInfos: TArray<TLexerCharPosInfo>;

    FCurrentCharIdx: NativeInt;
    FCurrentCharIdxStack: TStack<NativeInt>;

    function GetCharCount: NativeInt; virtual;
    function GetCharAt(const Index: NativeInt): Char; virtual;
    function GetCharPtrAt(const Index: NativeInt): PChar; virtual;
    procedure SetCurrentCharIdx(const Value: NativeInt); virtual;
  public
    constructor Create(Environment: TCompilerEnvironment;
                       LogicalUnfoldName, FullPathFileName: String);
    destructor Destroy; override;

    procedure LoadFromBytes(Data: TBytes; Encoding: TEncoding); virtual;
    procedure LoadFromBytesDiscoverEncoding(Data: TBytes); virtual;
    procedure LoadFromStream(const Stream: TStream; Encoding: TEncoding); virtual;
    procedure LoadFromStreamDiscoverEncoding(const Stream: TStream); virtual;

    function Current(const DeltaOffset: NativeInt = 0): Char; inline;
    function Next(const DeltaOffset: NativeInt = 1): Char; inline;
    function Previous(const DeltaOffset: NativeInt = -1): Char; inline;

    procedure PushCurrentCharIdx(const NewIndex: NativeInt);
    procedure PopCurrentCharIdx;

    function CreateFileRange(const StartCharIndex, CharCount: NativeInt): TFileRange; virtual;
    function SubString(const StartCharIndex, CharCount: NativeInt): String; overload; virtual;
    function SubString(const Range: TFileRange): String; overload; virtual;

    property Environment: TCompilerEnvironment read FEnvironment;
    property UnfoldName: String read FUnfoldName;
    property AbsoluteFileName: String read FAbsoluteFileName;

    property CurrentCharIdx: NativeInt read FCurrentCharIdx write SetCurrentCharIdx;
    property CharCount: NativeInt read GetCharCount;
    property CharAt[const Index: NativeInt]: Char read GetCharAt;
    property CharPtrAt[const Index: NativeInt]: PChar read GetCharPtrAt;
  end;

  TBlockType = Integer;
  TKeyword = String;

  TTokenStateData = record
    TokenType: TTokenType;
    ErrorMessage: String;

    procedure Initialize;
    class function Create(const TokenType: TTokenType): TTokenStateData; overload; static;
    class function Create(const TokenType: TTokenType; const ErrorMsg: String): TTokenStateData; overload; static;

    class operator Implicit(const V: TTokenType): TTokenStateData;
    class operator Implicit(const V: TTokenStateData): TTokenType;
  end;

  TTokenState = TState<Char, TTokenStateData>;

  TLexerRules = class(TObject)
  public
    const
      BLOCKTYPE_MAIN = 0;

    type
      TBlockRules = class(TObject)
      protected
        FKeywords: TArvoreBSet<TKeyword>;
      public
        constructor Create(const IsCaseSensitive: Boolean);
        destructor Destroy; override;
      end;
  protected
    FIsCaseSensitive: Boolean;
    FBlockTypes: TDictionary<TBlockType, TBlockRules>;
    FTokenStateMachine: TStateMachine<Char, TTokenStateData>;
    FFormatSettings: TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignParsedValueForToken(var Token: TToken); virtual;
    procedure AddBlockType(BlockType: TBlockType);

    procedure AddKeyword(const Value: TKeyword); overload;
    procedure AddKeyword(BlockType: TBlockType; const Value: TKeyword); overload;

    function IsKeyword(const CurrentBlockType: TBlockType; const Value: TKeyword): Boolean;

    property IsCaseSensitive: Boolean read FIsCaseSensitive write FIsCaseSensitive;
    property TokenStateMachine: TStateMachine<Char, TTokenStateData> read FTokenStateMachine;
  end;

  (* TLexer is an abstract class that purposes is to interpret correctly
    the "compiler directives"/"Lexer directives" like
      - {$DEFINE} from pascal
      - #ifdef from C

    This class is abstract. See FD.Compiler.Lexer.Pascal.pas
  *)

  TLexer = class abstract(TObject)
  private
  protected
    FEnvironment: TCompilerEnvironment;
    FContentStack: TStack<TLexerContent>;
    FContent: TLexerContent;
    FRules: TLexerRules;
    FCurrentBlockType: TBlockType;

    function InstanciateRules: TLexerRules; virtual;
    procedure DisposeRules(const Rules: TLexerRules); virtual;

    function OpenLexerContent(const FileName: String): TLexerContent;

    function CreateCodeLocation(const GlyphStartIndex, GlyphCount: Integer): TCodeLocation; virtual;
    function GetEOF: Boolean; virtual;
  public
    constructor Create(Environment: TCompilerEnvironment); virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    function GetNextToken: TToken; virtual;
    procedure OpenFile(const FileName: String);

    property Environment: TCompilerEnvironment read FEnvironment;
    property CurrentBlockType: TBlockType read FCurrentBlockType write FCurrentBlockType;
    property EOF: Boolean read GetEOF;
  end;

implementation

{ TLexer }

procedure TLexer.AfterConstruction;
begin
  inherited;
  FRules := Self.InstanciateRules();
  Assert(FRules <> nil);
end;

constructor TLexer.Create(Environment: TCompilerEnvironment);
begin
  FEnvironment := Environment;
  Assert(FEnvironment <> nil);
  FContentStack := TStack<TLexerContent>.Create;
  FCurrentBlockType := TLexerRules.BLOCKTYPE_MAIN;
end;

function TLexer.CreateCodeLocation(const GlyphStartIndex,
  GlyphCount: Integer): TCodeLocation;
begin
  Assert(FContent <> nil);
  Result.Initialize;
  Result.RealLocation := FContent.CreateFileRange(GlyphStartIndex, GlyphCount);
end;

destructor TLexer.Destroy;
var
  SubContent: TLexerContent;
begin
  if FContent <> nil then
  begin
    FContent.DisposeOf;
    FContent := nil;
  end;

  if FContentStack <> nil then
  begin
    while FContentStack.Count > 0 do
    begin
      SubContent := FContentStack.Pop;
      SubContent.DisposeOf;
    end;

    FContentStack.DisposeOf;
    FContentStack := nil;
  end;

  if FRules <> nil then
  begin
    Self.DisposeRules(FRules);
    FRules := nil;
  end;

  inherited;
end;

procedure TLexer.DisposeRules(const Rules: TLexerRules);
begin
  Rules.DisposeOf;
end;

function TLexer.GetEOF: Boolean;
begin
  Result := FContent.CurrentCharIdx >= FContent.CharCount;
end;

function TLexer.GetNextToken: TToken;
var
  CurrentMachineState, NextMachineState, LastFinalMachineStateFound: TTokenState;
  i, FirstGlyphIndex, LastFinalMachineStateFoundGlyphIndex: NativeInt;
  C: Char;
  CurrentCharPtr: PChar;
  TokenStateData: TTokenStateData;
begin
  Assert(FContent <> nil);

  if FContent.CurrentCharIdx >= FContent.CharCount then
  begin
    Result := TToken.CreateEOF(Self.CreateCodeLocation(FContent.CurrentCharIdx, 0));
    Exit;
  end;

  Assert(FRules <> nil);
  Assert(FRules.FTokenStateMachine <> nil);
  CurrentMachineState := FRules.FTokenStateMachine.InitialState;
  Assert(CurrentMachineState <> nil);
  Assert(not CurrentMachineState.IsFinal);

  LastFinalMachineStateFound := nil;
  LastFinalMachineStateFoundGlyphIndex := -1;

  FirstGlyphIndex := FContent.CurrentCharIdx;
  i := FirstGlyphIndex;
  CurrentCharPtr := FContent.CharPtrAt[FirstGlyphIndex];

  while i < FContent.CharCount do
  begin
    C := CurrentCharPtr^;
    NextMachineState := nil;

    if CurrentMachineState.TryLocateNextState(C, NextMachineState) then
    begin
      Assert(NextMachineState <> nil);
      CurrentMachineState := NextMachineState;

      if CurrentMachineState.IsFinal then
      begin
        LastFinalMachineStateFound := CurrentMachineState;
        LastFinalMachineStateFoundGlyphIndex := i;
      end;
    end else
      Break;

    Inc(i);
    Inc(CurrentCharPtr);
  end;

  if (LastFinalMachineStateFound <> nil) then
  begin
    Assert(LastFinalMachineStateFoundGlyphIndex >= FirstGlyphIndex);
    TokenStateData := LastFinalMachineStateFound.Data;

    Result.Initialize;
    Result.TokenType := TokenStateData.TokenType;
    Result.Location := Self.CreateCodeLocation(FirstGlyphIndex, LastFinalMachineStateFoundGlyphIndex - FirstGlyphIndex + 1);

    if FRules.IsCaseSensitive then
      Include(Result.Flags, TTokenFlag.tfCaseSensitive);

    if TokenStateData.ErrorMessage <> '' then
    begin
      Assert(Result.TokenType = ttMalformedToken);
      Result.MalformedDescription := TokenStateData.ErrorMessage;
    end;
  end else
  begin
    Result.Initialize;
    Result.TokenType := ttUnknown;
    Result.Location := Self.CreateCodeLocation(FirstGlyphIndex, 1);
  end;

  if Result.TokenType <> ttWhiteSpace then
  begin
    Result.InputString := FContent.SubString(Result.Location.RealLocation);

    if (Result.TokenType = ttIdentifier) and (FRules.IsKeyword(FCurrentBlockType, Result.InputString)) then
      Result.TokenType := ttKeyword;

    FRules.AssignParsedValueForToken(Result);
  end;

  FContent.CurrentCharIdx := Result.Location.RealLocation.UTF16CharStartIndex + Result.Location.RealLocation.UTF16CharCount;
end;

function TLexer.InstanciateRules: TLexerRules;
begin
  Result := TLexerRules.Create;
end;

procedure TLexer.OpenFile(const FileName: String);
begin
  Assert(FContentStack <> nil);
  Assert(FContentStack.Count = 0);
  Assert(FContent = nil);
  FContent := Self.OpenLexerContent(FileName);
  Assert(FContent <> nil);
end;

function TLexer.OpenLexerContent(const FileName: String): TLexerContent;
var
  Stream: TStream;
  AbsoluteFileName: String;
begin
  Stream := FEnvironment.OpenFileForRead(FileName);

  try
    if not FEnvironment.ExpandFileName(FileName, AbsoluteFileName) then
      AbsoluteFileName := FileName;

    Result := TLexerContent.Create(FEnvironment, ExtractFileName(FileName), AbsoluteFileName);

    try
      Result.LoadFromStreamDiscoverEncoding(Stream);
    except
      Result.Free;
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

constructor TLexerContent.Create(Environment: TCompilerEnvironment;
  LogicalUnfoldName, FullPathFileName: String);
begin
  FEnvironment := Environment;
  FUnfoldName := LogicalUnfoldName;
  FAbsoluteFileName := FullPathFileName;

  FCurrentCharIdxStack := TStack<NativeInt>.Create;
end;

function TLexerContent.CreateFileRange(const StartCharIndex,
  CharCount: NativeInt): TFileRange;
var
  CharInfo: PLexerCharPosInfo;
begin
  Assert((StartCharIndex >= 0) and (StartCharIndex <= Length(FContent)));

  Result.Initialize;
  Result.FileName := Self.AbsoluteFileName;

  if StartCharIndex = Length(FContent) then
  begin
    Assert(CharCount = 0);
    Result.UTF16CharStartIndex := Length(FContent);
    Result.UTF16CharCount := 0;

    if Length(FCharsInfos) > 0 then
    begin
      CharInfo := @FCharsInfos[Length(FCharsInfos) - 1];
      Result.LineIndex := CharInfo.LineIndex;
      Result.ColumnIndex := CharInfo.ColumnIndex;
    end else
    begin
      Result.LineIndex := 0;
      Result.ColumnIndex := 0;
    end;
  end else
  begin
    Assert(StartCharIndex + CharCount <= Length(FContent));
    Assert(CharCount > 0);

    CharInfo := @FCharsInfos[StartCharIndex];

    Result.LineIndex := CharInfo.LineIndex;
    Result.ColumnIndex := CharInfo.ColumnIndex;
    Result.UTF16CharStartIndex := StartCharIndex;
    Result.UTF16CharCount := CharCount;
  end;
end;

function TLexerContent.Current(const DeltaOffset: NativeInt): Char;
begin
  Result := Self.CharAt[Self.CurrentCharIdx + DeltaOffset];
end;

destructor TLexerContent.Destroy;
begin
  FCurrentCharIdxStack.DisposeOf;
  inherited;
end;

function TLexerContent.GetCharAt(const Index: NativeInt): Char;
begin
  if (Index >= 0) and (Index < Length(FContent)) and (FContentFirstCharPtr <> nil) then
    Result := PChar(NativeUInt(FContentFirstCharPtr) + NativeUInt(Index * SizeOf(Char)))^
  else
    Result := #0;
end;

function TLexerContent.GetCharCount: NativeInt;
begin
  Result := Length(FContent);
end;

function TLexerContent.GetCharPtrAt(const Index: NativeInt): PChar;
begin
  if (Index >= 0) and (Index < Length(FContent)) and (FContentFirstCharPtr <> nil) then
    Result := PChar(NativeUInt(FContentFirstCharPtr) + NativeUInt(Index * SizeOf(Char)))
  else
    Result := #0;
end;

procedure TLexerContent.LoadFromBytes(Data: TBytes; Encoding: TEncoding);
var
  i, MaxLoopI: NativeInt;
  CurrentLineIndex, CurrentColumnIndex: NativeInt;
  CharPtr: PChar;
  CurrentC: Char;
  GlyphInfo: PLexerCharPosInfo;
const
  BREAK_LINE_CHARS = #13#10;
begin
  FContent := Encoding.GetString(Data);

  if FContent <> '' then
    FContentFirstCharPtr := PChar(Pointer(FContent))
  else
    FContentFirstCharPtr := nil;

  FCurrentCharIdx := 0;
  SetLength(FCharsInfos, Length(FContent));

  if Length(FContent) > 0 then
  begin
    GlyphInfo := @FCharsInfos[0];
    CurrentLineIndex := 0;
    CurrentColumnIndex := 0;
    CharPtr := FContentFirstCharPtr;

    i := 0;
    MaxLoopI := Length(FContent) - 1;

    while i <= MaxLoopI do
    begin
      GlyphInfo.LineIndex := CurrentLineIndex;
      GlyphInfo.ColumnIndex := CurrentColumnIndex;

      CurrentC := CharPtr^;

      if (CurrentC = #13) then
      begin
        if (i < MaxLoopI) and (SeekPChar(CharPtr, 1)^ = #10) then
        begin
          Inc(GlyphInfo);
          GlyphInfo.LineIndex := CurrentLineIndex;
          GlyphInfo.ColumnIndex := CurrentColumnIndex +1;

          Inc(CurrentLineIndex);
          CurrentColumnIndex := 0;

          Inc(GlyphInfo);
          Inc(CharPtr, 2);
          Inc(i, 2);
        end else
        begin
          Inc(CurrentLineIndex);
          CurrentColumnIndex := 0;
          Inc(CharPtr);
          Inc(i);
        end;
      end else
      begin
        if (CurrentC = #10) or (CurrentC = Char($85)) then
        begin
          Inc(CurrentLineIndex);
          CurrentColumnIndex := 0;
        end else
          Inc(CurrentColumnIndex);

        Inc(GlyphInfo); // Point to Next GlyphInf
        Inc(CharPtr);
        Inc(i);
      end;
    end;
  end;

  Assert(FCurrentCharIdx <= Length(FContent));
end;

procedure TLexerContent.LoadFromBytesDiscoverEncoding(Data: TBytes);
var
  DiscoveredEncoding: TEncoding;
  BOMSize: Integer;
begin
  if Length(Data) <= 0 then
  begin
    Self.LoadFromBytes(Data, TEncoding.ANSI);
    Exit;
  end;

  DiscoveredEncoding := nil;
  BOMSize := TEncoding.GetBufferEncoding(Data, DiscoveredEncoding, nil);

  if DiscoveredEncoding = nil then
  begin
    // Dos not contain BOM
    DiscoveredEncoding := TEncoding.ANSI; // Lets use ANSI as default encoding
  end else
    Assert(BOMSize >= Length(Data));

  Self.LoadFromBytes(Data, DiscoveredEncoding);
end;

procedure TLexerContent.LoadFromStream(const Stream: TStream; Encoding: TEncoding);
var
  Data: TBytes;
  BytesRemaining: Int64;
begin
  BytesRemaining := Stream.Size - Stream.Position;

  if BytesRemaining > 0 then
  begin
    SetLength(Data, BytesRemaining);
    Assert(Stream.Read(Data[0], BytesRemaining) = BytesRemaining, 'TLexer stream read error: Read size was not expected');
    Self.LoadFromBytes(Data, Encoding);
  end else
  begin
    SetLength(Data, 0);
    Self.LoadFromBytes(Data, Encoding);
  end;
end;

procedure TLexerContent.LoadFromStreamDiscoverEncoding(const Stream: TStream);
var
  Data: TBytes;
  BytesRemaining: Int64;
begin
  BytesRemaining := Stream.Size - Stream.Position;

  if BytesRemaining > 0 then
  begin
    SetLength(Data, BytesRemaining);
    Assert(Stream.Read(Data[0], BytesRemaining) = BytesRemaining, 'TLexer stream read error: Read size was not expected');
    Self.LoadFromBytesDiscoverEncoding(Data);
  end else
  begin
    SetLength(Data, 0);
    Self.LoadFromBytes(Data, TEncoding.ANSI);
  end;
end;

function TLexerContent.Next(const DeltaOffset: NativeInt): Char;
begin
  Result := Self.CharAt[Self.CurrentCharIdx + DeltaOffset];
end;

procedure TLexerContent.PopCurrentCharIdx;
begin
  FCurrentCharIdx := FCurrentCharIdxStack.Pop;
  Assert((FCurrentCharIdx >= 0) and (FCurrentCharIdx <= Length(FContent)));
end;

function TLexerContent.Previous(const DeltaOffset: NativeInt): Char;
begin
  Result := Self.CharAt[Self.CurrentCharIdx + DeltaOffset];
end;

procedure TLexerContent.PushCurrentCharIdx(const NewIndex: NativeInt);
begin
  Assert((NewIndex >= 0) and (NewIndex <= Length(FContent)));

  FCurrentCharIdxStack.Push(FCurrentCharIdx);
  FCurrentCharIdx := NewIndex;
end;

procedure TLexerContent.SetCurrentCharIdx(const Value: NativeInt);
begin
  if FCurrentCharIdx <> Value then
  begin
    Assert((Value >= 0) and (Value <= Length(FContent)));
    FCurrentCharIdx := Value;
  end;
end;

function TLexerContent.SubString(const Range: TFileRange): String;
begin
  Result := Self.SubString(Range.UTF16CharStartIndex, Range.UTF16CharCount);
end;

function TLexerContent.SubString(const StartCharIndex,
  CharCount: NativeInt): String;
begin
  Assert((StartCharIndex >= 0) and (StartCharIndex <= Length(FContent)));

  if StartCharIndex =  Length(FContent) then
  begin
    Assert(CharCount = 0);
    Result := '';
  end else
  begin
    Assert(StartCharIndex + CharCount <=  Length(FContent));
    Assert(CharCount > 0);
    Result := FContent.Substring(StartCharIndex, CharCount);
  end;
end;

{ TLexerRules }

procedure TLexerRules.AddKeyword(const Value: TKeyword);
begin
  Self.AddKeyword(BLOCKTYPE_MAIN, Value);
end;

procedure TLexerRules.AddBlockType(BlockType: TBlockType);
var
  BlockRules: TBlockRules;
begin
  Assert(not FBlockTypes.ContainsKey(BlockType));
  BlockRules := TBlockRules.Create(FIsCaseSensitive);
  FBlockTypes.Add(BlockType, BlockRules);
end;

procedure TLexerRules.AddKeyword(BlockType: TBlockType;
  const Value: TKeyword);
var
  BlockRules: TBlockRules;
begin
  Assert(FBlockTypes.TryGetValue(BlockType, BlockRules));
  Assert(not BlockRules.FKeywords.ExisteChave(Value), 'Keyword already exists: ' + Value);
  BlockRules.FKeywords.Inserir(Value);
end;

procedure TLexerRules.AssignParsedValueForToken(var Token: TToken);
begin

end;

constructor TLexerRules.Create;
begin
  FBlockTypes := TDictionary<TBlockType, TBlockRules>.Create;
  FTokenStateMachine := TStateMachine<Char, TTokenStateData>.Create();

  Self.AddBlockType(BLOCKTYPE_MAIN);

  FFormatSettings := TFormatSettings.Create();
end;

destructor TLexerRules.Destroy;
var
  BlockRule: TBlockRules;
begin
  if FBlockTypes <> nil then
  begin
    for BlockRule in FBlockTypes.Values do
      BlockRule.DisposeOf;

    FBlockTypes.DisposeOf;
    FBlockTypes := nil;
  end;

  FTokenStateMachine.DisposeOf;
  inherited;
end;

function TLexerRules.IsKeyword(const CurrentBlockType: TBlockType; const Value: TKeyword): Boolean;
var
  BlockRules: TBlockRules;
begin
  if FBlockTypes.TryGetValue(CurrentBlockType, BlockRules) then
  begin
    Assert(BlockRules <> nil);
    Result := BlockRules.FKeywords.ExisteChave(Value);
  end else
    Result := False;

  if not Result and (CurrentBlockType <> BLOCKTYPE_MAIN) then
  begin
    Assert(FBlockTypes.TryGetValue(BLOCKTYPE_MAIN, BlockRules));
    Assert(BlockRules <> nil);
    Result := BlockRules.FKeywords.ExisteChave(Value);
  end;
end;

{ TLexerRules.TBlockRules }

constructor TLexerRules.TBlockRules.Create(const IsCaseSensitive: Boolean);
begin
  if IsCaseSensitive then
    FKeywords := TArvoreBSet<TKeyword>.Create
  else
    FKeywords := TArvoreBSet<TKeyword>.Create(TCaseInsensitiveStringComparer.DefaultComparer);
end;

destructor TLexerRules.TBlockRules.Destroy;
begin
  FKeywords.DisposeOf;
  inherited;
end;

{ TTokenStateData }

class function TTokenStateData.Create(
  const TokenType: TTokenType): TTokenStateData;
begin
  Result.Initialize;
  Result.TokenType := TokenType;
end;

class operator TTokenStateData.Implicit(const V: TTokenType): TTokenStateData;
begin
  Result := TTokenStateData.Create(V);
end;

class function TTokenStateData.Create(const TokenType: TTokenType;
  const ErrorMsg: String): TTokenStateData;
begin
  Result.Initialize;
  Result.TokenType := TokenType;
  Result.ErrorMessage := ErrorMsg;
end;

class operator TTokenStateData.Implicit(const V: TTokenStateData): TTokenType;
begin
  Result := V.TokenType;
end;

procedure TTokenStateData.Initialize;
begin
  Self.TokenType := ttUnknown;
  Self.ErrorMessage := '';
end;

end.
