unit FD.Compiler.Lexer;

interface

uses
  // Refactor Units
  RRPG.Unicode, RRPG.UTF16Glyphs, RRPG.UTF16Glyphs.UnicodeData,
  System.SysUtils, System.Classes, System.Generics.Collections, System.BTree,
  FD.Compiler.Lexer.Tokens, FD.Compiler.Environment, FD.Compiler.Exceptions,
  System.Comparers, FD.Compiler.StateMachine;

type
  TLexerGlyphPosInfo = packed record
    UTF16CharIndex: NativeInt;
    UTF16CharCount: Byte;
    LineIndex: NativeInt;
    ColumnIndex: NativeInt;
  end;

  PLexerGlyphPosInfo = ^TLexerGlyphPosInfo;

  TLexerContent = class(TObject)
  protected
    FEnvironment: TCompilerEnvironment;
    FUnfoldName: String;
    FAbsoluteFileName: String;
    FContent: String;

    FGlyphs: TArray<TUTF16Glyph>;
    FGlyphCount: NativeInt;  // Length(FGlyphs) can (and will be) greater than FGlyphCount
    FGlyphsInfos: TArray<TLexerGlyphPosInfo>;

    FCurrentGlyphIdx: NativeInt;
    FCurrentGlyphIdxStack: TStack<NativeInt>;

    function GetGlyphCount: NativeInt; virtual;
    function GetGlyph(const Index: NativeInt): TUTF16Glyph; virtual;
    procedure SetCurrentGlyphIdx(const Value: NativeInt); virtual;
    procedure LoadGlyphsArray(const InputString: String); virtual;
  public
    constructor Create(Environment: TCompilerEnvironment;
                       LogicalUnfoldName, FullPathFileName: String);
    destructor Destroy; override;

    procedure LoadFromBytes(Data: TBytes; Encoding: TEncoding); virtual;
    procedure LoadFromBytesDiscoverEncoding(Data: TBytes); virtual;
    procedure LoadFromStream(const Stream: TStream; Encoding: TEncoding); virtual;
    procedure LoadFromStreamDiscoverEncoding(const Stream: TStream); virtual;

    function Current(const DeltaOffset: NativeInt = 0): TUTF16Glyph; inline;
    function Next(const DeltaOffset: NativeInt = 1): TUTF16Glyph; inline;
    function Previous(const DeltaOffset: NativeInt = -1): TUTF16Glyph; inline;

    procedure PushCurrentGlyphIdx(const NewIndex: NativeInt);
    procedure PopCurrentGlyphIdx;

    function CreateFileRange(const StartGlyphIndex, GlyphCount: NativeInt): TFileRange; virtual;
    function SubString(const StartGlyphIndex, GlyphCount: NativeInt): String; virtual;

    property Environment: TCompilerEnvironment read FEnvironment;
    property UnfoldName: String read FUnfoldName;
    property AbsoluteFileName: String read FAbsoluteFileName;

    property CurrentGlyphIdx: NativeInt read FCurrentGlyphIdx write SetCurrentGlyphIdx;
    property GlyphCount: NativeInt read GetGlyphCount;
    property Glyph[const Index: NativeInt]: TUTF16Glyph read GetGlyph;
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

  TTokenState = TState<TUCS4Sequence, TTokenStateData>;

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
    FTokenStateMachine: TStateMachine<TUCS4Sequence, TTokenStateData>;
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
    property TokenStateMachine: TStateMachine<TUCS4Sequence, TTokenStateData> read FTokenStateMachine;
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
  Result := FContent.CurrentGlyphIdx >= FContent.GlyphCount;
end;

function TLexer.GetNextToken: TToken;
var
  CurrentMachineState, NextMachineState, LastFinalMachineStateFound: TTokenState;
  i, FirstGlyphIndex, LastFinalMachineStateFoundGlyphIndex: NativeInt;
  Glyph: TUTF16Glyph;
  GlyphUCS4Sequence: TUCS4Sequence;
  TokenStateData: TTokenStateData;
begin
  Assert(FContent <> nil);

  if FContent.CurrentGlyphIdx >= FContent.GlyphCount then
  begin
    Result := TToken.CreateEOF(Self.CreateCodeLocation(FContent.CurrentGlyphIdx, 0));
    Exit;
  end;

  Assert(FRules <> nil);
  Assert(FRules.FTokenStateMachine <> nil);
  CurrentMachineState := FRules.FTokenStateMachine.InitialState;
  Assert(CurrentMachineState <> nil);
  Assert(not CurrentMachineState.IsFinal);

  LastFinalMachineStateFound := nil;
  LastFinalMachineStateFoundGlyphIndex := -1;

  FirstGlyphIndex := FContent.CurrentGlyphIdx;
  i := FirstGlyphIndex;

  while i < FContent.GlyphCount do
  begin
    Glyph := FContent.Glyph[i];
    Glyph.ToUCS4SequenceSeek(GlyphUCS4Sequence);

    NextMachineState := nil;

    if CurrentMachineState.TryLocateNextState(GlyphUCS4Sequence, NextMachineState) then
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

  Result.InputString := FContent.SubString(Result.Location.RealLocation.GlyphStartIndex,
                                           Result.Location.RealLocation.GlyphCount);

  if (Result.TokenType = ttIdentifier) and (FRules.IsKeyword(FCurrentBlockType, Result.InputString)) then
    Result.TokenType := ttKeyword;

  FRules.AssignParsedValueForToken(Result);
  FContent.CurrentGlyphIdx := Result.Location.RealLocation.GlyphStartIndex + Result.Location.RealLocation.GlyphCount;
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

  FCurrentGlyphIdxStack := TStack<NativeInt>.Create;
end;

function TLexerContent.CreateFileRange(const StartGlyphIndex,
  GlyphCount: NativeInt): TFileRange;
var
  GlyphInfo: PLexerGlyphPosInfo;
begin
  Assert((StartGlyphIndex >= 0) and (StartGlyphIndex <= FGlyphCount));

  Result.Initialize;
  Result.FileName := Self.AbsoluteFileName;

  if StartGlyphIndex = FGlyphCount then
  begin
    Assert(GlyphCount = 0);
    Result.UTF16CharStartIndex := Length(FContent);
    Result.UTF16CharCount := 0;
    Result.GlyphStartIndex := Self.GlyphCount;
    Result.GlyphCount := 0;

    if Length(FGlyphsInfos) > 0 then
    begin
      GlyphInfo := @FGlyphsInfos[Length(FGlyphsInfos) - 1];
      Result.LineIndex := GlyphInfo.LineIndex;
      Result.ColumnIndex := GlyphInfo.ColumnIndex;
    end else
    begin
      Result.LineIndex := 0;
      Result.ColumnIndex := 0;
    end;
  end else
  begin
    Assert(StartGlyphIndex + GlyphCount <= FGlyphCount);
    Assert(GlyphCount > 0);

    GlyphInfo := @FGlyphsInfos[StartGlyphIndex];

    Result.GlyphStartIndex := StartGlyphIndex;
    Result.GlyphCount := GlyphCount;
    Result.LineIndex := GlyphInfo.LineIndex;
    Result.ColumnIndex := GlyphInfo.ColumnIndex;
    Result.UTF16CharStartIndex := GlyphInfo.UTF16CharIndex;

    GlyphInfo := @FGlyphsInfos[StartGlyphIndex + GlyphCount - 1];
    Result.UTF16CharCount := GlyphInfo.UTF16CharIndex + GlyphInfo.UTF16CharCount;

    Assert(Result.UTF16CharCount > 0);
  end;
end;

function TLexerContent.Current(const DeltaOffset: NativeInt): TUTF16Glyph;
begin
  Result := Self.Glyph[Self.CurrentGlyphIdx + DeltaOffset];
end;

destructor TLexerContent.Destroy;
begin
  FCurrentGlyphIdxStack.DisposeOf;
  inherited;
end;

function TLexerContent.GetGlyph(const Index: NativeInt): TUTF16Glyph;
begin
  if (Index >= 0) and (Index < FGlyphCount) then
    Result := FGlyphs[Index]
  else
    Result := TUTF16Glyph.Null;
end;

function TLexerContent.GetGlyphCount: NativeInt;
begin
  Result := FGlyphCount;
end;

procedure TLexerContent.LoadFromBytes(Data: TBytes; Encoding: TEncoding);
var
  i: NativeInt;
  AccumUTF16CharCount, ThisGlyphUTF16CharCount, CurrentLineIndex, CurrentColumnIndex: NativeInt;
  GPtr: PUTF16Glyph;
  GlyphInfo: PLexerGlyphPosInfo;
const
  BREAK_LINE_CHARS = #13#10;
begin
  FContent := Encoding.GetString(Data);
  FCurrentGlyphIdx := 0;

  Self.LoadGlyphsArray(FContent);

  SetLength(FGlyphsInfos, FGlyphCount);

  if FGlyphCount > 0 then
  begin
    AccumUTF16CharCount := 0;
    GlyphInfo := @FGlyphsInfos[0];
    CurrentLineIndex := 0;
    CurrentColumnIndex := 0;
    GPtr := @FGlyphs[0];

    for i := 0 to FGlyphCount - 1 do
    begin
      ThisGlyphUTF16CharCount := GPtr.TotalLength;

      GlyphInfo.UTF16CharIndex := AccumUTF16CharCount;
      GlyphInfo.UTF16CharCount := Byte(ThisGlyphUTF16CharCount);
      GlyphInfo.LineIndex := CurrentLineIndex;
      GlyphInfo.ColumnIndex := CurrentColumnIndex;

      if GPtr.IsBreakLine then
      begin
        Inc(CurrentLineIndex);
        CurrentColumnIndex := 0;
      end else
        Inc(CurrentColumnIndex);

      AccumUTF16CharCount := AccumUTF16CharCount + ThisGlyphUTF16CharCount;
      Inc(GlyphInfo); // Point to Next GlyphInfo
      Inc(GPtr);
    end;
  end;

  Assert(FCurrentGlyphIdx <= FGlyphCount);
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

procedure TLexerContent.LoadGlyphsArray(const InputString: String);
var
  Seeker: TUTF16GlyphsSequencialSeek;
  GPtr: PUTF16Glyph;
begin
  FGlyphCount := 0;
  SetLength(FGlyphs, Length(InputString) + 5);
  Seeker.PrepareForSequencialSeek(InputString);
  GPtr := @FGlyphs[0];

  while Seeker.FetchNext(GPtr^) do
  begin
    Inc(FGlyphCount);
    Inc(GPtr);
  end;
end;

function TLexerContent.Next(const DeltaOffset: NativeInt): TUTF16Glyph;
begin
  Result := Self.Glyph[Self.CurrentGlyphIdx + DeltaOffset];
end;

procedure TLexerContent.PopCurrentGlyphIdx;
begin
  FCurrentGlyphIdx := FCurrentGlyphIdxStack.Pop;
  Assert((FCurrentGlyphIdx >= 0) and (FCurrentGlyphIdx <= FGlyphCount));
end;

function TLexerContent.Previous(const DeltaOffset: NativeInt): TUTF16Glyph;
begin
  Result := Self.Glyph[Self.CurrentGlyphIdx + DeltaOffset];
end;

procedure TLexerContent.PushCurrentGlyphIdx(const NewIndex: NativeInt);
begin
  Assert((NewIndex >= 0) and (NewIndex <= FGlyphCount));

  FCurrentGlyphIdxStack.Push(FCurrentGlyphIdx);
  FCurrentGlyphIdx := NewIndex;
end;

procedure TLexerContent.SetCurrentGlyphIdx(const Value: NativeInt);
begin
  if FCurrentGlyphIdx <> Value then
  begin
    Assert((Value >= 0) and (Value <= FGlyphCount));
    FCurrentGlyphIdx := Value;
  end;
end;

function TLexerContent.SubString(const StartGlyphIndex,
  GlyphCount: NativeInt): String;
var
  GlyphInfo: PLexerGlyphPosInfo;
  StringStartIndex, StringCharCount: NativeInt;
begin
  Assert((StartGlyphIndex >= 0) and (StartGlyphIndex <= FGlyphCount));

  if StartGlyphIndex = FGlyphCount then
  begin
    Assert(GlyphCount = 0);
    Result := '';
  end else
  begin
    Assert(StartGlyphIndex + GlyphCount <= FGlyphCount);
    Assert(GlyphCount > 0);

    GlyphInfo := @FGlyphsInfos[StartGlyphIndex];
    StringStartIndex := GlyphInfo.UTF16CharIndex;

    GlyphInfo := @FGlyphsInfos[StartGlyphIndex + GlyphCount - 1];
    StringCharCount := (GlyphInfo.UTF16CharIndex + GlyphInfo.UTF16CharCount) - StringStartIndex;

    Assert(StringCharCount > 0);
    Result := FContent.Substring(StringStartIndex, StringCharCount);
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
  FTokenStateMachine := TStateMachine<TUCS4Sequence, TTokenStateData>.Create(TUCS4SequenceComparer.Create);

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
