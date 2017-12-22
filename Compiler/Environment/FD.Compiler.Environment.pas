unit FD.Compiler.Environment;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FD.Compiler.Exceptions;

type
  (* TCompilerEnvironment represents a single compilation environment process, and
        contains things like "Environment Defines", "Search Paths", etc...
  *)

  TCompilerEnvironment = class(TObject)
  private
    procedure SetBasePath(const Value: String);
  protected
    FSearchPath: TList<String>;
    FBasePath: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSearchPath(SearchPath: String); virtual;
    function ExpandFileName(FileName: String; var AbsoluteFileName: String): Boolean;
    function OpenFileForRead(FileName: String): TStream;

    property BasePath: String read FBasePath write SetBasePath;
  end;

implementation

uses
  System.IOUtils;

{ TCompilerEnvironment }

procedure TCompilerEnvironment.AddSearchPath(SearchPath: String);
begin
  FSearchPath.Add(IncludeTrailingPathDelimiter(SearchPath));
end;

constructor TCompilerEnvironment.Create;
begin
  FSearchPath := TList<String>.Create;
end;

destructor TCompilerEnvironment.Destroy;
begin
  FSearchPath.Free;
  inherited;
end;

function TCompilerEnvironment.ExpandFileName(FileName: String;
  var AbsoluteFileName: String): Boolean;
var
  i: Integer;

  function TryExpandWithASearchPath(const SP: String): Boolean;
  var
    FullFileName: String;
  begin
    // Try expand without the Base Path

    FullFileName := TPath.GetFullPath(TPath.Combine(SP, FileName));

    if TFile.Exists(FullFileName) then
    begin
      AbsoluteFileName := FullFileName;
      Exit(True);
    end;

    // Try expand with the Base Path

    FullFileName := TPath.GetFullPath(TPath.Combine(TPath.Combine(BasePath, SP), FileName));

    if TFile.Exists(FullFileName) then
    begin
      AbsoluteFileName := FullFileName;
      Exit(True);
    end;

    Result := False;
  end;
begin
  if TryExpandWithASearchPath('') then
    Exit(True);

  for i := 0 to FSearchPath.Count - 1 do
    if TryExpandWithASearchPath(FSearchPath[i]) then
      Exit(True);

  Result := False;
end;

function TCompilerEnvironment.OpenFileForRead(FileName: String): TStream;
var
  AbsFileName: String;
begin
  if not Self.ExpandFileName(FileName, AbsFileName) then
    raise EFDFileNotFound.Create('File not found: "' + FileName + '"');

  Result := TFileStream.Create(AbsFileName, fmOpenRead or fmShareDenyNone);
end;

procedure TCompilerEnvironment.SetBasePath(const Value: String);
begin
  FBasePath := TPath.GetFullPath(IncludeTrailingPathDelimiter(Value));
end;

end.
