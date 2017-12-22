unit System.Comparers;

interface

uses
  System.Generics.Defaults;

type
  TCaseInsensitiveStringComparer = class(TInterfacedObject, IComparer<String>)
  private
    class function GetDefaultComparer: IComparer<String>; static;
  protected
    class var
      FDefaultStrComparer: IComparer<String>;
  public
    function Compare(const Left, Right: String): Integer;

    class property DefaultComparer: IComparer<String> read GetDefaultComparer;
  end;

function CompareStrCaseInsensitive(const Left, Right: String): Integer;

implementation

uses
  System.Character;

{ TInsensitiveStringComparer }

function CaseInsensitiveChar(const C: Char): Char; inline;
begin
  Result := C.ToUpper();
end;

function CompareStrCaseInsensitive(const Left, Right: String): Integer;
var
  P1, P2: PChar;
  C1, C2: Char;
begin
  if (Left = '') then
  begin
    if Right = '' then
      Exit(0)
    else
      Exit(-1);
  end else
    if Right = '' then
      Exit(+1);

  P1 := PChar(Left);
  P2 := PChar(Right);

  while True do
  begin
    C1 := CaseInsensitiveChar(P1^);
    C2 := CaseInsensitiveChar(P2^);

    if (C1 <> C2) or (C1 = #0) then
      Exit(Ord(C1) - Ord(C2));

    Inc(P1);
    Inc(P2);
  end;
end;

function TCaseInsensitiveStringComparer.Compare(const Left, Right: String): Integer;
begin
  Result := CompareStrCaseInsensitive(Left, Right);
end;

class function TCaseInsensitiveStringComparer.GetDefaultComparer: IComparer<String>;
begin
  if TCaseInsensitiveStringComparer.FDefaultStrComparer = nil then
    TCaseInsensitiveStringComparer.FDefaultStrComparer := TCaseInsensitiveStringComparer.Create();

  Result := TCaseInsensitiveStringComparer.FDefaultStrComparer;
end;

end.
