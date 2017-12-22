{******************************************************************************}
{*                                                                            *}
{*                              UtilArvoreB.pas                               *}
{*                                                                            *}
{*  Esta unit contém uma implementação em Delphi de uma Árvore B, uma         *}
{*  estrutura de dados que permite a inserção, remoção e localização de itens *}
{*  de forma rápida( complexidade de tempo O(log n) tanto para inserção,      *}
{*  remoção e localização) na memória.                                        *}
{*                                                                            *}
{*  Um grande diferencial da Árvore B em relação a tabela hash (classe        *}
{*  TDictionary no delphi) é que os dados são armazenados de forma ordenada,  *}
{*  permitindo, assim, navegar nos dados de forma ORDENADA e buscas rápidas   *}
{*  por desigualdade (Exemplo: O primeiro registro cuja chave seja maior que  *}
{*  10).                                                                      *}
{*                                                                            *}
{*  Criado por Alysson Cunha para o RRPG Firecast em 2013                     *}
{*                                                                            *}
{******************************************************************************}

unit System.BTree;

interface

uses
  SysUtils, Generics.Defaults, System.SyncObjs;

const
  ARV_GRAU = 5;

  MAX_CHAVES = (ARV_GRAU * 2) - 1;
  MAX_FILHOS = ARV_GRAU * 2;

  MIN_CHAVES = ARV_GRAU - 1;
  MIN_FILHOS = ARV_GRAU;

type
  {*****************************************************************************
   *
   *  TArvoreBInequalityOp
   *
   *  Tipo enumerado para controlar como deve ser localizado um registro por
   *  desigualdade na Árvore B. <, <=, >, >= ?
   *
   *}
  TArvoreBInequalityOp = (abioMenor, abioMenorIgual, abioMaior, abioMaiorIgual);

  {*****************************************************************************
   *
   *  TFuncaoArvoreBCollate
   *
   *  Caso queira utilizar a funcionalidade Collate da Árvore B, você deve programar
   *  uma classe que implementa esta interface. Basicamente é uma classe
   *  responsável por preparar chaves para ser comparada antes da busca/inserção/remoção.
   *
   *  Exemplo: Se você quiser que em uma TArvoreB<String,X> as chaves
   *    "Maria" e 'mARIA" sem tratados como valores iguais, você pode programar
   *    uma classe collate que transforma o valor da chave para Uppercase.
   *}
  TFuncaoArvoreBCollate<T> = interface
    procedure Preparar(const Antigo: T; var Novo: T);
  end;

  {*****************************************************************************
   *
   *  IArvoreBNavigator<T, U>
   *
   *  Esta é uma interface importante usada para NAVEGAR nos registros da
   *  ÁrvoreB. Para obter um novo Navegador, utilize a função "CriarNavigator"
   *  ou "CriarNavigatorAndGotoLast" da classe TArvoreB.
   *
   *  Através de um Navigator é póssível percorrer os registros de uma ÁrvoreB
   *  de forma ordenada e bidirecional (tanto de frente para traz, quanto
   *  de traz para frente).
   *
   *  Importante: Nunca navegue nos dados de e altere os dados da árvore B
   *    ao mesmo tempo. Após inserir/remover dados da árvore, é necessário utilizar
   *    alguma das funções "Goto*" do navegador para reposicionar corretamente o cursor.
   *
   *  Observações: É possível existir vários Navigators de uma mesma Árvore B ao
   *    mesmo tempo operando de forma separada (posicionadas em registros distintos).
   *
   *  Exemplo de uso de Navigator:
   *      var
   *        Arvore: TArvoreB<Integer, String>;
   *        Nav: IArvoreBNavigator<Integer, String>;
   *      begin
   *        Arvore := TArvoreB<Integer, String>.Create;
   *        // Inserção de vários dados em Árvore.
   *        Nav := Arvore.CriarNavigator();
   *
   *        while not Nav.EOF do
   *        begin
   *          ShowMesage(IntToStr(Nav.ChaveAtual));
   *          ShowMesage(Nav.ValorAtual);
   *          Nav.Next;
   *        end;
   *
   *        Nav := nil;
   *        Arvore.Free; // De preferencia através de um try-finally
   *      end;
   *}
  IArvoreBNavigator<T, U> = interface
    function GetBOF: Boolean;
    function GetEOF: Boolean;
    function GetChaveAtual: T;
    function GetValorAtual: U;
    function GetPtrValorAtual: Pointer;

    { GotoFirst - Posiciona o cursor do Navegador no primeiro registro da Árvore B }
    procedure GotoFirst;

    { GotoFirst - Posiciona o cursor do Navegador no último registro da Árvore B }
    procedure GotoLast;

    { GotoFirstPartialMatch - Posiciona o cursor do Navegador no primeiro registro
         cuja comparação feita pelo PartialComparer retornar 0 (igualdade). Retorna True se
         encontrou algum registro.

         Exemplo: Em uma Árvore B cuja chave seja uma chave composta de <CODIGO, DATA>,
                  usando essa função é possível posicionar o cursor no primeiro registro
                  cujo CODIGO seja igual a um determinado valor, independente da DATA.}
    function GotoFirstPartialMatch(const PartialValue: T; PartialComparer: IComparer<T>): Boolean;

    { GotoLastPartialMatch - Identico à função GotoFirstPartialMatch, porém posiciona o cursor
                           no último registro que o PartialComparer retornar 0}
    function GotoLastPartialMatch(const PartialValue: T; PartialComparer: IComparer<T>): Boolean;

    { GotoFirstInequalityMatch - Posiciona o cursor do Navegador no primeiro registro
        que respeite a uma comparação de desigualdade definida pelos parâmetros
        Inequality e Chave. Retorna true se localizou algum registro que respeite a
        desigualdade.

        Exemplos: Primeiro registro > 5, Primeiro registro >= 10, Primeiro registro < 6, etc.. }
    function GotoFirstInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;

    { GotoLastInequalityMatch - Idêntico à função GotoFirstInequalityMatch, porém posiciona
         o cursor no último registro que respeite a uma desigualdade }
    function GotoLastInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;

    {Previous - Posiciona o cursor no registro anterior ao atualmente posicionado.
       Como a navegação ocorre de forma ordenada, o a chave do novo foco será
       a primeira chave menor que a do atualmente focado.}
    procedure Previous;

    {Next - Posiciona o cursor no próximo registro ao atualmente posicionado.
       Como a navegação ocorre de forma ordenada, o a chave do novo foco será
       a primeira chave maior que a do atualmente focado.}
    procedure Next;

    property EOF: Boolean read GetEOF; // Retorna TRUE se o cursor está no fim e não há mais registros para frente
    property BOF: Boolean read GetBOF; // Retorna TRUE se o cursor está no início e não há mais registros para trás
    property ChaveAtual: T read GetChaveAtual; // Retorna a CHAVE do registro atualmente focado pelo navegador
    property ValorAtual: U read GetValorAtual;  // Retorna o VALOR do registro atualmente focado pelo navegador.
    property PtrValorAtual: Pointer read GetPtrValorAtual;  // Retorna um Ponteiro para o valor do registro atualmente focado pelo navegador
  end;

  TArvoreBProcessor<T, U> = reference to procedure(const Chave: T; var Valor: U; const IsNew: Boolean);
  TArvoreBIteratorOnValor<U> = reference to procedure (const Valor: U);

  TArvoreBPair<T, U> = record
    Chave: T;
    Valor: U;
  end;

  {*****************************************************************************
   *
   *  TArvoreB<T, U>
   *
   *  Esta é a principal classe da implementação, a Árvore B de fato. É uma classe
   *  que mapeia o valor de uma chave à um valor associado através de uma implementação
   *  genérica, isto é, você pode escolher qualquer tipo de dados para ser a CHAVE
   *  e qualquer tipo de dados para ser o VALOR ASSOCIADO à chave.
   *
   *  Exemplo: var
   *      A1: TArvoreB<String, Integer>; // Mapeia Strings para Integers
   *      A2: TArvoreB<Integer, Boolean>; // Mapeia Integers para Boolean
   *
   *  Importante: Os dados são armazenados ordenados pela Chave de forma
   *              CRESCENTE na memória, e, portanto, existe um mecanismo
   *              de comparação de chaves para determinar a ordem das mesmas.
   *              Para chaves com tipos de dados primitivos simples
   *              (Integer, Int64, String, Boolean, etc..) a própria classe utiliza
   *              um Comparer adequado automaticamente, mas para chaves com tipo de dados
   *              "compostos" (records, objetos, etc..) é importante informar para
   *              a árvore uma interface IComparer que é responsável por determinar se uma
   *              chave é menor que outra.
   *}
  TArvoreB<T, U> = class(TObject)
  public
    type
      TArrayChave = TArray<T>;

      TTipoChave = T;
      TTipoValor = U;

      TChave = record
        ValorChave: TTipoChave;
        Valor: TTipoValor;
      end;

      PPaginaB = ^TPaginaB;
      PPointerToU = ^U;

      TPaginaB = record // Uma página da Árvore B. Ela armazena várias chaves-valores e links para várias páginas filhas.
        CountChaves: Integer;
        Chaves: array [0 .. MAX_CHAVES - 1] of TChave;
        Filhos: array [0 .. MAX_FILHOS - 1] of PPaginaB;

        Pai: PPaginaB;
        IndiceNoPai: Integer;

        procedure SetCountChaves(const V: Integer); inline;
      end;

      INavigator = IArvoreBNavigator<T, U>;


    TArvoreBNavigator = class(TInterfacedObject, IArvoreBNavigator<T, U>)
    private
      FVersaoArvore: Integer;
      FArvore: TArvoreB<T, U>;
      FPaginaAtual: PPaginaB;
      FIndiceAtual: Integer;
      FEof: Boolean;
      FBof: Boolean;
    protected
      function GetChaveAtual: T;
      function GetEOF: Boolean;
      function GetBOF: Boolean;
      function GetValorAtual: U;
      function GetPtrValorAtual: Pointer;
      procedure NeedVersaoSincronizada; inline;
    public
      constructor Create;
      destructor Destroy; override;

      procedure GotoFirst;
      procedure GotoLast;
      function GotoFirstPartialMatch(const PartialValue: T; PartialComparer: IComparer<T>): Boolean;
      function GotoLastPartialMatch(const PartialValue: T; PartialComparer: IComparer<T>): Boolean;
      function GotoFirstInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
      function GotoLastInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
      procedure Previous;
      procedure Next;

      property ChaveAtual: T read GetChaveAtual;
      property Eof: Boolean read FEof;
      property Bof: Boolean read FBof;
    end;

  protected
    FVersao: Integer;
    FCount: Integer;
    FRaiz: PPaginaB;
    FChaveDefault: TTipoChave;
    FValorDefault: TTipoValor;
    FComparer: IComparer<T>;
    FCollate: TFuncaoArvoreBCollate<T>;
    FQtNavigatorsCriado: Integer;

    procedure NavGotoFirst(Nav: TArvoreBNavigator);
    procedure NavGotoLast(Nav: TArvoreBNavigator);
    procedure NavGotoPrevious(Nav: TArvoreBNavigator);
    procedure NavGotoNext(Nav: TArvoreBNavigator);
    function NavGotoFirstPartialMatch(Nav: TArvoreBNavigator; const PartialValue: T; const PartialComparer: IComparer<T>): Boolean;
    function NavGotoLastPartialMatch(Nav: TArvoreBNavigator; const PartialValue: T; const PartialComparer: IComparer<T>): Boolean;
    function NavGotoFirstInequalityMatch(Nav: TArvoreBNavigator; const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
    function NavGotoLastInequalityMatch(Nav: TArvoreBNavigator; const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
  protected
    procedure ValidarNavigatorsCriados;
    procedure IncrementarVersaoDaArvore; inline;

    function AlocarPagina: PPaginaB;
    procedure DestruirPagina(var P: PPaginaB);
    procedure LiberarPaginaRecursivamente(var P: PPaginaB);

    procedure InnerAlocarEspaco(var Pagina: PPaginaB; var IndiceEspaco: Integer);

    function InnerDeletePucharDeCima(var Pagina: PPaginaB; var Indice: Integer): Integer;

    procedure InnerFindLowest(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
    procedure InnerFindHighest(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
    procedure InnerFindLowestPartialMatch(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
                                          const PartialValue: T; const Comparer: IComparer<T>);
    procedure InnerFindHighestPartialMatch(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
                                           const PartialValue: T; const Comparer: IComparer<T>);

    procedure InnerFindLowestIneqMatch(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
                                       const Chave: T; const Inequality: TArvoreBInequalityOp);

    procedure InnerFindHighestIneqMatch(PaginaOrigem: PPaginaB; out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
                                        const Chave: T; const Inequality: TArvoreBInequalityOp);
  public
    // Procedimentos públicos para manipular a Árvore B de forma avançada
    procedure InnerLocalizarForInsertCollated(PaginaOrigem: PPaginaB; ChaveALocalizar: TTipoChave;
                                              out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
    procedure InnerLocateCollated(PaginaOrigem: PPaginaB; ChaveALocalizar: TTipoChave;
                                  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
    procedure InnerInsertCollated(InserirEm: PPaginaB; const IndiceInserir: Integer; Chave: T; Valor: U);

    procedure InnerLocalizarForInsert(PaginaOrigem: PPaginaB; ChaveALocalizar: TTipoChave;
                                      out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
    procedure InnerLocate(PaginaOrigem: PPaginaB; ChaveALocalizar: TTipoChave;
                          out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);

    procedure InnerDelete(Pagina: PPaginaB; const IndiceEspaco: Integer);
    procedure InnerInsert(InserirEm: PPaginaB; const IndiceInserir: Integer; Chave: T; Valor: U);

    procedure InserirCollated(const Chave: T; const Valor: U);
    procedure AlterarCollated(const Chave: T; const Valor: U);
    procedure InserirOuAlterarCollated(const Chave: T; const Valor: U);
    procedure ProcessarValorCollated(const Chave: T; const Processor: TArvoreBProcessor<T, U>);
  public
    constructor Create(); overload;
    constructor Create(Comparer: IComparer<T>; Collate: TFuncaoArvoreBCollate<T>); overload;
    constructor Create(Comparer: IComparer<T>); overload;
    constructor Create(Collate: TFuncaoArvoreBCollate<T>); overload;
    destructor Destroy; override;

    procedure Inserir(const Chave: T; const Valor: U);
    procedure InserirOuAlterar(const Chave: T; const Valor: U);
    procedure Alterar(const Chave: T; const Valor: U);
    procedure Remover(const Chave: T);
    procedure Limpar;

    function ExisteChave(const Chave: T): Boolean;

    function TryGet(const Chave: T; out Valor: U): Boolean;
    function ObterMenorChave(var Chave: T): Boolean;
    function ObterMaiorChave(var Chave: T): Boolean;

    procedure ProcessarValor(const Chave: T; const Processor: TArvoreBProcessor<T, U>);
    procedure ProcessarValorTodasChaves(const Processor: TArvoreBProcessor<T, U>);
    procedure IterarSobValores(const Iterator: TArvoreBIteratorOnValor<U>);


    function GetArrayDeChaves: TArrayChave;
    function GetArrayDeValores: TArray<U>;
    function GetArray: TArray<TArvoreBPair<T, U>>;
    function TryGetPtr(const Chave: T; out PtrValor: Pointer): Boolean;

    function ExistePartialMatch(const Chave: T; PartialComparer: IComparer<T>): Boolean;
    function ExisteInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
    function TryGetFirstInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp; var RetChave: T; var RetValue: U): Boolean;
    function TryGetLastInequalityMatch(const Chave: T; const Inequality: TArvoreBInequalityOp; var RetChave: T; var RetValue: U): Boolean;


    function CriarNavigator: IArvoreBNavigator<T, U>;
    function CriarNavigatorAndGotoLast: IArvoreBNavigator<T, U>;

    property Count: Integer read FCount;
    property Raiz: PPaginaB read FRaiz;
    property Collate: TFuncaoArvoreBCollate<T> read FCollate;
    property Comparer: IComparer<T> read FComparer;
  end;

  TEmptyRecord = packed record

  end;

  TArvoreBSet<T> = class(TObject)
  public
    type
      INavigator = IArvoreBNavigator<T, TEmptyRecord>;
  private
    FArvore: TArvoreB<T, TEmptyRecord>;
    function GetCollate: TFuncaoArvoreBCollate<T>;
    function GetCount: Integer;
  public
    constructor Create(); overload;
    constructor Create(Comparer: IComparer<T>; Collate: TFuncaoArvoreBCollate<T>); overload;
    constructor Create(Comparer: IComparer<T>); overload;
    constructor Create(Collate: TFuncaoArvoreBCollate<T>); overload;

    destructor Destroy; override;

    procedure Inserir(const Chave: T);
    procedure InserirSeNaoExistir(const Chave: T);

    function ExisteChave(const Chave: T): Boolean;
    function ExistePartialMatch(const Chave: T; PartialComparer: IComparer<T>): Boolean;
    procedure Remover(const Chave: T);
    procedure Limpar;

    function ObterMenorChave(var Chave: T): Boolean;
    function ObterMaiorChave(var Chave: T): Boolean;
    function GetAsArray: TArray<T>;

    function CriarNavigator: IArvoreBNavigator<T, TEmptyRecord>;
    function CriarNavigatorAndGotoLast: IArvoreBNavigator<T, TEmptyRecord>;

    property Count: Integer read GetCount;
    property Collate: TFuncaoArvoreBCollate<T> read GetCollate;
    property Arvore: TArvoreB<T, TEmptyRecord> read FArvore;
  end;

{$IFDEF ESTATISTICAS}
var
  EstArvoreB_GlobalPageCount: Integer;
  EstArvoreB_MemoryOfPages: Int64;
{$ELSE}
const
  EstArvoreB_GlobalPageCount = 0;
  EstArvoreB_MemoryOfPages = 0;
{$ENDIF}

implementation

{ TArvoreB<T, U> }

function TArvoreB<T, U>.AlocarPagina: PPaginaB;
begin
  GetMem(Result, SizeOf(TPaginaB));
  FillChar(Result^, SizeOf(TPaginaB), 0);

  {$IFDEF ESTATISTICAS}
  TInterlocked.Increment(EstArvoreB_GlobalPageCount);
  TInterlocked.Add(EstArvoreB_MemoryOfPages, SizeOf(TPaginaB));
  {$ENDIF}
end;

constructor TArvoreB<T, U>.Create(Comparer: IComparer<T>; Collate: TFuncaoArvoreBCollate<T>);
begin
  FComparer := Comparer;
  FCollate := Collate;

  if FComparer = nil then
    FComparer := TComparer<T>.Default;
end;

function TArvoreB<T, U>.CriarNavigator: IArvoreBNavigator<T, U>;
var
  Nav: TArvoreBNavigator;
begin
  Nav := TArvoreBNavigator.Create;
  Nav.FArvore := Self;
  TInterlocked.Increment(FQtNavigatorsCriado);
  Nav.GotoFirst();

  Result := Nav;
end;

function TArvoreB<T, U>.CriarNavigatorAndGotoLast: IArvoreBNavigator<T, U>;
var
  Nav: TArvoreBNavigator;
begin
  Nav := TArvoreBNavigator.Create;
  Nav.FArvore := Self;
  TInterlocked.Increment(FQtNavigatorsCriado);
  Nav.GotoLast();

  Result := Nav;
end;

constructor TArvoreB<T, U>.Create;
begin
  Self.Create(nil, nil);
end;

destructor TArvoreB<T, U>.Destroy;
begin
  Self.ValidarNavigatorsCriados();
  Self.Limpar();

  if FRaiz <> nil then
  begin
    Self.DestruirPagina(FRaiz);
    FRaiz := nil;
  end;

  inherited;
end;

procedure TArvoreB<T, U>.DestruirPagina(var P: PPaginaB);
var
  i: Integer;
begin
  for i := 0 to High(P.Chaves) do
  begin
    P.Chaves[i].ValorChave := Default(T);
    P.Chaves[i].Valor := Default(U);
  end;

  FreeMem(P);

  {$IFDEF ESTATISTICAS}
  TInterlocked.Decrement(EstArvoreB_GlobalPageCount);
  TInterlocked.Add(EstArvoreB_MemoryOfPages, -SizeOf(TPaginaB));
  {$ENDIF}
end;

function TArvoreB<T, U>.ExisteChave(const Chave: T): Boolean;
var
  PEncontrado: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerLocateCollated(FRaiz, ChaveCollated, PEncontrado, Indice);
  Result := PEncontrado <> nil;
end;

function TArvoreB<T, U>.ExistePartialMatch(const Chave: T;
  PartialComparer: IComparer<T>): Boolean;
var
  PEncontrada: PPaginaB;
  Indice: Integer;
begin
  if FRaiz <> nil then
  begin
    PEncontrada := nil;
    Self.InnerFindLowestPartialMatch(FRaiz, PEncontrada, Indice,
                                     Chave, PartialComparer);

    Result := PEncontrada <> nil;
  end else
    Result := False;
end;

function TArvoreB<T, U>.ExisteInequalityMatch(const Chave: T;
  const Inequality: TArvoreBInequalityOp): Boolean;
var
  PEncontrada: PPaginaB;
  Indice: Integer;
begin
  if FRaiz <> nil then
  begin
    PEncontrada := nil;
    Self.InnerFindLowestIneqMatch(FRaiz, PEncontrada, Indice,
                                  Chave, Inequality);

    Result := PEncontrada <> nil;
  end else
    Result := False;
end;

function TArvoreB<T, U>.GetArray: TArray<TArvoreBPair<T, U>>;
var
  Nav: INavigator;
  Idx: NativeInt;
begin
  SetLength(Result, Self.Count);

  if Self.Count > 0 then
  begin
    Nav := Self.CriarNavigator;
    Idx := 0;

    while not Nav.EOF do
    begin
      Result[Idx].Chave := Nav.ChaveAtual;
      Result[Idx].Valor := Nav.ValorAtual;
      Inc(Idx);
      Nav.Next;
    end;

    Nav := nil;
  end;
end;

function TArvoreB<T, U>.GetArrayDeChaves: TArrayChave;
var
  Nav: INavigator;
  Idx: NativeInt;
begin
  SetLength(Result, Self.Count);

  if Self.Count > 0 then
  begin
    Nav := Self.CriarNavigator;
    Idx := 0;

    while not Nav.EOF do
    begin
      Result[Idx] := Nav.ChaveAtual;
      Inc(Idx);
      Nav.Next;
    end;

    Nav := nil;
  end;
end;

function TArvoreB<T, U>.GetArrayDeValores: TArray<U>;
var
  Nav: IArvoreBNavigator<T, U>;
  IndiceAtual: Integer;
begin
  SetLength(Result, Self.Count);

  if Self.Count > 0 then
  begin
    IndiceAtual := 0;
    Nav := Self.CriarNavigator;

    while not Nav.EOF do
    begin
      Result[IndiceAtual] := Nav.ValorAtual;
      Inc(IndiceAtual);
      Nav.Next;
    end;

    Nav := nil;
  end;
end;

procedure TArvoreB<T, U>.InnerLocalizarForInsert(PaginaOrigem: PPaginaB;
  ChaveALocalizar: TTipoChave; out PEncontrada: PPaginaB;
  out IndiceEncontrado: Integer);
var
  ChaveCollated: TTipoChave;
begin
  if FCollate <> nil then
    FCollate.Preparar(ChaveALocalizar, ChaveCollated)
  else
    ChaveCollated := ChaveALocalizar;

  Self.InnerLocalizarForInsertCollated(PaginaOrigem, ChaveCollated, PEncontrada,
                                       IndiceEncontrado);
end;

procedure TArvoreB<T, U>.InnerLocalizarForInsertCollated(PaginaOrigem: PPaginaB;
  ChaveALocalizar: TTipoChave; out PEncontrada: PPaginaB;
  out IndiceEncontrado: Integer);
var
  Ind, Comp: Integer;
  POrg: PPaginaB;
  Min, Max, Meio: Integer;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := FComparer.Compare(ChaveALocalizar, POrg^.Chaves[Meio].ValorChave);

      if Comp = 0 then
      begin
        PEncontrada := POrg;
        IndiceEncontrado := Meio;
        Exit;
      end else
        if Comp < 0 then
          Max := Meio - 1
        else
          Min := Meio + 1;
    end;

    if POrg.Filhos[Min] <> nil then
    begin
      POrg := POrg.Filhos[Min];
      Continue;
    end;

    PEncontrada := POrg;
    IndiceEncontrado := Min;
    Exit;
  end;
end;

procedure TArvoreB<T, U>.InnerLocate(PaginaOrigem: PPaginaB;
  ChaveALocalizar: TTipoChave; out PEncontrada: PPaginaB;
  out IndiceEncontrado: Integer);
var
  ChaveCollated: TTipoChave;
begin
  if FCollate <> nil then
    FCollate.Preparar(ChaveALocalizar, ChaveCollated)
  else
    ChaveCollated := ChaveALocalizar;

  Self.InnerLocateCollated(PaginaOrigem, ChaveCollated, PEncontrada,
                           IndiceEncontrado);
end;

procedure TArvoreB<T, U>.InnerLocateCollated(PaginaOrigem: PPaginaB;
  ChaveALocalizar: TTipoChave; out PEncontrada: PPaginaB;
  out IndiceEncontrado: Integer);
var
  Comp, Min, Max, Meio: Integer;
  POrg: PPaginaB;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    // Realizar busca binária
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := FComparer.Compare(ChaveALocalizar, POrg^.Chaves[Meio].ValorChave);

      if Comp = 0 then
      begin
        PEncontrada := POrg;
        IndiceEncontrado := Meio;
        Exit;
      end else
        if Comp < 0 then
          Max := Meio - 1
        else
          Min := Meio + 1;
    end;

    // Não encontrou, vamos pesquisar nos filhos, se tiver
    POrg := POrg.Filhos[Min];
  end;
end;

procedure TArvoreB<T, U>.IncrementarVersaoDaArvore;
begin
  TInterlocked.Increment(FVersao);
end;

procedure TArvoreB<T, U>.InnerAlocarEspaco(var Pagina: PPaginaB;
  var IndiceEspaco: Integer);
var
  Mediana, IndiceNoPai, i, CountToLeft, CountToRight, CountSemMediana, QtRetirado: Integer;
  NovoIrmaoEsq: PPaginaB;
  OPai: PPaginaB;
begin
  if Pagina.CountChaves >= MAX_CHAVES then
  begin
    // Vamos precisar dividir a Pagina em 2 páginas
    Mediana := Pagina.CountChaves div 2;

    OPai := Pagina.Pai;

    if OPai = nil then
    begin
      // Estamos na raiz
      OPai := Self.AlocarPagina();
      FRaiz := OPai;
    end;

    IndiceNoPai := Pagina.IndiceNoPai;

    // Subir Mediana
    Self.InnerAlocarEspaco(OPai, IndiceNoPai);
    OPai.Chaves[IndiceNoPai] := Pagina.Chaves[Mediana];

    NovoIrmaoEsq := AlocarPagina();

    // Copiar as chaves que estão antes da Mediana para o Novo irmão da esquerda
    CountSemMediana := Pagina.CountChaves - 1;
    CountToLeft := CountSemMediana div 2;          // Metade para a esquerda
    CountToRight := CountSemMediana - CountToLeft; // O restante para a direita
    QtRetirado := Pagina.CountChaves - CountToRight;

    for i := 0 to CountToLeft - 1 do
      NovoIrmaoEsq.Chaves[i] := Pagina.Chaves[i];

    for i := 0 to CountToLeft do
    begin
      NovoIrmaoEsq.Filhos[i] := Pagina.Filhos[i];

      if NovoIrmaoEsq.Filhos[i] <> nil then
        NovoIrmaoEsq.Filhos[i].Pai := NovoIrmaoEsq;
    end;

    // Reduzir o tamanho da Direita

    for i := 0 to CountToRight - 1 do
      Pagina.Chaves[i] := Pagina.Chaves[i + QtRetirado];

    for i := 0 to CountToRight do
    begin
      Pagina.Filhos[i] := Pagina.Filhos[i + QtRetirado];

      if Pagina.Filhos[i] <> nil then
        Pagina.Filhos[i].IndiceNoPai := i;
    end;

    for i := CountToRight + 1 to High(Pagina.Filhos) do
      Pagina.Filhos[i] := nil;

    NovoIrmaoEsq.SetCountChaves(CountToLeft);
    Pagina.SetCountChaves(CountToRight);
    {NovoIrmaoEsq.CountChaves := CountToLeft;
    Pagina.CountChaves := CountToRight;}

    // Ligar os pontos no OPai
    OPai.Filhos[IndiceNoPai] := NovoIrmaoEsq;
    OPai.Filhos[IndiceNoPai + 1] := Pagina;

    NovoIrmaoEsq.Pai := OPai;
    NovoIrmaoEsq.IndiceNoPai := IndiceNoPai;

    Pagina.Pai := OPai;
    Pagina.IndiceNoPai := IndiceNoPai + 1;

    if IndiceEspaco > Mediana then
    begin
      IndiceEspaco := IndiceEspaco - QtRetirado;
    end else
      Pagina := NovoIrmaoEsq;
  end;

  for i := Pagina.CountChaves downto IndiceEspaco + 1 do
    Pagina.Chaves[i] := Pagina.Chaves[i - 1];

  for i := Pagina.CountChaves + 1 downto IndiceEspaco + 2 do
  begin
    Pagina.Filhos[i] := Pagina.Filhos[i - 1];

    if Pagina.Filhos[i] <> nil then
      Pagina.Filhos[i].IndiceNoPai := i;
  end;

  Pagina.Filhos[IndiceEspaco + 1] := nil;
  Pagina.SetCountChaves(Pagina.CountChaves + 1);
end;

procedure TArvoreB<T, U>.InnerDelete(Pagina: PPaginaB;
  const IndiceEspaco: Integer);
var
  OPai, FilhoEsq, FilhoDir, PagToDelete: PPaginaB;
  i, IndiceToDelete: Integer;
begin
  Self.IncrementarVersaoDaArvore();
  OPai := Pagina.Pai;

  if (Pagina.Filhos[IndiceEspaco] = nil) and (Pagina.Filhos[IndiceEspaco + 1] = nil) then
  begin
    // É uma Página Folha

    if (OPai <> nil) and (Pagina.CountChaves <= MIN_CHAVES) then
    begin
      // Não é ROOT e vai atingir o limite minimo
      IndiceToDelete := IndiceEspaco;
      Self.InnerDeletePucharDeCima(Pagina, IndiceToDelete);
    end else
    begin
      for i := IndiceEspaco to Pagina.CountChaves - 2 do
        Pagina.Chaves[i] := Pagina.Chaves[i + 1];

      //Dec(Pagina.CountChaves);
      Pagina.SetCountChaves(Pagina.CountChaves - 1);
    end;
  end else
  begin
    FilhoEsq := Pagina.Filhos[IndiceEspaco];
    FilhoDir := Pagina.Filhos[IndiceEspaco + 1];

    if FilhoDir <> nil then
    begin
      Self.InnerFindLowest(FilhoDir, PagToDelete, IndiceToDelete);

      if PagToDelete = nil then
        raise Exception.Create('Ops');

      Pagina.Chaves[IndiceEspaco] := PagToDelete.Chaves[IndiceToDelete];
      Self.InnerDelete(PagToDelete, IndiceToDelete);
      Exit;
    end;

    if FilhoEsq <> nil then
    begin
      Self.InnerFindHighest(FilhoEsq, PagToDelete, IndiceToDelete);

      if PagToDelete = nil then
        raise Exception.Create('Ops');

      Pagina.Chaves[IndiceEspaco] := PagToDelete.Chaves[IndiceToDelete];
      Self.InnerDelete(PagToDelete, IndiceToDelete);
      Exit;
    end;

    raise Exception.Create('Ops');
  end;
end;

function TArvoreB<T, U>.InnerDeletePucharDeCima(var Pagina: PPaginaB;
  var Indice: Integer): Integer;
var
  OPai, IrmaoEsq, IrmaoDir, PaginaAux: PPaginaB;
  IndiceTemp, i, QtACopiar: Integer;
  TempChave: TChave;
begin
  OPai := Pagina.Pai;
  Result := Indice;

  if OPai = nil then
  begin
    // Estamos na Raiz. Não temos irmão e nem pai. Não temos de onde puxar

    // Remover a Chave
    for i := Indice to Pagina.CountChaves - 2 do
      Pagina.Chaves[i] := Pagina.Chaves[i + 1];

    // Mover os Filhos

    for i := Indice + 1 to Pagina.CountChaves - 1 do
    begin
      Pagina.Filhos[i] := Pagina.Filhos[i + 1];

      if Pagina.Filhos[i] <> nil then
        Pagina.Filhos[i].IndiceNoPai := i;
    end;

    //Dec(Pagina.CountChaves);
    Pagina.SetCountChaves(Pagina.CountChaves - 1);
    Exit;
  end;

  if Pagina.IndiceNoPai > 0 then
    IrmaoEsq := OPai.Filhos[Pagina.IndiceNoPai - 1]
  else
    IrmaoEsq := nil;

  if Pagina.IndiceNoPai < OPai.CountChaves then
    IrmaoDir := OPai.Filhos[Pagina.IndiceNoPai + 1]
  else
    IrmaoDir := nil;

  if IrmaoDir <> nil then
    if IrmaoDir.CountChaves > MIN_CHAVES then
    begin
      // Vamos pegar uma chave do irmão da direita

      // Remover a chave do Indice
      for i := Indice to Pagina.CountChaves - 2 do
        Pagina.Chaves[i] := Pagina.Chaves[i + 1];

      for i := Indice + 1 to Pagina.CountChaves - 1 do
      begin
        Pagina.Filhos[i] := Pagina.Filhos[i + 1];

        if Pagina.Filhos[i] <> nil then
          Pagina.Filhos[i].IndiceNoPai := i;
      end;

      // Copiar a Chave do Pai
      Pagina.Chaves[Pagina.CountChaves - 1] := OPai.Chaves[Pagina.IndiceNoPai];

      // Pegar os primeiros filhos do irmão da direita
      PaginaAux := IrmaoDir.Filhos[0];
      Pagina.Filhos[Pagina.CountChaves] := PaginaAux;

      if PaginaAux <> nil then
      begin
        PaginaAux.Pai := Pagina;
        PaginaAux.IndiceNoPai := Pagina.CountChaves;
      end;

      // Mover a Chave do irmão direita para o Pai
      OPai.Chaves[Pagina.IndiceNoPai] := IrmaoDir.Chaves[0];

      // Excluir a Chave do Irmão da Direita

      for i := 0 to IrmaoDir.CountChaves - 2 do
        IrmaoDir.Chaves[i] := IrmaoDir.Chaves[i + 1];

      for i := 0 to IrmaoDir.CountChaves - 1 do
      begin
        IrmaoDir.Filhos[i] := IrmaoDir.Filhos[i + 1];

        if IrmaoDir.Filhos[i] <> nil then
          IrmaoDir.Filhos[i].IndiceNoPai := i;
      end;

      //Dec(IrmaoDir.CountChaves);
      IrmaoDir.SetCountChaves(IrmaoDir.CountChaves - 1);
      Exit;
    end;

  if IrmaoEsq <> nil then // Não conseguimos negociar com o irmão da direita, vamos tentar no da esquerda
    if IrmaoEsq.CountChaves > MIN_CHAVES then
    begin
      // Remover a chave do Indice abrindo espaço no inicio para caber a Chave do Pai
      QtACopiar := 1;

      // Copiar os que estão do lado esquerdo da Chave que vai ser apagada
      for i := Indice + QtACopiar downto QtACopiar + 1 do  // Chaves
        Pagina.Chaves[i - 1] := Pagina.Chaves[i - QtACopiar - 1];

      for i := Indice + QtACopiar downto QtACopiar do  // Filhos
      begin
        Pagina.Filhos[i] := Pagina.Filhos[i - QtACopiar];

        if Pagina.Filhos[i] <> nil then
          Pagina.Filhos[i].IndiceNoPai := i;
      end;

      // Copiar a Chave do Pai
      Pagina.Chaves[0] := OPai.Chaves[Pagina.IndiceNoPai - 1];

      // Pegar os ultimos filhos do irmão da esquerda
      PaginaAux := IrmaoEsq.Filhos[IrmaoEsq.CountChaves];
      Pagina.Filhos[0] := PaginaAux;

      if PaginaAux <> nil then
      begin
        PaginaAux.Pai := Pagina;
        PaginaAux.IndiceNoPai := 0;
      end;

      // Mover a Chave do irmão da esquerda para o Pai
      OPai.Chaves[Pagina.IndiceNoPai - 1] := IrmaoEsq.Chaves[IrmaoEsq.CountChaves - 1];

      // Excluir a Chave do Irmão da Direita
      //Dec(IrmaoEsq.CountChaves);
      IrmaoEsq.SetCountChaves(IrmaoEsq.CountChaves - 1);
      Exit;
    end;

  // Se chegou aqui, os irmãos não podem doar uma Chave... Devemos mesclar

  if IrmaoDir <> nil then
  begin
    // Mesclar com o irmão da direita
    TempChave := OPai.Chaves[Pagina.IndiceNoPai]; // Obtendo a chave que vai vir de cima

    // Remover a chave do Indice
    for i := Indice to Pagina.CountChaves - 2 do
      Pagina.Chaves[i] := Pagina.Chaves[i + 1];

    for i := Indice + 1 to Pagina.CountChaves - 1 do
    begin
      Pagina.Filhos[i] := Pagina.Filhos[i + 1];

      if Pagina.Filhos[i] <> nil then
        Pagina.Filhos[i].IndiceNoPai := i;
    end;

    // Copiar uma Chave do Pai
    Pagina.Chaves[Pagina.CountChaves - 1] := TempChave;

    // Copiar as Chaves e Filhos do Irmão
    QtACopiar := IrmaoDir.CountChaves;

    for i := Pagina.CountChaves to Pagina.CountChaves + QtACopiar - 1 do
      Pagina.Chaves[i] := IrmaoDir.Chaves[i - Pagina.CountChaves];

    for i := Pagina.CountChaves to Pagina.CountChaves + QtACopiar do
    begin
      Pagina.Filhos[i] := IrmaoDir.Filhos[i - Pagina.CountChaves];

      if Pagina.Filhos[i] <> nil then
      begin
        Pagina.Filhos[i].Pai := Pagina;
        Pagina.Filhos[i].IndiceNoPai := i;
      end;
    end;

    //Pagina.CountChaves := Pagina.CountChaves + IrmaoDir.CountChaves;
    Pagina.SetCountChaves(Pagina.CountChaves + IrmaoDir.CountChaves);

    if OPai.CountChaves > MIN_CHAVES then
    begin
      // Remover a Chave

      for i := Pagina.IndiceNoPai to OPai.CountChaves - 2 do
        OPai.Chaves[i] := OPai.Chaves[i + 1];

      // Mover os Filhos

      for i := Pagina.IndiceNoPai + 1 to OPai.CountChaves - 1 do
      begin
        OPai.Filhos[i] := OPai.Filhos[i + 1];

        if OPai.Filhos[i] <> nil then
          OPai.Filhos[i].IndiceNoPai := i;
      end;

      //Dec(OPai.CountChaves);
      OPai.SetCountChaves(OPai.CountChaves - 1);
    end else
    begin
      Self.InnerDeletePucharDeCima(OPai, Pagina.IndiceNoPai);

      if OPai.CountChaves = 0 then
      begin
        FRaiz := Pagina;
        Pagina.Pai := nil;
        Pagina.IndiceNoPai := 0;
        Self.DestruirPagina(OPai);
        OPai := nil;
      end;
    end;

    Self.DestruirPagina(IrmaoDir);
    Exit;
  end;

  if IrmaoEsq <> nil then
  begin
    // Mesclar com o irmão da esquerda
    TempChave := OPai.Chaves[Pagina.IndiceNoPai - 1]; // Obtendo a chave que vai vir de ecima

    // Remover a chave do Indice abrindo espaço no inicio para caber a Chave do Pai + Chaves do Irmao da Esquerda
    QtACopiar := IrmaoEsq.CountChaves;

    // Copiar os que estão do lado direito da Chave que vai ser apagada
    for i := Pagina.CountChaves + QtACopiar - 1 downto Indice + QtACopiar + 1 do
      Pagina.Chaves[i] := Pagina.Chaves[i - QtACopiar];

    for i := Pagina.CountChaves + QtACopiar downto Indice + QtACopiar + 1 do
    begin
      Pagina.Filhos[i] := Pagina.Filhos[i - QtACopiar];

      if Pagina.Filhos[i] <> nil then
        Pagina.Filhos[i].IndiceNoPai := i;
    end;

    // Copiar os que estão do lado esquerdo da Chave que vai ser apagada
    for i := Indice + QtACopiar downto QtACopiar + 1 do  // Chaves
      Pagina.Chaves[i] := Pagina.Chaves[i - QtACopiar - 1];

    for i := Indice + QtACopiar downto QtACopiar do  // Filhos
    begin
      Pagina.Filhos[i + 1] := Pagina.Filhos[i - QtACopiar];

      if Pagina.Filhos[i + 1] <> nil then
        Pagina.Filhos[i + 1].IndiceNoPai := i + 1;
    end;

    // Copiar a Chave do Pai
    Pagina.Chaves[IrmaoEsq.CountChaves] := TempChave;

    // Copiar as Chaves e Filhos do Irmão da Esquerda
    for i := 0 to IrmaoEsq.CountChaves - 1 do
      Pagina.Chaves[i] := IrmaoEsq.Chaves[i];

    for i := 0 to IrmaoEsq.CountChaves do
    begin
      Pagina.Filhos[i] := IrmaoEsq.Filhos[i];

      if Pagina.Filhos[i] <> nil then
      begin
        Pagina.Filhos[i].Pai := Pagina;
        Pagina.Filhos[i].IndiceNoPai := i;
      end;
    end;

    Pagina.SetCountChaves(Pagina.CountChaves + IrmaoEsq.CountChaves);

    if OPai.CountChaves > MIN_CHAVES then
    begin
      // Remover a Chave

      for i := Pagina.IndiceNoPai to OPai.CountChaves - 2 do
        OPai.Chaves[i] := OPai.Chaves[i + 1];

      // Mover os Filhos

      for i := Pagina.IndiceNoPai - 1 to OPai.CountChaves - 1 do
      begin
        OPai.Filhos[i] := OPai.Filhos[i + 1];

        if OPai.Filhos[i] <> nil then
          OPai.Filhos[i].IndiceNoPai := i;
      end;

      //Dec(OPai.CountChaves);
      OPai.SetCountChaves(OPai.CountChaves - 1);
    end else
    begin
      IndiceTemp := Pagina.IndiceNoPai - 1;
      Self.InnerDeletePucharDeCima(OPai, IndiceTemp);

      if OPai.CountChaves = 0 then
      begin
        FRaiz := Pagina;
        Pagina.Pai := nil;
        Pagina.IndiceNoPai := 0;
        Self.DestruirPagina(OPai);
        OPai := nil;
      end;
    end;

    if OPai <> nil then
      OPai.Filhos[IrmaoEsq.IndiceNoPai] := Pagina;

    Pagina.IndiceNoPai := IrmaoEsq.IndiceNoPai;

    Self.DestruirPagina(IrmaoEsq);
    Exit;
  end;

  raise Exception.Create('Ops. Não deveria ter chegado até aqui');
end;

procedure TArvoreB<T, U>.InnerFindHighest(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
var
  Ind: Integer;
  POrg: PPaginaB;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Ind := POrg.CountChaves - 1;

    if POrg.Filhos[Ind + 1] <> nil then
    begin
      POrg := POrg.Filhos[Ind + 1];
      Continue;
    end;

    PEncontrada := POrg;
    IndiceEncontrado := Ind;
    Exit;
  end;
end;

procedure TArvoreB<T, U>.InnerFindHighestIneqMatch(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer; const Chave: T;
  const Inequality: TArvoreBInequalityOp);
var
  POrg, LastFoundPOrg: PPaginaB;
  LastFoundIndex: Integer;
  R: Integer;
  Min, Max, Meio, Comp: Integer;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;

  LastFoundPOrg := nil;
  LastFoundIndex := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := FComparer.Compare(POrg^.Chaves[Meio].ValorChave, Chave);

      case Inequality of
        abioMenor:
          if Comp < 0 then
          begin
            Min := Meio + 1; // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end else
            Max := Meio - 1;

        abioMenorIgual:
          if Comp <= 0 then
          begin
            Min := Meio + 1; // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end else
            Max := Meio - 1;

        abioMaior:
        begin
          if Comp > 0 then
          begin
            // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end;

          Min := Meio + 1;
        end;

        abioMaiorIgual:
        begin
          if Comp >= 0 then
          begin
            // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end;

          Min := Meio + 1;
        end

        else
          Min := Meio + 1; // ???
      end;
    end;

    if POrg.Filhos[Max + 1] <> nil then
      POrg := POrg.Filhos[Max + 1]
    else
      Break;
  end;

  if LastFoundPOrg <> nil then
  begin
    PEncontrada := LastFoundPOrg;
    IndiceEncontrado := LastFoundIndex;
  end;
end;

procedure TArvoreB<T, U>.InnerFindHighestPartialMatch(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
  const PartialValue: T; const Comparer: IComparer<T>);
var
  POrg, LastFoundPOrg: PPaginaB;
  LastFoundIndex: Integer;
  R: Integer;

  Min, Max, Meio, Comp: Integer;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;

  LastFoundPOrg := nil;
  LastFoundIndex := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := Comparer.Compare(POrg^.Chaves[Meio].ValorChave, PartialValue);

      if Comp = 0 then
      begin
        Min := Meio + 1; // Achou 1 parcial, mas vamos continuar olhando
        LastFoundIndex := Meio;
        LastFoundPOrg := POrg;
      end else
        if Comp > 0 then
          Max := Meio - 1
        else
          Min := Meio + 1;
    end;

    if POrg.Filhos[Max + 1] <> nil then
      POrg := POrg.Filhos[Max + 1]
    else
      Break;
  end;

  if LastFoundPOrg <> nil then
  begin
    PEncontrada := LastFoundPOrg;
    IndiceEncontrado := LastFoundIndex;
  end;
end;

procedure TArvoreB<T, U>.InnerFindLowest(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer);
var
  POrg: PPaginaB;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    if POrg.Filhos[0] <> nil then
    begin
      POrg := POrg.Filhos[0];
      Continue;
    end;

    PEncontrada := POrg;
    IndiceEncontrado := 0;
    Exit;
  end;
end;

procedure TArvoreB<T, U>.InnerFindLowestIneqMatch(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer; const Chave: T;
  const Inequality: TArvoreBInequalityOp);
var
  POrg, LastFoundPOrg: PPaginaB;
  LastFoundIndex: Integer;
  R: Integer;
  Min, Max, Meio, Comp: Integer;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;

  LastFoundPOrg := nil;
  LastFoundIndex := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := FComparer.Compare(POrg^.Chaves[Meio].ValorChave, Chave);

      case Inequality of
        abioMenor:
        begin
          if Comp < 0 then
          begin
            // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end;

          Max := Meio - 1;
        end;

        abioMenorIgual:
        begin
          if Comp <= 0 then
          begin
            // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end;

          Max := Meio - 1;
        end;

        abioMaior:
          if Comp > 0 then
          begin
            Max := Meio - 1; // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end else
            Min := Meio + 1;

        abioMaiorIgual:
          if Comp >= 0 then
          begin
            Max := Meio - 1; // Achou 1 parcial, mas vamos continuar olhando
            LastFoundIndex := Meio;
            LastFoundPOrg := POrg;
          end else
            Min := Meio + 1;

        else
          Max := Meio - 1;  // ?
      end;
    end;

    if POrg.Filhos[Min] <> nil then
      POrg := POrg.Filhos[Min]
    else
      Break;
  end;

  if LastFoundPOrg <> nil then
  begin
    PEncontrada := LastFoundPOrg;
    IndiceEncontrado := LastFoundIndex;
  end;
end;

procedure TArvoreB<T, U>.InnerFindLowestPartialMatch(PaginaOrigem: PPaginaB;
  out PEncontrada: PPaginaB; out IndiceEncontrado: Integer;
  const PartialValue: T; const Comparer: IComparer<T>);
var
  POrg, LastFoundPOrg: PPaginaB;
  LastFoundIndex: Integer;
  R: Integer;

  Min, Max, Meio, Comp: Integer;
begin
  PEncontrada := nil;
  IndiceEncontrado := -1;

  LastFoundPOrg := nil;
  LastFoundIndex := -1;
  POrg := PaginaOrigem;

  while POrg <> nil do
  begin
    Min := 0;
    Max := POrg.CountChaves - 1;

    while Min <= Max do
    begin
      Meio := (Min + Max) div 2;
      Comp := Comparer.Compare(POrg^.Chaves[Meio].ValorChave, PartialValue);

      if Comp = 0 then
      begin
        Max := Meio - 1; // Achou 1 parcial, mas vamos continuar olhando
        LastFoundIndex := Meio;
        LastFoundPOrg := POrg;
      end else
        if Comp > 0 then
          Max := Meio - 1
        else
          Min := Meio + 1;
    end;

    if POrg.Filhos[Min] <> nil then
      POrg := POrg.Filhos[Min]
    else
      Break;
  end;

  if LastFoundPOrg <> nil then
  begin
    PEncontrada := LastFoundPOrg;
    IndiceEncontrado := LastFoundIndex;
  end;
end;

procedure TArvoreB<T, U>.InnerInsert(InserirEm: PPaginaB; const IndiceInserir: Integer;
  Chave: T; Valor: U);
var
  ChaveCollated: T;
begin
  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerInsertCollated(InserirEm, IndiceInserir, ChaveCollated, Valor);
end;

procedure TArvoreB<T, U>.InnerInsertCollated(InserirEm: PPaginaB;
  const IndiceInserir: Integer; Chave: T; Valor: U);
var
  Indice: Integer;
begin
  Indice := IndiceInserir;
  Self.InnerAlocarEspaco(InserirEm, Indice);
  InserirEm.Chaves[Indice].ValorChave := Chave;
  InserirEm.Chaves[Indice].Valor := Valor;

  Inc(FCount);
  Self.IncrementarVersaoDaArvore();
end;

procedure TArvoreB<T, U>.Inserir(const Chave: T; const Valor: U);
var
  ChaveCollated: T;
begin
  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InserirCollated(ChaveCollated, Valor);
end;

procedure TArvoreB<T, U>.InserirCollated(const Chave: T; const Valor: U);
var
  InserirEm: PPaginaB;
  IndiceInserir: Integer;
begin
  if FRaiz = nil then
    FRaiz := AlocarPagina();

  Self.InnerLocalizarForInsertCollated(FRaiz, Chave, InserirEm,
                                       IndiceInserir);

  if InserirEm = nil then
    raise Exception.Create('InnerLocalizarForInsert retornou nil');

  with InserirEm^ do
    if IndiceInserir <= CountChaves - 1 then
      if FComparer.Compare(Chaves[IndiceInserir].ValorChave, Chave) = 0 then
        raise Exception.Create('Esta chave já existe');

  Self.InnerInsertCollated(InserirEm, IndiceInserir, Chave, Valor)
end;

procedure TArvoreB<T, U>.InserirOuAlterar(const Chave: T; const Valor: U);
var
  ChaveCollated: T;
begin
  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InserirOuAlterarCollated(ChaveCollated, Valor);
end;

procedure TArvoreB<T, U>.InserirOuAlterarCollated(const Chave: T;
  const Valor: U);
var
  InserirEm: PPaginaB;
  IndiceInserir: Integer;
  Alterou: Boolean;
begin
  if FRaiz = nil then
    FRaiz := AlocarPagina();

  Alterou := False;

  Self.InnerLocalizarForInsertCollated(FRaiz, Chave, InserirEm,
                                       IndiceInserir);

  if InserirEm = nil then
    raise Exception.Create('InnerLocalizarForInsert retornou nil');

  with InserirEm^ do
    if IndiceInserir <= CountChaves - 1 then
      if FComparer.Compare(Chaves[IndiceInserir].ValorChave, Chave) = 0 then
      begin
        Chaves[IndiceInserir].Valor := Valor;
        Alterou := True;
      end;

  if not Alterou then
    Self.InnerInsertCollated(InserirEm, IndiceInserir, Chave, Valor)
end;

procedure TArvoreB<T, U>.LiberarPaginaRecursivamente(var P: PPaginaB);
var
  i: Integer;
begin
  for i := 0 to P.CountChaves do
    if P.Filhos[i] <> nil then
      Self.LiberarPaginaRecursivamente(P.Filhos[i]);

  Self.DestruirPagina(P);
end;

procedure TArvoreB<T, U>.Limpar;
begin
  if FRaiz <> nil then
  begin
    Self.LiberarPaginaRecursivamente(FRaiz);
    FRaiz := nil;
  end;

  FCount := 0;
  Self.IncrementarVersaoDaArvore();
end;

procedure TArvoreB<T, U>.NavGotoFirst(Nav: TArvoreBNavigator);
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindLowest(FRaiz, Pagina, Indice)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := False;
      Nav.FBof := True;
      Exit;
    end;
  end;

  Nav.FPaginaAtual := nil;
  Nav.FIndiceAtual := -1;
  Nav.FEof := True;
  Nav.FBof := True;
end;

function TArvoreB<T, U>.NavGotoFirstInequalityMatch(Nav: TArvoreBNavigator;
  const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  Result := False;

  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindLowestIneqMatch(FRaiz, Pagina, Indice, Chave, Inequality)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Result := True;

      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := False;
      Nav.FBof := False;
    end;
  end;
end;

function TArvoreB<T, U>.NavGotoFirstPartialMatch(Nav: TArvoreBNavigator;
  const PartialValue: T; const PartialComparer: IComparer<T>): Boolean;
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  Result := False;

  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindLowestPartialMatch(FRaiz, Pagina, Indice, PartialValue, PartialComparer)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Result := True;

      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := False;
      Nav.FBof := False;
    end;
  end;
end;

procedure TArvoreB<T, U>.NavGotoLast(Nav: TArvoreBNavigator);
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindHighest(FRaiz, Pagina, Indice)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := True;
      Nav.FBof := False;
      Exit;
    end;
  end;

  Nav.FPaginaAtual := nil;
  Nav.FIndiceAtual := -1;
  Nav.FEof := True;
  Nav.FBof := True;
end;

function TArvoreB<T, U>.NavGotoLastInequalityMatch(Nav: TArvoreBNavigator;
  const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  Result := False;

  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindHighestIneqMatch(FRaiz, Pagina, Indice, Chave, Inequality)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Result := True;

      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := False;
      Nav.FBof := False;
    end;
  end;
end;

function TArvoreB<T, U>.NavGotoLastPartialMatch(Nav: TArvoreBNavigator;
  const PartialValue: T; const PartialComparer: IComparer<T>): Boolean;
var
  Pagina: PPaginaB;
  Indice: Integer;
begin
  Result := False;

  if FRaiz <> nil then
  begin
    if FCount > 0 then
      Self.InnerFindHighestPartialMatch(FRaiz, Pagina, Indice, PartialValue, PartialComparer)
    else
      Pagina := nil;

    if Pagina <> nil then
    begin
      Result := True;

      Nav.FPaginaAtual := Pagina;
      Nav.FIndiceAtual := Indice;

      Nav.FEof := False;
      Nav.FBof := False;
    end;
  end;
end;

procedure TArvoreB<T, U>.NavGotoNext(Nav: TArvoreBNavigator);
var
  PagAtual, PagEncontrada, OPai: PPaginaB;
  IndAtual, IndEncontrado, IndPai: Integer;
begin
  PagAtual := Nav.FPaginaAtual;
  IndAtual := Nav.FIndiceAtual;

  if (PagAtual = nil) or (IndAtual = -1) then
  begin
    Nav.FEof := True;
    Exit;
  end;

  if PagAtual.Filhos[IndAtual + 1] <> nil then
  begin
    PagEncontrada := nil;
    Self.InnerFindLowest(PagAtual.Filhos[IndAtual + 1], PagEncontrada, IndEncontrado);

    if PagEncontrada = nil then
      raise Exception.Create('Bug na navegação do indice. TArvoreB<T, U>.NavGotoNext');

    Nav.FEof := False;
    Nav.FBof := False;
    Nav.FPaginaAtual := PagEncontrada;
    Nav.FIndiceAtual := IndEncontrado;
  end else
    if IndAtual < PagAtual.CountChaves - 1 then
    begin
      Inc(Nav.FIndiceAtual);
    end else
    begin
      // Já estamos apontando para o último desta página, vamos procurar recursivamente nos pais

      OPai := PagAtual.Pai;
      IndPai := PagAtual.IndiceNoPai;

      while True do
      begin
        if OPai = nil then
        begin
          // chegamos no fim da raiz, marcar como EOF
          Nav.FEof := True;
          Break;
        end;

        if IndPai <= OPai.CountChaves - 1 then
        begin
          Nav.FPaginaAtual := OPai;
          Nav.FIndiceAtual := IndPai;
          Break;
        end else
        begin
          IndPai := OPai.IndiceNoPai;
          OPai := OPai.Pai;
          Continue;
        end;
      end;
    end;
end;

procedure TArvoreB<T, U>.NavGotoPrevious(Nav: TArvoreBNavigator);
var
  PagAtual, PagEncontrada, OPai: PPaginaB;
  IndAtual, IndEncontrado, IndPai: Integer;
begin
  PagAtual := Nav.FPaginaAtual;
  IndAtual := Nav.FIndiceAtual;

  if (PagAtual = nil) or (IndAtual = -1) then
  begin
    Nav.FBof := True;
    Exit;
  end;

  if PagAtual.Filhos[IndAtual] <> nil then
  begin
    PagEncontrada := nil;
    Self.InnerFindHighest(PagAtual.Filhos[IndAtual], PagEncontrada, IndEncontrado);

    if PagEncontrada = nil then
      raise Exception.Create('Bug na navegação do indice. TArvoreB<T, U>.NavGotoNext');

    Nav.FEof := False;
    Nav.FBof := False;
    Nav.FPaginaAtual := PagEncontrada;
    Nav.FIndiceAtual := IndEncontrado;
  end else
    if IndAtual > 0 then
    begin
      Dec(Nav.FIndiceAtual);
    end else
    begin
      // Já estamos apontando para o primeiro desta página, vamos procurar recursivamente nos pais

      OPai := PagAtual.Pai;
      IndPai := PagAtual.IndiceNoPai;

      while True do
      begin
        if OPai = nil then
        begin
          // chegamos no fim da raiz, marcar como EOF
          Nav.FBof := True;
          Break;
        end;

        if IndPai > 0 then
        begin
          Nav.FPaginaAtual := OPai;
          Nav.FIndiceAtual := IndPai - 1;
          Break;
        end else
        begin
          IndPai := OPai.IndiceNoPai;
          OPai := OPai.Pai;
          Continue;
        end;
      end;
    end;
end;

function TArvoreB<T, U>.ObterMaiorChave(var Chave: T): Boolean;
var
  PEncontrada: PPaginaB;
  IndiceEncontrado: Integer;
begin
  if (FRaiz = nil) or (FCount = 0) then
  begin
    Result := False;
    Exit;
  end;

  Self.InnerFindHighest(FRaiz, PEncontrada, IndiceEncontrado);

  if PEncontrada <> nil then
  begin
    Chave := PEncontrada.Chaves[IndiceEncontrado].ValorChave;
    Result := True;
  end else
    Result := False;
end;

function TArvoreB<T, U>.ObterMenorChave(var Chave: T): Boolean;
var
  PEncontrada: PPaginaB;
  IndiceEncontrado: Integer;
begin
  if (FRaiz = nil) or (FCount = 0) then
  begin
    Result := False;
    Exit;
  end;

  Self.InnerFindLowest(FRaiz, PEncontrada, IndiceEncontrado);

  if PEncontrada <> nil then
  begin
    Chave := PEncontrada.Chaves[IndiceEncontrado].ValorChave;
    Result := True;
  end else
    Result := False;
end;

procedure TArvoreB<T, U>.ProcessarValor(const Chave: T;
  const Processor: TArvoreBProcessor<T, U>);
var
  ChaveCollated: T;
begin
  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.ProcessarValorCollated(ChaveCollated, Processor);
end;

procedure TArvoreB<T, U>.ProcessarValorCollated(const Chave: T;
  const Processor: TArvoreBProcessor<T, U>);
var
  InserirEm: PPaginaB;
  IndiceInserir: Integer;
  Alterou: Boolean;
  Valor: U;
begin
  if FRaiz = nil then
    FRaiz := AlocarPagina();

  Alterou := False;

  Self.InnerLocalizarForInsertCollated(FRaiz, Chave, InserirEm,
                                       IndiceInserir);

  if InserirEm = nil then
    raise Exception.Create('InnerLocalizarForInsert retornou nil');

  with InserirEm^ do
    if IndiceInserir <= CountChaves - 1 then
      if FComparer.Compare(Chaves[IndiceInserir].ValorChave, Chave) = 0 then
      begin
        Processor(Chaves[IndiceInserir].ValorChave, Chaves[IndiceInserir].Valor, False);
        Alterou := True;
      end;

  if not Alterou then
  begin
    Processor(Chave, Valor, True);
    Self.InnerInsertCollated(InserirEm, IndiceInserir, Chave, Valor)
  end;
end;

procedure TArvoreB<T, U>.ProcessarValorTodasChaves(const Processor: TArvoreBProcessor<T, U>);
var
  Nav: IArvoreBNavigator<T, U>;
begin
  Nav := Self.CriarNavigator();

  while not Nav.EOF do
  begin
    Processor(Nav.ChaveAtual, PPointerToU(Nav.PtrValorAtual)^, False);
    Nav.Next;
  end;
end;

procedure TArvoreB<T, U>.IterarSobValores(const Iterator: TArvoreBIteratorOnValor<U>);
var
  Nav: IArvoreBNavigator<T, U>;
begin
  Nav := Self.CriarNavigator();

  while not Nav.EOF do
  begin
    Iterator(Nav.ValorAtual);
    Nav.Next;
  end;
end;

procedure TArvoreB<T, U>.Remover(const Chave: T);
var
  Pagina: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
    Exit;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerLocateCollated(FRaiz, ChaveCollated, Pagina, Indice);

  if Pagina = nil then
    Exit;

  Pagina.Chaves[Indice].ValorChave := Default(T);
  Pagina.Chaves[Indice].Valor := Default(U);
  Self.InnerDelete(Pagina, Indice);
  Dec(FCount);
end;

function TArvoreB<T, U>.TryGet(const Chave: T; out Valor: U): Boolean;
var
  PEncontrado: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerLocateCollated(FRaiz, ChaveCollated, PEncontrado, Indice);

  if PEncontrado <> nil then
  begin
    Valor := PEncontrado^.Chaves[Indice].Valor;
    Result := True;
  end else
    Result := False;
end;

function TArvoreB<T, U>.TryGetFirstInequalityMatch(const Chave: T;
  const Inequality: TArvoreBInequalityOp; var RetChave: T; var RetValue: U): Boolean;
var
  PEncontrado: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerFindLowestIneqMatch(FRaiz, PEncontrado, Indice, ChaveCollated,
                                Inequality);

  if PEncontrado <> nil then
  begin
    RetChave := PEncontrado^.Chaves[Indice].ValorChave;
    RetValue := PEncontrado^.Chaves[Indice].Valor;
    Result := True;
  end else
    Result := False;
end;

function TArvoreB<T, U>.TryGetLastInequalityMatch(const Chave: T;
  const Inequality: TArvoreBInequalityOp; var RetChave: T; var RetValue: U): Boolean;
var
  PEncontrado: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerFindHighestIneqMatch(FRaiz, PEncontrado, Indice, ChaveCollated,
                                 Inequality);

  if PEncontrado <> nil then
  begin
    RetChave := PEncontrado^.Chaves[Indice].ValorChave;
    RetValue := PEncontrado^.Chaves[Indice].Valor;
    Result := True;
  end else
    Result := False;
end;

function TArvoreB<T, U>.TryGetPtr(const Chave: T; out PtrValor: Pointer): Boolean;
var
  PEncontrado: PPaginaB;
  Indice: Integer;
  ChaveCollated: T;
begin
  if FRaiz = nil then
  begin
    Result := False;
    Exit;
  end;

  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.InnerLocateCollated(FRaiz, ChaveCollated, PEncontrado, Indice);

  if PEncontrado <> nil then
  begin
    PtrValor := @PEncontrado^.Chaves[Indice].Valor;
    Result := True;
  end else
    Result := False;
end;

procedure TArvoreB<T, U>.ValidarNavigatorsCriados;
begin
  try
    if FQtNavigatorsCriado > 0 then
      raise Exception.Create('Programador, existem navigators criados de uma Árvore B. '+
                             'Isto é uma operação não segura! Você deve destruir os navigators primeiro.');
  except
    // Apenas logar exceptions em LogErros
  end;
end;

constructor TArvoreB<T, U>.Create(Comparer: IComparer<T>);
begin
  Self.Create(Comparer, nil);
end;

procedure TArvoreB<T, U>.Alterar(const Chave: T; const Valor: U);
var
  ChaveCollated: T;
begin
  if FCollate <> nil then
    FCollate.Preparar(Chave, ChaveCollated)
  else
    ChaveCollated := Chave;

  Self.AlterarCollated(ChaveCollated, Valor);
end;

procedure TArvoreB<T, U>.AlterarCollated(const Chave: T; const Valor: U);
var
  PEncontrado: PPaginaB;
  Indice: Integer;
begin
  if FRaiz = nil then
    raise Exception.Create('Esta chave não existe para que seu valor seja alterado');

  Self.InnerLocateCollated(FRaiz, Chave, PEncontrado, Indice);

  if PEncontrado <> nil then
    PEncontrado^.Chaves[Indice].Valor := Valor
  else
    raise Exception.Create('Esta chave não existe para que seu valor seja alterado');
end;

constructor TArvoreB<T, U>.Create(Collate: TFuncaoArvoreBCollate<T>);
begin
  Self.Create(nil, Collate);
end;

{ TArvoreB<T, U>.TArvoreBNavigator }

constructor TArvoreB<T, U>.TArvoreBNavigator.Create;
begin
  FPaginaAtual := nil;
  FEof := True;
  FBof := True;
end;

destructor TArvoreB<T, U>.TArvoreBNavigator.Destroy;
begin
  if FArvore <> nil then
    TInterlocked.Decrement(FArvore.FQtNavigatorsCriado);

  inherited;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GetBOF: Boolean;
begin
  Result := FBof;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GetChaveAtual: T;
begin
  Self.NeedVersaoSincronizada();
  Result := FPaginaAtual.Chaves[FIndiceAtual].ValorChave;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GetEOF: Boolean;
begin
  Result:= FEof;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GetPtrValorAtual: Pointer;
begin
  Self.NeedVersaoSincronizada();
  Result := @FPaginaAtual.Chaves[FIndiceAtual].Valor;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GetValorAtual: U;
begin
  Self.NeedVersaoSincronizada();
  Result := FPaginaAtual.Chaves[FIndiceAtual].Valor;
end;

procedure TArvoreB<T, U>.TArvoreBNavigator.GotoFirst;
begin
  FVersaoArvore := FArvore.FVersao;
  FArvore.NavGotoFirst(Self);
end;

function TArvoreB<T, U>.TArvoreBNavigator.GotoFirstInequalityMatch(
  const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
begin
  Result := FArvore.NavGotoFirstInequalityMatch(Self, Chave, Inequality);

  if Result then
    FVersaoArvore := FArvore.FVersao;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GotoFirstPartialMatch(
  const PartialValue: T; PartialComparer: IComparer<T>): Boolean;
begin
  Result := FArvore.NavGotoFirstPartialMatch(Self, PartialValue, PartialComparer);

  if Result then
    FVersaoArvore := FArvore.FVersao;
end;

procedure TArvoreB<T, U>.TArvoreBNavigator.GotoLast;
begin
  FVersaoArvore := FArvore.FVersao;
  FArvore.NavGotoLast(Self);
end;

function TArvoreB<T, U>.TArvoreBNavigator.GotoLastInequalityMatch(
  const Chave: T; const Inequality: TArvoreBInequalityOp): Boolean;
begin
  Result := FArvore.NavGotoLastInequalityMatch(Self, Chave, Inequality);

  if Result then
    FVersaoArvore := FArvore.FVersao;
end;

function TArvoreB<T, U>.TArvoreBNavigator.GotoLastPartialMatch(
  const PartialValue: T; PartialComparer: IComparer<T>): Boolean;
begin
  Result := FArvore.NavGotoLastPartialMatch(Self, PartialValue, PartialComparer);

  if Result then
    FVersaoArvore := FArvore.FVersao;
end;

procedure TArvoreB<T, U>.TArvoreBNavigator.NeedVersaoSincronizada;
begin
  if FArvore.FVersao <> FVersaoArvore then
    raise Exception.Create('Ops. Ocorreram mudanças na Arvore B. Este navigator deve ser ressincronizado com algum método GotoXXXX');
end;

procedure TArvoreB<T, U>.TArvoreBNavigator.Next;
begin
  Self.NeedVersaoSincronizada();
  FArvore.NavGotoNext(Self);
end;

procedure TArvoreB<T, U>.TArvoreBNavigator.Previous;
begin
  Self.NeedVersaoSincronizada();
  FArvore.NavGotoPrevious(Self);
end;

{ TArvoreB<T, U>.TPaginaB }

procedure TArvoreB<T, U>.TPaginaB.SetCountChaves(const V: Integer);
var
  i: Integer;
begin
  if V < Self.CountChaves then
    for i := V to Self.CountChaves - 1 do
    begin
      Self.Chaves[i].ValorChave := Default(T);
      Self.Chaves[i].Valor := Default(U);
  	  Self.Filhos[i + 1] := nil;
    end;

  Self.CountChaves := V;
end;

{ TArvoreBSet<T> }

constructor TArvoreBSet<T>.Create(Comparer: IComparer<T>;
  Collate: TFuncaoArvoreBCollate<T>);
begin
  FArvore := TArvoreB<T, TEmptyRecord>.Create(Comparer, Collate);
end;

constructor TArvoreBSet<T>.Create;
begin
  FArvore := TArvoreB<T, TEmptyRecord>.Create;
end;

constructor TArvoreBSet<T>.Create(Collate: TFuncaoArvoreBCollate<T>);
begin
  FArvore := TArvoreB<T, TEmptyRecord>.Create(Collate);
end;

function TArvoreBSet<T>.CriarNavigator: IArvoreBNavigator<T, TEmptyRecord>;
begin
  Result := FArvore.CriarNavigator();
end;

function TArvoreBSet<T>.CriarNavigatorAndGotoLast: IArvoreBNavigator<T, TEmptyRecord>;
begin
  Result := FArvore.CriarNavigatorAndGotoLast();
end;

constructor TArvoreBSet<T>.Create(Comparer: IComparer<T>);
begin
  FArvore := TArvoreB<T, TEmptyRecord>.Create(Comparer);
end;

destructor TArvoreBSet<T>.Destroy;
begin
  FreeAndNil(FArvore);
  inherited;
end;

function TArvoreBSet<T>.ExisteChave(const Chave: T): Boolean;
begin
  Result := FArvore.ExisteChave(Chave);
end;

function TArvoreBSet<T>.ExistePartialMatch(const Chave: T;
  PartialComparer: IComparer<T>): Boolean;
begin
  Result := FArvore.ExistePartialMatch(Chave, PartialComparer);
end;

function TArvoreBSet<T>.GetAsArray: TArray<T>;
begin
  Result := FArvore.GetArrayDeChaves();
end;

function TArvoreBSet<T>.GetCollate: TFuncaoArvoreBCollate<T>;
begin
  Result := FArvore.Collate;
end;

function TArvoreBSet<T>.GetCount: Integer;
begin
  Result := FArvore.Count;
end;

procedure TArvoreBSet<T>.Inserir(const Chave: T);
var
  Nada: TEmptyRecord;
begin
  FArvore.Inserir(Chave, Nada);
end;

procedure TArvoreBSet<T>.InserirSeNaoExistir(const Chave: T);
var
  Nada: TEmptyRecord;
begin
  FArvore.InserirOuAlterar(Chave, Nada);
end;

procedure TArvoreBSet<T>.Limpar;
begin
  FArvore.Limpar();
end;

function TArvoreBSet<T>.ObterMaiorChave(var Chave: T): Boolean;
begin
  Result := FArvore.ObterMaiorChave(Chave);
end;

function TArvoreBSet<T>.ObterMenorChave(var Chave: T): Boolean;
begin
  Result := FArvore.ObterMenorChave(Chave);
end;

procedure TArvoreBSet<T>.Remover(const Chave: T);
begin
  FArvore.Remover(Chave);
end;

end.

