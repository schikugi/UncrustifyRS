//{$WARNINGS OFF}
//=============================================================================
//  パイプ処理によりコマンド実行と結果をリダイレクトするためのユニット
//
//  利用方法はこのユニットを付属しているサンプルプログラム参照
//
//  Fusaさんの
//  http://delfusa.main.jp/delfusafloor/opensource/delfusa_library_f.html
//  にあるコードを
//  (1) 好みのスタイルにコードを整形
//  (2) 関数を1つ追加
//  (3) Delphi2009以降対応のコードを追加
//  (4) エラー内容を標準出力にリダイレクト
//  (5) 各関数の先頭にコメントを追加
//  (6) コマンドライン解析用のコードは削除
//
//  ライセンス等については以下を参照のこと
//  http://delfusa.main.jp/delfusafloor/opensource/first.shtml
//  Fusaさんサンクスです
//
//-----------------------------------------------------------------------------
//
//  2010年01月26日
//
//  2010年02月09日
//    ・間違ったユニットファイルを添付していたのでこのファイル添付に修正
//    ・CreateProcessの引数のDETACHED_PROCESSをCREATE_NEW_CONSOLEに変更
//    ・TerminateProcess(ProcessInfo.hProcess, 0);を追加(Mr.XRAY)
//
//  2010年03月17日
//    ・UniqueString(CommandLine);を追加(参照カウンタ対策)
//
//
//  2010年03月25日
//    ・Result := S;をResult := UnicodeString(S);と明示的にキャスト
//
//-----------------------------------------------------------------------------
//
//  【動作確認環境】
//
//  Windows XP(SP3)
//    Delphi6(UP2) Pro
//    Delphi7 Pro
//    Delphi2007-R2 Pro
//    Delphi2009(UP3) Pro
//    Delphi2010(UP5) Pro
//
//  Windows Vista
//    Delphi2009(UP3) Pro
//    Delphi2010(UP5) Pro
//
//  Presented by Mr.XRAY
//  http://mrxray.on.coocan.jp/
//=============================================================================
unit uCommandLineUnit;

interface

uses
  SysUtils, Windows, Classes, Forms;

type
  TCommandLineUnit = class
  private
    FLoopProcessMessages: Boolean;
//    function GrabStdOut(CommandLine: string; StdIn: TMemoryStream): TMemoryStream;
    function GetStringFromStream(const Stream: TStream): String;
  public
    constructor Create;
    function GrabStdOut(CommandLine: string; StdIn: TMemoryStream): TMemoryStream;
    function GrabStdOutText(CommandLine: string): String; overload;
    function GrabStdOutText(CommandLine: string; StdInput: TStrings):
      String; overload;
    property LoopProcessMessages: Boolean read FLoopProcessMessages
                                          write FLoopProcessMessages;
  end;

implementation

//-----------------------------------------------------------------------------
//  実際にコマンドラインのコマンドを実行してリダイレクトする関数
//-----------------------------------------------------------------------------
function TCommandLineUnit.GrabStdOut(CommandLine: string;
  StdIn: TMemoryStream): TMemoryStream;
const
  BUFFER_SIZE = 8192;
var
  hReadPipe          : THandle;
  hWritePipe         : THandle;
  hStdInReadPipe     : THandle;
  hStdInWritePipe    : THandle;
  hStdInWritePipeDup : THandle;
  hErrReadPipe       : THandle;
  hErrWritePipe      : THandle;

  sa            : TSecurityAttributes;
  StartupInfo   : TStartupInfo;
  ProcessInfo   : TProcessInformation;
  bufStdOut     : array[0..BUFFER_SIZE] of Byte;
  bufErrOut     : array[0..BUFFER_SIZE] of Byte;
  bufStdIn      : array[0..BUFFER_SIZE] of Byte;
  dwStdOut      : DWord;
  dwErrOut      : DWord;
  dwRet         : DWord;
  StreamBufferSize : DWord;
  nWritten         : DWord;
begin
  Result := nil;

  with sa do
  begin
    nLength := sizeof(TSecurityAttributes);
    lpSecurityDescriptor := nil;
    bInheritHandle := true;
  end;

  hReadPipe     := 0;
  hWritePipe    := 0;
  hErrReadPipe  := 0;
  hErrWritePipe := 0;

  StdIn.Position := 0;

  CreatePipe(hStdInReadPipe, hStdInWritePipe, @sa, BUFFER_SIZE);
  DuplicateHandle(GetCurrentProcess(), hStdInWritePipe, GetCurrentProcess(),
                  @hStdInWritePipeDup, 0, false, DUPLICATE_SAME_ACCESS);
  CloseHandle(hStdInWritePipe);

  CreatePipe(hReadPipe, hWritePipe, @sa, BUFFER_SIZE);
  try
    CreatePipe(hErrReadPipe, hErrWritePipe, @sa, BUFFER_SIZE);
    try
      ZeroMemory(@StartupInfo, sizeof(TStartupInfo));
      with StartupInfo do
      begin
        cb := sizeof(TStartupInfo);
        dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        // これがないと DOS 窓が表示されてしまう
        wShowWindow := SW_HIDE;
        // 標準 IO にパイプの端っこを指定してやる
        hStdInput  := hStdInReadPipe;
        hStdOutput := hWritePipe;
        hStdError  := hErrWritePipe;
      end;

      //コンソールアプリ起動
      //DETACHED_PROCESSをCREATE_NEW_CONSOLEに変更(Mr.XRAY)
      UniqueString(CommandLine);
      if CreateProcess(nil,
                       PChar(CommandLine),
                       @sa,
                       nil,
                       True,
                       CREATE_NEW_CONSOLE,
                       nil,
                       nil,
                       StartupInfo,
                       ProcessInfo) = True then
      begin
        // 入力待ちになるまで待ってから，
        WaitForInputIdle(ProcessInfo.hProcess, 1000);
        StreamBufferSize := BUFFER_SIZE;
        while StreamBufferSize = BUFFER_SIZE do
        begin
          // 入力を与える
          StreamBufferSize := StdIn.Read(bufStdIn, BUFFER_SIZE);
          WriteFile(hStdInWritePipeDup, bufStdIn, StreamBufferSize, nWritten, nil);
        end;
        // 入力を与え終わった
        CloseHandle(hStdInWritePipeDup);

        Result := TMemoryStream.Create;
        Result.Clear;
        try
          repeat
            if FLoopProcessMessages then
            begin
              Application.ProcessMessages;
              Sleep(50);
            end;

            // 標準出力パイプの内容を調べる
            PeekNamedPipe(hReadPipe, nil, 0, nil, @dwStdOut, nil);
            if (dwStdOut <> 0) then
            begin
              // 内容が存在すれば、読み取る
              ReadFile(hReadPipe, bufStdOut, Length(bufStdOut) - 1, dwStdOut, nil);
              Result.Write(bufStdOut, dwStdOut);
            end;

            // 同様にエラー出力の処理
            //GetExitCodeProcess(ProcessInfo.hProcess, dwRet);
            PeekNamedPipe(hErrReadPipe, nil, 0, nil, @dwErrOut, nil);
            if (dwErrOut <> 0) then
            begin
              ReadFile(hErrReadPipe, bufErrOut, Length(bufErrOut)-1,dwErrOut,nil);
              // このデータは使わない（バッファから吐くだけ）
              // このデータが必要であれば，StdOut の例にならってコードを追加せよ

              //エラー内容を標準出力に出力(2010/01/26 Mr.XRAY追加)
              //Result.Write(bufErrOut, dwStdOut);
            end;

            dwRet := WaitForSingleObject(ProcessInfo.hProcess, 0);
          // コンソールアプリのプロセスが存在している間
          until (dwRet = WAIT_OBJECT_0); 
        finally
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
          CloseHandle(hStdInReadPipe);
        end;
      end;
    finally
      TerminateProcess(ProcessInfo.hProcess, 0);  //追加(Mr.XRAY)
      CloseHandle(hErrReadPipe);
      CloseHandle(hErrWritePipe);
    end;
  finally
    CloseHandle(hReadPipe);
    CloseHandle(hWritePipe);
  end;
end;

//-----------------------------------------------------------------------------
constructor TCommandLineUnit.Create;
begin
  FLoopProcessMessages := False;
end;

//-----------------------------------------------------------------------------
//  Unicode(Delphi2009以降に対応
//  取得した文字コードを変更しないようにRawByteStringで定義した文字列に読みこ
//  んだ後，結果の文字列に代入する(暗黙の型変換の警告あり)
//-----------------------------------------------------------------------------
function TCommandLineUnit.GetStringFromStream(const Stream: TStream): String;

//  Delphi6～Delphi2007-R2以下
//  Delphi5以前にはCompilerVersionという指令がない

{$IF CompilerVersion <= 18.6}
begin
  SetLength(Result, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Result[1], Stream.Size);
end;

//  Delphi2009以降
{$ELSE}
var
  L : Integer;
  S : RawByteString;
begin
  Stream.Read(L, SizeOf(Integer));
  SetLength(S, L);
  Stream.Position := 0;
  Stream.Read(Pointer(S)^, L * SizeOf(Char));

  //→UTF-16なのでロスレス変換なのだけれども
  //単純に代入するとコンパイラがワーニングを吐くのでキャスト
  Result := UnicodeString(S);
end;
{$IFEND}

//-----------------------------------------------------------------------------
//  外部から利用する公開関数　
//  出力先だけをリダイレクトする場合
//-----------------------------------------------------------------------------
function TCommandLineUnit.GrabStdOutText(CommandLine: string): String;
var
  msin  : TMemoryStream;
  msout : TMemoryStream;
begin
  msin := TMemoryStream.Create;
  try
    msout := GrabStdOut(CommandLine, msin);

    if msout <> nil then
      Result := GetStringFromStream(msout);
  finally
    FreeAndNil(msout);
    FreeAndNil(msin);
  end;
end;

//-----------------------------------------------------------------------------
//  外部から利用する公開関数　
//  入力と出力をリダイレクトする場合
//-----------------------------------------------------------------------------
function TCommandLineUnit.GrabStdOutText(CommandLine: string;
  StdInput: TStrings): String;
var
  msin  : TMemoryStream;
  msout : TMemoryStream;
begin
  msin := TMemoryStream.Create;
  StdInput.SaveToStream(msin);
  try
    msout := GrabStdOut(CommandLine, msin);

    if msout <> nil then
      Result := GetStringFromStream(msout);
  finally
    FreeAndNil(msout);
    FreeAndNil(msin);
  end;
end;

end.
