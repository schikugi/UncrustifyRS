unit uFormatterOption;

interface

uses
Classes, SysUtils, StrUtils, Forms, Registry, IOUtils;

type
  TFormatterOption = class(TObject)
  public
    FExePath: string;     // 実行ファイルのパス
    FConfigPath: string;  // 設定ファイルのパス
    FCommandLine: string; // コマンドライン
    FUseUTF8:Boolean;     // テンポラリファイルはUTF-8で出力
    FFileExts: TStringList;

    constructor Create;
    destructor Destroy; override;

    procedure SaveOption();
    function CheckExt(Ext: string): Boolean;
    function BuildCommandLine() : string;
    function Encoding : TEncoding;
  end;

implementation

const
  RegRoot = '\Software\dragonhouse-software.jp\UncrustifyUI';
  RSSection = 'CodeFormatter';
  CppExts = '.c;.cpp;.cxx;.cc;.h;.hpp;.hxx;.hh';

constructor TFormatterOption.Create;
var
  Reg: TRegistryIniFile;
  ExeDir: string;
  ExePath: string;
  ConfigPath: string;
  CommandLine: string;
  Exts: string;
begin
  FFileExts := TStringList.Create;

  ExeDir := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(Application.ExeName));
  ExePath := ExeDir + 'Uncrustify.exe';
  ConfigPath := ExeDir + 'Uncrustify.cfg';
  CommandLine := '$(EXEPATH) -c $(CONFIGPATH) -q';

  Reg := TRegistryIniFile.Create(RegRoot);

  FExePath     := Reg.ReadString(RSSection, 'ExePath',     ExePath);
  FConfigPath  := Reg.ReadString(RSSection, 'ConfigPath',  ConfigPath);
  FCommandLine := Reg.ReadString(RSSection, 'CommandLine', CommandLine);
  FUseUTF8     := Reg.ReadBool  (RSSection, 'UseUTF8',     false);
  Exts         := Reg.ReadString(RSSection, 'CPPExts',     CppExts);

  FFileExts.Text := AnsiReplaceText(LowerCase(Exts), ';', #13);

  FreeAndNil(Reg);
end;

destructor TFormatterOption.Destroy;
begin
  FreeAndNil(FFileExts);
  inherited;
end;


procedure TFormatterOption.SaveOption();
var
  Reg: TRegistryIniFile;
begin
  Reg := TRegistryIniFile.Create(RegRoot);

  Reg.WriteString(RSSection, 'ExePath',     FExePath);
  Reg.WriteString(RSSection, 'ConfigPath',  FConfigPath);
  Reg.WriteString(RSSection, 'CommandLine', FCommandLine);
  Reg.WriteBool  (RSSection, 'UseUTF8',     FUseUTF8);

  FreeAndNil(Reg);
end;

function TFormatterOption.BuildCommandLine() : string;
var
  ExePath: string;
  ConfigPath: string;
begin
  ExePath := AnsiQuotedStr(FExePath, '"');
  ConfigPath := AnsiQuotedStr(FConfigPath, '"');
//  Infile := AnsiQuotedStr(Infile, '"');
//  OutFile := AnsiQuotedStr(OutFile, '"');

  Result := AnsiReplaceText(FCommandLine, '$(EXEPATH)', ExePath);
  Result := AnsiReplaceText(Result, '$(CONFIGPATH)', ConfigPath);
//  Result := AnsiReplaceText(Result, '$(INFILE)', Infile);
//  Result := AnsiReplaceText(Result, '$(OUTFILE)', OutFile);
end;

function TFormatterOption.CheckExt(Ext: string): Boolean;
begin
  Result := false;

  Ext := LowerCase(Ext);
  if FFileExts.IndexOf(Ext) <> -1 then
    Result := true;

end;


function TFormatterOption.Encoding : TEncoding;
begin
  if not FUseUTF8 then
    Result := TEncoding.Default
  else
    Result := TEncoding.UTF8;
end;

initialization

end.
